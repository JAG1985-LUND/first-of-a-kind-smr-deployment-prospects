# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Supplementary Fig. 7: regional and national FOAK SMR deployment
# trajectories.
#
# Required input:
# data/Supplementary_Data/sfig07_source_data_regional_foaks_2050.xlsx
#
# Outputs:
# outputs/Supplementary_Figures/Figure_07/sfig07_regional_foaks_2050.png
# outputs/Supplementary_Figures/Figure_07/sfig07_regional_foaks_2050.pdf
# outputs/Supplementary_Figures/Figure_07/sfig07_regional_foaks_2050.rds

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
})

rm(list = ls())
graphics.off()

# ============================================================
# SUPPLEMENTARY FIGURE 7
# REGIONAL FOAK SMR DEPLOYMENT
# Baseline panels: a-f
# Accelerated panels: g-l
# ============================================================

# ------------------------------------------------------------
# FIND REPOSITORY ROOT
# ------------------------------------------------------------

find_repo_root <- function(start_dir = getwd()) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  
  repeat {
    has_git     <- dir.exists(file.path(current_dir, ".git"))
    has_data    <- dir.exists(file.path(current_dir, "data"))
    has_scripts <- dir.exists(file.path(current_dir, "scripts"))
    
    if (has_git && has_data && has_scripts) {
      return(current_dir)
    }
    
    parent_dir <- dirname(current_dir)
    
    if (identical(parent_dir, current_dir)) {
      stop(
        "Repository root not found. Run this script from inside the ",
        "first-of-a-kind-smr-deployment-prospects repository."
      )
    }
    
    current_dir <- parent_dir
  }
}

ROOT_DIR <- find_repo_root()

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

FILE_CANDIDATES <- c(
  "sfig07_source_data_regional_foaks_2050.xlsx",
  "sfig07_regional_foaks_2050.xlsx"
)

FILE_IN_CANDIDATES <- file.path(
  ROOT_DIR,
  "data",
  "Supplementary_Data",
  FILE_CANDIDATES
)

FILE_IN <- FILE_IN_CANDIDATES[file.exists(FILE_IN_CANDIDATES)][1]

if (is.na(FILE_IN)) {
  stop(
    "Input file not found. Checked: ",
    paste(FILE_IN_CANDIDATES, collapse = " | ")
  )
}

FILE <- basename(FILE_IN)

OUT_DIR <- file.path(
  ROOT_DIR,
  "outputs",
  "Supplementary_Figures",
  "Figure_07"
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# SETTINGS
# ------------------------------------------------------------

region_order <- c(
  "North America",
  "Europe",
  "OECD-Asia",
  "Russia",
  "China",
  "RoW"
)

region_cols <- c(
  "North America" = "#228B22",
  "Europe"        = "#0072B2",
  "OECD-Asia"     = "#1B7575",
  "Russia"        = "#B22222",
  "China"         = "#C45A32",
  "RoW"           = "#7A3DB8"
)

X_MIN <- 2018
X_MAX <- 2053
X_BREAKS <- c(2020, 2030, 2040, 2050)

Y_MAX <- 18
Y_BREAKS <- c(0, 5, 10, 15)

SAVE_W_MM <- 195
SAVE_H_MM <- 210
SAVE_DPI  <- 600

FILE_STUB <- "sfig07_regional_foaks_2050"

hide_zero <- function(x) {
  ifelse(x == 0, "", x)
}

# ============================================================
# LOAD AND CLEAN DATA
# ============================================================

df_raw <- read_excel(FILE_IN)

if (ncol(df_raw) < 8) {
  stop(
    "Expected at least 8 columns: year, region, bas_p10, bas_p50, bas_p90, ",
    "acc_p10, acc_p50, acc_p90. Available columns are: ",
    paste(names(df_raw), collapse = ", ")
  )
}

df <- df_raw[, 1:8]

names(df) <- c(
  "year",
  "region",
  "bas_p10",
  "bas_p50",
  "bas_p90",
  "acc_p10",
  "acc_p50",
  "acc_p90"
)

df <- df %>%
  transmute(
    Year    = as.integer(year),
    Region  = trimws(as.character(region)),
    bas_p10 = as.numeric(bas_p10),
    bas_p50 = as.numeric(bas_p50),
    bas_p90 = as.numeric(bas_p90),
    acc_p10 = as.numeric(acc_p10),
    acc_p50 = as.numeric(acc_p50),
    acc_p90 = as.numeric(acc_p90)
  ) %>%
  mutate(
    Region = case_when(
      grepl("^north", Region, ignore.case = TRUE)  ~ "North America",
      grepl("^europe", Region, ignore.case = TRUE) ~ "Europe",
      grepl("^oecd", Region, ignore.case = TRUE)   ~ "OECD-Asia",
      grepl("^russia", Region, ignore.case = TRUE) ~ "Russia",
      grepl("^china", Region, ignore.case = TRUE)  ~ "China",
      grepl("^row$", Region, ignore.case = TRUE)   ~ "RoW",
      TRUE ~ Region
    ),
    Region = factor(Region, levels = region_order)
  )

# ============================================================
# LONG FORMAT
# ============================================================

df_long <- df %>%
  pivot_longer(
    cols = c(bas_p10, bas_p50, bas_p90, acc_p10, acc_p50, acc_p90),
    names_to = c("scenario_raw", "quantile"),
    names_pattern = "^(bas|acc)_p(10|50|90)$",
    values_to = "value"
  ) %>%
  mutate(
    Scenario = recode(
      scenario_raw,
      "bas" = "Baseline",
      "acc" = "Accelerated"
    ),
    Quantile = paste0("P", quantile),
    Scenario = factor(
      Scenario,
      levels = c("Baseline", "Accelerated")
    )
  ) %>%
  select(Year, Region, Scenario, Quantile, value) %>%
  pivot_wider(
    names_from = Quantile,
    values_from = value
  ) %>%
  group_by(Region, Scenario) %>%
  complete(Year = X_MIN:2050) %>%
  arrange(Year, .by_group = TRUE) %>%
  fill(P10, P50, P90, .direction = "down") %>%
  mutate(
    P10 = ifelse(is.na(P10), 0, P10),
    P50 = ifelse(is.na(P50), 0, P50),
    P90 = ifelse(is.na(P90), 0, P90)
  ) %>%
  ungroup() %>%
  filter(Year >= X_MIN, Year <= 2050)

# ============================================================
# STEP RIBBON HELPER
# ============================================================

stepify_ribbon <- function(d) {
  d <- d %>%
    arrange(Year)
  
  if (nrow(d) <= 1) {
    return(d)
  }
  
  bind_rows(
    d[1, ],
    bind_rows(lapply(2:nrow(d), function(i) {
      bind_rows(
        d[i - 1, ] %>% mutate(Year = d$Year[i]),
        d[i, ]
      )
    }))
  )
}

# ============================================================
# PANEL FUNCTION
# ============================================================

make_panel <- function(region_name, scenario_name, tag_txt,
                       show_y_title = FALSE,
                       show_x_title = FALSE) {
  
  d <- df_long %>%
    filter(
      Region == region_name,
      Scenario == scenario_name
    )
  
  if (nrow(d) == 0) {
    stop(
      "No data found for region = ", region_name,
      " and scenario = ", scenario_name
    )
  }
  
  d_ribbon <- stepify_ribbon(d)
  
  ggdraw(
    ggplot(d, aes(x = Year)) +
      geom_ribbon(
        data = d_ribbon,
        aes(ymin = P10, ymax = P90),
        fill = region_cols[[region_name]],
        alpha = 0.24,
        colour = NA
      ) +
      geom_step(
        aes(y = P50),
        colour = region_cols[[region_name]],
        linewidth = 0.95,
        direction = "hv"
      ) +
      scale_x_continuous(
        limits = c(X_MIN, X_MAX),
        breaks = X_BREAKS,
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        limits = c(0, Y_MAX),
        breaks = Y_BREAKS,
        labels = hide_zero,
        expand = c(0, 0)
      ) +
      labs(
        title = region_name,
        x = if (show_x_title) "Year" else NULL,
        y = if (show_y_title) "Cumulative\nFOAK SMRs" else NULL
      ) +
      theme_minimal(base_size = 8) +
      theme(
        panel.grid = element_blank(),
        axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
        axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
        axis.ticks = element_line(colour = "grey35", linewidth = 0.3),
        axis.text = element_text(colour = "grey25", size = 8),
        axis.title.x = element_text(size = 9, margin = margin(t = 4)),
        axis.title.y = element_text(size = 9, margin = margin(r = 4)),
        plot.title = element_text(
          size = 10,
          face = "plain",
          hjust = 0.5,
          margin = margin(b = 5)
        ),
        plot.margin = margin(4, 12, 4, 5),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA)
      )
  ) +
    draw_plot_label(
      tag_txt,
      x = 0.01,
      y = 0.99,
      hjust = 0,
      vjust = 1,
      size = 12,
      fontface = "bold"
    )
}

# ============================================================
# BUILD PANELS
# ============================================================

p_a <- make_panel("North America", "Baseline",    "a", TRUE,  FALSE)
p_b <- make_panel("Europe",        "Baseline",    "b", FALSE, FALSE)
p_c <- make_panel("OECD-Asia",     "Baseline",    "c", FALSE, FALSE)

p_d <- make_panel("Russia",        "Baseline",    "d", TRUE,  FALSE)
p_e <- make_panel("China",         "Baseline",    "e", FALSE, FALSE)
p_f <- make_panel("RoW",           "Baseline",    "f", FALSE, FALSE)

p_g <- make_panel("North America", "Accelerated", "g", TRUE,  FALSE)
p_h <- make_panel("Europe",        "Accelerated", "h", FALSE, FALSE)
p_i <- make_panel("OECD-Asia",     "Accelerated", "i", FALSE, FALSE)

p_j <- make_panel("Russia",        "Accelerated", "j", TRUE,  TRUE)
p_k <- make_panel("China",         "Accelerated", "k", FALSE, TRUE)
p_l <- make_panel("RoW",           "Accelerated", "l", FALSE, TRUE)

fig_s07 <- plot_grid(
  p_a, p_b, p_c,
  p_d, p_e, p_f,
  p_g, p_h, p_i,
  p_j, p_k, p_l,
  ncol = 3,
  align = "hv",
  axis = "tblr",
  rel_widths = c(1, 1, 1),
  rel_heights = c(1, 1, 1, 1)
)

print(fig_s07)

# ============================================================
# SAVE SUPPLEMENTARY FIGURE 7
# ============================================================

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".png")),
  plot = fig_s07,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".pdf")),
  plot = fig_s07,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  device = grDevices::cairo_pdf,
  bg = "white"
)

saveRDS(
  object = fig_s07,
  file = file.path(OUT_DIR, paste0(FILE_STUB, ".rds"))
)

fig_s07