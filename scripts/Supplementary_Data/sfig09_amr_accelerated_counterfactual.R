# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Supplementary Fig. 9: AMR-specific late-stage risk sensitivity
# for the active design cohort.
#
# Required input:
# data/Supplementary_Data/sfig09_source_data_amr_accelerated_counterfactual.xlsx
#
# Outputs:
# outputs/Supplementary_Figures/Figure_09/sfig09_amr_accelerated_counterfactual.png
# outputs/Supplementary_Figures/Figure_09/sfig09_amr_accelerated_counterfactual.pdf
# outputs/Supplementary_Figures/Figure_09/sfig09_amr_accelerated_counterfactual.rds

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(cowplot)
  library(grid)
})

rm(list = ls())
graphics.off()

# ============================================================
# SUPPLEMENTARY FIGURE 9
# AMR-SPECIFIC LATE-STAGE RISK SENSITIVITY
# a) Accelerated scenario, active AMR cohort
# b) AMR-specific late-stage risk sensitivity
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
  "sfig09_source_data_amr_accelerated_counterfactual.xlsx",
  "sfig09_amr_accelerated_counterfactual.xlsx"
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
  "Figure_09"
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_DPI  <- 600
SAVE_W_MM <- 180
SAVE_H_MM <- 72

FILE_STUB <- "sfig09_amr_accelerated_counterfactual"

# ------------------------------------------------------------
# STYLE
# ------------------------------------------------------------

BASE_SIZE <- 9

line_cols <- c(
  "OECD"     = "#1b9e77",
  "Non-OECD" = "#A67C00"
)

fill_cols <- c(
  "OECD"     = "#1b9e77",
  "Non-OECD" = "#A67C00"
)

hide_zero <- function(x) {
  ifelse(x == 0, "", as.character(x))
}

base_theme <- theme_minimal(base_size = BASE_SIZE) +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks = element_line(colour = "grey35", linewidth = 0.3),
    axis.text = element_text(size = BASE_SIZE),
    axis.title = element_text(size = BASE_SIZE),
    plot.title = element_text(size = BASE_SIZE, face = "plain", hjust = 0.5),
    plot.margin = margin(6, 8, 6, 8),
    legend.position = "bottom",
    legend.title = element_text(size = BASE_SIZE),
    legend.text = element_text(size = BASE_SIZE),
    legend.key.width = unit(12, "mm"),
    legend.key.height = unit(4, "mm"),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.box.background = element_rect(fill = "white", colour = NA)
  )

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------

clean_names_local <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

pick_col <- function(df, candidates, label) {
  hit <- intersect(candidates, names(df))
  
  if (length(hit) == 0) {
    stop(
      "Missing required column for ", label, ". Checked: ",
      paste(candidates, collapse = ", "),
      "\nAvailable columns are: ",
      paste(names(df), collapse = ", ")
    )
  }
  
  hit[1]
}

clean_counterfactual <- function(x) {
  if (is.logical(x)) {
    return(x)
  }
  
  x_clean <- str_to_lower(str_squish(as.character(x)))
  
  dplyr::case_when(
    x_clean %in% c("true", "t", "1", "yes", "y") ~ TRUE,
    x_clean %in% c("false", "f", "0", "no", "n") ~ FALSE,
    TRUE ~ NA
  )
}

clean_group <- function(x) {
  x_clean <- str_to_lower(str_squish(as.character(x)))
  
  dplyr::case_when(
    x_clean == "global" ~ "Global",
    x_clean == "oecd" ~ "OECD",
    x_clean %in% c("non_oecd", "non-oecd", "non oecd") ~ "Non-OECD",
    TRUE ~ as.character(x)
  )
}

# ============================================================
# LOAD AND STANDARDISE DATA
# ============================================================

raw <- read_excel(FILE_IN, sheet = 1)

names(raw) <- clean_names_local(names(raw))

year_col <- pick_col(raw, c("year"), "year")
group_col <- pick_col(raw, c("group"), "group")
counterfactual_col <- pick_col(raw, c("counterfactual"), "counterfactual")
p10_col <- pick_col(raw, c("p10"), "P10")
p50_col <- pick_col(raw, c("p50"), "P50")
p90_col <- pick_col(raw, c("p90"), "P90")

df <- raw %>%
  transmute(
    year = as.numeric(.data[[year_col]]),
    group = clean_group(.data[[group_col]]),
    counterfactual = clean_counterfactual(.data[[counterfactual_col]]),
    P10 = as.numeric(.data[[p10_col]]),
    P50 = as.numeric(.data[[p50_col]]),
    P90 = as.numeric(.data[[p90_col]])
  ) %>%
  filter(
    group %in% c("OECD", "Non-OECD"),
    !is.na(counterfactual),
    year <= 2045
  ) %>%
  arrange(
    counterfactual,
    group,
    year
  )

if (nrow(df) == 0) {
  stop("No plottable rows found after filtering. Check group and counterfactual columns.")
}

# ============================================================
# BUILD PANELS
# ============================================================

p_a_core <- ggplot(
  df %>% filter(counterfactual == FALSE),
  aes(x = year)
) +
  geom_ribbon(
    aes(ymin = P10, ymax = P90, fill = group),
    alpha = 0.18,
    colour = NA
  ) +
  geom_step(
    aes(y = P50, colour = group),
    linewidth = 0.85,
    direction = "hv"
  ) +
  scale_colour_manual(
    name = NULL,
    values = line_cols,
    breaks = c("OECD", "Non-OECD")
  ) +
  scale_fill_manual(
    name = NULL,
    values = fill_cols,
    breaks = c("OECD", "Non-OECD"),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(2025, 2045.6),
    breaks = c(2025, 2030, 2035, 2040, 2045),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 20),
    breaks = c(0, 5, 10, 15, 20),
    labels = hide_zero,
    expand = c(0, 0)
  ) +
  labs(
    x = "Year",
    y = "Cumulative FOAK SMRs"
  ) +
  base_theme

p_b_core <- ggplot(
  df %>% filter(counterfactual == TRUE),
  aes(x = year)
) +
  geom_ribbon(
    aes(ymin = P10, ymax = P90, fill = group),
    alpha = 0.18,
    colour = NA
  ) +
  geom_step(
    aes(y = P50, colour = group),
    linewidth = 0.85,
    direction = "hv"
  ) +
  scale_colour_manual(
    name = NULL,
    values = line_cols,
    breaks = c("OECD", "Non-OECD")
  ) +
  scale_fill_manual(
    name = NULL,
    values = fill_cols,
    breaks = c("OECD", "Non-OECD"),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(2025, 2045.6),
    breaks = c(2025, 2030, 2035, 2040, 2045),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 20),
    breaks = c(0, 5, 10, 15, 20),
    labels = hide_zero,
    expand = c(0, 0)
  ) +
  labs(
    x = "Year",
    y = "Cumulative FOAK SMRs"
  ) +
  base_theme

# ============================================================
# SHARED LEGEND
# ============================================================

shared_legend <- cowplot::get_legend(
  p_a_core +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0)
    )
)

p_a <- ggdraw(p_a_core + theme(legend.position = "none")) +
  draw_plot_label(
    "a",
    x = 0.01,
    y = 0.99,
    hjust = 0,
    vjust = 1,
    size = 12,
    fontface = "bold"
  )

p_b <- ggdraw(p_b_core + theme(legend.position = "none")) +
  draw_plot_label(
    "b",
    x = 0.01,
    y = 0.99,
    hjust = 0,
    vjust = 1,
    size = 12,
    fontface = "bold"
  )

# ============================================================
# COMBINE PANELS
# ============================================================

panels_only <- plot_grid(
  p_a,
  p_b,
  ncol = 2,
  align = "hv",
  axis = "tblr",
  rel_widths = c(1, 1)
)

fig_combined <- plot_grid(
  panels_only,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.10)
)

print(fig_combined)

# ============================================================
# SAVE SUPPLEMENTARY FIGURE 9
# ============================================================

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".png")),
  plot = fig_combined,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".pdf")),
  plot = fig_combined,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  device = grDevices::cairo_pdf,
  bg = "white"
)

saveRDS(
  object = fig_combined,
  file = file.path(OUT_DIR, paste0(FILE_STUB, ".rds"))
)

fig_combined