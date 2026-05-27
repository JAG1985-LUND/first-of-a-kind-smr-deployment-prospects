# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Supplementary Fig. 8: association between cumulative development
# duration and technology readiness level (TRL) across global, OECD and
# non-OECD SMR designs.
#
# Required input:
# data/Supplementary_Data/sfig08_development_duration_trl.xlsx
#
# Outputs:
# outputs/Supplementary_Data/sfig08_development_duration_trl.png
# outputs/Supplementary_Data/sfig08_development_duration_trl.pdf

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
# SUPPLEMENTARY FIGURE 8 — DEVELOPMENT DURATION vs TRL
# a) Global
# b) OECD
# c) Non-OECD
# Solid line in panel c = all Non-OECD designs
# Dashed line in panel c = Non-OECD designs excluding operational designs
# ============================================================

# ------------------------------------------------------------
# PATHS — GITHUB RUN
# ------------------------------------------------------------

DATA_DIR <- file.path(
  "data",
  "Supplementary_Data"
)

FILE_IN <- file.path(
  DATA_DIR,
  "sfig08_development_duration_trl.xlsx"
)

OUT_DIR <- file.path(
  "outputs",
  "Supplementary_Data"
)

if (!file.exists(FILE_IN)) {
  stop("Input file not found: ", FILE_IN)
}

dir.create(
  OUT_DIR,
  showWarnings = FALSE,
  recursive = TRUE
)

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 180
SAVE_H_MM <- 145
SAVE_DPI  <- 600

FILE_STUB <- "sfig08_development_duration_trl"

BASE_SIZE <- 7

# ------------------------------------------------------------
# PHASE SETTINGS
# ------------------------------------------------------------

phase_cols <- c(
  "Conceptual design" = "#FDE0D2",
  "Basic design"      = "#F4A582",
  "Detailed design"   = "#D6604D",
  "Licensing"         = "#D1E5F0",
  "Construction"      = "#92C5DE",
  "Operational"       = "#4393C3"
)

phase_order <- c(
  "Conceptual design",
  "Basic design",
  "Detailed design",
  "Licensing",
  "Construction",
  "Operational"
)

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------

clean_names_local <- function(x) {
  x <- trimws(x)
  x <- gsub("[[:space:][:punct:]]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

pick_col <- function(nms, patterns) {
  for (p in patterns) {
    hit <- grep(
      p,
      nms,
      ignore.case = TRUE,
      value = TRUE
    )
    
    if (length(hit) > 0) {
      return(hit[1])
    }
  }
  
  NA_character_
}

theme_base <- theme_classic(base_size = BASE_SIZE) +
  theme(
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks  = element_line(colour = "grey35", linewidth = 0.3),
    axis.text   = element_text(size = BASE_SIZE),
    axis.title  = element_text(size = BASE_SIZE),
    plot.background   = element_rect(fill = "white", colour = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.box.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(4, 4, 4, 4)
  )

# ============================================================
# LOAD AND STANDARDISE DATA
# ============================================================

raw <- read_excel(
  FILE_IN,
  sheet = 1
)

names(raw) <- clean_names_local(names(raw))

phase_col <- pick_col(
  names(raw),
  c(
    "^phase$",
    "current_phase",
    "development_phase"
  )
)

trl_col <- pick_col(
  names(raw),
  c(
    "^trl$",
    "technology_readiness_level",
    "technology_readiness_level_trl"
  )
)

dur_col <- pick_col(
  names(raw),
  c(
    "^development_duration$",
    "^duration$",
    "development_duration_years",
    "cumulative_development_duration"
  )
)

oecd_col <- pick_col(
  names(raw),
  c("oecd")
)

if (any(is.na(c(phase_col, trl_col, dur_col, oecd_col)))) {
  stop(
    "Required columns were not found in ",
    FILE_IN,
    "\nAvailable columns are: ",
    paste(names(raw), collapse = ", ")
  )
}

df <- raw %>%
  transmute(
    Phase_raw = as.character(.data[[phase_col]]),
    TRL = as.numeric(.data[[trl_col]]),
    development_duration = as.numeric(.data[[dur_col]]),
    OECD_raw = .data[[oecd_col]]
  ) %>%
  mutate(
    Phase = case_when(
      str_to_lower(str_squish(Phase_raw)) %in% c("conceptual design", "conceptual") ~ "Conceptual design",
      str_to_lower(str_squish(Phase_raw)) %in% c("basic design", "basic") ~ "Basic design",
      str_to_lower(str_squish(Phase_raw)) %in% c("detailed design", "detailed") ~ "Detailed design",
      str_to_lower(str_squish(Phase_raw)) %in% c("licensing", "licensed", "under licensing") ~ "Licensing",
      str_to_lower(str_squish(Phase_raw)) %in% c("construction", "under construction") ~ "Construction",
      str_to_lower(str_squish(Phase_raw)) %in% c("operational", "operation", "operating") ~ "Operational",
      TRUE ~ NA_character_
    ),
    OECD_group = case_when(
      tolower(trimws(as.character(OECD_raw))) %in% c("yes", "y", "true", "1", "oecd") ~ "OECD",
      tolower(trimws(as.character(OECD_raw))) %in% c("no", "n", "false", "0", "non-oecd", "non oecd", "non_oecd") ~ "Non-OECD",
      suppressWarnings(as.numeric(as.character(OECD_raw))) == 1 ~ "OECD",
      suppressWarnings(as.numeric(as.character(OECD_raw))) == 0 ~ "Non-OECD",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(TRL),
    !is.na(development_duration),
    !is.na(Phase),
    !is.na(OECD_group)
  ) %>%
  mutate(
    Phase = factor(
      Phase,
      levels = phase_order
    ),
    OECD_group = factor(
      OECD_group,
      levels = c("OECD", "Non-OECD")
    )
  )

df_global <- df
df_oecd   <- df %>% filter(OECD_group == "OECD")
df_non    <- df %>% filter(OECD_group == "Non-OECD")
df_non_trl16 <- df_non %>%
  filter(
    TRL <= 6,
    Phase != "Operational"
  )

x_max <- ceiling(
  max(df_global$development_duration, na.rm = TRUE) / 5
) * 5

x_max <- max(x_max, 40)

# ============================================================
# PLOT HELPERS
# ============================================================

build_base_scatter <- function(dat, show_y = TRUE) {
  ggplot(dat, aes(x = development_duration, y = TRL, fill = Phase)) +
    geom_point(
      shape = 21,
      size = 2.6,
      stroke = 0.25,
      colour = "grey30",
      alpha = 0.95,
      position = position_jitter(
        width = 0.15,
        height = 0.05,
        seed = 42
      )
    ) +
    scale_fill_manual(
      values = phase_cols,
      breaks = phase_order,
      drop = FALSE
    ) +
    scale_y_continuous(
      breaks = 1:9,
      limits = c(0.92, 9.25),
      expand = c(0, 0)
    ) +
    scale_x_continuous(
      limits = c(0, x_max),
      breaks = seq(0, x_max, by = 10),
      expand = c(0, 0)
    ) +
    labs(
      x = "Development duration (years)",
      y = if (show_y) "TRL" else NULL
    ) +
    theme_base +
    theme(
      legend.position = "none",
      plot.margin = margin(6, 6, 4, 10)
    )
}

add_panel_label <- function(plot_obj, panel_label) {
  ggdraw(plot_obj) +
    draw_plot_label(
      label = panel_label,
      x = 0.01,
      y = 0.99,
      hjust = 0,
      vjust = 1,
      size = 12,
      fontface = "plain"
    )
}

# ============================================================
# BUILD PANELS
# ============================================================

p_a_core <- build_base_scatter(
  df_global,
  show_y = TRUE
) +
  geom_smooth(
    data = df_global,
    aes(x = development_duration, y = TRL),
    inherit.aes = FALSE,
    method = "lm",
    se = FALSE,
    linewidth = 0.7,
    colour = "grey25"
  )

p_b_core <- build_base_scatter(
  df_oecd,
  show_y = TRUE
) +
  geom_smooth(
    data = df_oecd,
    aes(x = development_duration, y = TRL),
    inherit.aes = FALSE,
    method = "lm",
    se = FALSE,
    linewidth = 0.7,
    colour = "grey25"
  )

p_c_core <- build_base_scatter(
  df_non,
  show_y = TRUE
) +
  geom_smooth(
    data = df_non,
    aes(x = development_duration, y = TRL),
    inherit.aes = FALSE,
    method = "lm",
    se = FALSE,
    linewidth = 0.7,
    colour = "grey25"
  ) +
  geom_smooth(
    data = df_non_trl16,
    aes(x = development_duration, y = TRL),
    inherit.aes = FALSE,
    method = "lm",
    se = FALSE,
    linewidth = 0.7,
    linetype = "dashed",
    colour = "grey45"
  )

p_a <- add_panel_label(p_a_core, "a")
p_b <- add_panel_label(p_b_core, "b")
p_c <- add_panel_label(p_c_core, "c")

# ============================================================
# SHARED LEGEND
# ============================================================

legend_plot <- ggplot(
  df_global,
  aes(x = development_duration, y = TRL, fill = Phase)
) +
  geom_point(
    shape = 21,
    size = 2.8,
    stroke = 0.25,
    colour = "grey30"
  ) +
  scale_fill_manual(
    values = phase_cols,
    breaks = phase_order,
    drop = FALSE
  ) +
  guides(
    fill = guide_legend(
      title = NULL,
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        shape = 21,
        size = 3.0,
        alpha = 1,
        colour = "grey30"
      )
    )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.text  = element_text(size = BASE_SIZE),
    legend.key.width  = unit(0.55, "cm"),
    legend.key.height = unit(0.35, "cm"),
    legend.spacing.x  = unit(0.08, "cm"),
    legend.spacing.y  = unit(0.00, "cm"),
    legend.box.spacing = unit(0.00, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    plot.background   = element_rect(fill = "white", colour = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(0, 0, 0, 0)
  )

legend_shared <- get_legend(legend_plot)

# ============================================================
# COMBINE PANELS
# ============================================================

bottom_row <- plot_grid(
  p_b,
  p_c,
  nrow = 1,
  rel_widths = c(1, 1),
  align = "hv",
  axis = "tblr"
)

fig_combined <- plot_grid(
  p_a,
  bottom_row,
  legend_shared,
  ncol = 1,
  rel_heights = c(1.02, 0.96, 0.11)
)

print(fig_combined)

# ============================================================
# SAVE SUPPLEMENTARY FIGURE 8
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
  device = cairo_pdf,
  bg = "white"
)

fig_combined