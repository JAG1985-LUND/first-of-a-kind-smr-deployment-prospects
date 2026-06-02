# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Extended Data Fig. 5: scenario-bounded median feasibility
# space for FOAK SMR deployment.
#
# Required input:
# data/Extended_Data/edfig05_source_data_scenario_space.xlsx
#
# Outputs:
# edfig05_scenario_space.png
# edfig05_scenario_space.pdf

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(cowplot)
})

rm(list = ls())
graphics.off()

# ============================================================
# EXTENDED DATA FIGURE 5
# SCENARIO-BOUNDED MEDIAN FEASIBILITY SPACE
# ============================================================

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

FILE <- file.path(
  "data",
  "Extended_Data",
  "edfig05_source_data_scenario_space.xlsx"
)

OUT_DIR <- "."

if (!file.exists(FILE)) {
  stop("Input file not found: ", FILE)
}

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 180
SAVE_H_MM <- 100
SAVE_DPI  <- 600

FILE_STUB <- "edfig05_scenario_space"

# ------------------------------------------------------------
# FIGURE SETTINGS
# ------------------------------------------------------------

BASE_SIZE         <- 7
AXIS_TEXT_SIZE    <- 6.5
AXIS_TITLE_SIZE   <- 7
LEGEND_TITLE_SIZE <- 7
LEGEND_TEXT_SIZE  <- 6.5

LEGEND_TITLE <- "Sensitivity-based feasibility space"

LEGEND_X_LEFT <- 2019.0
LEGEND_X_TEXT <- 2020.55

LEGEND_TITLE_Y <- 62.70
LEGEND_ROW1_Y  <- 58.16
LEGEND_ROW2_Y  <- 55.44
LEGEND_ROW3_Y  <- 52.72
LEGEND_ROW4_Y  <- 50.00
LEGEND_ROW5_Y  <- 47.28
LEGEND_ROW6_Y  <- 44.56
LEGEND_ROW7_Y  <- 41.84

LEGEND_KEY_WIDTH      <- 1.20
LEGEND_KEY_HALFHEIGHT <- 0.80

colour_map <- c(
  "Current phase completion" = "#2ca02c",
  "ADVANCE-OECD licensing"   = "#1b9e77",
  "Construction cascade"     = "#f46d43",
  "Late-stage risk"          = "#7f0000",
  "Stalled development"      = "#d62728"
)

counterfactual_scenarios <- names(colour_map)

scenario_levels <- c(
  "reference",
  counterfactual_scenarios
)

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------

clean_names <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

clean_scenario <- function(x) {
  x <- trimws(as.character(x))
  
  dplyr::case_when(
    x %in% c("reference", "Reference") ~ "reference",
    x %in% c("phase_completion", "Current phase completion") ~ "Current phase completion",
    x %in% c("oecd_advance", "ADVANCE-OECD licensing") ~ "ADVANCE-OECD licensing",
    x %in% c("construction_cascade", "Construction cascade") ~ "Construction cascade",
    x %in% c("late_stage_risk", "Late stage risk", "Late stage risk ", "Late-stage risk") ~ "Late-stage risk",
    x %in% c("stalled_development", "Stalled development") ~ "Stalled development",
    TRUE ~ x
  )
}

find_col <- function(data_names, candidates) {
  hit <- candidates[candidates %in% data_names]
  
  if (length(hit) > 0) {
    return(hit[1])
  }
  
  stop(
    paste0(
      "Could not find required column. Tried: ",
      paste(candidates, collapse = ", "),
      "\nAvailable columns are: ",
      paste(data_names, collapse = ", ")
    ),
    call. = FALSE
  )
}

make_step_band <- function(x, x_end = 2050) {
  x <- x %>%
    arrange(year) %>%
    mutate(
      xnext = pmin(lead(year, default = x_end), x_end)
    ) %>%
    filter(xnext > year)
  
  data.frame(
    year = as.vector(rbind(x$year, x$xnext)),
    ymin = as.vector(rbind(x$ymin, x$ymin)),
    ymax = as.vector(rbind(x$ymax, x$ymax))
  )
}

base_theme <- function() {
  theme_classic(base_size = BASE_SIZE) +
    theme(
      axis.text.x = element_text(size = AXIS_TEXT_SIZE, colour = "grey20"),
      axis.text.y = element_text(size = AXIS_TEXT_SIZE, colour = "grey20"),
      axis.title.x = element_text(size = AXIS_TITLE_SIZE, colour = "black", margin = margin(t = 6)),
      axis.title.y = element_text(size = AXIS_TITLE_SIZE, colour = "black", margin = margin(r = 6)),
      axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
      axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
      axis.ticks = element_line(colour = "grey35", linewidth = 0.3),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_blank(),
      plot.margin = margin(4, 8, 4, 8)
    )
}

tag_panel <- function(p, tag) {
  ggdraw() +
    draw_plot(p) +
    draw_plot_label(
      tag,
      x = 0.01,
      y = 0.99,
      hjust = 0,
      vjust = 1,
      size = 10,
      fontface = "bold"
    )
}

add_internal_legend <- function(p, median_label) {
  p +
    annotate(
      "text",
      x = LEGEND_X_LEFT,
      y = LEGEND_TITLE_Y,
      label = LEGEND_TITLE,
      hjust = 0,
      vjust = 1,
      size = LEGEND_TITLE_SIZE / .pt
    ) +
    annotate(
      "segment",
      x = LEGEND_X_LEFT,
      xend = LEGEND_X_LEFT + LEGEND_KEY_WIDTH,
      y = LEGEND_ROW1_Y,
      yend = LEGEND_ROW1_Y,
      colour = "black",
      linewidth = 1.3
    ) +
    annotate(
      "text",
      x = LEGEND_X_TEXT,
      y = LEGEND_ROW1_Y,
      label = median_label,
      hjust = 0,
      vjust = 0.5,
      size = LEGEND_TEXT_SIZE / .pt
    ) +
    annotate(
      "rect",
      xmin = LEGEND_X_LEFT,
      xmax = LEGEND_X_LEFT + LEGEND_KEY_WIDTH,
      ymin = LEGEND_ROW2_Y - LEGEND_KEY_HALFHEIGHT,
      ymax = LEGEND_ROW2_Y + LEGEND_KEY_HALFHEIGHT,
      fill = "grey60",
      alpha = 0.5,
      colour = NA
    ) +
    annotate(
      "text",
      x = LEGEND_X_TEXT,
      y = LEGEND_ROW2_Y,
      label = "Reference uncertainty (P10–P90)",
      hjust = 0,
      vjust = 0.5,
      size = LEGEND_TEXT_SIZE / .pt
    ) +
    annotate(
      "segment",
      x = LEGEND_X_LEFT,
      xend = LEGEND_X_LEFT + LEGEND_KEY_WIDTH,
      y = LEGEND_ROW3_Y,
      yend = LEGEND_ROW3_Y,
      colour = colour_map[["Current phase completion"]],
      linewidth = 0.55
    ) +
    annotate(
      "text",
      x = LEGEND_X_TEXT,
      y = LEGEND_ROW3_Y,
      label = "Current phase completion",
      hjust = 0,
      vjust = 0.5,
      size = LEGEND_TEXT_SIZE / .pt
    ) +
    annotate(
      "segment",
      x = LEGEND_X_LEFT,
      xend = LEGEND_X_LEFT + LEGEND_KEY_WIDTH,
      y = LEGEND_ROW4_Y,
      yend = LEGEND_ROW4_Y,
      colour = colour_map[["ADVANCE-OECD licensing"]],
      linewidth = 0.55
    ) +
    annotate(
      "text",
      x = LEGEND_X_TEXT,
      y = LEGEND_ROW4_Y,
      label = "ADVANCE-OECD licensing",
      hjust = 0,
      vjust = 0.5,
      size = LEGEND_TEXT_SIZE / .pt
    ) +
    annotate(
      "segment",
      x = LEGEND_X_LEFT,
      xend = LEGEND_X_LEFT + LEGEND_KEY_WIDTH,
      y = LEGEND_ROW5_Y,
      yend = LEGEND_ROW5_Y,
      colour = colour_map[["Construction cascade"]],
      linewidth = 0.55
    ) +
    annotate(
      "text",
      x = LEGEND_X_TEXT,
      y = LEGEND_ROW5_Y,
      label = "Construction cascade",
      hjust = 0,
      vjust = 0.5,
      size = LEGEND_TEXT_SIZE / .pt
    ) +
    annotate(
      "segment",
      x = LEGEND_X_LEFT,
      xend = LEGEND_X_LEFT + LEGEND_KEY_WIDTH,
      y = LEGEND_ROW6_Y,
      yend = LEGEND_ROW6_Y,
      colour = colour_map[["Late-stage risk"]],
      linewidth = 0.55
    ) +
    annotate(
      "text",
      x = LEGEND_X_TEXT,
      y = LEGEND_ROW6_Y,
      label = "Late-stage risk",
      hjust = 0,
      vjust = 0.5,
      size = LEGEND_TEXT_SIZE / .pt
    ) +
    annotate(
      "segment",
      x = LEGEND_X_LEFT,
      xend = LEGEND_X_LEFT + LEGEND_KEY_WIDTH,
      y = LEGEND_ROW7_Y,
      yend = LEGEND_ROW7_Y,
      colour = colour_map[["Stalled development"]],
      linewidth = 0.55
    ) +
    annotate(
      "text",
      x = LEGEND_X_TEXT,
      y = LEGEND_ROW7_Y,
      label = "Stalled development",
      hjust = 0,
      vjust = 0.5,
      size = LEGEND_TEXT_SIZE / .pt
    )
}

read_source_data <- function(file) {
  raw <- read_excel(file, sheet = 1) %>%
    rename_with(clean_names) %>%
    mutate(
      year     = as.numeric(year),
      group    = as.character(group),
      scenario = clean_scenario(scenario)
    )
  
  nms <- names(raw)
  
  p10_base_col <- find_col(nms, c("p10_baseline_scenario", "p10_basel", "p10_base", "conservative_baseline_scenario"))
  p50_base_col <- find_col(nms, c("p50_baseline_scenario", "p50_basel", "p50_base", "median_baseline_scenario"))
  p90_base_col <- find_col(nms, c("p90_baseline_scenario", "p90_basel", "p90_base", "optimistic_baseline_scenario"))
  
  p10_accel_col <- find_col(nms, c("p10_accelerated_scenario", "p10_accel", "p10_accelerated", "conservative_accelerated_scenario"))
  p50_accel_col <- find_col(nms, c("p50_accelerated_scenario", "p50_accel", "p50_accelerated", "median_accelerated_scenario"))
  p90_accel_col <- find_col(nms, c("p90_accelerated_scenario", "p90_accerlerated_scenario", "p90_accel", "p90_accelerated", "optimistic_accelerated_scenario", "optimistic_accerlerated_scenario"))
  
  df_base <- raw %>%
    transmute(
      year,
      group,
      scenario,
      total_p10 = as.numeric(.data[[p10_base_col]]),
      total_p50 = as.numeric(.data[[p50_base_col]]),
      total_p90 = as.numeric(.data[[p90_base_col]])
    )
  
  df_accel <- raw %>%
    transmute(
      year,
      group,
      scenario,
      total_p10 = as.numeric(.data[[p10_accel_col]]),
      total_p50 = as.numeric(.data[[p50_accel_col]]),
      total_p90 = as.numeric(.data[[p90_accel_col]])
    )
  
  list(base = df_base, accel = df_accel)
}

prepare_panel_data <- function(df) {
  df <- df %>%
    filter(group == "global") %>%
    mutate(
      scenario = factor(clean_scenario(scenario), levels = scenario_levels)
    )
  
  ref_band <- df %>%
    filter(scenario == "reference") %>%
    transmute(
      year,
      ymin = total_p10,
      y    = total_p50,
      ymax = total_p90
    ) %>%
    arrange(year)
  
  ref_line <- ref_band %>%
    transmute(
      year,
      y = y
    )
  
  counterfactual_lines <- df %>%
    filter(scenario %in% counterfactual_scenarios) %>%
    transmute(
      year,
      scenario,
      value_plot = total_p50
    ) %>%
    arrange(scenario, year)
  
  list(
    df = df,
    ref_line = ref_line,
    counterfactual_lines = counterfactual_lines,
    ref_band = make_step_band(ref_band)
  )
}

make_panel <- function(panel_data, median_label) {
  p <- ggplot() +
    geom_ribbon(
      data = panel_data$ref_band,
      aes(x = year, ymin = ymin, ymax = ymax),
      fill = "grey60",
      alpha = 0.5,
      colour = NA
    ) +
    geom_step(
      data = panel_data$counterfactual_lines,
      aes(x = year, y = value_plot, colour = scenario, group = scenario),
      linewidth = 0.45,
      alpha = 0.95,
      direction = "hv",
      lineend = "round"
    ) +
    geom_step(
      data = panel_data$ref_line,
      aes(x = year, y = y),
      colour = "black",
      linewidth = 1.3,
      direction = "hv",
      lineend = "round"
    ) +
    scale_colour_manual(
      values = colour_map,
      breaks = counterfactual_scenarios,
      guide = "none"
    ) +
    scale_x_continuous(
      breaks = c(2020, 2030, 2040, 2050),
      limits = c(min(panel_data$df$year, na.rm = TRUE), 2050),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = seq(10, 60, by = 10),
      limits = c(0, 66),
      expand = c(0, 0)
    ) +
    labs(
      x = "Year",
      y = "Cumulative FOAK SMRs"
    ) +
    base_theme()
  
  add_internal_legend(p, median_label = median_label)
}

# ------------------------------------------------------------
# LOAD AND PREPARE DATA
# ------------------------------------------------------------

source_data <- read_source_data(FILE)

df_base  <- source_data$base
df_accel <- source_data$accel

stopifnot(all(c("year", "group", "scenario", "total_p10", "total_p50", "total_p90") %in% names(df_base)))
stopifnot(all(c("year", "group", "scenario", "total_p10", "total_p50", "total_p90") %in% names(df_accel)))

panel_base  <- prepare_panel_data(df_base)
panel_accel <- prepare_panel_data(df_accel)

# ------------------------------------------------------------
# BUILD PANELS
# ------------------------------------------------------------

p_a <- make_panel(
  panel_base,
  median_label = "Median baseline trajectory"
)

p_b <- make_panel(
  panel_accel,
  median_label = "Median accelerated trajectory"
)

# ------------------------------------------------------------
# COMBINE PANELS
# ------------------------------------------------------------

p_a_tag <- tag_panel(p_a, "a")
p_b_tag <- tag_panel(p_b, "b")

final_plot <- plot_grid(
  p_a_tag,
  p_b_tag,
  nrow = 1,
  align = "hv",
  axis = "tblr",
  rel_widths = c(1, 1)
)

print(final_plot)

# ------------------------------------------------------------
# SAVE EXTENDED DATA FIGURE 5
# ------------------------------------------------------------

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".png")),
  plot = final_plot,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".pdf")),
  plot = final_plot,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  device = cairo_pdf,
  bg = "white"
)

final_plot
