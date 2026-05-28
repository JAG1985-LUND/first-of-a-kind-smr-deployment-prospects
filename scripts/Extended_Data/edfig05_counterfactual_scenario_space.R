# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Extended Data Fig. 5: global FOAK SMR deployment across
# reference and sensitivity scenarios.
#
# Panels a-c show cumulative FOAK SMR deployment under the Baseline scenario
# at the P10, P50 and P90 levels, respectively. Panels d-f show cumulative
# FOAK SMR deployment under the Accelerated scenario at the P10, P50 and P90
# levels, respectively. The reference trajectory is shown in black and compared
# with five scenario-based sensitivity analyses: current-phase completion,
# ADVANCE-OECD licensing, late-stage risk, construction cascade and stalled
# development.
#
# Required input:
# edfig05_source_data_counterfactual_scenarios.xlsx
#
# Outputs:
# edfig05_counterfactual_scenario_space.png
# edfig05_counterfactual_scenario_space.pdf

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(cowplot)
  library(grid)
})

rm(list = ls())
graphics.off()

# ============================================================
# EXTENDED DATA FIGURE 5
# GLOBAL FOAK SMR DEPLOYMENT ACROSS REFERENCE AND SENSITIVITY SCENARIOS
# ============================================================

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

FILE_IN <- "edfig05_source_data_counterfactual_scenarios.xlsx"
OUT_DIR <- "."

if (!file.exists(FILE_IN)) {
  stop("Input file not found: ", FILE_IN)
}

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 180
SAVE_H_MM <- 170
SAVE_DPI  <- 600

FILE_STUB <- "edfig05_counterfactual_scenario_space"

# ------------------------------------------------------------
# TYPOGRAPHY
# ------------------------------------------------------------

BASE_SIZE        <- 9
AXIS_TEXT_SIZE   <- 8
AXIS_TITLE_SIZE  <- 8
LEGEND_TEXT_SIZE <- 8
PANEL_TAG_SIZE   <- 11

# ============================================================
# LOAD AND STANDARDISE DATA
# ============================================================

df <- readxl::read_excel(FILE_IN)

names(df) <- trimws(names(df))
names(df) <- gsub("[[:space:]]+", "_", names(df))

if ("p90_accerlerated_scenario" %in% names(df)) {
  names(df)[names(df) == "p90_accerlerated_scenario"] <- "p90_accelerated_scenario"
}

df <- df %>%
  transmute(
    year = as.numeric(year),
    group = as.character(group),
    scenario = as.character(scenario),
    p10_baseline_scenario = as.numeric(p10_baseline_scenario),
    p50_baseline_scenario = as.numeric(p50_baseline_scenario),
    p90_baseline_scenario = as.numeric(p90_baseline_scenario),
    p10_accelerated_scenario = as.numeric(p10_accelerated_scenario),
    p50_accelerated_scenario = as.numeric(p50_accelerated_scenario),
    p90_accelerated_scenario = as.numeric(p90_accelerated_scenario)
  ) %>%
  filter(group == "global") %>%
  mutate(
    scenario = trimws(scenario),
    scenario = recode(
      scenario,
      "reference" = "Reference case",
      "Reference" = "Reference case",
      "Reference case" = "Reference case",
      "phase_completion" = "Current phase completion",
      "Phase completion" = "Current phase completion",
      "current_phase_completion" = "Current phase completion",
      "Current phase completion" = "Current phase completion",
      "oecd_advance" = "ADVANCE-OECD licensing",
      "ADVANCE OECD licensing" = "ADVANCE-OECD licensing",
      "Advance-OECD licensing" = "ADVANCE-OECD licensing",
      "ADVANCE-OECD licensing" = "ADVANCE-OECD licensing",
      "late_stage_risk" = "Late-stage risk",
      "Late stage risk" = "Late-stage risk",
      "Late-stage risk" = "Late-stage risk",
      "construction_cascade" = "Construction cascade",
      "Construction cascade" = "Construction cascade",
      "stalled_development" = "Stalled development",
      "Stalled Development" = "Stalled development",
      "Stalled development" = "Stalled development"
    )
  ) %>%
  arrange(scenario, year)

# ============================================================
# SCENARIO ORDER AND COLOURS
# ============================================================

scenario_levels <- c(
  "Reference case",
  "Current phase completion",
  "ADVANCE-OECD licensing",
  "Late-stage risk",
  "Construction cascade",
  "Stalled development"
)

df <- df %>%
  mutate(
    scenario_label = factor(
      scenario,
      levels = scenario_levels
    )
  )

colour_map <- c(
  "Reference case"           = "black",
  "Current phase completion" = "#2ca02c",
  "ADVANCE-OECD licensing"   = "#1b9e77",
  "Late-stage risk"          = "#7f0000",
  "Construction cascade"     = "#f46d43",
  "Stalled development"      = "#d62728"
)

# ============================================================
# THEME
# ============================================================

base_theme <- theme_classic(base_size = BASE_SIZE) +
  theme(
    plot.title = element_blank(),
    axis.title.x = element_text(
      size = AXIS_TITLE_SIZE,
      margin = margin(t = 8)
    ),
    axis.title.y = element_text(
      size = AXIS_TITLE_SIZE,
      margin = margin(r = 8)
    ),
    axis.text.x = element_text(
      size = AXIS_TEXT_SIZE,
      colour = "black"
    ),
    axis.text.y = element_text(
      size = AXIS_TEXT_SIZE,
      colour = "black"
    ),
    axis.line.x = element_line(
      colour = "grey35",
      linewidth = 0.3
    ),
    axis.line.y = element_line(
      colour = "grey35",
      linewidth = 0.3
    ),
    axis.ticks = element_line(
      colour = "grey35",
      linewidth = 0.3
    ),
    axis.ticks.length = unit(1.8, "mm"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = LEGEND_TEXT_SIZE),
    legend.key.width = unit(1.15, "cm"),
    legend.key.height = unit(0.35, "cm"),
    legend.spacing.x = unit(0.25, "cm"),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.box.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(6, 8, 4, 8)
  )

# ============================================================
# PANEL FUNCTION
# ============================================================

make_panel <- function(value_col) {
  ggplot(
    df %>% filter(scenario_label != "Reference case"),
    aes(
      x = year,
      y = .data[[value_col]],
      colour = scenario_label,
      group = scenario_label
    )
  ) +
    geom_step(
      linewidth = 0.70,
      direction = "hv",
      lineend = "round"
    ) +
    geom_step(
      data = df %>% filter(scenario_label == "Reference case"),
      aes(
        x = year,
        y = .data[[value_col]],
        colour = scenario_label,
        group = scenario_label
      ),
      linewidth = 1.00,
      direction = "hv",
      lineend = "round"
    ) +
    scale_colour_manual(
      values = colour_map,
      breaks = scenario_levels,
      drop = FALSE
    ) +
    scale_x_continuous(
      breaks = c(2020, 2030, 2040, 2050),
      limits = c(min(df$year, na.rm = TRUE), 2051),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = c(15, 30, 45, 60),
      limits = c(0, 62),
      expand = c(0, 0)
    ) +
    labs(
      x = "Year",
      y = NULL
    ) +
    base_theme
}

add_panel_tag <- function(plot_obj, tag) {
  ggdraw() +
    draw_plot(
      plot_obj,
      x = 0,
      y = 0,
      width = 1,
      height = 0.955
    ) +
    draw_plot_label(
      tag,
      x = 0.01,
      y = 0.995,
      hjust = 0,
      vjust = 1,
      size = PANEL_TAG_SIZE,
      fontface = "bold"
    )
}

# ============================================================
# BUILD PANELS
# ============================================================

p_a <- make_panel("p10_baseline_scenario")
p_b <- make_panel("p50_baseline_scenario")
p_c <- make_panel("p90_baseline_scenario")
p_d <- make_panel("p10_accelerated_scenario")
p_e <- make_panel("p50_accelerated_scenario")
p_f <- make_panel("p90_accelerated_scenario")

# ============================================================
# LEGEND
# ============================================================

legend <- cowplot::get_legend(
  p_a +
    theme(
      legend.position = "bottom",
      plot.margin = margin(0, 0, 0, 0)
    ) +
    guides(
      colour = guide_legend(
        nrow = 2,
        byrow = TRUE,
        override.aes = list(
          linewidth = c(1.00, rep(0.70, 5))
        )
      )
    )
)

# ============================================================
# CLEAN PANELS
# ============================================================

p_a_clean <- p_a + theme(legend.position = "none")
p_b_clean <- p_b + theme(legend.position = "none")
p_c_clean <- p_c + theme(legend.position = "none")
p_d_clean <- p_d + theme(legend.position = "none")
p_e_clean <- p_e + theme(legend.position = "none")
p_f_clean <- p_f + theme(legend.position = "none")

p_a_tag <- add_panel_tag(p_a_clean, "a")
p_b_tag <- add_panel_tag(p_b_clean, "b")
p_c_tag <- add_panel_tag(p_c_clean, "c")
p_d_tag <- add_panel_tag(p_d_clean, "d")
p_e_tag <- add_panel_tag(p_e_clean, "e")
p_f_tag <- add_panel_tag(p_f_clean, "f")

# ============================================================
# ASSEMBLE FIGURE
# ============================================================

top_row <- plot_grid(
  p_a_tag,
  p_b_tag,
  p_c_tag,
  nrow = 1,
  rel_widths = c(1, 1, 1),
  align = "hv",
  axis = "tblr"
)

bottom_row <- plot_grid(
  p_d_tag,
  p_e_tag,
  p_f_tag,
  nrow = 1,
  rel_widths = c(1, 1, 1),
  align = "hv",
  axis = "tblr"
)

panel_block <- plot_grid(
  top_row,
  bottom_row,
  ncol = 1,
  rel_heights = c(1, 1),
  align = "v"
)

panel_block_with_y <- ggdraw() +
  draw_plot(
    panel_block,
    x = 0.06,
    y = 0,
    width = 0.94,
    height = 1
  ) +
  draw_label(
    "Cumulative FOAK SMRs",
    x = 0.018,
    y = 0.5,
    angle = 90,
    size = 10
  )

final_plot <- plot_grid(
  panel_block_with_y,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.18)
)

print(final_plot)

# ============================================================
# SAVE EXTENDED DATA FIGURE 5
# ============================================================

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
  bg = "white",
  useDingbats = FALSE
)

final_plot
