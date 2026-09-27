# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Extended Data Fig. 8:
# Cross-validation of modelled FOAK deployment timing against
# technology and market readiness indicators.
#
# NOTE ON RENUMBERING:
# This figure was Extended Data Fig. 7 in the previous manuscript version.
# Following insertion of the regional FOAK competition sensitivity as the
# new Extended Data Fig. 7, this figure is now Extended Data Fig. 8.
#
# Panel a:
# Technology readiness level (TRL) vs median FOAK deployment year.
#
# Panel b:
# Market readiness vs median FOAK deployment year.
#
# Manuscript-facing scenario terminology:
#   Baseline    -> Reference
#   Accelerated -> Proliferation
#
# IMPORTANT:
# Internal source-data columns retain the original names:
#   baseline_median
#   accelerated_median
#
# Required input:
# data/Extended_Data/edfig07_source_data_market_readiness.xlsx
#
# Source workbook sheets:
#   kang_2026_trl
#   NEA_market_readiness
#
# Outputs:
# outputs/Extended_Data/Figure_08/edfig08_market_readiness_v2.png
# outputs/Extended_Data/Figure_08/edfig08_market_readiness_v2.pdf


# ============================================================
# 1. PACKAGES
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
  library(grid)
})

rm(list = ls())
graphics.off()


# ============================================================
# 2. EXTENDED DATA FIGURE 08
# TECHNOLOGY AND MARKET READINESS
# ============================================================


# ============================================================
# 3. REPOSITORY PATHS
# ============================================================

FILE_IN <- file.path(
  "data",
  "Extended_Data",
  "edfig07_source_data_market_readiness.xlsx"
)

SHEET_A <- "kang_2026_trl"
SHEET_B <- "NEA_market_readiness"

OUT_DIR <- file.path(
  "outputs",
  "Extended_Data",
  "Figure_08"
)

dir.create(
  OUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(FILE_IN)) {
  stop(
    paste0(
      "Input file not found: ",
      FILE_IN,
      "\nRun this script from the repository root."
    )
  )
}


# ============================================================
# 4. EXPORT SETTINGS
# ============================================================

SAVE_W_MM <- 180
SAVE_H_MM <- 165
SAVE_DPI  <- 600
BASE_SIZE <- 7

FILE_STUB <- "edfig08_market_readiness_v2"


# ============================================================
# 5. PHASE COLOURS
# ============================================================

phase_cols <- c(
  "Conceptual design" = "#FDE0D2",
  "Basic design"      = "#F4A582",
  "Detailed design"   = "#D6604D",
  "Licensing"         = "#D1E5F0",
  "Construction"      = "#92C5DE"
)

phase_order <- c(
  "Conceptual design",
  "Basic design",
  "Detailed design",
  "Licensing",
  "Construction"
)


# ============================================================
# 6. SCENARIO COLOURS
# ============================================================

scenario_cols <- c(
  "Reference"     = "grey40",
  "Proliferation" = "#228B22"
)


# ============================================================
# 7. HELPER FUNCTIONS
# ============================================================

clean_names_local <- function(x) {
  
  x <- trimws(x)
  
  x <- gsub(
    "[[:space:][:punct:]]+",
    "_",
    x
  )
  
  x <- gsub(
    "_+",
    "_",
    x
  )
  
  x <- gsub(
    "^_|_$",
    "",
    x
  )
  
  tolower(x)
}


pick_col <- function(
    nms,
    patterns
) {
  
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


map_phase <- function(x) {
  
  dplyr::case_when(
    
    trimws(x) %in% c(
      "CD Phase",
      "Conceptual design",
      "Conceptual"
    ) ~ "Conceptual design",
    
    trimws(x) %in% c(
      "BD Phase",
      "Basic design",
      "Basic"
    ) ~ "Basic design",
    
    trimws(x) %in% c(
      "DD Phase",
      "Detailed design",
      "Detailed"
    ) ~ "Detailed design",
    
    trimws(x) %in% c(
      "Licensing Phase",
      "Licensing",
      "Licensed",
      "Under licensing"
    ) ~ "Licensing",
    
    trimws(x) %in% c(
      "Construction Phase",
      "Construction",
      "Under construction"
    ) ~ "Construction",
    
    TRUE ~ NA_character_
  )
}


# ============================================================
# 8. COMMON THEME
# ============================================================

theme_base <- theme_classic(
  base_size = BASE_SIZE
) +
  theme(
    
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
    
    axis.text = element_text(
      size = BASE_SIZE
    ),
    
    axis.title = element_text(
      size = BASE_SIZE
    ),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    legend.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    legend.box.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    plot.margin = margin(
      4,
      4,
      2,
      10
    )
  )


# ============================================================
# 9. PANEL a DATA
# TECHNOLOGY READINESS LEVEL
# ============================================================

raw_a <- read_excel(
  FILE_IN,
  sheet = SHEET_A
)

names(raw_a) <- clean_names_local(
  names(raw_a)
)


phase_col_a <- pick_col(
  names(raw_a),
  c(
    "^phase$",
    "^cohort$"
  )
)

trl_col_a <- pick_col(
  names(raw_a),
  c(
    "^trl$"
  )
)

bas_col_a <- pick_col(
  names(raw_a),
  c(
    "^baseline_median$",
    "^baseline_r$",
    "^bas_p50$",
    "baseline"
  )
)

acc_col_a <- pick_col(
  names(raw_a),
  c(
    "^accelerated_median$",
    "^accelerate.*$",
    "^acc_p50$",
    "accelerated"
  )
)


if (
  any(
    is.na(
      c(
        phase_col_a,
        trl_col_a,
        bas_col_a,
        acc_col_a
      )
    )
  )
) {
  
  stop(
    "Required columns were not found in sheet kang_2026_trl"
  )
}


df_a <- raw_a %>%
  
  transmute(
    
    Phase_raw = as.character(
      .data[[phase_col_a]]
    ),
    
    TRL = as.numeric(
      .data[[trl_col_a]]
    ),
    
    baseline_median = as.numeric(
      .data[[bas_col_a]]
    ),
    
    accelerated_median = as.numeric(
      .data[[acc_col_a]]
    )
  ) %>%
  
  mutate(
    Phase = map_phase(
      Phase_raw
    )
  ) %>%
  
  filter(
    !is.na(TRL),
    !is.na(baseline_median),
    !is.na(accelerated_median),
    !is.na(Phase),
    Phase %in% phase_order
  ) %>%
  
  mutate(
    Phase = factor(
      Phase,
      levels = phase_order
    )
  )


# ============================================================
# 10. PANEL a LONG FORMAT
# ============================================================

df_a_long <- df_a %>%
  
  select(
    Phase,
    TRL,
    baseline_median,
    accelerated_median
  ) %>%
  
  pivot_longer(
    cols = c(
      baseline_median,
      accelerated_median
    ),
    names_to = "Scenario",
    values_to = "FOAK_year"
  ) %>%
  
  mutate(
    
    Scenario = recode(
      Scenario,
      baseline_median    = "Reference",
      accelerated_median = "Proliferation"
    ),
    
    Scenario = factor(
      Scenario,
      levels = c(
        "Reference",
        "Proliferation"
      )
    ),
    
    Phase = factor(
      Phase,
      levels = phase_order
    )
  ) %>%
  
  filter(
    !is.na(FOAK_year)
  )


# ============================================================
# 11. PANEL b DATA
# MARKET READINESS
# ============================================================

raw_b <- read_excel(
  FILE_IN,
  sheet = SHEET_B
)

names(raw_b) <- clean_names_local(
  names(raw_b)
)


phase_col_b <- pick_col(
  names(raw_b),
  c(
    "^cohort$",
    "^phase$"
  )
)

bas_col_b <- pick_col(
  names(raw_b),
  c(
    "^baseline_median$",
    "^baseline_r$",
    "^bas_p50$",
    "baseline"
  )
)

acc_col_b <- pick_col(
  names(raw_b),
  c(
    "^accelerated_median$",
    "^accelerate.*$",
    "^acc_p50$",
    "accelerated"
  )
)

mr_col_b <- pick_col(
  names(raw_b),
  c(
    "^market_readiness$",
    "^market_readiness_.*$",
    "market_readiness",
    "market"
  )
)


if (
  any(
    is.na(
      c(
        phase_col_b,
        bas_col_b,
        acc_col_b,
        mr_col_b
      )
    )
  )
) {
  
  stop(
    "Required columns were not found in sheet NEA_market_readiness"
  )
}


df_b <- raw_b %>%
  
  transmute(
    
    Phase_raw = as.character(
      .data[[phase_col_b]]
    ),
    
    market_readiness = as.numeric(
      .data[[mr_col_b]]
    ),
    
    baseline_median = as.numeric(
      .data[[bas_col_b]]
    ),
    
    accelerated_median = as.numeric(
      .data[[acc_col_b]]
    )
  ) %>%
  
  mutate(
    Phase = map_phase(
      Phase_raw
    )
  ) %>%
  
  filter(
    !is.na(market_readiness),
    !is.na(baseline_median),
    !is.na(accelerated_median),
    !is.na(Phase),
    Phase %in% phase_order
  ) %>%
  
  mutate(
    Phase = factor(
      Phase,
      levels = phase_order
    )
  )


# ============================================================
# 12. PANEL b LONG FORMAT
# ============================================================

df_b_long <- df_b %>%
  
  select(
    Phase,
    market_readiness,
    baseline_median,
    accelerated_median
  ) %>%
  
  pivot_longer(
    cols = c(
      baseline_median,
      accelerated_median
    ),
    names_to = "Scenario",
    values_to = "FOAK_year"
  ) %>%
  
  mutate(
    
    Scenario = recode(
      Scenario,
      baseline_median    = "Reference",
      accelerated_median = "Proliferation"
    ),
    
    Scenario = factor(
      Scenario,
      levels = c(
        "Reference",
        "Proliferation"
      )
    ),
    
    Phase = factor(
      Phase,
      levels = phase_order
    )
  ) %>%
  
  filter(
    !is.na(FOAK_year)
  )


# ============================================================
# 13. JITTER SETTINGS
# ============================================================

jit_a_ref <- position_jitter(
  width = 0.12,
  height = 0.04,
  seed = 42
)

jit_a_pro <- position_jitter(
  width = 0.12,
  height = 0.04,
  seed = 42
)

jit_b_ref <- position_jitter(
  width = 0.12,
  height = 0.04,
  seed = 42
)

jit_b_pro <- position_jitter(
  width = 0.12,
  height = 0.04,
  seed = 42
)


# ============================================================
# 14. AXIS SETTINGS
# ============================================================

x_limits <- c(
  2024.8,
  2050.5
)

x_breaks <- seq(
  2025,
  2050,
  5
)

y_limits <- c(
  0.95,
  6.25
)

y_breaks <- 1:6


# ============================================================
# 15. PANEL a PLOT
# TECHNOLOGY READINESS LEVEL
# ============================================================

p_a_core <- ggplot() +
  
  geom_smooth(
    data = filter(
      df_a_long,
      Scenario == "Reference"
    ),
    aes(
      x = FOAK_year,
      y = TRL,
      colour = "Reference"
    ),
    method = "lm",
    se = FALSE,
    linewidth = 0.7
  ) +
  
  geom_smooth(
    data = filter(
      df_a_long,
      Scenario == "Proliferation"
    ),
    aes(
      x = FOAK_year,
      y = TRL,
      colour = "Proliferation"
    ),
    method = "lm",
    se = FALSE,
    linewidth = 0.7
  ) +
  
  geom_point(
    data = filter(
      df_a_long,
      Scenario == "Reference"
    ),
    aes(
      x = FOAK_year,
      y = TRL,
      fill = Phase
    ),
    shape = 21,
    size = 2.6,
    stroke = 0.25,
    colour = "grey30",
    alpha = 0.35,
    position = jit_a_ref,
    show.legend = FALSE
  ) +
  
  geom_point(
    data = filter(
      df_a_long,
      Scenario == "Proliferation"
    ),
    aes(
      x = FOAK_year,
      y = TRL,
      fill = Phase
    ),
    shape = 21,
    size = 2.6,
    stroke = 0.25,
    colour = "grey30",
    alpha = 0.95,
    position = jit_a_pro,
    show.legend = FALSE
  ) +
  
  scale_colour_manual(
    values = scenario_cols,
    breaks = c(
      "Reference",
      "Proliferation"
    )
  ) +
  
  scale_fill_manual(
    values = phase_cols,
    breaks = phase_order,
    drop = FALSE
  ) +
  
  scale_y_continuous(
    breaks = y_breaks,
    limits = y_limits,
    expand = c(
      0,
      0
    )
  ) +
  
  scale_x_continuous(
    limits = x_limits,
    breaks = x_breaks,
    expand = c(
      0,
      0
    )
  ) +
  
  labs(
    x = "Median FOAK deployment year",
    y = "Technology readiness level"
  ) +
  
  theme_base +
  
  theme(
    legend.position = "none"
  )


# ============================================================
# 16. PANEL a LABEL
# ============================================================

p_a <- ggdraw(
  p_a_core
) +
  
  draw_plot_label(
    label = "a",
    x = 0.001,
    y = 0.992,
    hjust = 0,
    vjust = 1,
    size = 11,
    fontface = "bold"
  )


# ============================================================
# 17. PANEL b PLOT
# MARKET READINESS
# ============================================================

p_b_core <- ggplot() +
  
  geom_smooth(
    data = filter(
      df_b_long,
      Scenario == "Reference"
    ),
    aes(
      x = FOAK_year,
      y = market_readiness,
      colour = "Reference"
    ),
    method = "lm",
    se = FALSE,
    linewidth = 0.7
  ) +
  
  geom_smooth(
    data = filter(
      df_b_long,
      Scenario == "Proliferation"
    ),
    aes(
      x = FOAK_year,
      y = market_readiness,
      colour = "Proliferation"
    ),
    method = "lm",
    se = FALSE,
    linewidth = 0.7
  ) +
  
  geom_point(
    data = filter(
      df_b_long,
      Scenario == "Reference"
    ),
    aes(
      x = FOAK_year,
      y = market_readiness,
      fill = Phase
    ),
    shape = 21,
    size = 2.6,
    stroke = 0.25,
    colour = "grey30",
    alpha = 0.35,
    position = jit_b_ref,
    show.legend = FALSE
  ) +
  
  geom_point(
    data = filter(
      df_b_long,
      Scenario == "Proliferation"
    ),
    aes(
      x = FOAK_year,
      y = market_readiness,
      fill = Phase
    ),
    shape = 21,
    size = 2.6,
    stroke = 0.25,
    colour = "grey30",
    alpha = 0.95,
    position = jit_b_pro,
    show.legend = FALSE
  ) +
  
  scale_colour_manual(
    values = scenario_cols,
    breaks = c(
      "Reference",
      "Proliferation"
    )
  ) +
  
  scale_fill_manual(
    values = phase_cols,
    breaks = phase_order,
    drop = FALSE
  ) +
  
  scale_y_continuous(
    breaks = y_breaks,
    limits = y_limits,
    expand = c(
      0,
      0
    )
  ) +
  
  scale_x_continuous(
    limits = x_limits,
    breaks = x_breaks,
    expand = c(
      0,
      0
    )
  ) +
  
  labs(
    x = "Median FOAK deployment year",
    y = "Market readiness"
  ) +
  
  theme_base +
  
  theme(
    legend.position = "none"
  )


# ============================================================
# 18. PANEL b LABEL
# ============================================================

p_b <- ggdraw(
  p_b_core
) +
  
  draw_plot_label(
    label = "b",
    x = 0.001,
    y = 0.992,
    hjust = 0,
    vjust = 1,
    size = 11,
    fontface = "bold"
  )


# ============================================================
# 19. SHARED PHASE LEGEND
# ============================================================

legend_phase_plot <- ggplot(
  df_a,
  aes(
    x = baseline_median,
    y = TRL,
    fill = Phase
  )
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
      title = "Phase",
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
    
    legend.title = element_text(
      size = BASE_SIZE
    ),
    
    legend.text = element_text(
      size = BASE_SIZE
    ),
    
    legend.key.width = unit(
      0.55,
      "cm"
    ),
    
    legend.key.height = unit(
      0.32,
      "cm"
    ),
    
    legend.spacing.x = unit(
      0.06,
      "cm"
    ),
    
    legend.spacing.y = unit(
      0.00,
      "cm"
    ),
    
    legend.box.spacing = unit(
      0.00,
      "cm"
    ),
    
    legend.margin = margin(
      0,
      0,
      0,
      0
    ),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    legend.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    plot.margin = margin(
      0,
      0,
      0,
      0
    )
  )


legend_phase <- get_legend(
  legend_phase_plot
)


# ============================================================
# 20. SHARED SCENARIO LEGEND
# ============================================================

legend_scenario_plot <- ggplot(
  
  data.frame(
    
    x = c(
      1,
      2
    ),
    
    y = c(
      1,
      2
    ),
    
    Scenario = factor(
      c(
        "Reference",
        "Proliferation"
      ),
      levels = c(
        "Reference",
        "Proliferation"
      )
    )
  ),
  
  aes(
    x = x,
    y = y,
    colour = Scenario
  )
) +
  
  geom_line(
    linewidth = 0.8
  ) +
  
  scale_colour_manual(
    values = scenario_cols,
    breaks = c(
      "Reference",
      "Proliferation"
    )
  ) +
  
  guides(
    colour = guide_legend(
      title = "Scenario",
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        linewidth = 0.9
      )
    )
  ) +
  
  theme_void() +
  
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.justification = "center",
    
    legend.title = element_text(
      size = BASE_SIZE
    ),
    
    legend.text = element_text(
      size = BASE_SIZE
    ),
    
    legend.key.width = unit(
      0.75,
      "cm"
    ),
    
    legend.key.height = unit(
      0.32,
      "cm"
    ),
    
    legend.spacing.x = unit(
      0.06,
      "cm"
    ),
    
    legend.spacing.y = unit(
      0.00,
      "cm"
    ),
    
    legend.box.spacing = unit(
      0.00,
      "cm"
    ),
    
    legend.margin = margin(
      0,
      0,
      0,
      0
    ),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    legend.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    plot.margin = margin(
      0,
      0,
      0,
      0
    )
  )


legend_scenario <- get_legend(
  legend_scenario_plot
)


# ============================================================
# 21. COMBINE LEGENDS
# ============================================================

legend_block <- plot_grid(
  legend_phase,
  legend_scenario,
  ncol = 1,
  rel_heights = c(
    1,
    1
  )
)


# ============================================================
# 22. COMBINE FIGURE
# ============================================================

fig_combined <- plot_grid(
  p_a,
  p_b,
  legend_block,
  ncol = 1,
  rel_heights = c(
    1,
    1,
    0.17
  ),
  align = "v",
  axis = "lr"
)


# ============================================================
# 23. DISPLAY
# ============================================================

print(
  fig_combined
)


# ============================================================
# 24. EXPORT PNG
# ============================================================

ggsave(
  filename = file.path(
    OUT_DIR,
    paste0(
      FILE_STUB,
      ".png"
    )
  ),
  plot = fig_combined,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)


# ============================================================
# 25. EXPORT PDF
# ============================================================

ggsave(
  filename = file.path(
    OUT_DIR,
    paste0(
      FILE_STUB,
      ".pdf"
    )
  ),
  plot = fig_combined,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  bg = "white"
)


# ============================================================
# 26. CONFIRM OUTPUTS
# ============================================================

cat(
  "\nExtended Data Figure 8 v2 successfully exported:\n",
  file.path(
    OUT_DIR,
    paste0(
      FILE_STUB,
      ".png"
    )
  ),
  "\n",
  file.path(
    OUT_DIR,
    paste0(
      FILE_STUB,
      ".pdf"
    )
  ),
  "\n"
)


fig_combined


# ============================================================
# END
# ============================================================