# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Supplementary Fig. 6: scenario-bounded median feasibility
# space for FOAK SMR deployment.
#
# Required input:
# data/Supplementary_Data/sfig06_source_data_scenario_space.xlsx
#
# Outputs:
# outputs/Supplementary_Data/sfig06_scenario_space.png
# outputs/Supplementary_Data/sfig06_scenario_space.pdf

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
  library(stringr)
  library(grid)
})

rm(list = ls())
graphics.off()

# ============================================================
# SUPPLEMENTARY FIGURE 6
# SCENARIO-BOUNDED MEDIAN FEASIBILITY SPACE
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
  "sfig06_source_data_scenario_space.xlsx"
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
SAVE_H_MM <- 115
SAVE_DPI  <- 600

FILE_STUB <- "sfig06_scenario_space"

# ------------------------------------------------------------
# STYLE
# ------------------------------------------------------------

BASE_SIZE <- 7
axis_line_col <- "grey35"
axis_line_lw  <- 0.3

x_breaks <- c(2025, 2030, 2035, 2040, 2045, 2050)

case_levels <- c(
  "Reference",
  "Current-phase completion",
  "ADVANCE-OECD licensing",
  "Late-stage risk",
  "Construction cascade",
  "Stalled development"
)

case_cols <- c(
  "Reference"                = "black",
  "Current-phase completion" = "#1F78B4",
  "ADVANCE-OECD licensing"   = "#33A02C",
  "Late-stage risk"          = "#E31A1C",
  "Construction cascade"     = "#FF7F00",
  "Stalled development"      = "#6A3D9A"
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

standardise_scenario <- function(x) {
  x <- str_to_lower(str_squish(as.character(x)))
  case_when(
    str_detect(x, "baseline") ~ "Baseline",
    str_detect(x, "accelerat") ~ "Accelerated",
    TRUE ~ NA_character_
  )
}

standardise_case <- function(x) {
  x <- str_to_lower(str_squish(as.character(x)))
  case_when(
    str_detect(x, "reference|baseline scenario|accelerated scenario|main scenario") ~ "Reference",
    str_detect(x, "current[- ]?phase|phase completion") ~ "Current-phase completion",
    str_detect(x, "advance.*oecd|oecd licensing|faster licensing") ~ "ADVANCE-OECD licensing",
    str_detect(x, "late[- ]?stage") ~ "Late-stage risk",
    str_detect(x, "construction cascade|cascade") ~ "Construction cascade",
    str_detect(x, "stalled development|stalled") ~ "Stalled development",
    TRUE ~ NA_character_
  )
}

first_existing_col <- function(df, candidates) {
  out <- intersect(candidates, names(df))
  if (length(out) == 0) return(NA_character_)
  out[1]
}

read_sheet_standardised <- function(sheet_name) {
  raw <- read_excel(FILE_IN, sheet = sheet_name)
  names(raw) <- clean_names_local(names(raw))
  
  year_col <- first_existing_col(raw, c("year", "x", "time"))
  scenario_col <- first_existing_col(raw, c("scenario", "reference_scenario"))
  case_col <- first_existing_col(raw, c("case", "counterfactual", "sensitivity", "scenario_case"))
  p10_col <- first_existing_col(raw, c("p10", "q10", "lower", "lower_p10", "p_10"))
  p50_col <- first_existing_col(raw, c("p50", "median", "q50", "mid", "p_50"))
  p90_col <- first_existing_col(raw, c("p90", "q90", "upper", "upper_p90", "p_90"))
  
  if (is.na(year_col) || is.na(p50_col)) {
    stop(
      "Could not identify required columns in sheet '", sheet_name, "'. ",
      "Need at least year and p50/median columns. Available columns are: ",
      paste(names(raw), collapse = ", ")
    )
  }
  
  out <- raw %>%
    transmute(
      year = suppressWarnings(as.numeric(.data[[year_col]])),
      scenario_raw = if (!is.na(scenario_col)) as.character(.data[[scenario_col]]) else sheet_name,
      case_raw = if (!is.na(case_col)) as.character(.data[[case_col]]) else "Reference",
      p10 = if (!is.na(p10_col)) suppressWarnings(as.numeric(.data[[p10_col]])) else NA_real_,
      p50 = suppressWarnings(as.numeric(.data[[p50_col]])),
      p90 = if (!is.na(p90_col)) suppressWarnings(as.numeric(.data[[p90_col]])) else NA_real_
    ) %>%
    mutate(
      Scenario = standardise_scenario(scenario_raw),
      Case = standardise_case(case_raw)
    ) %>%
    filter(!is.na(year), !is.na(p50))
  
  out
}

# ------------------------------------------------------------
# LOAD AND STANDARDISE DATA
# ------------------------------------------------------------

sheet_names <- excel_sheets(FILE_IN)

dat <- bind_rows(lapply(sheet_names, read_sheet_standardised))

if (!("Scenario" %in% names(dat)) || all(is.na(dat$Scenario))) {
  stop("Could not identify Baseline / Accelerated scenario information in workbook: ", FILE_IN)
}

if (!("Case" %in% names(dat)) || all(is.na(dat$Case))) {
  stop("Could not identify counterfactual / case information in workbook: ", FILE_IN)
}

dat <- dat %>%
  filter(
    Scenario %in% c("Baseline", "Accelerated"),
    Case %in% case_levels
  ) %>%
  distinct(Scenario, Case, year, .keep_all = TRUE) %>%
  mutate(
    Case = factor(Case, levels = case_levels),
    Scenario = factor(Scenario, levels = c("Baseline", "Accelerated"))
  )

# ------------------------------------------------------------
# PANEL FUNCTION
# ------------------------------------------------------------

build_panel <- function(scenario_name, panel_tag) {
  
  df_s <- dat %>%
    filter(Scenario == scenario_name)
  
  ref_df <- df_s %>%
    filter(Case == "Reference") %>%
    arrange(year)
  
  cf_df <- df_s %>%
    filter(Case != "Reference") %>%
    arrange(Case, year)
  
  y_max <- max(
    c(df_s$p90, df_s$p50),
    na.rm = TRUE
  )
  
  y_upper <- ceiling(y_max / 5) * 5
  
  p <- ggplot() +
    geom_ribbon(
      data = ref_df,
      aes(x = year, ymin = p10, ymax = p90),
      fill = "grey80",
      alpha = 1
    ) +
    geom_line(
      data = cf_df,
      aes(x = year, y = p50, colour = Case),
      linewidth = 0.7
    ) +
    geom_line(
      data = ref_df,
      aes(x = year, y = p50),
      colour = "black",
      linewidth = 0.8
    ) +
    scale_colour_manual(
      values = case_cols,
      breaks = case_levels[-1],
      name = NULL
    ) +
    scale_x_continuous(
      limits = c(2025, 2050),
      breaks = x_breaks,
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      limits = c(0, y_upper),
      breaks = seq(0, y_upper, by = 5),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      x = "Year",
      y = "Cumulative FOAK SMRs",
      title = scenario_name
    ) +
    theme_classic(base_size = BASE_SIZE) +
    theme(
      axis.line.x = element_line(colour = axis_line_col, linewidth = axis_line_lw),
      axis.line.y = element_line(colour = axis_line_col, linewidth = axis_line_lw),
      axis.ticks = element_line(colour = axis_line_col, linewidth = axis_line_lw),
      axis.ticks.length = unit(0.12, "cm"),
      axis.title = element_text(size = BASE_SIZE),
      axis.text = element_text(size = BASE_SIZE),
      plot.title = element_text(size = BASE_SIZE, face = "bold", hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = BASE_SIZE),
      legend.key.width = unit(0.7, "cm"),
      legend.key.height = unit(0.35, "cm"),
      plot.margin = margin(4, 4, 4, 4),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
  
  ggdraw(p) +
    draw_plot_label(
      label = panel_tag,
      x = 0.01,
      y = 0.99,
      hjust = 0,
      vjust = 1,
      size = 12,
      fontface = "plain"
    )
}

# ------------------------------------------------------------
# BUILD PANELS
# ------------------------------------------------------------

p_a <- build_panel("Baseline", "a")
p_b <- build_panel("Accelerated", "b")

legend_plot <- ggplot(
  dat %>% filter(Case != "Reference"),
  aes(x = year, y = p50, colour = Case)
) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(
    values = case_cols,
    breaks = case_levels[-1],
    name = NULL
  ) +
  theme_void(base_size = BASE_SIZE) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = BASE_SIZE),
    legend.key.width = unit(0.7, "cm"),
    legend.key.height = unit(0.35, "cm")
  )

legend_grob <- cowplot::get_legend(legend_plot)

top_panels <- plot_grid(
  p_a,
  p_b,
  ncol = 2,
  align = "hv",
  axis = "tblr"
)

final_plot <- plot_grid(
  top_panels,
  legend_grob,
  ncol = 1,
  rel_heights = c(1, 0.12)
)

print(final_plot)

# ------------------------------------------------------------
# SAVE SUPPLEMENTARY FIGURE 6
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