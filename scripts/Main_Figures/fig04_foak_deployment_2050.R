# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Figure 4: vendor-announced and modelled FOAK SMR deployment trajectories.
#
# Panel a shows cumulative FOAK SMR deployments for the subset of designs with
# vendor-stated target dates, compared with modelled deployment trajectories.
# The black line shows historical FOAK operation, the blue line shows
# vendor-announced targets, and grey and green lines show Baseline and
# Accelerated median projections with P10-P90 ranges. The vertical dashed line
# marks 2036, and horizontal dashed lines indicate the corresponding
# vendor-announced, Baseline and Accelerated deployment levels.
#
# Panel b shows historical FOAK SMR deployment and projected cumulative FOAK
# deployment for the full modelled pipeline, including currently active designs
# and simulated post-2025 design additions. The black line shows historical FOAK
# operation, the blue line shows vendor-announced targets, and grey and green
# lines show Baseline and Accelerated median projections with scenario-space
# ranges. Shaded bands indicate the corresponding uncertainty ranges.
#
# Required input:
# fig04_source_foak_deployment_2050.xlsx
#   - sheet: global_outlook
#   - sheet: baseline_counterfactuals
#   - sheet: accelerated_counterfactuals
#
# Outputs:
# fig04_vendor_and_global_foak_outlook.png
# fig04_vendor_and_global_foak_outlook.pdf
# fig04_vendor_and_global_foak_outlook.rds

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
# FIGURE 4 — VENDOR AND GLOBAL FOAK DEPLOYMENT OUTLOOK
# a) Vendor-announced FOAK outlook
# b) Global FOAK deployment outlook
# ============================================================

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

FILE <- "fig04_source_foak_deployment_2050.xlsx"
OUT_DIR <- "."

if (!file.exists(FILE)) {
  stop("Input file not found: ", FILE)
}

available_sheets <- readxl::excel_sheets(FILE)

required_sheets <- c(
  "global_outlook",
  "baseline_counterfactuals",
  "accelerated_counterfactuals"
)

missing_sheets <- required_sheets[!required_sheets %in% available_sheets]

if (length(missing_sheets) > 0) {
  stop(
    "Missing required sheet(s) in workbook:\n",
    paste(missing_sheets, collapse = "\n"),
    "\n\nAvailable sheets are:\n",
    paste(available_sheets, collapse = "\n")
  )
}

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 180
SAVE_H_MM <- 132
SAVE_DPI  <- 600

FILE_STUB <- "fig04_vendor_and_global_foak_outlook"

# ------------------------------------------------------------
# SETTINGS
# ------------------------------------------------------------

display_start_year  <- 2018
display_end_year    <- 2050.4
observed_end_year   <- 2025
projection_start    <- 2025
scenario_start_year <- 2026
vendor_horizon      <- 2036

# ------------------------------------------------------------
# TYPOGRAPHY
# ------------------------------------------------------------

BASE_SIZE         <- 7
AXIS_TEXT_SIZE    <- 6.5
AXIS_TITLE_SIZE   <- 7
LEGEND_TEXT_SIZE  <- 7.2
TITLE_SIZE        <- 7
TAG_SIZE          <- 11

# ------------------------------------------------------------
# LINE WIDTHS
# ------------------------------------------------------------

LW_HIST   <- 1.05
LW_VENDOR <- 1.00
LW_BASE   <- 1.05
LW_ACC    <- 1.05
LW_GUIDE  <- 0.45

# ------------------------------------------------------------
# COLOURS
# ------------------------------------------------------------

col_hist_line   <- "black"
col_vendor_line <- "#1F4E79"

col_bas_line  <- "grey35"
col_bas_fill  <- "grey70"
col_acc_line  <- "#228B22"
col_acc_fill  <- "#90EE90"

ALPHA_BASE <- 0.50
ALPHA_ACC  <- 0.40

legend_levels <- c(
  "Historical operation",
  "Vendor-announced targets",
  "Baseline scenario",
  "Accelerated scenario"
)

scenario_levels <- c(
  "reference",
  "phase_completion",
  "oecd_advance",
  "late_stage_risk",
  "construction_cascade",
  "stalled_development"
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

hide_zero <- function(x) {
  ifelse(x == 0, "", as.character(x))
}

get_first_col <- function(data, patterns, label) {
  hits <- names(data)[Reduce(`|`, lapply(patterns, function(p) str_detect(names(data), p)))]
  hits <- hits[!duplicated(hits)]
  
  if (length(hits) == 0) {
    stop(
      "Could not find column for: ", label,
      "\nAvailable columns are:\n",
      paste(names(data), collapse = "\n")
    )
  }
  
  hits[1]
}

standardise_sheet <- function(sheet) {
  df <- readxl::read_excel(FILE, sheet = sheet) %>%
    dplyr::select(dplyr::where(~ !all(is.na(.x))))
  
  names(df) <- clean_names_local(names(df))
  df
}

read_counterfactual_sheet <- function(sheet, value_patterns) {
  df <- standardise_sheet(sheet)
  
  if (!"group" %in% names(df)) {
    stop("No group column found in sheet: ", sheet)
  }
  
  if (!"scenario" %in% names(df)) {
    stop("No scenario column found in sheet: ", sheet)
  }
  
  if (!"year" %in% names(df)) {
    stop("No year column found in sheet: ", sheet)
  }
  
  value_col <- get_first_col(df, value_patterns, paste0(sheet, " median value"))
  
  df %>%
    dplyr::mutate(
      group = clean_names_local(as.character(group)),
      scenario = clean_names_local(as.character(scenario)),
      year = as.numeric(year),
      value = as.numeric(.data[[value_col]])
    ) %>%
    dplyr::select(year, group, scenario, value)
}

theme_locked <- theme_classic(base_size = BASE_SIZE) +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks  = element_line(colour = "grey35", linewidth = 0.3),
    axis.text.x = element_text(size = AXIS_TEXT_SIZE, colour = "grey20"),
    axis.text.y = element_text(size = AXIS_TEXT_SIZE, colour = "grey20"),
    axis.title.x = element_text(size = AXIS_TITLE_SIZE, margin = margin(t = 4)),
    axis.title.y = element_text(size = AXIS_TITLE_SIZE, margin = margin(r = 4)),
    legend.title = element_blank(),
    legend.text  = element_text(size = LEGEND_TEXT_SIZE),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.key = element_rect(fill = NA, colour = NA),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

add_panel_decorations <- function(plot_obj, tag, title_text, title_x = 0.56, title_y = 0.982) {
  ggdraw(plot_obj) +
    draw_plot_label(
      tag,
      x = 0.01,
      y = 0.99,
      hjust = 0,
      vjust = 1,
      size = TAG_SIZE,
      fontface = "bold"
    ) +
    draw_label(
      title_text,
      x = title_x,
      y = title_y,
      hjust = 0.5,
      vjust = 1,
      size = TITLE_SIZE,
      fontface = "bold"
    )
}

# ============================================================
# HISTORICAL OPERATION
# ============================================================

df_hist_source <- read_counterfactual_sheet(
  sheet = "baseline_counterfactuals",
  value_patterns = c("median.*baseline", "baseline.*median", "^p50$", "p50", "median")
) %>%
  dplyr::filter(group == "global", scenario == "reference")

historical_line <- df_hist_source %>%
  dplyr::filter(year >= display_start_year, year <= observed_end_year) %>%
  dplyr::transmute(
    year,
    value,
    label = factor("Historical operation", levels = legend_levels)
  ) %>%
  dplyr::arrange(year)

# ============================================================
# PANEL A — VENDOR-ANNOUNCED FOAK OUTLOOK
# ============================================================

df0_a <- standardise_sheet("global_outlook")

dup_acc_p50_a <- which(names(df0_a) == "acc_p50_total")
if (length(dup_acc_p50_a) >= 2) {
  names(df0_a)[dup_acc_p50_a[2]] <- "acc_p90_total"
}

names(df0_a) <- sub("^bas_p10_total$", "bas_p10", names(df0_a))
names(df0_a) <- sub("^bas_p50_total$", "bas_p50", names(df0_a))
names(df0_a) <- sub("^bas_p90_total$", "bas_p90", names(df0_a))
names(df0_a) <- sub("^acc_p10_total$", "acc_p10", names(df0_a))
names(df0_a) <- sub("^acc_p50_total$", "acc_p50", names(df0_a))
names(df0_a) <- sub("^acc_p90_total$", "acc_p90", names(df0_a))

names(df0_a) <- sub("^vendors_bas_p10_total$", "vendor_bas_p10", names(df0_a))
names(df0_a) <- sub("^vendors_bas_p50_total$", "vendor_bas_p50", names(df0_a))
names(df0_a) <- sub("^vendors_bas_p90_total$", "vendor_bas_p90", names(df0_a))
names(df0_a) <- sub("^vendors_acc_p10_total$", "vendor_acc_p10", names(df0_a))
names(df0_a) <- sub("^vendors_acc_p50_total$", "vendor_acc_p50", names(df0_a))
names(df0_a) <- sub("^vendors_acc_p90_total$", "vendor_acc_p90", names(df0_a))

required_cols_a <- c(
  "year",
  "category",
  "vendor_rollout",
  "vendor_bas_p10",
  "vendor_bas_p50",
  "vendor_bas_p90",
  "vendor_acc_p10",
  "vendor_acc_p50",
  "vendor_acc_p90"
)

missing_cols_a <- required_cols_a[!required_cols_a %in% names(df0_a)]

if (length(missing_cols_a) > 0) {
  stop(
    "Missing required columns in global_outlook:\n",
    paste(missing_cols_a, collapse = "\n"),
    "\n\nAvailable columns are:\n",
    paste(names(df0_a), collapse = "\n")
  )
}

df_vendor_a <- df0_a %>%
  dplyr::transmute(
    year           = as.numeric(year),
    category       = str_squish(as.character(category)),
    vendor_rollout = as.numeric(vendor_rollout),
    vendor_bas_p10 = as.numeric(vendor_bas_p10),
    vendor_bas_p50 = as.numeric(vendor_bas_p50),
    vendor_bas_p90 = as.numeric(vendor_bas_p90),
    vendor_acc_p10 = as.numeric(vendor_acc_p10),
    vendor_acc_p50 = as.numeric(vendor_acc_p50),
    vendor_acc_p90 = as.numeric(vendor_acc_p90)
  ) %>%
  dplyr::mutate(
    category_clean = dplyr::case_when(
      str_to_lower(category) %in% c("global") ~ "Global",
      TRUE ~ category
    )
  ) %>%
  dplyr::filter(category_clean == "Global") %>%
  dplyr::filter(year >= projection_start, year <= 2050) %>%
  dplyr::arrange(year)

if (nrow(df_vendor_a) == 0) {
  stop("No usable Global vendor data found in sheet 'global_outlook'.")
}

vendor_line_a <- df_vendor_a %>%
  dplyr::transmute(
    year,
    value = vendor_rollout,
    label = factor("Vendor-announced targets", levels = legend_levels)
  )

baseline_line_a <- df_vendor_a %>%
  dplyr::transmute(
    year,
    value = vendor_bas_p50,
    label = factor("Baseline scenario", levels = legend_levels)
  )

accelerated_line_a <- df_vendor_a %>%
  dplyr::transmute(
    year,
    value = vendor_acc_p50,
    label = factor("Accelerated scenario", levels = legend_levels)
  )

baseline_ribbon_a <- df_vendor_a %>%
  dplyr::transmute(
    year,
    ymin = vendor_bas_p10,
    ymax = vendor_bas_p90,
    label = factor("Baseline scenario", levels = legend_levels)
  )

accelerated_ribbon_a <- df_vendor_a %>%
  dplyr::transmute(
    year,
    ymin = vendor_acc_p10,
    ymax = vendor_acc_p90,
    label = factor("Accelerated scenario", levels = legend_levels)
  )

vendor_target_h <- vendor_line_a %>%
  dplyr::filter(year == vendor_horizon) %>%
  dplyr::pull(value)

vendor_bas_h <- baseline_line_a %>%
  dplyr::filter(year == vendor_horizon) %>%
  dplyr::pull(value)

vendor_acc_h <- accelerated_line_a %>%
  dplyr::filter(year == vendor_horizon) %>%
  dplyr::pull(value)

if (length(vendor_target_h) == 0) stop("No vendor target value found for vendor_horizon.")
if (length(vendor_bas_h) == 0) stop("No Baseline value found for vendor_horizon.")
if (length(vendor_acc_h) == 0) stop("No Accelerated value found for vendor_horizon.")

p_a_core <- ggplot() +
  geom_ribbon(
    data = baseline_ribbon_a,
    aes(x = year, ymin = ymin, ymax = ymax, fill = label),
    alpha = ALPHA_BASE,
    colour = NA
  ) +
  geom_ribbon(
    data = accelerated_ribbon_a,
    aes(x = year, ymin = ymin, ymax = ymax, fill = label),
    alpha = ALPHA_ACC,
    colour = NA
  ) +
  geom_segment(
    aes(x = display_start_year, xend = vendor_horizon, y = vendor_target_h, yend = vendor_target_h),
    colour = col_vendor_line,
    linewidth = LW_GUIDE,
    linetype = "22",
    lineend = "butt"
  ) +
  geom_segment(
    aes(x = vendor_horizon, xend = vendor_horizon, y = 0, yend = vendor_target_h),
    colour = col_vendor_line,
    linewidth = LW_GUIDE,
    linetype = "22",
    lineend = "butt"
  ) +
  geom_segment(
    aes(x = display_start_year, xend = vendor_horizon, y = vendor_bas_h, yend = vendor_bas_h),
    colour = col_bas_line,
    linewidth = LW_GUIDE,
    linetype = "22",
    lineend = "butt"
  ) +
  geom_segment(
    aes(x = display_start_year, xend = vendor_horizon, y = vendor_acc_h, yend = vendor_acc_h),
    colour = col_acc_line,
    linewidth = LW_GUIDE,
    linetype = "22",
    lineend = "butt"
  ) +
  geom_step(
    data = historical_line,
    aes(x = year, y = value, colour = label),
    linewidth = LW_HIST,
    direction = "hv",
    lineend = "round"
  ) +
  geom_step(
    data = vendor_line_a,
    aes(x = year, y = value, colour = label),
    linewidth = LW_VENDOR,
    direction = "hv",
    lineend = "round"
  ) +
  geom_step(
    data = baseline_line_a,
    aes(x = year, y = value, colour = label),
    linewidth = LW_BASE,
    direction = "hv",
    lineend = "round"
  ) +
  geom_step(
    data = accelerated_line_a,
    aes(x = year, y = value, colour = label),
    linewidth = LW_ACC,
    direction = "hv",
    lineend = "round"
  ) +
  scale_colour_manual(
    name = NULL,
    breaks = legend_levels,
    values = c(
      "Historical operation"     = col_hist_line,
      "Vendor-announced targets" = col_vendor_line,
      "Baseline scenario"        = col_bas_line,
      "Accelerated scenario"     = col_acc_line
    ),
    drop = FALSE
  ) +
  scale_fill_manual(
    name = NULL,
    breaks = legend_levels,
    values = c(
      "Historical operation"     = "transparent",
      "Vendor-announced targets" = "transparent",
      "Baseline scenario"        = col_bas_fill,
      "Accelerated scenario"     = col_acc_fill
    ),
    drop = FALSE
  ) +
  scale_x_continuous(
    limits = c(display_start_year, display_end_year),
    breaks = c(2020, 2030, 2040, 2050),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = c(0, 10, 20, 30, 40, 50, 60),
    labels = hide_zero,
    expand = c(0, 0)
  ) +
  labs(
    x = "Year",
    y = "Cumulative FOAK SMRs"
  ) +
  theme_locked +
  theme(
    legend.position = "none",
    plot.margin = margin(6, 6, 6, 6)
  ) +
  guides(fill = "none", colour = "none")

p_a <- add_panel_decorations(
  p_a_core,
  tag = "a",
  title_text = "Vendor-announced FOAK outlook",
  title_x = 0.56,
  title_y = 0.982
)

# ============================================================
# PANEL B — GLOBAL FOAK DEPLOYMENT OUTLOOK
# ============================================================

df_base_b <- read_counterfactual_sheet(
  sheet = "baseline_counterfactuals",
  value_patterns = c("median.*baseline", "baseline.*median", "^p50$", "p50", "median")
) %>%
  dplyr::filter(group == "global") %>%
  dplyr::mutate(scenario = factor(scenario, levels = scenario_levels))

base_ref_b <- df_base_b %>%
  dplyr::filter(scenario == "reference") %>%
  dplyr::mutate(label = factor("Baseline scenario", levels = legend_levels))

base_bounds_b <- df_base_b %>%
  dplyr::filter(scenario != "reference", year >= scenario_start_year) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    ymin = min(value, na.rm = TRUE),
    ymax = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(label = factor("Baseline scenario", levels = legend_levels))

df_acc_b <- read_counterfactual_sheet(
  sheet = "accelerated_counterfactuals",
  value_patterns = c("median.*accelerated", "accelerated.*median", "^p50$", "p50", "median")
) %>%
  dplyr::filter(group == "global") %>%
  dplyr::mutate(scenario = factor(scenario, levels = scenario_levels))

acc_ref_b <- df_acc_b %>%
  dplyr::filter(scenario == "reference") %>%
  dplyr::mutate(label = factor("Accelerated scenario", levels = legend_levels))

acc_bounds_b <- df_acc_b %>%
  dplyr::filter(scenario != "reference", year >= scenario_start_year) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    ymin = min(value, na.rm = TRUE),
    ymax = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(label = factor("Accelerated scenario", levels = legend_levels))

observed_line_b <- base_ref_b %>%
  dplyr::filter(year <= observed_end_year) %>%
  dplyr::select(year, value) %>%
  dplyr::mutate(label = factor("Historical operation", levels = legend_levels))

observed_bridge_b <- observed_line_b %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::mutate(year = scenario_start_year)

observed_line_b <- dplyr::bind_rows(observed_line_b, observed_bridge_b) %>%
  dplyr::arrange(year)

vendor_line_b <- vendor_line_a

baseline_line_b <- base_ref_b %>%
  dplyr::filter(year >= scenario_start_year)

accelerated_line_b <- acc_ref_b %>%
  dplyr::filter(year >= scenario_start_year)

p_b_core <- ggplot() +
  geom_ribbon(
    data = base_bounds_b,
    aes(x = year, ymin = ymin, ymax = ymax, fill = label),
    alpha = ALPHA_BASE,
    colour = NA
  ) +
  geom_ribbon(
    data = acc_bounds_b,
    aes(x = year, ymin = ymin, ymax = ymax, fill = label),
    alpha = ALPHA_ACC,
    colour = NA
  ) +
  geom_step(
    data = observed_line_b,
    aes(x = year, y = value, colour = label),
    linewidth = LW_HIST,
    direction = "hv",
    lineend = "round"
  ) +
  geom_step(
    data = vendor_line_b,
    aes(x = year, y = value, colour = label),
    linewidth = LW_VENDOR,
    direction = "hv",
    lineend = "round"
  ) +
  geom_step(
    data = baseline_line_b,
    aes(x = year, y = value, colour = label),
    linewidth = LW_BASE,
    direction = "hv",
    lineend = "round"
  ) +
  geom_step(
    data = accelerated_line_b,
    aes(x = year, y = value, colour = label),
    linewidth = LW_ACC,
    direction = "hv",
    lineend = "round"
  ) +
  scale_colour_manual(
    name = NULL,
    breaks = legend_levels,
    values = c(
      "Historical operation"     = col_hist_line,
      "Vendor-announced targets" = col_vendor_line,
      "Baseline scenario"        = col_bas_line,
      "Accelerated scenario"     = col_acc_line
    ),
    drop = FALSE
  ) +
  scale_fill_manual(
    name = NULL,
    breaks = legend_levels,
    values = c(
      "Historical operation"     = "transparent",
      "Vendor-announced targets" = "transparent",
      "Baseline scenario"        = col_bas_fill,
      "Accelerated scenario"     = col_acc_fill
    ),
    drop = FALSE
  ) +
  scale_x_continuous(
    limits = c(display_start_year, display_end_year),
    breaks = c(2020, 2030, 2040, 2050),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = c(0, 10, 20, 30, 40, 50, 60),
    labels = hide_zero,
    expand = c(0, 0)
  ) +
  labs(
    x = "Year",
    y = "Cumulative FOAK SMRs"
  ) +
  theme_locked +
  theme(
    legend.position = "none",
    plot.margin = margin(6, 6, 6, 6)
  ) +
  guides(fill = "none", colour = "none")

p_b <- add_panel_decorations(
  p_b_core,
  tag = "b",
  title_text = "Global FOAK deployment outlook",
  title_x = 0.56,
  title_y = 0.982
)

# ============================================================
# SHARED LEGEND
# ============================================================

legend_plot <- ggplot() +
  geom_segment(
    aes(x = 0, xend = 1, y = 1, yend = 1,
        colour = factor("Historical operation", levels = legend_levels)),
    linewidth = LW_HIST,
    lineend = "round"
  ) +
  geom_segment(
    aes(x = 0, xend = 1, y = 1, yend = 1,
        colour = factor("Vendor-announced targets", levels = legend_levels)),
    linewidth = LW_VENDOR,
    lineend = "round"
  ) +
  geom_segment(
    aes(x = 0, xend = 1, y = 1, yend = 1,
        colour = factor("Baseline scenario", levels = legend_levels)),
    linewidth = LW_BASE,
    lineend = "round"
  ) +
  geom_segment(
    aes(x = 0, xend = 1, y = 1, yend = 1,
        colour = factor("Accelerated scenario", levels = legend_levels)),
    linewidth = LW_ACC,
    lineend = "round"
  ) +
  scale_colour_manual(
    name = NULL,
    breaks = legend_levels,
    values = c(
      "Historical operation"     = col_hist_line,
      "Vendor-announced targets" = col_vendor_line,
      "Baseline scenario"        = col_bas_line,
      "Accelerated scenario"     = col_acc_line
    ),
    drop = FALSE
  ) +
  theme_void(base_size = BASE_SIZE) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = LEGEND_TEXT_SIZE),
    legend.key.width = unit(10, "mm"),
    legend.key.height = unit(3.4, "mm"),
    legend.spacing.x = unit(2.5, "mm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0)
  ) +
  guides(
    colour = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        linewidth = c(LW_HIST, LW_VENDOR, LW_BASE, LW_ACC)
      )
    )
  )

shared_legend <- get_legend(legend_plot)

# ============================================================
# COMBINE PANELS + LEGEND
# ============================================================

panels_ab <- plot_grid(
  p_a,
  p_b,
  ncol = 2,
  rel_widths = c(1, 1),
  align = "h",
  axis = "tb"
)

combined_ab <- plot_grid(
  panels_ab,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.105)
)

print(combined_ab)

# ============================================================
# SAVE
# ============================================================

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".png")),
  plot = combined_ab,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".pdf")),
  plot = combined_ab,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  bg = "white",
  useDingbats = FALSE
)

saveRDS(
  combined_ab,
  file = file.path(OUT_DIR, paste0(FILE_STUB, ".rds"))
)

combined_ab
