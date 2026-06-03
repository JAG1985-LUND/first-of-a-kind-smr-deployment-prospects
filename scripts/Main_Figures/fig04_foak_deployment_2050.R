# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Figure 4: vendor-announced and modelled FOAK SMR deployment trajectories,
# including 2050 sensitivity overlays.
#
# Required input:
# fig04_source_foak_deployment_2050.xlsx
#   - sheet: global_outlook
#   - sheet: baseline_counterfactuals
#   - sheet: accelerated_counterfactuals
#
# Outputs:
# fig04_vendor_and_global_foak_outlook_with_2050_sensitivity.png
# fig04_vendor_and_global_foak_outlook_with_2050_sensitivity.pdf
# fig04_vendor_and_global_foak_outlook_with_2050_sensitivity.rds

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
# b) Global FOAK deployment outlook with 2050 sensitivity overlay
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
SAVE_H_MM <- 150
SAVE_DPI  <- 600

FILE_STUB <- "fig04_vendor_and_global_foak_outlook_with_2050_sensitivity"

# ------------------------------------------------------------
# SETTINGS
# ------------------------------------------------------------

display_start_year <- 2018
display_end_year_a <- 2050.4
display_end_year_b <- 2053.2

observed_end_year   <- 2025
projection_start    <- 2025
scenario_start_year <- 2026
vendor_horizon      <- 2036
overlay_year        <- 2050

# ------------------------------------------------------------
# TYPOGRAPHY
# ------------------------------------------------------------

BASE_SIZE        <- 7
AXIS_TEXT_SIZE   <- 6.5
AXIS_TITLE_SIZE  <- 7
TITLE_SIZE       <- 7
SUBTITLE_SIZE    <- 6.2
TAG_SIZE         <- 11

LEGEND_TEXT_MAIN <- 5.5
LEGEND_TEXT_SENS <- 5.5
LEGEND_TEXT_CASE <- 5.5

# ------------------------------------------------------------
# LINE WIDTHS
# ------------------------------------------------------------

LW_HIST   <- 1.05
LW_VENDOR <- 1.00
LW_BASE   <- 1.05
LW_ACC    <- 1.05
LW_GUIDE  <- 0.45
LW_ERR    <- 0.38

POINT_SIZE <- 1.65

# ------------------------------------------------------------
# COLOURS
# ------------------------------------------------------------

col_hist_line   <- "black"
col_vendor_line <- "#1F4E79"

col_bas_line <- "grey35"
col_bas_fill <- "grey70"

col_acc_line <- "#228B22"
col_acc_fill <- "#90EE90"

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

scenario_cols <- c(
  "Current phase completion" = "#2ca02c",
  "ADVANCE-OECD licensing"   = "#009E8E",
  "Construction cascade"     = "#E66101",
  "Late-stage risk"          = "#7F0000",
  "Stalled development"      = "#E31A1C"
)

scenario_labs <- c(
  "phase_completion"     = "Current phase completion",
  "oecd_advance"         = "ADVANCE-OECD licensing",
  "construction_cascade" = "Construction cascade",
  "late_stage_risk"      = "Late-stage risk",
  "stalled_development"  = "Stalled development"
)

scenario_order <- c(
  "Current phase completion",
  "ADVANCE-OECD licensing",
  "Construction cascade",
  "Late-stage risk",
  "Stalled development"
)

scenario_x <- c(
  "Current phase completion" = 2051.00,
  "ADVANCE-OECD licensing"   = 2051.45,
  "Construction cascade"     = 2051.90,
  "Late-stage risk"          = 2052.35,
  "Stalled development"      = 2052.80
)

model_offsets <- c(
  "Baseline sensitivity"    = -0.055,
  "Accelerated sensitivity" =  0.055
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
    legend.position = "none",
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

add_panel_decorations <- function(plot_obj, tag, title_text, subtitle_text = NULL,
                                  title_x = 0.56, title_y = 0.982) {
  out <- ggdraw(plot_obj) +
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
  
  if (!is.null(subtitle_text)) {
    out <- out +
      draw_label(
        subtitle_text,
        x = title_x,
        y = title_y - 0.045,
        hjust = 0.5,
        vjust = 1,
        size = SUBTITLE_SIZE,
        colour = "grey25"
      )
  }
  
  out
}

legend_line_symbol <- function(x0, x1, y, col, lw = 1.05) {
  draw_line(
    x = c(x0, x1),
    y = c(y, y),
    colour = col,
    linewidth = lw,
    lineend = "round"
  )
}

legend_ribbon_symbol <- function(x0, x1, y, line_col, fill_col, alpha = 0.45, lw = 1.05) {
  list(
    draw_grob(
      grid::rectGrob(
        x = unit((x0 + x1) / 2, "npc"),
        y = unit(y, "npc"),
        width = unit(x1 - x0, "npc"),
        height = unit(0.26, "npc"),
        gp = grid::gpar(
          fill = grDevices::adjustcolor(fill_col, alpha.f = alpha),
          col = NA
        )
      )
    ),
    draw_line(
      x = c(x0, x1),
      y = c(y, y),
      colour = line_col,
      linewidth = lw,
      lineend = "round"
    )
  )
}

legend_interval_symbol <- function(x0, x1, y, col) {
  list(
    draw_line(
      x = c(x0, x1),
      y = c(y, y),
      colour = col,
      linewidth = 0.42,
      lineend = "butt"
    ),
    draw_line(
      x = c(x0, x0),
      y = c(y - 0.16, y + 0.16),
      colour = col,
      linewidth = 0.42,
      lineend = "butt"
    ),
    draw_line(
      x = c(x1, x1),
      y = c(y - 0.16, y + 0.16),
      colour = col,
      linewidth = 0.42,
      lineend = "butt"
    ),
    draw_grob(
      grid::pointsGrob(
        x = unit((x0 + x1) / 2, "npc"),
        y = unit(y, "npc"),
        pch = 16,
        gp = grid::gpar(col = col, fill = col, fontsize = 4.5)
      )
    )
  )
}

legend_open_circle <- function(x, y) {
  draw_grob(
    grid::pointsGrob(
      x = unit(x, "npc"),
      y = unit(y, "npc"),
      pch = 1,
      gp = grid::gpar(col = "black", fill = NA, fontsize = 5.2)
    )
  )
}

legend_filled_circle <- function(x, y) {
  draw_grob(
    grid::pointsGrob(
      x = unit(x, "npc"),
      y = unit(y, "npc"),
      pch = 16,
      gp = grid::gpar(col = "black", fill = "black", fontsize = 5.2)
    )
  )
}

legend_main_line_cell <- function(label, line_col, lw) {
  ggdraw() +
    legend_line_symbol(0.06, 0.31, 0.50, line_col, lw) +
    draw_label(
      label,
      0.39,
      0.50,
      hjust = 0,
      vjust = 0.5,
      size = LEGEND_TEXT_MAIN
    )
}

legend_main_ribbon_cell <- function(label, line_col, fill_col, alpha, lw) {
  ggdraw() +
    legend_ribbon_symbol(0.06, 0.31, 0.50, line_col, fill_col, alpha, lw) +
    draw_label(
      label,
      0.39,
      0.50,
      hjust = 0,
      vjust = 0.5,
      size = LEGEND_TEXT_MAIN
    )
}

legend_text_width_npc <- function(label, size, fontface = "plain",
                                  row_width_mm = SAVE_W_MM * 0.98 * 0.99) {
  width_mm <- tryCatch(
    grid::convertWidth(
      grid::grobWidth(
        grid::textGrob(
          label,
          gp = grid::gpar(fontsize = size, fontface = fontface)
        )
      ),
      "mm",
      valueOnly = TRUE
    ),
    error = function(e) NA_real_
  )
  
  if (!is.finite(width_mm)) {
    width_mm <- nchar(label) * size * 0.3528 * 0.46
  }
  
  (width_mm * 1.04) / row_width_mm
}

make_sensitivity_row <- function() {
  header <- "Sensitivity analysis (P10–P90)"
  
  sens_labels <- c(
    "Current phase completion",
    "ADVANCE-OECD licensing",
    "Construction cascade",
    "Late-stage risk",
    "Stalled development"
  )
  
  sens_colours <- scenario_cols[sens_labels]
  
  left_margin <- 0.006
  right_margin <- 0.006
  symbol_width <- 0.024
  symbol_label_gap <- 0.008
  desired_group_gap <- 0.016
  
  header_width <- legend_text_width_npc(
    header,
    LEGEND_TEXT_SENS,
    fontface = "bold"
  )
  
  label_widths <- vapply(
    sens_labels,
    legend_text_width_npc,
    numeric(1),
    size = LEGEND_TEXT_SENS,
    fontface = "plain"
  )
  
  fixed_width <- left_margin + right_margin +
    header_width +
    length(sens_labels) * symbol_width +
    length(sens_labels) * symbol_label_gap +
    sum(label_widths)
  
  n_gaps <- length(sens_labels)
  group_gap <- min(
    desired_group_gap,
    (0.998 - fixed_width) / n_gaps
  )
  group_gap <- max(group_gap, 0.006)
  
  if ((fixed_width + n_gaps * group_gap) > 0.998) {
    overflow <- fixed_width + n_gaps * group_gap - 0.998
    symbol_width <- max(0.020, symbol_width - overflow / length(sens_labels))
  }
  
  row <- ggdraw()
  
  x <- left_margin
  
  row <- row +
    draw_label(
      header,
      x,
      0.50,
      hjust = 0,
      vjust = 0.5,
      size = LEGEND_TEXT_SENS,
      fontface = "bold"
    )
  
  x <- x + header_width + group_gap
  
  for (i in seq_along(sens_labels)) {
    x0 <- x
    x1 <- x + symbol_width
    label_x <- x1 + symbol_label_gap
    
    row <- row +
      legend_interval_symbol(x0, x1, 0.50, sens_colours[[i]]) +
      draw_label(
        sens_labels[[i]],
        label_x,
        0.50,
        hjust = 0,
        vjust = 0.5,
        size = LEGEND_TEXT_SENS
      )
    
    x <- label_x + label_widths[[i]]
    
    if (i < length(sens_labels)) {
      x <- x + group_gap
    }
  }
  
  row
}

# ============================================================
# GLOBAL OUTLOOK DATA
# ============================================================

df0 <- standardise_sheet("global_outlook")

dup_acc_p50 <- which(names(df0) == "acc_p50_total")
if (length(dup_acc_p50) >= 2) {
  names(df0)[dup_acc_p50[2]] <- "acc_p90_total"
}

names(df0) <- sub("^bas_p10_total$", "bas_p10", names(df0))
names(df0) <- sub("^bas_p50_total$", "bas_p50", names(df0))
names(df0) <- sub("^bas_p90_total$", "bas_p90", names(df0))
names(df0) <- sub("^acc_p10_total$", "acc_p10", names(df0))
names(df0) <- sub("^acc_p50_total$", "acc_p50", names(df0))
names(df0) <- sub("^acc_p90_total$", "acc_p90", names(df0))

names(df0) <- sub("^vendors_bas_p10_total$", "vendor_bas_p10", names(df0))
names(df0) <- sub("^vendors_bas_p50_total$", "vendor_bas_p50", names(df0))
names(df0) <- sub("^vendors_bas_p90_total$", "vendor_bas_p90", names(df0))
names(df0) <- sub("^vendors_acc_p10_total$", "vendor_acc_p10", names(df0))
names(df0) <- sub("^vendors_acc_p50_total$", "vendor_acc_p50", names(df0))
names(df0) <- sub("^vendors_acc_p90_total$", "vendor_acc_p90", names(df0))

required_cols <- c(
  "year",
  "category",
  "vendor_rollout",
  "bas_p10",
  "bas_p50",
  "bas_p90",
  "acc_p10",
  "acc_p50",
  "acc_p90",
  "vendor_bas_p10",
  "vendor_bas_p50",
  "vendor_bas_p90",
  "vendor_acc_p10",
  "vendor_acc_p50",
  "vendor_acc_p90"
)

missing_cols <- required_cols[!required_cols %in% names(df0)]

if (length(missing_cols) > 0) {
  stop(
    "Missing required columns in global_outlook:\n",
    paste(missing_cols, collapse = "\n"),
    "\n\nAvailable columns are:\n",
    paste(names(df0), collapse = "\n")
  )
}

df_global <- df0 %>%
  dplyr::transmute(
    year            = as.numeric(year),
    category        = str_squish(as.character(category)),
    vendor_rollout  = as.numeric(vendor_rollout),
    bas_p10         = as.numeric(bas_p10),
    bas_p50         = as.numeric(bas_p50),
    bas_p90         = as.numeric(bas_p90),
    acc_p10         = as.numeric(acc_p10),
    acc_p50         = as.numeric(acc_p50),
    acc_p90         = as.numeric(acc_p90),
    vendor_bas_p10  = as.numeric(vendor_bas_p10),
    vendor_bas_p50  = as.numeric(vendor_bas_p50),
    vendor_bas_p90  = as.numeric(vendor_bas_p90),
    vendor_acc_p10  = as.numeric(vendor_acc_p10),
    vendor_acc_p50  = as.numeric(vendor_acc_p50),
    vendor_acc_p90  = as.numeric(vendor_acc_p90)
  ) %>%
  dplyr::filter(str_to_lower(category) == "global") %>%
  dplyr::arrange(year)

if (nrow(df_global) == 0) {
  stop("No usable Global data found in sheet 'global_outlook'.")
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

df_vendor_a <- df_global %>%
  dplyr::filter(year >= projection_start, year <= 2050) %>%
  dplyr::arrange(year)

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
    limits = c(display_start_year, display_end_year_a),
    breaks = c(2020, 2030, 2040, 2050),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 66),
    breaks = c(0, 10, 20, 30, 40, 50, 60),
    labels = hide_zero,
    expand = c(0, 0)
  ) +
  labs(
    x = "Year",
    y = "Cumulative FOAK SMRs"
  ) +
  theme_locked +
  theme(plot.margin = margin(6, 6, 6, 6)) +
  guides(fill = "none", colour = "none")

p_a <- add_panel_decorations(
  p_a_core,
  tag = "a",
  title_text = "Vendor-announced FOAK outlook",
  subtitle_text = "Designs with vendor-stated FOAK targets",
  title_x = 0.56,
  title_y = 0.982
)

# ============================================================
# PANEL B — GLOBAL FOAK DEPLOYMENT OUTLOOK
# Uses the original GitHub counterfactual-sheet logic
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

vendor_line_b <- df_global %>%
  dplyr::filter(year >= projection_start, year <= vendor_horizon) %>%
  dplyr::transmute(
    year,
    value = vendor_rollout,
    label = factor("Vendor-announced targets", levels = legend_levels)
  )

baseline_line_b <- base_ref_b %>%
  dplyr::filter(year >= scenario_start_year)

accelerated_line_b <- acc_ref_b %>%
  dplyr::filter(year >= scenario_start_year)

# ------------------------------------------------------------
# 2050 SENSITIVITY OVERLAY DATA
# ------------------------------------------------------------

baseline_sens <- standardise_sheet("baseline_counterfactuals") %>%
  dplyr::mutate(
    group = clean_names_local(as.character(group)),
    scenario = clean_names_local(as.character(scenario)),
    year = as.numeric(year)
  ) %>%
  dplyr::filter(group == "global", scenario != "reference", year == overlay_year) %>%
  dplyr::transmute(
    year,
    scenario,
    model_case = "Baseline sensitivity",
    p10 = as.numeric(conservative_baseline_scenario),
    p50 = as.numeric(median_baseline_scenario),
    p90 = as.numeric(optimistic_baseline_scenario)
  )

accelerated_sens <- standardise_sheet("accelerated_counterfactuals") %>%
  dplyr::mutate(
    group = clean_names_local(as.character(group)),
    scenario = clean_names_local(as.character(scenario)),
    year = as.numeric(year)
  ) %>%
  dplyr::filter(group == "global", scenario != "reference", year == overlay_year) %>%
  dplyr::transmute(
    year,
    scenario,
    model_case = "Accelerated sensitivity",
    p10 = as.numeric(conservative_accelerated_scenario),
    p50 = as.numeric(median_accelerated_scenario),
    p90 = as.numeric(optimistic_accelerated_scenario)
  )

sens_overlay <- dplyr::bind_rows(baseline_sens, accelerated_sens) %>%
  dplyr::mutate(
    scenario_label = dplyr::recode(scenario, !!!scenario_labs),
    scenario_label = factor(scenario_label, levels = scenario_order),
    model_case = factor(
      model_case,
      levels = c("Baseline sensitivity", "Accelerated sensitivity")
    ),
    x_plot = scenario_x[as.character(scenario_label)] +
      model_offsets[as.character(model_case)]
  ) %>%
  dplyr::filter(!is.na(scenario_label)) %>%
  dplyr::arrange(model_case, scenario_label)

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
  geom_errorbar(
    data = sens_overlay,
    aes(
      x = x_plot,
      ymin = p10,
      ymax = p90,
      colour = scenario_label
    ),
    width = 0.08,
    linewidth = LW_ERR
  ) +
  geom_point(
    data = sens_overlay,
    aes(
      x = x_plot,
      y = p50,
      colour = scenario_label,
      shape = model_case
    ),
    size = POINT_SIZE,
    stroke = 0.48
  ) +
  scale_colour_manual(
    name = NULL,
    breaks = c(legend_levels, scenario_order),
    values = c(
      "Historical operation"      = col_hist_line,
      "Vendor-announced targets"  = col_vendor_line,
      "Baseline scenario"         = col_bas_line,
      "Accelerated scenario"      = col_acc_line,
      scenario_cols
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
  scale_shape_manual(
    name = NULL,
    values = c(
      "Baseline sensitivity" = 1,
      "Accelerated sensitivity" = 16
    ),
    drop = FALSE
  ) +
  scale_x_continuous(
    limits = c(display_start_year, display_end_year_b),
    breaks = c(2020, 2030, 2040, 2050),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 66),
    breaks = c(0, 10, 20, 30, 40, 50, 60),
    labels = hide_zero,
    expand = c(0, 0)
  ) +
  labs(
    x = "Year",
    y = "Cumulative FOAK SMRs"
  ) +
  theme_locked +
  theme(plot.margin = margin(6, 8, 6, 6)) +
  guides(fill = "none", colour = "none", shape = "none")

p_b <- add_panel_decorations(
  p_b_core,
  tag = "b",
  title_text = "Global FOAK deployment outlook",
  subtitle_text = "Current pipeline plus simulated post-2025 design additions",
  title_x = 0.56,
  title_y = 0.982
)

# ============================================================
# MANUAL LEGEND BLOCK
# ============================================================

legend_row_1 <- plot_grid(
  legend_main_line_cell(
    "Historical operation",
    col_hist_line,
    LW_HIST
  ),
  legend_main_line_cell(
    "Vendor-announced targets",
    col_vendor_line,
    LW_VENDOR
  ),
  legend_main_ribbon_cell(
    "Baseline scenario (P10–P90)",
    col_bas_line,
    col_bas_fill,
    ALPHA_BASE,
    LW_BASE
  ),
  legend_main_ribbon_cell(
    "Accelerated scenario (P10–P90)",
    col_acc_line,
    col_acc_fill,
    ALPHA_ACC,
    LW_ACC
  ),
  nrow = 1,
  rel_widths = c(0.200, 0.240, 0.280, 0.280)
)

legend_row_2 <- make_sensitivity_row()

legend_row_3 <- ggdraw() +
  draw_label(
    "Sensitivity case",
    0.398,
    0.50,
    hjust = 1,
    vjust = 0.5,
    size = LEGEND_TEXT_CASE,
    fontface = "bold"
  ) +
  legend_open_circle(0.424, 0.50) +
  draw_label("Baseline sensitivity", 0.445, 0.50, hjust = 0, vjust = 0.5, size = LEGEND_TEXT_CASE) +
  legend_filled_circle(0.585, 0.50) +
  draw_label("Accelerated sensitivity", 0.606, 0.50, hjust = 0, vjust = 0.5, size = LEGEND_TEXT_CASE)

legend_block_inner <- plot_grid(
  legend_row_1,
  legend_row_2,
  legend_row_3,
  ncol = 1,
  rel_heights = c(0.34, 0.39, 0.27)
)

legend_block <- plot_grid(
  NULL,
  legend_block_inner,
  NULL,
  nrow = 1,
  rel_widths = c(0.005, 0.990, 0.005)
)

# ============================================================
# COMBINE PANELS + LEGENDS
# ============================================================

panels_ab <- plot_grid(
  p_a,
  p_b,
  ncol = 2,
  rel_widths = c(1, 1.08),
  align = "h",
  axis = "tb"
)

combined_ab <- plot_grid(
  panels_ab,
  legend_block,
  ncol = 1,
  rel_heights = c(1, 0.175)
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
