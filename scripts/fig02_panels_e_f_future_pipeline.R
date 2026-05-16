# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Figure 2e,f: FOAK development and deployment pathways under the
# Baseline and Accelerated scenarios.
#
# Panels e and f show the annual distribution of SMR designs across development
# stages under the Baseline and Accelerated scenarios. Negative bars indicate
# phase-specific project attrition. Phase colours are consistent across panels.
#
# Required input:
# data/fig02_source_data_foak_framework.xlsx
#   - sheet: future_pipeline
#
# Outputs:
# outputs/Figure_02/intermediate/fig02_panels_e_f_future_pipeline.png
# outputs/Figure_02/intermediate/fig02_panels_e_f_future_pipeline.pdf
# outputs/Figure_02/intermediate/fig02_panels_e_f_future_pipeline.rds
# outputs/Figure_02/intermediate/fig02_panel_e_future_pipeline_baseline.rds
# outputs/Figure_02/intermediate/fig02_panel_f_future_pipeline_accelerated.rds

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
# FIGURE 2 — PANELS E + F
# Future FOAK development and deployment pathways
# ============================================================

# ------------------------------------------------------------
# PATHS — LOCAL JAG RUN
# ------------------------------------------------------------

FILE <- file.path(
  "C:/Users/jo3151go/OneDrive/Nature_Energy_SMRs_2026",
  "Main Data",
  "Main_Data_Files",
  "fig02_source_data_foak_framework.xlsx"
)

OUT_DIR <- file.path(
  "C:/Users/jo3151go/OneDrive/Nature_Energy_SMRs_2026",
  "Figure_02"
)

if (!file.exists(FILE)) {
  stop("Input file not found: ", FILE)
}

dir.create(
  OUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

YEAR_MIN        <- 2025
BASELINE_X_MAX  <- 2070
ACCEL_X_MAX     <- 2066

SAVE_DPI       <- 600
BASE_SIZE      <- 7
COMBINED_W_MM  <- 180
COMBINED_H_MM  <- 95

FILE_STUB <- "fig02_panels_e_f_future_pipeline"

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------

clean_names <- function(x) {
  x <- trimws(x)
  x <- gsub("[[:space:]]+", "_", x)
  x <- gsub("[^A-Za-z0-9_]", "", x)
  tolower(x)
}

num_or_na <- function(x) {
  suppressWarnings(as.numeric(x))
}

impute_failure_year <- function(
    fail_phase,
    cd_start,
    bd_start,
    dd_start,
    licensing_start,
    construction_start,
    operational_start,
    scenario
) {
  if (is.na(fail_phase) || fail_phase == "") return(NA_real_)
  
  lag_years <- ifelse(scenario == "accelerated", 2, 3)
  
  phase_start <- dplyr::case_when(
    fail_phase == "conceptual_design" ~ cd_start,
    fail_phase == "basic_design"      ~ bd_start,
    fail_phase == "detailed_design"   ~ dd_start,
    fail_phase == "licensing"         ~ licensing_start,
    fail_phase == "construction"      ~ construction_start,
    TRUE                              ~ NA_real_
  )
  
  next_start <- dplyr::case_when(
    fail_phase == "conceptual_design" ~ bd_start,
    fail_phase == "basic_design"      ~ dd_start,
    fail_phase == "detailed_design"   ~ licensing_start,
    fail_phase == "licensing"         ~ construction_start,
    fail_phase == "construction"      ~ operational_start,
    TRUE                              ~ NA_real_
  )
  
  fail_year <- phase_start + lag_years
  
  if (!is.na(next_start)) {
    fail_year <- min(fail_year, next_start - 1e-6)
  }
  
  fail_year
}

phase_for_year <- function(y, cd, bd, dd, lic, con, op, fail_y) {
  if (!is.na(fail_y) && y >= fail_y) return(NA_character_)
  
  starts <- c(
    conceptual_design = cd,
    basic_design      = bd,
    detailed_design   = dd,
    licensing         = lic,
    construction      = con,
    operational       = op
  )
  
  starts <- starts[!is.na(starts)]
  if (length(starts) == 0) return(NA_character_)
  
  valid <- starts[starts <= y]
  if (length(valid) == 0) return(NA_character_)
  
  names(valid)[which.max(valid)]
}

# ------------------------------------------------------------
# PHASE ORDER AND COLOURS
# ------------------------------------------------------------

phase_levels <- c(
  "conceptual_design",
  "basic_design",
  "detailed_design",
  "licensing",
  "construction",
  "operational"
)

failure_levels <- c(
  "conceptual_design",
  "basic_design",
  "detailed_design",
  "licensing",
  "construction"
)

phase_labels <- c(
  conceptual_design = "Conceptual design",
  basic_design      = "Basic design",
  detailed_design   = "Detailed design",
  licensing         = "Licensing",
  construction      = "Construction",
  operational       = "Operational"
)

phase_cols <- c(
  conceptual_design = "#FDE0D2",
  basic_design      = "#F4A582",
  detailed_design   = "#D6604D",
  licensing         = "#D1E5F0",
  construction      = "#92C5DE",
  operational       = "#4393C3"
)

fail_cols <- c(
  conceptual_design = "#FDE0D2",
  basic_design      = "#F4A582",
  detailed_design   = "#D6604D",
  licensing         = "#B3B3B3",
  construction      = "#7F7F7F"
)

# ------------------------------------------------------------
# DATA BUILDER
# ------------------------------------------------------------

build_panel_data <- function(dat, scenario_name, year_min, year_max) {
  years <- year_min:year_max
  
  dat_s <- dat %>%
    dplyr::filter(scenario == scenario_name) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      failure_year = impute_failure_year(
        fail_phase = fail_phase,
        cd_start = cd_start,
        bd_start = bd_start,
        dd_start = dd_start,
        licensing_start = licensing_start,
        construction_start = construction_start,
        operational_start = operational_start,
        scenario = scenario
      )
    ) %>%
    dplyr::ungroup()
  
  phase_long <- tidyr::expand_grid(
    design = dat_s$design,
    year = years
  ) %>%
    dplyr::left_join(dat_s, by = "design") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      phase = phase_for_year(
        y = year,
        cd = cd_start,
        bd = bd_start,
        dd = dd_start,
        lic = licensing_start,
        con = construction_start,
        op = operational_start,
        fail_y = failure_year
      ),
      failed_this_year = ifelse(
        !is.na(failure_year) & floor(failure_year) == year,
        1L,
        0L
      )
    ) %>%
    dplyr::ungroup()
  
  phase_counts <- phase_long %>%
    dplyr::filter(!is.na(phase)) %>%
    dplyr::mutate(
      phase = factor(phase, levels = phase_levels)
    ) %>%
    dplyr::count(year, phase, name = "n") %>%
    tidyr::complete(
      year = years,
      phase = factor(phase_levels, levels = phase_levels),
      fill = list(n = 0)
    )
  
  fail_counts <- phase_long %>%
    dplyr::filter(failed_this_year == 1, !is.na(fail_phase)) %>%
    dplyr::mutate(
      fail_phase = factor(fail_phase, levels = failure_levels)
    ) %>%
    dplyr::count(year, fail_phase, name = "n") %>%
    tidyr::complete(
      year = years,
      fail_phase = factor(failure_levels, levels = failure_levels),
      fill = list(n = 0)
    ) %>%
    dplyr::mutate(n = -n)
  
  list(
    phase_counts = phase_counts,
    fail_counts = fail_counts
  )
}

# ------------------------------------------------------------
# PANEL PLOTTER
# ------------------------------------------------------------

make_panel_plot <- function(
    phase_counts,
    fail_counts,
    x_limits,
    x_breaks,
    panel_tag,
    show_y_title = TRUE
) {
  p <- ggplot() +
    geom_col(
      data = phase_counts,
      aes(x = year, y = n, fill = phase),
      width = 0.9,
      colour = NA,
      position = position_stack(reverse = TRUE)
    ) +
    geom_col(
      data = fail_counts,
      aes(x = year, y = n, fill = fail_phase),
      width = 0.9,
      colour = NA,
      alpha = 0.95,
      position = position_stack(reverse = TRUE)
    ) +
    geom_hline(yintercept = 0, colour = "grey35", linewidth = 0.3) +
    scale_fill_manual(
      values = c(phase_cols, fail_cols),
      breaks = names(phase_cols),
      labels = unname(phase_labels),
      name = NULL
    ) +
    scale_x_continuous(
      limits = x_limits,
      breaks = x_breaks,
      expand = expansion(mult = c(0.005, 0.005))
    ) +
    scale_y_continuous(
      limits = c(-5, 52),
      breaks = c(-5, 10, 20, 30, 40, 50),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      x = "Year",
      y = if (show_y_title) "Number of SMR designs" else NULL
    ) +
    theme_minimal(base_size = BASE_SIZE) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
      axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
      axis.ticks = element_blank(),
      axis.text = element_text(colour = "black", size = 6.5),
      axis.title.x = element_text(colour = "black", size = 7),
      axis.title.y = element_text(colour = "black", size = 7),
      legend.position = "none",
      plot.margin = margin(6, 6, 6, 6)
    )
  
  ggdraw(p) +
    draw_plot_label(
      label = panel_tag,
      x = 0.01,
      y = 0.99,
      hjust = 0,
      vjust = 1,
      size = 11,
      fontface = "bold"
    )
}

# ------------------------------------------------------------
# READ DATA
# ------------------------------------------------------------

sheet_name <- readxl::excel_sheets(FILE)
sheet_name <- sheet_name[grepl("future_pipeline", tolower(sheet_name))][1]

if (is.na(sheet_name)) {
  stop("Could not find a sheet matching 'future_pipeline'.")
}

dat <- readxl::read_excel(FILE, sheet = sheet_name)
names(dat) <- clean_names(names(dat))

required_cols <- c(
  "design",
  "scenario",
  "entry_year",
  "fail_phase",
  "cd_start",
  "bd_start",
  "dd_start",
  "licensing_start",
  "construction_start",
  "operational_start",
  "foak_year"
)

missing_cols <- setdiff(required_cols, names(dat))

if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

dat <- dat %>%
  dplyr::mutate(
    dplyr::across(
      c(
        entry_year,
        cd_start,
        bd_start,
        dd_start,
        licensing_start,
        construction_start,
        operational_start,
        foak_year
      ),
      num_or_na
    ),
    design = as.character(design),
    scenario = tolower(trimws(as.character(scenario))),
    fail_phase = trimws(as.character(fail_phase)),
    fail_phase = ifelse(fail_phase %in% c("", "NA", "NaN"), NA, fail_phase)
  )

# ------------------------------------------------------------
# BUILD PANELS
# ------------------------------------------------------------

baseline_dat <- build_panel_data(
  dat = dat,
  scenario_name = "baseline",
  year_min = YEAR_MIN,
  year_max = BASELINE_X_MAX
)

accelerated_dat <- build_panel_data(
  dat = dat,
  scenario_name = "accelerated",
  year_min = YEAR_MIN,
  year_max = ACCEL_X_MAX
)

panel_e_plot <- make_panel_plot(
  phase_counts = baseline_dat$phase_counts,
  fail_counts = baseline_dat$fail_counts,
  x_limits = c(YEAR_MIN, BASELINE_X_MAX),
  x_breaks = c(2030, 2040, 2050, 2060, 2070),
  panel_tag = "e",
  show_y_title = TRUE
)

panel_f_plot <- make_panel_plot(
  phase_counts = accelerated_dat$phase_counts,
  fail_counts = accelerated_dat$fail_counts,
  x_limits = c(YEAR_MIN, ACCEL_X_MAX),
  x_breaks = c(2030, 2040, 2050, 2060),
  panel_tag = "f",
  show_y_title = TRUE
)

# ------------------------------------------------------------
# SHARED PHASE LEGEND
# ------------------------------------------------------------

legend_df <- data.frame(
  phase = factor(names(phase_cols), levels = names(phase_cols)),
  x = 1,
  y = 1
)

legend_plot <- ggplot(legend_df, aes(x = x, y = y, fill = phase)) +
  geom_tile() +
  scale_fill_manual(
    values = phase_cols,
    breaks = names(phase_cols),
    labels = unname(phase_labels),
    name = "Phase"
  ) +
  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE,
      reverse = FALSE
    )
  ) +
  theme_void(base_size = 7) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 6.5, face = "plain"),
    legend.text = element_text(size = 6),
    legend.key.width = unit(8, "mm"),
    legend.key.height = unit(3.5, "mm"),
    legend.spacing.x = unit(2, "mm"),
    legend.box.spacing = unit(0, "mm"),
    legend.margin = margin(0, 0, 0, 0)
  )

shared_legend <- cowplot::get_legend(legend_plot)

# ------------------------------------------------------------
# COMBINE PANELS E + F
# ------------------------------------------------------------

combined_top <- plot_grid(
  panel_e_plot,
  panel_f_plot,
  nrow = 1,
  rel_widths = c(1, 1),
  align = "h",
  axis = "tb"
)

combined_figure <- plot_grid(
  combined_top,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.12)
)

print(combined_figure)

# ------------------------------------------------------------
# SAVE
# ------------------------------------------------------------

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".png")),
  plot = combined_figure,
  width = COMBINED_W_MM,
  height = COMBINED_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white",
  limitsize = FALSE
)

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".pdf")),
  plot = combined_figure,
  width = COMBINED_W_MM,
  height = COMBINED_H_MM,
  units = "mm",
  bg = "white",
  limitsize = FALSE
)

saveRDS(
  combined_figure,
  file = file.path(OUT_DIR, paste0(FILE_STUB, ".rds"))
)

saveRDS(
  panel_e_plot,
  file = file.path(OUT_DIR, "fig02_panel_e_future_pipeline_baseline.rds")
)

saveRDS(
  panel_f_plot,
  file = file.path(OUT_DIR, "fig02_panel_f_future_pipeline_accelerated.rds")
)