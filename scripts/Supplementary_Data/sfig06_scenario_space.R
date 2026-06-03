# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Supplementary Fig. 6: global FOAK SMR deployment across
# reference and sensitivity scenarios.
#
# Panels a-c show cumulative FOAK SMR deployment under the Baseline scenario
# at the P10, P50 and P90 levels, respectively. Panels d-f show cumulative
# FOAK SMR deployment under the Accelerated scenario at the P10, P50 and P90
# levels, respectively.
#
# Required input:
# data/Supplementary_Data/sfig06_source_data_scenario_space.xlsx
#
# Outputs:
# outputs/Supplementary_Figures/Figure_06/sfig06_scenario_space.png
# outputs/Supplementary_Figures/Figure_06/sfig06_scenario_space.pdf
# outputs/Supplementary_Figures/Figure_06/sfig06_scenario_space.rds

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
# SUPPLEMENTARY FIGURE 6
# SIX-PANEL COUNTERFACTUAL SCENARIO SPACE
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
  "sfig06_source_data_scenario_space.xlsx",
  "sfig06_source_data_counterfactual_scenarios.xlsx"
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
  "Figure_06"
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 180
SAVE_H_MM <- 170
SAVE_DPI  <- 600

FILE_STUB <- "sfig06_scenario_space"

# ------------------------------------------------------------
# TYPOGRAPHY
# ------------------------------------------------------------

BASE_SIZE        <- 9
AXIS_TEXT_SIZE   <- 8
AXIS_TITLE_SIZE  <- 8
LEGEND_TEXT_SIZE <- 8
PANEL_TAG_SIZE   <- 11

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
  out <- intersect(candidates, names(df))
  
  if (length(out) == 0) {
    stop(
      "Missing required column for ", label, ". Checked: ",
      paste(candidates, collapse = ", "),
      "\nAvailable columns are: ",
      paste(names(df), collapse = ", ")
    )
  }
  
  out[1]
}

standardise_scenario <- function(x) {
  x <- trimws(as.character(x))
  
  dplyr::case_when(
    x %in% c(
      "reference",
      "Reference",
      "Reference case",
      "reference_case"
    ) ~ "Reference case",
    
    x %in% c(
      "phase_completion",
      "Phase completion",
      "current_phase_completion",
      "Current phase completion",
      "Current-phase completion"
    ) ~ "Current phase completion",
    
    x %in% c(
      "oecd_advance",
      "ADVANCE OECD licensing",
      "advance_oecd_licensing",
      "Advance-OECD licensing",
      "ADVANCE-OECD licensing"
    ) ~ "ADVANCE-OECD licensing",
    
    x %in% c(
      "late_stage_risk",
      "Late stage risk",
      "Late stage risk ",
      "Late-stage risk"
    ) ~ "Late-stage risk",
    
    x %in% c(
      "construction_cascade",
      "Construction cascade"
    ) ~ "Construction cascade",
    
    x %in% c(
      "stalled_development",
      "Stalled Development",
      "Stalled development"
    ) ~ "Stalled development",
    
    TRUE ~ x
  )
}

# ============================================================
# LOAD AND STANDARDISE DATA
# ============================================================

df_raw <- readxl::read_excel(FILE_IN)

names(df_raw) <- clean_names_local(names(df_raw))

year_col <- pick_col(
  df_raw,
  c("year"),
  "year"
)

group_col <- pick_col(
  df_raw,
  c("group"),
  "group"
)

scenario_col <- pick_col(
  df_raw,
  c("scenario"),
  "scenario"
)

p10_baseline_col <- pick_col(
  df_raw,
  c("p10_baseline_scenario", "conservative_baseline_scenario"),
  "baseline P10"
)

p50_baseline_col <- pick_col(
  df_raw,
  c("p50_baseline_scenario", "median_baseline_scenario"),
  "baseline P50"
)

p90_baseline_col <- pick_col(
  df_raw,
  c("p90_baseline_scenario", "optimistic_baseline_scenario"),
  "baseline P90"
)

p10_accelerated_col <- pick_col(
  df_raw,
  c("p10_accelerated_scenario", "conservative_accelerated_scenario"),
  "accelerated P10"
)

p50_accelerated_col <- pick_col(
  df_raw,
  c("p50_accelerated_scenario", "median_accelerated_scenario"),
  "accelerated P50"
)

p90_accelerated_col <- pick_col(
  df_raw,
  c(
    "p90_accelerated_scenario",
    "p90_accerlerated_scenario",
    "optimistic_accelerated_scenario",
    "optimistic_accerlerated_scenario"
  ),
  "accelerated P90"
)

df <- df_raw %>%
  transmute(
    year = as.numeric(.data[[year_col]]),
    group = tolower(trimws(as.character(.data[[group_col]]))),
    scenario = standardise_scenario(.data[[scenario_col]]),
    p10_baseline_scenario = as.numeric(.data[[p10_baseline_col]]),
    p50_baseline_scenario = as.numeric(.data[[p50_baseline_col]]),
    p90_baseline_scenario = as.numeric(.data[[p90_baseline_col]]),
    p10_accelerated_scenario = as.numeric(.data[[p10_accelerated_col]]),
    p50_accelerated_scenario = as.numeric(.data[[p50_accelerated_col]]),
    p90_accelerated_scenario = as.numeric(.data[[p90_accelerated_col]])
  ) %>%
  filter(group == "global") %>%
  arrange(scenario, year)

if (nrow(df) == 0) {
  stop("No rows found where group == 'global'. Check the source workbook.")
}

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
  ) %>%
  filter(!is.na(scenario_label))

if (nrow(df) == 0) {
  stop("No recognised scenario rows found after standardisation.")
}

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
# SAVE SUPPLEMENTARY FIGURE 6
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
  device = grDevices::cairo_pdf,
  bg = "white"
)

saveRDS(
  object = final_plot,
  file = file.path(OUT_DIR, paste0(FILE_STUB, ".rds"))
)

final_plot