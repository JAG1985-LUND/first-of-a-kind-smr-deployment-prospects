# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Figure 5: projected FOAK SMR deployment by OECD membership
# and reactor cooling type.
#
# Required input:
# data/Main_Figures/fig05_source_data_oecd_cooling.xlsx
#   - sheet: oecd_membership
#   - sheet: cooling_type
#
# Outputs:
# outputs/Figure_05/fig05_oecd_cooling.png
# outputs/Figure_05/fig05_oecd_cooling.pdf
# outputs/Figure_05/fig05_oecd_cooling.rds

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
  library(grid)
})

rm(list = ls())
graphics.off()

# ============================================================
# FIGURE 5 — PROJECTED FOAK SMR DEPLOYMENT BY OECD MEMBERSHIP
# AND REACTOR COOLING TYPE
# ============================================================

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

SOURCE_FILE <- file.path("data", "Main_Figures", "fig05_source_data_oecd_cooling.xlsx")

find_repo_root <- function(start_dir = getwd(), target_file = SOURCE_FILE) {
  current <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  
  repeat {
    if (file.exists(file.path(current, target_file))) {
      return(current)
    }
    
    parent <- dirname(current)
    
    if (identical(parent, current)) {
      stop(
        "Could not locate repository root.\n",
        "Expected to find: ", target_file, "\n",
        "Start directory was: ", normalizePath(start_dir, winslash = "/", mustWork = TRUE), "\n\n",
        "Run this script from inside the unzipped GitHub repository, or set the working directory to the repository root."
      )
    }
    
    current <- parent
  }
}

ROOT_DIR <- find_repo_root()

FILE <- file.path(ROOT_DIR, SOURCE_FILE)
OUT_DIR <- file.path(ROOT_DIR, "outputs", "Figure_05")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(FILE)) {
  stop("Input file not found: ", FILE)
}

available_sheets <- readxl::excel_sheets(FILE)

required_sheets <- c(
  "oecd_membership",
  "cooling_type"
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
# TARGETED EXPORT NAME
# ------------------------------------------------------------

FIGURE_TITLE <- "oecd_cooling"

make_slug <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}

FIGURE_STEM <- paste0("fig05_", make_slug(FIGURE_TITLE))

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 180
SAVE_H_MM <- 180
SAVE_DPI  <- 600

# ------------------------------------------------------------
# TYPOGRAPHY
# ------------------------------------------------------------

BASE_SIZE         <- 7
AXIS_TEXT_SIZE    <- 6.5
AXIS_TITLE_SIZE   <- 7
LEGEND_TEXT_SIZE  <- 6.2
LEGEND_TITLE_SIZE <- 7
TITLE_SIZE        <- 7
TAG_SIZE          <- 12

# ------------------------------------------------------------
# LINE WIDTHS
# ------------------------------------------------------------

LW_SMALL <- 0.75

# ------------------------------------------------------------
# COLOURS
# ------------------------------------------------------------

col_oecd_line    <- "#1b9e77"
col_oecd_fill    <- "#1b9e77"
col_nonoecd_line <- "#A67C00"
col_nonoecd_fill <- "#A67C00"

col_water_line <- "#2C72B0"
col_water_fill <- "#8DBBE8"
col_amr_line   <- "#C45A32"
col_amr_fill   <- "#E79A73"

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

base_theme <- theme_classic(base_size = BASE_SIZE) +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks  = element_line(colour = "grey35", linewidth = 0.3),
    axis.text.x = element_text(size = AXIS_TEXT_SIZE, colour = "grey20"),
    axis.text.y = element_text(size = AXIS_TEXT_SIZE, colour = "grey20"),
    axis.title.x = element_text(size = AXIS_TITLE_SIZE, margin = margin(t = 4)),
    axis.title.y = element_text(size = AXIS_TITLE_SIZE),
    plot.title = element_blank(),
    legend.title = element_text(size = LEGEND_TITLE_SIZE),
    legend.text  = element_text(size = LEGEND_TEXT_SIZE),
    legend.position = c(0.06, 0.96),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key = element_rect(fill = NA, colour = NA),
    legend.spacing.y = unit(0.02, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    plot.margin = margin(6, 6, 6, 6),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

read_disaggregated_sheet <- function(sheet, group_type = c("oecd", "cooling")) {
  group_type <- match.arg(group_type)
  
  raw <- readxl::read_excel(FILE, sheet = sheet) %>%
    dplyr::select(dplyr::where(~ !all(is.na(.x))))
  
  names(raw) <- clean_names_local(names(raw))
  
  if (ncol(raw) < 8) {
    stop(
      "Expected at least eight columns in sheet: ",
      sheet,
      "\nAvailable columns are:\n",
      paste(names(raw), collapse = "\n")
    )
  }
  
  if (!"year" %in% names(raw)) {
    names(raw)[1] <- "year"
  }
  
  names(raw)[2] <- "group"
  
  metric_cols <- names(raw)[3:ncol(raw)]
  
  if (length(metric_cols) < 6) {
    stop(
      "Expected at least six metric columns in sheet: ",
      sheet,
      "\nAvailable columns are:\n",
      paste(names(raw), collapse = "\n")
    )
  }
  
  metric_cols <- metric_cols[1:6]
  
  out <- raw %>%
    transmute(
      year = as.numeric(year),
      group = as.character(group),
      baseline_p10 = as.numeric(.data[[metric_cols[1]]]),
      baseline_p50 = as.numeric(.data[[metric_cols[2]]]),
      baseline_p90 = as.numeric(.data[[metric_cols[3]]]),
      accelerated_p10 = as.numeric(.data[[metric_cols[4]]]),
      accelerated_p50 = as.numeric(.data[[metric_cols[5]]]),
      accelerated_p90 = as.numeric(.data[[metric_cols[6]]])
    ) %>%
    filter(!is.na(year), !is.na(group))
  
  if (group_type == "oecd") {
    out <- out %>%
      mutate(
        group = case_when(
          str_detect(str_to_lower(group), "non") ~ "Non-OECD",
          str_detect(str_to_lower(group), "oecd") ~ "OECD",
          TRUE ~ group
        ),
        group = factor(group, levels = c("OECD", "Non-OECD"))
      )
  }
  
  if (group_type == "cooling") {
    out <- out %>%
      mutate(
        group = case_when(
          str_detect(str_to_lower(group), "water") ~ "Water-cooled",
          str_detect(str_to_lower(group), "amr|advanced") ~ "AMRs",
          TRUE ~ group
        ),
        group = factor(group, levels = c("Water-cooled", "AMRs"))
      )
  }
  
  bind_rows(
    out %>%
      transmute(
        year,
        group,
        scenario = "Baseline scenario",
        p10 = baseline_p10,
        p50 = baseline_p50,
        p90 = baseline_p90
      ),
    out %>%
      transmute(
        year,
        group,
        scenario = "Accelerated scenario",
        p10 = accelerated_p10,
        p50 = accelerated_p50,
        p90 = accelerated_p90
      )
  )
}

make_disaggregated_panel <- function(data, scenario_name, legend_title, colour_values, fill_values) {
  data_plot <- data %>%
    filter(scenario == scenario_name) %>%
    arrange(group, year)
  
  ggplot(data_plot) +
    geom_ribbon(
      aes(x = year, ymin = p10, ymax = p90, fill = group),
      alpha = 0.18,
      colour = NA
    ) +
    geom_step(
      aes(x = year, y = p50, colour = group),
      direction = "hv",
      linewidth = LW_SMALL,
      lineend = "round"
    ) +
    scale_colour_manual(
      name = legend_title,
      values = colour_values,
      drop = FALSE
    ) +
    scale_fill_manual(
      name = legend_title,
      values = fill_values,
      drop = FALSE
    ) +
    scale_x_continuous(
      limits = c(2018, 2050.4),
      breaks = c(2020, 2030, 2040, 2050),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 42),
      breaks = c(0, 10, 20, 30, 40),
      labels = hide_zero,
      expand = c(0, 0)
    ) +
    labs(
      x = "Year",
      y = "Median cumulative FOAK SMRs"
    ) +
    base_theme +
    theme(
      legend.position = c(0.05, 0.92),
      legend.justification = c(0, 1),
      legend.key.width = unit(0.45, "cm"),
      legend.key.height = unit(0.28, "cm"),
      legend.background = element_rect(fill = NA, colour = NA),
      legend.box.background = element_rect(fill = NA, colour = NA)
    ) +
    guides(
      fill = "none",
      colour = guide_legend(
        override.aes = list(linewidth = 1.0)
      )
    )
}

# ============================================================
# PANELS A-B — OECD MEMBERSHIP
# ============================================================

oecd_data <- read_disaggregated_sheet(
  sheet = "oecd_membership",
  group_type = "oecd"
)

oecd_colour_values <- c(
  "OECD" = col_oecd_line,
  "Non-OECD" = col_nonoecd_line
)

oecd_fill_values <- c(
  "OECD" = col_oecd_fill,
  "Non-OECD" = col_nonoecd_fill
)

p_a_core <- make_disaggregated_panel(
  data = oecd_data,
  scenario_name = "Baseline scenario",
  legend_title = "OECD membership",
  colour_values = oecd_colour_values,
  fill_values = oecd_fill_values
)

p_a <- add_panel_decorations(
  p_a_core,
  "a",
  "Baseline scenario",
  title_x = 0.56,
  title_y = 0.982
)

p_b_core <- make_disaggregated_panel(
  data = oecd_data,
  scenario_name = "Accelerated scenario",
  legend_title = "OECD membership",
  colour_values = oecd_colour_values,
  fill_values = oecd_fill_values
)

p_b <- add_panel_decorations(
  p_b_core,
  "b",
  "Accelerated scenario",
  title_x = 0.56,
  title_y = 0.982
)

# ============================================================
# PANELS C-D — REACTOR COOLING TYPE
# ============================================================

cooling_data <- read_disaggregated_sheet(
  sheet = "cooling_type",
  group_type = "cooling"
)

cooling_colour_values <- c(
  "Water-cooled" = col_water_line,
  "AMRs" = col_amr_line
)

cooling_fill_values <- c(
  "Water-cooled" = col_water_fill,
  "AMRs" = col_amr_fill
)

p_c_core <- make_disaggregated_panel(
  data = cooling_data,
  scenario_name = "Baseline scenario",
  legend_title = "Reactor cooling type",
  colour_values = cooling_colour_values,
  fill_values = cooling_fill_values
)

p_c <- add_panel_decorations(
  p_c_core,
  "c",
  "Baseline scenario",
  title_x = 0.56,
  title_y = 0.982
)

p_d_core <- make_disaggregated_panel(
  data = cooling_data,
  scenario_name = "Accelerated scenario",
  legend_title = "Reactor cooling type",
  colour_values = cooling_colour_values,
  fill_values = cooling_fill_values
)

p_d <- add_panel_decorations(
  p_d_core,
  "d",
  "Accelerated scenario",
  title_x = 0.56,
  title_y = 0.982
)

# ============================================================
# COMBINE PANELS
# ============================================================

top_row <- plot_grid(
  p_a, p_b,
  ncol = 2,
  rel_widths = c(1, 1),
  align = "hv",
  axis = "tblr"
)

bottom_row <- plot_grid(
  p_c, p_d,
  ncol = 2,
  rel_widths = c(1, 1),
  align = "hv",
  axis = "tblr"
)

fig05_oecd_cooling <- plot_grid(
  top_row,
  bottom_row,
  ncol = 1,
  rel_heights = c(1, 1),
  align = "v",
  axis = "tblr"
)

print(fig05_oecd_cooling)

# ============================================================
# EXPORT
# ============================================================

ggsave(
  filename = file.path(OUT_DIR, paste0(FIGURE_STEM, ".png")),
  plot = fig05_oecd_cooling,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, paste0(FIGURE_STEM, ".pdf")),
  plot = fig05_oecd_cooling,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  device = grDevices::cairo_pdf,
  bg = "white"
)

saveRDS(
  fig05_oecd_cooling,
  file = file.path(OUT_DIR, paste0(FIGURE_STEM, ".rds"))
)

fig05_oecd_cooling
