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
# FIGURE 7 — OECD MEMBERSHIP AND REACTOR COOLING TYPE
#
# GITHUB-REPRODUCIBLE VERSION
#
# a) OECD vs non-OECD — Reference scenario
# b) OECD vs non-OECD — Proliferation scenario
# c) Water-cooled vs AMRs — Reference scenario
# d) Water-cooled vs AMRs — Proliferation scenario
#
# Revised manuscript
# ============================================================


# ------------------------------------------------------------
# REPOSITORY PATHS
# ------------------------------------------------------------

find_repo_root <- function(start = getwd()) {
  
  current <- normalizePath(
    start,
    winslash = "/",
    mustWork = TRUE
  )
  
  repeat {
    
    source_file <- file.path(
      current,
      "revision",
      "data",
      "Main_Figures",
      "fig07_source_data_oecd_cooling.xlsx"
    )
    
    if (file.exists(source_file)) {
      return(current)
    }
    
    parent <- dirname(current)
    
    if (identical(parent, current)) {
      
      stop(
        paste0(
          "Could not locate repository root.\n",
          "Expected to find: ",
          "revision/data/Main_Figures/",
          "fig07_source_data_oecd_cooling.xlsx\n",
          "Start directory was: ",
          start,
          "\n\n",
          "Run this script from inside the GitHub repository."
        )
      )
    }
    
    current <- parent
  }
}


ROOT_DIR <- find_repo_root()


FILE <- file.path(
  ROOT_DIR,
  "revision",
  "data",
  "Main_Figures",
  "fig07_source_data_oecd_cooling.xlsx"
)


OUT_DIR <- file.path(
  ROOT_DIR,
  "revision",
  "outputs",
  "Main_Figures"
)


dir.create(
  OUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


if (!file.exists(FILE)) {
  
  stop(
    "Input file not found: ",
    FILE
  )
}


# ------------------------------------------------------------
# REQUIRED SHEETS
# ------------------------------------------------------------

available_sheets <- excel_sheets(
  FILE
)


required_sheets <- c(
  "oecd_membership",
  "cooling_type"
)


missing_sheets <- required_sheets[
  !required_sheets %in%
    available_sheets
]


if (length(missing_sheets) > 0) {
  
  stop(
    "Missing required sheet(s) in Figure 7 source workbook:\n",
    paste(
      missing_sheets,
      collapse = "\n"
    ),
    "\n\nAvailable sheets are:\n",
    paste(
      available_sheets,
      collapse = "\n"
    )
  )
}


# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 180
SAVE_H_MM <- 180
SAVE_DPI  <- 600

FILE_STUB <- "fig07_oecd_cooling"


# ------------------------------------------------------------
# TYPOGRAPHY
# ------------------------------------------------------------

BASE_SIZE         <- 7
AXIS_TEXT_SIZE    <- 6.5
AXIS_TITLE_SIZE   <- 7
LEGEND_TEXT_SIZE  <- 6.2
LEGEND_TITLE_SIZE <- 7
TITLE_SIZE        <- 7
TAG_SIZE          <- 11


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

col_amr_line <- "#C45A32"
col_amr_fill <- "#E79A73"


# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------

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


hide_zero <- function(x) {
  
  ifelse(
    x == 0,
    "",
    as.character(x)
  )
}


add_panel_decorations <- function(
    plot_obj,
    tag,
    title_text,
    title_x = 0.56,
    title_y = 0.982
) {
  
  ggdraw(
    plot_obj
  ) +
    
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


base_theme <- theme_classic(
  base_size = BASE_SIZE
) +
  
  theme(
    
    panel.grid = element_blank(),
    
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
    
    axis.text.x = element_text(
      size = AXIS_TEXT_SIZE,
      colour = "grey20"
    ),
    
    axis.text.y = element_text(
      size = AXIS_TEXT_SIZE,
      colour = "grey20"
    ),
    
    axis.title.x = element_text(
      size = AXIS_TITLE_SIZE,
      margin = margin(t = 4)
    ),
    
    axis.title.y = element_text(
      size = AXIS_TITLE_SIZE
    ),
    
    plot.title = element_blank(),
    
    legend.title = element_text(
      size = LEGEND_TITLE_SIZE
    ),
    
    legend.text = element_text(
      size = LEGEND_TEXT_SIZE
    ),
    
    legend.position = c(
      0.06,
      0.96
    ),
    
    legend.justification = c(
      0,
      1
    ),
    
    legend.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    legend.key = element_rect(
      fill = NA,
      colour = NA
    ),
    
    legend.spacing.y = unit(
      0.02,
      "cm"
    ),
    
    legend.margin = margin(
      0,
      0,
      0,
      0
    ),
    
    legend.box.margin = margin(
      0,
      0,
      0,
      0
    ),
    
    plot.margin = margin(
      6,
      6,
      6,
      6
    ),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    panel.background = element_rect(
      fill = "white",
      colour = NA
    )
  )


# ------------------------------------------------------------
# READ AND RESHAPE SOURCE DATA
# ------------------------------------------------------------

read_disaggregated_sheet <- function(
    sheet,
    group_type = c(
      "oecd",
      "cooling"
    )
) {
  
  group_type <- match.arg(
    group_type
  )
  
  raw <- readxl::read_excel(
    FILE,
    sheet = sheet
  ) %>%
    
    dplyr::select(
      dplyr::where(
        ~ !all(is.na(.x))
      )
    )
  
  names(raw) <- clean_names_local(
    names(raw)
  )
  
  if (!"year" %in% names(raw)) {
    
    names(raw)[1] <- "year"
  }
  
  names(raw)[2] <- "group"
  
  metric_cols <- names(raw)[
    3:ncol(raw)
  ]
  
  if (length(metric_cols) < 6) {
    
    stop(
      paste0(
        "Expected at least six metric columns in sheet: ",
        sheet
      )
    )
  }
  
  metric_cols <- metric_cols[
    1:6
  ]
  
  
  # ----------------------------------------------------------
  # Keep original workbook Baseline / Accelerated columns.
  # Only manuscript-facing scenario terminology is changed.
  # ----------------------------------------------------------
  
  out <- raw %>%
    
    transmute(
      
      year =
        as.numeric(year),
      
      group =
        as.character(group),
      
      baseline_p10 =
        as.numeric(
          .data[[metric_cols[1]]]
        ),
      
      baseline_p50 =
        as.numeric(
          .data[[metric_cols[2]]]
        ),
      
      baseline_p90 =
        as.numeric(
          .data[[metric_cols[3]]]
        ),
      
      accelerated_p10 =
        as.numeric(
          .data[[metric_cols[4]]]
        ),
      
      accelerated_p50 =
        as.numeric(
          .data[[metric_cols[5]]]
        ),
      
      accelerated_p90 =
        as.numeric(
          .data[[metric_cols[6]]]
        )
    ) %>%
    
    filter(
      !is.na(year),
      !is.na(group)
    )
  
  
  # ----------------------------------------------------------
  # OECD MEMBERSHIP
  # ----------------------------------------------------------
  
  if (group_type == "oecd") {
    
    out <- out %>%
      
      mutate(
        
        group = case_when(
          
          str_detect(
            str_to_lower(group),
            "non"
          ) ~ "Non-OECD",
          
          str_detect(
            str_to_lower(group),
            "oecd"
          ) ~ "OECD",
          
          TRUE ~ group
        ),
        
        group = factor(
          group,
          levels = c(
            "OECD",
            "Non-OECD"
          )
        )
      )
  }
  
  
  # ----------------------------------------------------------
  # REACTOR COOLING TYPE
  # ----------------------------------------------------------
  
  if (group_type == "cooling") {
    
    out <- out %>%
      
      mutate(
        
        group = case_when(
          
          str_detect(
            str_to_lower(group),
            "water"
          ) ~ "Water-cooled",
          
          str_detect(
            str_to_lower(group),
            "amr|advanced"
          ) ~ "AMRs",
          
          TRUE ~ group
        ),
        
        group = factor(
          group,
          levels = c(
            "Water-cooled",
            "AMRs"
          )
        )
      )
  }
  
  
  # ----------------------------------------------------------
  # LONG FORMAT WITH REVISED SCENARIO LABELS
  # ----------------------------------------------------------
  
  bind_rows(
    
    out %>%
      
      transmute(
        year,
        group,
        
        scenario =
          "Reference scenario",
        
        p10 =
          baseline_p10,
        
        p50 =
          baseline_p50,
        
        p90 =
          baseline_p90
      ),
    
    out %>%
      
      transmute(
        year,
        group,
        
        scenario =
          "Proliferation scenario",
        
        p10 =
          accelerated_p10,
        
        p50 =
          accelerated_p50,
        
        p90 =
          accelerated_p90
      )
  )
}


# ------------------------------------------------------------
# PANEL FUNCTION
# ------------------------------------------------------------

make_disaggregated_panel <- function(
    data,
    scenario_name,
    legend_title,
    colour_values,
    fill_values
) {
  
  data_plot <- data %>%
    
    filter(
      scenario ==
        scenario_name
    ) %>%
    
    arrange(
      group,
      year
    )
  
  
  ggplot(
    data_plot
  ) +
    
    geom_ribbon(
      aes(
        x = year,
        ymin = p10,
        ymax = p90,
        fill = group
      ),
      alpha = 0.18,
      colour = NA
    ) +
    
    geom_step(
      aes(
        x = year,
        y = p50,
        colour = group
      ),
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
      limits = c(
        2018,
        2050.4
      ),
      breaks = c(
        2020,
        2030,
        2040,
        2050
      ),
      expand = c(
        0,
        0
      )
    ) +
    
    scale_y_continuous(
      limits = c(
        0,
        42
      ),
      breaks = c(
        0,
        10,
        20,
        30,
        40
      ),
      labels = hide_zero,
      expand = c(
        0,
        0
      )
    ) +
    
    labs(
      x = "Year",
      y = "Median cumulative FOAK SMRs"
    ) +
    
    base_theme +
    
    theme(
      
      legend.position = c(
        0.05,
        0.92
      ),
      
      legend.justification = c(
        0,
        1
      ),
      
      legend.key.width = unit(
        0.45,
        "cm"
      ),
      
      legend.key.height = unit(
        0.28,
        "cm"
      ),
      
      legend.background = element_rect(
        fill = NA,
        colour = NA
      ),
      
      legend.box.background = element_rect(
        fill = NA,
        colour = NA
      )
    ) +
    
    guides(
      
      fill = "none",
      
      colour = guide_legend(
        override.aes = list(
          linewidth = 1.0
        )
      )
    )
}


# ============================================================
# PANELS A–B — OECD MEMBERSHIP
# ============================================================

oecd_data <- read_disaggregated_sheet(
  sheet = "oecd_membership",
  group_type = "oecd"
)


oecd_colour_values <- c(
  "OECD" =
    col_oecd_line,
  
  "Non-OECD" =
    col_nonoecd_line
)


oecd_fill_values <- c(
  "OECD" =
    col_oecd_fill,
  
  "Non-OECD" =
    col_nonoecd_fill
)


# ------------------------------------------------------------
# PANEL A — REFERENCE
# ------------------------------------------------------------

p_a_core <- make_disaggregated_panel(
  data = oecd_data,
  scenario_name = "Reference scenario",
  legend_title = "OECD membership",
  colour_values = oecd_colour_values,
  fill_values = oecd_fill_values
)


p_a <- add_panel_decorations(
  p_a_core,
  "a",
  "Reference scenario",
  title_x = 0.56,
  title_y = 0.982
)


# ------------------------------------------------------------
# PANEL B — PROLIFERATION
# ------------------------------------------------------------

p_b_core <- make_disaggregated_panel(
  data = oecd_data,
  scenario_name = "Proliferation scenario",
  legend_title = "OECD membership",
  colour_values = oecd_colour_values,
  fill_values = oecd_fill_values
)


p_b <- add_panel_decorations(
  p_b_core,
  "b",
  "Proliferation scenario",
  title_x = 0.56,
  title_y = 0.982
)


# ============================================================
# PANELS C–D — REACTOR COOLING TYPE
# ============================================================

cooling_data <- read_disaggregated_sheet(
  sheet = "cooling_type",
  group_type = "cooling"
)


cooling_colour_values <- c(
  "Water-cooled" =
    col_water_line,
  
  "AMRs" =
    col_amr_line
)


cooling_fill_values <- c(
  "Water-cooled" =
    col_water_fill,
  
  "AMRs" =
    col_amr_fill
)


# ------------------------------------------------------------
# PANEL C — REFERENCE
# ------------------------------------------------------------

p_c_core <- make_disaggregated_panel(
  data = cooling_data,
  scenario_name = "Reference scenario",
  legend_title = "Reactor cooling type",
  colour_values = cooling_colour_values,
  fill_values = cooling_fill_values
)


p_c <- add_panel_decorations(
  p_c_core,
  "c",
  "Reference scenario",
  title_x = 0.56,
  title_y = 0.982
)


# ------------------------------------------------------------
# PANEL D — PROLIFERATION
# ------------------------------------------------------------

p_d_core <- make_disaggregated_panel(
  data = cooling_data,
  scenario_name = "Proliferation scenario",
  legend_title = "Reactor cooling type",
  colour_values = cooling_colour_values,
  fill_values = cooling_fill_values
)


p_d <- add_panel_decorations(
  p_d_core,
  "d",
  "Proliferation scenario",
  title_x = 0.56,
  title_y = 0.982
)


# ============================================================
# COMBINE PANELS
# ============================================================

top_row <- plot_grid(
  p_a,
  p_b,
  ncol = 2,
  rel_widths = c(
    1,
    1
  ),
  align = "hv",
  axis = "tblr"
)


bottom_row <- plot_grid(
  p_c,
  p_d,
  ncol = 2,
  rel_widths = c(
    1,
    1
  ),
  align = "hv",
  axis = "tblr"
)


fig07_oecd_cooling <- plot_grid(
  top_row,
  bottom_row,
  ncol = 1,
  rel_heights = c(
    1,
    1
  ),
  align = "v",
  axis = "tblr"
)


print(
  fig07_oecd_cooling
)


# ============================================================
# EXPORT
# ============================================================

PNG_FILE <- file.path(
  OUT_DIR,
  paste0(
    FILE_STUB,
    ".png"
  )
)


PDF_FILE <- file.path(
  OUT_DIR,
  paste0(
    FILE_STUB,
    ".pdf"
  )
)


# ------------------------------------------------------------
# SAVE PNG
# ------------------------------------------------------------

ggsave(
  filename = PNG_FILE,
  plot = fig07_oecd_cooling,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)


# ------------------------------------------------------------
# SAVE PDF
# ------------------------------------------------------------

ggsave(
  filename = PDF_FILE,
  plot = fig07_oecd_cooling,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  device = grDevices::cairo_pdf,
  bg = "white"
)


# ============================================================
# CONFIRM OUTPUTS
# ============================================================

cat(
  "\nFigure 7 successfully generated.\n"
)


cat(
  "Repository root:\n",
  ROOT_DIR,
  "\n\n"
)


cat(
  "Input:\n",
  FILE,
  "\n\n"
)


cat(
  "Outputs:\n",
  PNG_FILE,
  "\n",
  PDF_FILE,
  "\n"
)


fig07_oecd_cooling