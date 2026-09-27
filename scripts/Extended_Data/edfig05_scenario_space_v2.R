# ============================================================
# EXTENDED DATA FIGURE 5 v2
# SENSITIVITY-BASED FEASIBILITY SPACE FOR FOAK SMR DEPLOYMENT
#
# Revised manuscript-facing terminology:
#   Baseline    -> Reference
#   Accelerated -> Proliferation
#
# IMPORTANT:
# Internal source-data names are retained where present
# (e.g. baseline / accelerated). Only manuscript-facing
# terminology is changed.
#
# Required input:
# data/Extended_Data/edfig05_source_data_scenario_space.xlsx
#
# Outputs:
# outputs/Extended_Data/Figure_05/
#   edfig05_scenario_space_v2.png
#   edfig05_scenario_space_v2.pdf
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(cowplot)
  library(grid)
  library(scales)
})

rm(list = ls())
graphics.off()


# ============================================================
# 2. REPOSITORY PATHS
# ============================================================

FILE_IN <- file.path(
  "data",
  "Extended_Data",
  "edfig05_source_data_scenario_space.xlsx"
)

OUT_DIR <- file.path(
  "outputs",
  "Extended_Data",
  "Figure_05"
)

if (!dir.exists(OUT_DIR)) {
  dir.create(
    OUT_DIR,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

if (!file.exists(FILE_IN)) {
  stop(
    paste0(
      "\nSource workbook not found:\n",
      FILE_IN,
      "\n\nRun this script from the repository root."
    )
  )
}


# ============================================================
# 3. OUTPUT FILES
# ============================================================

PNG_FILE <- file.path(
  OUT_DIR,
  "edfig05_scenario_space_v2.png"
)

PDF_FILE <- file.path(
  OUT_DIR,
  "edfig05_scenario_space_v2.pdf"
)


# ============================================================
# 4. EXPORT SETTINGS
# ============================================================

SAVE_W_MM <- 180
SAVE_H_MM <- 100
SAVE_DPI  <- 600


# ============================================================
# 5. TYPOGRAPHY
# ============================================================

BASE_SIZE       <- 8
AXIS_TEXT_SIZE  <- 7
AXIS_TITLE_SIZE <- 7.5
LEGEND_SIZE     <- 6.8
LEGEND_TITLE    <- 7
PANEL_TAG_SIZE  <- 9


# ============================================================
# 6. COLOURS
# ============================================================

COL_REFERENCE <- "black"

COL_REFERENCE_RIBBON <- "grey80"

COL_CURRENT_PHASE <- "#32CD32"

COL_ADVANCE_OECD <- "#1B9E77"

COL_CONSTRUCTION <- "#FF6F4C"

COL_LATE_STAGE <- "#8B1A1A"

COL_STALLED <- "#E53935"


colour_map <- c(
  "Current phase completion" = COL_CURRENT_PHASE,
  "ADVANCE-OECD licensing"   = COL_ADVANCE_OECD,
  "Construction cascade"     = COL_CONSTRUCTION,
  "Late-stage risk"          = COL_LATE_STAGE,
  "Stalled development"      = COL_STALLED
)


scenario_levels <- c(
  "reference",
  "Current phase completion",
  "ADVANCE-OECD licensing",
  "Construction cascade",
  "Late-stage risk",
  "Stalled development"
)


# ============================================================
# 7. COMMON THEME
# ============================================================

base_theme <- theme_classic(base_size = BASE_SIZE) +
  theme(
    axis.line = element_line(
      colour = "grey35",
      linewidth = 0.30
    ),
    
    axis.ticks = element_line(
      colour = "grey35",
      linewidth = 0.30
    ),
    
    axis.text = element_text(
      size = AXIS_TEXT_SIZE,
      colour = "grey20"
    ),
    
    axis.title = element_text(
      size = AXIS_TITLE_SIZE,
      colour = "grey10"
    ),
    
    axis.title.x = element_text(
      margin = margin(t = 3)
    ),
    
    axis.title.y = element_text(
      margin = margin(r = 4)
    ),
    
    legend.position = "none",
    
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    plot.margin = margin(
      t = 3,
      r = 4,
      b = 3,
      l = 3
    )
  )


# ============================================================
# 8. HELPER FUNCTIONS
# ============================================================

clean_names <- function(x) {
  
  x <- tolower(x)
  
  x <- gsub("[^a-z0-9]+", "_", x)
  
  x <- gsub("^_|_$", "", x)
  
  x
}


clean_scenario <- function(x) {
  
  x <- as.character(x)
  
  x <- str_trim(x)
  
  x_low <- tolower(x)
  
  case_when(
    
    x_low %in% c(
      "baseline",
      "reference",
      "reference scenario"
    ) ~ "reference",
    
    str_detect(
      x_low,
      "current.*phase.*completion"
    ) ~ "Current phase completion",
    
    str_detect(
      x_low,
      "advance.*oecd"
    ) ~ "ADVANCE-OECD licensing",
    
    str_detect(
      x_low,
      "construction.*cascade"
    ) ~ "Construction cascade",
    
    str_detect(
      x_low,
      "late.*stage.*risk"
    ) ~ "Late-stage risk",
    
    str_detect(
      x_low,
      "stalled.*development"
    ) ~ "Stalled development",
    
    TRUE ~ x
  )
}


find_col <- function(data_names, candidates) {
  
  hit <- candidates[
    candidates %in% data_names
  ]
  
  if (length(hit) == 0) {
    return(NA_character_)
  }
  
  hit[1]
}


tag_panel <- function(p, tag) {
  
  ggdraw(p) +
    draw_plot_label(
      label = tag,
      x = 0.01,
      y = 0.99,
      hjust = 0,
      vjust = 1,
      size = PANEL_TAG_SIZE,
      fontface = "bold"
    )
}


# ============================================================
# 9. READ WORKBOOK
# ============================================================

sheet_names <- excel_sheets(FILE_IN)

message(
  "Workbook sheets: ",
  paste(sheet_names, collapse = ", ")
)


raw_sheets <- lapply(
  sheet_names,
  function(s) {
    
    x <- read_excel(
      FILE_IN,
      sheet = s
    )
    
    names(x) <- clean_names(names(x))
    
    x$source_sheet <- s
    
    x
  }
)

names(raw_sheets) <- sheet_names


# ============================================================
# 10. IDENTIFY REFERENCE AND PROLIFERATION DATA
# ============================================================

sheet_low <- tolower(sheet_names)


reference_sheet_index <- which(
  str_detect(
    sheet_low,
    "baseline|reference"
  )
)


proliferation_sheet_index <- which(
  str_detect(
    sheet_low,
    "accelerated|proliferation"
  )
)


# If the workbook uses two separate sheets, use those.
# Otherwise, fall back to the first available sheets and
# subsequently identify scenario columns.

if (length(reference_sheet_index) > 0) {
  
  reference_raw <- raw_sheets[
    [reference_sheet_index[1]]
  ]
  
} else {
  
  reference_raw <- raw_sheets[[1]]
}


if (length(proliferation_sheet_index) > 0) {
  
  proliferation_raw <- raw_sheets[
    [proliferation_sheet_index[1]]
  ]
  
} else if (length(raw_sheets) >= 2) {
  
  proliferation_raw <- raw_sheets[[2]]
  
} else {
  
  proliferation_raw <- raw_sheets[[1]]
}


# ============================================================
# 11. STANDARDISE A SCENARIO TABLE
# ============================================================

standardise_table <- function(dat) {
  
  nm <- names(dat)
  
  year_col <- find_col(
    nm,
    c(
      "year",
      "foak_year",
      "calendar_year"
    )
  )
  
  scenario_col <- find_col(
    nm,
    c(
      "scenario",
      "sensitivity",
      "sensitivity_scenario",
      "case",
      "analysis"
    )
  )
  
  p10_col <- find_col(
    nm,
    c(
      "p10",
      "q10",
      "lower",
      "lower_bound",
      "p10_foak"
    )
  )
  
  p50_col <- find_col(
    nm,
    c(
      "p50",
      "median",
      "q50",
      "p50_foak"
    )
  )
  
  p90_col <- find_col(
    nm,
    c(
      "p90",
      "q90",
      "upper",
      "upper_bound",
      "p90_foak"
    )
  )
  
  
  if (
    !is.na(year_col) &&
    !is.na(scenario_col) &&
    !is.na(p50_col)
  ) {
    
    out <- dat %>%
      transmute(
        year = as.numeric(.data[[year_col]]),
        
        scenario = clean_scenario(
          .data[[scenario_col]]
        ),
        
        p10 = if (!is.na(p10_col)) {
          as.numeric(.data[[p10_col]])
        } else {
          NA_real_
        },
        
        p50 = as.numeric(
          .data[[p50_col]]
        ),
        
        p90 = if (!is.na(p90_col)) {
          as.numeric(.data[[p90_col]])
        } else {
          NA_real_
        }
      )
    
    return(out)
  }
  
  
  # ----------------------------------------------------------
  # Alternative structure:
  # one year column + scenario-specific columns
  # ----------------------------------------------------------
  
  if (is.na(year_col)) {
    
    stop(
      "Could not identify the year column in the source workbook."
    )
  }
  
  
  long <- dat %>%
    select(-any_of("source_sheet")) %>%
    pivot_longer(
      cols = -all_of(year_col),
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(
      year = as.numeric(
        .data[[year_col]]
      ),
      value = suppressWarnings(
        as.numeric(value)
      )
    )
  
  
  long <- long %>%
    mutate(
      
      quantile = case_when(
        
        str_detect(variable, "p10|q10|lower") ~ "p10",
        
        str_detect(variable, "p50|median|q50") ~ "p50",
        
        str_detect(variable, "p90|q90|upper") ~ "p90",
        
        TRUE ~ NA_character_
      ),
      
      scenario = case_when(
        
        str_detect(
          variable,
          "baseline|reference"
        ) ~ "reference",
        
        str_detect(
          variable,
          "current.*phase"
        ) ~ "Current phase completion",
        
        str_detect(
          variable,
          "advance.*oecd"
        ) ~ "ADVANCE-OECD licensing",
        
        str_detect(
          variable,
          "construction.*cascade"
        ) ~ "Construction cascade",
        
        str_detect(
          variable,
          "late.*stage"
        ) ~ "Late-stage risk",
        
        str_detect(
          variable,
          "stalled"
        ) ~ "Stalled development",
        
        TRUE ~ NA_character_
      )
    ) %>%
    filter(
      !is.na(quantile),
      !is.na(scenario)
    )
  
  
  out <- long %>%
    select(
      year,
      scenario,
      quantile,
      value
    ) %>%
    pivot_wider(
      names_from = quantile,
      values_from = value
    )
  
  
  if (!"p10" %in% names(out)) {
    out$p10 <- NA_real_
  }
  
  if (!"p50" %in% names(out)) {
    out$p50 <- NA_real_
  }
  
  if (!"p90" %in% names(out)) {
    out$p90 <- NA_real_
  }
  
  
  out %>%
    select(
      year,
      scenario,
      p10,
      p50,
      p90
    )
}


# ============================================================
# 12. STANDARDISE BOTH PANELS
# ============================================================

reference_data <- standardise_table(
  reference_raw
)


proliferation_data <- standardise_table(
  proliferation_raw
)


reference_data <- reference_data %>%
  filter(
    scenario %in% scenario_levels
  )


proliferation_data <- proliferation_data %>%
  filter(
    scenario %in% scenario_levels
  )


# ============================================================
# 13. CHECK DATA
# ============================================================

if (
  nrow(reference_data) == 0 ||
  nrow(proliferation_data) == 0
) {
  
  stop(
    "No usable scenario data were identified in the workbook."
  )
}


if (
  !"reference" %in% reference_data$scenario ||
  !"reference" %in% proliferation_data$scenario
) {
  
  stop(
    paste0(
      "The Reference trajectory could not be identified. ",
      "Check the source workbook scenario labels."
    )
  )
}


# ============================================================
# 14. REFERENCE TRAJECTORIES
# ============================================================

ref_reference <- reference_data %>%
  filter(
    scenario == "reference"
  ) %>%
  arrange(year)


prolif_reference <- proliferation_data %>%
  filter(
    scenario == "reference"
  ) %>%
  arrange(year)


# ============================================================
# 15. SENSITIVITY DATA
# ============================================================

reference_sens <- reference_data %>%
  filter(
    scenario != "reference"
  ) %>%
  mutate(
    scenario = factor(
      scenario,
      levels = scenario_levels[-1]
    )
  )


proliferation_sens <- proliferation_data %>%
  filter(
    scenario != "reference"
  ) %>%
  mutate(
    scenario = factor(
      scenario,
      levels = scenario_levels[-1]
    )
  )


# ============================================================
# 16. COMMON AXES
# ============================================================

all_years <- c(
  reference_data$year,
  proliferation_data$year
)


X_MIN <- min(
  all_years,
  na.rm = TRUE
)

X_MAX <- max(
  all_years,
  na.rm = TRUE
)


if (
  X_MIN <= 2020 &&
  X_MAX >= 2050
) {
  
  X_BREAKS <- c(
    2020,
    2030,
    2040,
    2050
  )
  
} else {
  
  X_BREAKS <- pretty(
    c(X_MIN, X_MAX),
    n = 4
  )
}


all_y <- c(
  reference_data$p10,
  reference_data$p50,
  reference_data$p90,
  proliferation_data$p10,
  proliferation_data$p50,
  proliferation_data$p90
)


Y_MAX <- ceiling(
  max(
    all_y,
    na.rm = TRUE
  ) / 10
) * 10


Y_BREAKS <- seq(
  10,
  Y_MAX,
  by = 10
)


# ============================================================
# 17. PANEL FUNCTION
# ============================================================

make_panel <- function(
    reference_df,
    sensitivity_df,
    median_label
) {
  
  p <- ggplot()
  
  
  # ----------------------------------------------------------
  # Reference P10-P90 uncertainty
  # ----------------------------------------------------------
  
  if (
    any(!is.na(reference_df$p10)) &&
    any(!is.na(reference_df$p90))
  ) {
    
    p <- p +
      geom_ribbon(
        data = reference_df,
        aes(
          x = year,
          ymin = p10,
          ymax = p90
        ),
        fill = COL_REFERENCE_RIBBON,
        alpha = 0.85
      )
  }
  
  
  # ----------------------------------------------------------
  # Sensitivity median trajectories
  # ----------------------------------------------------------
  
  p <- p +
    geom_step(
      data = sensitivity_df,
      aes(
        x = year,
        y = p50,
        colour = scenario,
        group = scenario
      ),
      direction = "hv",
      linewidth = 0.65,
      na.rm = TRUE
    )
  
  
  # ----------------------------------------------------------
  # Reference median trajectory
  # ----------------------------------------------------------
  
  p <- p +
    geom_step(
      data = reference_df,
      aes(
        x = year,
        y = p50
      ),
      direction = "hv",
      colour = COL_REFERENCE,
      linewidth = 1.05,
      na.rm = TRUE
    )
  
  
  p +
    scale_colour_manual(
      values = colour_map,
      drop = FALSE
    ) +
    scale_x_continuous(
      limits = c(
        X_MIN,
        X_MAX
      ),
      breaks = X_BREAKS,
      expand = expansion(
        mult = c(
          0.01,
          0.01
        )
      )
    ) +
    scale_y_continuous(
      limits = c(
        0,
        Y_MAX
      ),
      breaks = Y_BREAKS,
      expand = expansion(
        mult = c(
          0,
          0.02
        )
      )
    ) +
    labs(
      x = "Year",
      y = "Cumulative FOAK SMRs"
    ) +
    base_theme
}


# ============================================================
# 18. CREATE PANEL a
# ============================================================

p_a_core <- make_panel(
  reference_df = ref_reference,
  sensitivity_df = reference_sens,
  median_label = "Median reference trajectory"
)


# ============================================================
# 19. CREATE PANEL b
# ============================================================

p_b_core <- make_panel(
  reference_df = prolif_reference,
  sensitivity_df = proliferation_sens,
  median_label = "Median proliferation trajectory"
)


# ============================================================
# 20. MANUAL LEGEND SETTINGS
# ============================================================

LEGEND_X_LEFT <- X_MIN + 0.8

LEGEND_Y_TOP <- Y_MAX - 2.0

LEGEND_ROW_GAP <- Y_MAX * 0.038

LEGEND_KEY_WIDTH <- (X_MAX - X_MIN) * 0.055


# ============================================================
# 21. PANEL a LEGEND
# ============================================================

p_a_core <- p_a_core +
  
  annotate(
    "text",
    x = LEGEND_X_LEFT,
    y = LEGEND_Y_TOP,
    label = "Sensitivity-based feasibility space",
    hjust = 0,
    vjust = 1,
    size = 2.55
  ) +
  
  annotate(
    "segment",
    x = LEGEND_X_LEFT,
    xend = LEGEND_X_LEFT + LEGEND_KEY_WIDTH,
    y = LEGEND_Y_TOP - LEGEND_ROW_GAP,
    yend = LEGEND_Y_TOP - LEGEND_ROW_GAP,
    colour = COL_REFERENCE,
    linewidth = 1.05
  ) +
  
  annotate(
    "text",
    x = LEGEND_X_LEFT + LEGEND_KEY_WIDTH * 1.30,
    y = LEGEND_Y_TOP - LEGEND_ROW_GAP,
    label = "Median reference trajectory",
    hjust = 0,
    vjust = 0.5,
    size = 2.35
  ) +
  
  annotate(
    "rect",
    xmin = LEGEND_X_LEFT,
    xmax = LEGEND_X_LEFT + LEGEND_KEY_WIDTH,
    ymin = LEGEND_Y_TOP - 2.30 * LEGEND_ROW_GAP,
    ymax = LEGEND_Y_TOP - 1.70 * LEGEND_ROW_GAP,
    fill = COL_REFERENCE_RIBBON,
    colour = NA
  ) +
  
  annotate(
    "text",
    x = LEGEND_X_LEFT + LEGEND_KEY_WIDTH * 1.30,
    y = LEGEND_Y_TOP - 2.0 * LEGEND_ROW_GAP,
    label = "Reference uncertainty (P10-P90)",
    hjust = 0,
    vjust = 0.5,
    size = 2.35
  )


legend_rows <- c(
  "Current phase completion",
  "ADVANCE-OECD licensing",
  "Construction cascade",
  "Late-stage risk",
  "Stalled development"
)


for (i in seq_along(legend_rows)) {
  
  y_pos <- LEGEND_Y_TOP -
    (i + 2) * LEGEND_ROW_GAP
  
  p_a_core <- p_a_core +
    
    annotate(
      "segment",
      x = LEGEND_X_LEFT,
      xend = LEGEND_X_LEFT + LEGEND_KEY_WIDTH,
      y = y_pos,
      yend = y_pos,
      colour = colour_map[
        legend_rows[i]
      ],
      linewidth = 0.65
    ) +
    
    annotate(
      "text",
      x = LEGEND_X_LEFT +
        LEGEND_KEY_WIDTH * 1.30,
      y = y_pos,
      label = legend_rows[i],
      hjust = 0,
      vjust = 0.5,
      size = 2.35
    )
}


# ============================================================
# 22. PANEL b LEGEND
# ============================================================

p_b_core <- p_b_core +
  
  annotate(
    "text",
    x = LEGEND_X_LEFT,
    y = LEGEND_Y_TOP,
    label = "Sensitivity-based feasibility space",
    hjust = 0,
    vjust = 1,
    size = 2.55
  ) +
  
  annotate(
    "segment",
    x = LEGEND_X_LEFT,
    xend = LEGEND_X_LEFT + LEGEND_KEY_WIDTH,
    y = LEGEND_Y_TOP - LEGEND_ROW_GAP,
    yend = LEGEND_Y_TOP - LEGEND_ROW_GAP,
    colour = COL_REFERENCE,
    linewidth = 1.05
  ) +
  
  annotate(
    "text",
    x = LEGEND_X_LEFT +
      LEGEND_KEY_WIDTH * 1.30,
    y = LEGEND_Y_TOP - LEGEND_ROW_GAP,
    label = "Median proliferation trajectory",
    hjust = 0,
    vjust = 0.5,
    size = 2.35
  ) +
  
  annotate(
    "rect",
    xmin = LEGEND_X_LEFT,
    xmax = LEGEND_X_LEFT + LEGEND_KEY_WIDTH,
    ymin = LEGEND_Y_TOP - 2.30 * LEGEND_ROW_GAP,
    ymax = LEGEND_Y_TOP - 1.70 * LEGEND_ROW_GAP,
    fill = COL_REFERENCE_RIBBON,
    colour = NA
  ) +
  
  annotate(
    "text",
    x = LEGEND_X_LEFT +
      LEGEND_KEY_WIDTH * 1.30,
    y = LEGEND_Y_TOP - 2.0 * LEGEND_ROW_GAP,
    label = "Proliferation uncertainty (P10-P90)",
    hjust = 0,
    vjust = 0.5,
    size = 2.35
  )


for (i in seq_along(legend_rows)) {
  
  y_pos <- LEGEND_Y_TOP -
    (i + 2) * LEGEND_ROW_GAP
  
  p_b_core <- p_b_core +
    
    annotate(
      "segment",
      x = LEGEND_X_LEFT,
      xend = LEGEND_X_LEFT + LEGEND_KEY_WIDTH,
      y = y_pos,
      yend = y_pos,
      colour = colour_map[
        legend_rows[i]
      ],
      linewidth = 0.65
    ) +
    
    annotate(
      "text",
      x = LEGEND_X_LEFT +
        LEGEND_KEY_WIDTH * 1.30,
      y = y_pos,
      label = legend_rows[i],
      hjust = 0,
      vjust = 0.5,
      size = 2.35
    )
}


# ============================================================
# 23. ADD PANEL TAGS
# ============================================================

p_a <- tag_panel(
  p_a_core,
  "a"
)

p_b <- tag_panel(
  p_b_core,
  "b"
)


# ============================================================
# 24. COMBINE PANELS
# ============================================================

aligned_panels <- align_plots(
  p_a,
  p_b,
  align = "hv",
  axis = "tblr"
)


final_plot <- plot_grid(
  aligned_panels[[1]],
  aligned_panels[[2]],
  nrow = 1,
  rel_widths = c(
    1,
    1
  )
)


# ============================================================
# 25. DISPLAY
# ============================================================

print(final_plot)


# ============================================================
# 26. EXPORT PNG
# ============================================================

ggsave(
  filename = PNG_FILE,
  plot = final_plot,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)


# ============================================================
# 27. EXPORT PDF
# ============================================================

ggsave(
  filename = PDF_FILE,
  plot = final_plot,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  device = grDevices::cairo_pdf,
  bg = "white"
)


# ============================================================
# 28. CONFIRM OUTPUTS
# ============================================================

cat(
  "\nExtended Data Figure 5 v2 successfully exported:\n",
  PNG_FILE,
  "\n",
  PDF_FILE,
  "\n"
)


final_plot