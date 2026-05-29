# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Supplementary Fig. 5: historical vendor-target and global SMR
# design pipelines, 1984–2025.
#
# Panel a shows the vendor SMR design pipeline. Panel b shows the global SMR
# design pipeline.
#
# Required input:
# sfig05_source_data_historical_vendors_pipeline.xlsx
#
# Outputs:
# sfig05_historical_vendors_global_pipeline_ab.png
# sfig05_historical_vendors_global_pipeline_ab.pdf

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(cowplot)
  library(grid)
})

rm(list = ls())
graphics.off()

# ============================================================
# SUPPLEMENTARY FIGURE 5
# HISTORICAL VENDOR-TARGET AND GLOBAL SMR DESIGN PIPELINES
# ============================================================

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

FILE <- "sfig05_source_data_historical_vendors_pipeline.xlsx"

OUT_DIR <- "."

if (!file.exists(FILE)) {
  stop("Input file not found: ", FILE)
}

SHEET_GLOBAL <- "global_pipeline"
SHEET_VENDOR <- "vendors_pipeline"

sheet_names <- readxl::excel_sheets(FILE)

if (!all(c(SHEET_GLOBAL, SHEET_VENDOR) %in% sheet_names)) {
  stop(
    "Required sheets not found in workbook. Expected: ",
    paste(c(SHEET_GLOBAL, SHEET_VENDOR), collapse = ", "),
    "\nAvailable sheets are: ",
    paste(sheet_names, collapse = ", ")
  )
}

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 180
SAVE_H_MM <- 180
SAVE_DPI  <- 600

FILE_STUB <- "sfig05_historical_vendors_global_pipeline_ab"

# ------------------------------------------------------------
# STYLE
# ------------------------------------------------------------

BASE_SIZE <- 7
YEAR_END  <- 2025L
X_BREAKS  <- c(1985, 1995, 2005, 2015, 2025)

phase_order <- c(
  "Conceptual design",
  "Basic design",
  "Detailed design",
  "Licensing",
  "Construction",
  "Operational"
)

phase_legend_order <- rev(phase_order)

phase_cols <- c(
  "Conceptual design" = "#FDE0D2",
  "Basic design"      = "#F4A582",
  "Detailed design"   = "#D6604D",
  "Licensing"         = "#D1E5F0",
  "Construction"      = "#92C5DE",
  "Operational"       = "#4393C3"
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

normalize_phase <- function(x) {
  x <- trimws(as.character(x))
  
  dplyr::case_when(
    x %in% c("CD", "Conceptual design", "Conceptual") ~ "Conceptual design",
    x %in% c("BD", "Basic design", "Basic") ~ "Basic design",
    x %in% c("DD", "Detailed design", "Detailed") ~ "Detailed design",
    x %in% c("Licensing", "Licensed", "Licensing phase", "Under licensing") ~ "Licensing",
    x %in% c("Construction", "Under construction", "Construction phase") ~ "Construction",
    x %in% c("Operational", "Operating", "Operation") ~ "Operational",
    x %in% c("Failed design", "Failed") ~ "Failed design",
    TRUE ~ x
  )
}

clean_numeric <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[tolower(x) %in% c("n/a", "na", "", "null", "-")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

year_seq <- function(start_year, end_year_exclusive, year_end = YEAR_END) {
  if (is.na(start_year) || is.na(end_year_exclusive)) return(integer(0))
  
  s <- as.integer(floor(start_year))
  e <- min(as.integer(ceiling(end_year_exclusive)) - 1L, year_end)
  
  if (e < s) return(integer(0))
  
  seq.int(s, e)
}

# ------------------------------------------------------------
# PANEL FUNCTION
# ------------------------------------------------------------

build_pipeline_panel <- function(sheet_name, plot_title, panel_tag, y_limits, y_breaks) {
  
  raw <- readxl::read_excel(
    FILE,
    sheet = sheet_name,
    na = c("N/A", "NA", "")
  )
  
  names(raw) <- clean_names_local(names(raw))
  
  req_cols <- c(
    "smr",
    "phase",
    "cd_start",
    "bd_start",
    "dd_start",
    "lic_start",
    "con_start",
    "op_start"
  )
  
  miss_cols <- setdiff(req_cols, names(raw))
  
  if (length(miss_cols) > 0) {
    stop(
      "Missing required columns in sheet ",
      sheet_name,
      ": ",
      paste(miss_cols, collapse = ", "),
      "\nAvailable columns are: ",
      paste(names(raw), collapse = ", ")
    )
  }
  
  start_cols <- c(
    "cd_start",
    "bd_start",
    "dd_start",
    "lic_start",
    "con_start",
    "op_start"
  )
  
  failure_col <- intersect(
    c(
      "failure_year",
      "fail_year",
      "year_of_failure",
      "inactive_year",
      "termination_year",
      "cancelled_year",
      "canceled_year"
    ),
    names(raw)
  )[1]
  
  phase_map <- c(
    cd_start  = "Conceptual design",
    bd_start  = "Basic design",
    dd_start  = "Detailed design",
    lic_start = "Licensing",
    con_start = "Construction",
    op_start  = "Operational"
  )
  
  df <- raw %>%
    mutate(
      smr = trimws(as.character(.data$smr)),
      phase = normalize_phase(.data$phase),
      row_id = row_number(),
      across(all_of(start_cols), clean_numeric),
      failure_year = if (!is.na(failure_col)) clean_numeric(.data[[failure_col]]) else NA_real_
    )
  
  phase_stints <- df %>%
    select(
      row_id,
      smr,
      phase_status = phase,
      failure_year,
      all_of(start_cols)
    ) %>%
    pivot_longer(
      cols = all_of(start_cols),
      names_to = "phase_col",
      values_to = "start_year"
    ) %>%
    mutate(
      phase = unname(phase_map[phase_col])
    ) %>%
    filter(!is.na(start_year)) %>%
    group_by(row_id, smr, phase_status, failure_year) %>%
    arrange(match(phase, phase_order), .by_group = TRUE) %>%
    mutate(
      next_start = lead(start_year),
      last_start = max(start_year, na.rm = TRUE),
      failed_end = case_when(
        phase_status == "Failed design" & !is.na(failure_year) ~ pmin(failure_year + 1L, YEAR_END + 1L),
        phase_status == "Failed design" & is.na(failure_year)  ~ pmin(last_start + 2L, YEAR_END + 1L),
        TRUE ~ NA_real_
      ),
      end_year = case_when(
        !is.na(next_start) ~ next_start,
        phase_status == "Failed design" ~ failed_end,
        TRUE ~ YEAR_END + 1L
      )
    ) %>%
    ungroup()
  
  phase_years <- phase_stints %>%
    mutate(
      year = purrr::map2(start_year, end_year, year_seq)
    ) %>%
    unnest(cols = c(year))
  
  phase_counts <- phase_years %>%
    filter(phase %in% phase_order) %>%
    count(year, phase, name = "n_designs") %>%
    mutate(
      phase = factor(phase, levels = phase_order)
    )
  
  p_core <- ggplot(phase_counts, aes(x = year, y = n_designs, fill = phase)) +
    geom_col(
      width = 0.82,
      position = position_stack(reverse = TRUE)
    ) +
    scale_fill_manual(
      values = phase_cols,
      breaks = phase_legend_order,
      name = "Phase",
      drop = TRUE
    ) +
    scale_x_continuous(
      limits = c(1985, YEAR_END + 0.6),
      breaks = X_BREAKS,
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = y_limits,
      breaks = y_breaks,
      expand = c(0, 0)
    ) +
    labs(
      title = plot_title,
      x = "Year",
      y = "Number of SMR designs"
    ) +
    theme_classic(base_size = BASE_SIZE) +
    theme(
      panel.grid = element_blank(),
      axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
      axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
      axis.ticks = element_line(colour = "grey35", linewidth = 0.3),
      axis.text = element_text(size = BASE_SIZE),
      axis.title = element_text(size = BASE_SIZE),
      plot.title = element_text(size = BASE_SIZE, face = "bold", hjust = 0.5),
      legend.position = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.title = element_text(size = BASE_SIZE, face = "plain"),
      legend.text = element_text(size = BASE_SIZE),
      legend.key.width = unit(6, "mm"),
      legend.key.height = unit(4, "mm"),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      plot.margin = margin(4, 4, 4, 4),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
  
  ggdraw(p_core) +
    draw_plot_label(
      label = panel_tag,
      x = 0.01,
      y = 0.99,
      hjust = 0,
      vjust = 1,
      size = 12,
      fontface = "bold"
    )
}

# ============================================================
# BUILD PANELS
# ============================================================

p_a <- build_pipeline_panel(
  sheet_name = SHEET_VENDOR,
  plot_title = "Vendor SMR design pipeline",
  panel_tag  = "a",
  y_limits   = c(0, 65),
  y_breaks   = c(10, 20, 30, 40, 50, 60)
)

p_b <- build_pipeline_panel(
  sheet_name = SHEET_GLOBAL,
  plot_title = "Global SMR design pipeline",
  panel_tag  = "b",
  y_limits   = c(0, 65),
  y_breaks   = c(10, 20, 30, 40, 50, 60)
)

final_plot <- plot_grid(
  p_a,
  p_b,
  ncol = 1,
  align = "v",
  rel_heights = c(1, 1)
)

print(final_plot)

# ============================================================
# SAVE SUPPLEMENTARY FIGURE 5
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
  device = cairo_pdf,
  bg = "white"
)

final_plot
