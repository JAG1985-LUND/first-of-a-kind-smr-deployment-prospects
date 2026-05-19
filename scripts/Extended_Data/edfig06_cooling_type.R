# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Supplementary Fig. 7:
# Global cumulative FOAK SMR deployment by reactor cooling type across
# P10, P50 and P90.
#
# Panels a-c show Baseline P10, P50 and P90.
# Panels d-f show Accelerated P10, P50 and P90.
#
# Required input:
# data/Supplementary_Data/sfig07_cooling_type_full.xlsx
#
# Outputs:
# outputs/Supplementary_Figures/Figure_07/sfig07_cooling_type_full.png
# outputs/Supplementary_Figures/Figure_07/sfig07_cooling_type_full.pdf

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(cowplot)
  library(grid)
})

rm(list = ls())
graphics.off()

# ============================================================
# SUPPLEMENTARY FIGURE 7
# GLOBAL FOAK SMR DEPLOYMENT BY REACTOR COOLING TYPE
# P10, P50 AND P90
# ============================================================

# ------------------------------------------------------------
# PATHS — GITHUB RUN
# ------------------------------------------------------------

FILE <- file.path(
  "data",
  "Supplementary_Data",
  "sfig07_cooling_type_full.xlsx"
)

OUT_DIR <- file.path(
  "outputs",
  "Supplementary_Figures",
  "Figure_07"
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

SAVE_W_MM <- 180
SAVE_H_MM <- 150
SAVE_DPI  <- 600

FILE_STUB <- "sfig07_cooling_type_full"

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
# COLOUR PALETTE
# ------------------------------------------------------------

cooling_palette <- c(
  "Water-cooled" = "#1f77b4",
  "Gas-cooled"   = "#d62728",
  "Metal-cooled" = "#A9A9A9",
  "Salt-cooled"  = "#1b9e77"
)

cooling_levels <- c(
  "Water-cooled",
  "Gas-cooled",
  "Metal-cooled",
  "Salt-cooled"
)

panel_levels <- c(
  "Baseline P10",
  "Baseline P50",
  "Baseline P90",
  "Accelerated P10",
  "Accelerated P50",
  "Accelerated P90"
)

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------

clean_names <- function(x) {
  x %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("\\s+", "_") %>%
    stringr::str_replace_all("\\.+", "_") %>%
    stringr::str_replace_all("[^A-Za-z0-9_]+", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_replace_all("_+$", "") %>%
    stringr::str_to_lower()
}

normalise_cooling <- function(x) {
  x_raw <- stringr::str_to_lower(
    stringr::str_trim(as.character(x))
  )
  
  dplyr::case_when(
    x_raw %in% c(
      "water",
      "water-cooled",
      "water cooled",
      "water_cooled"
    ) ~ "Water-cooled",
    
    x_raw %in% c(
      "gas",
      "gas-cooled",
      "gas cooled",
      "gas_cooled"
    ) ~ "Gas-cooled",
    
    x_raw %in% c(
      "metal",
      "metal-cooled",
      "metal cooled",
      "metal_cooled"
    ) ~ "Metal-cooled",
    
    x_raw %in% c(
      "salt",
      "salt-cooled",
      "salt cooled",
      "salt_cooled",
      "molten salt",
      "molten-salt",
      "molten_salt",
      "molten-salt-cooled",
      "molten salt cooled",
      "molten_salt_cooled"
    ) ~ "Salt-cooled",
    
    TRUE ~ NA_character_
  )
}

common_theme <- theme_classic(base_size = BASE_SIZE) +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks  = element_line(colour = "grey35", linewidth = 0.3),
    plot.title = element_text(
      size = TITLE_SIZE,
      face = "bold",
      hjust = 0.5
    ),
    axis.title.x = element_text(
      size = AXIS_TITLE_SIZE,
      margin = margin(t = 4)
    ),
    axis.title.y = element_text(
      size = AXIS_TITLE_SIZE,
      margin = margin(r = 4)
    ),
    axis.text.x = element_text(
      size = AXIS_TEXT_SIZE,
      colour = "grey20",
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(
      size = AXIS_TEXT_SIZE,
      colour = "grey20"
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key = element_rect(fill = NA, colour = NA),
    legend.key.height = unit(0.28, "cm"),
    legend.key.width  = unit(0.35, "cm"),
    legend.spacing.x  = unit(0.16, "cm"),
    legend.margin     = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = LEGEND_TITLE_SIZE),
    legend.text  = element_text(size = LEGEND_TEXT_SIZE),
    plot.margin = margin(t = 6, r = 8, b = 6, l = 6),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

make_cooling_stack <- function(panel_data, panel_title, show_y = TRUE, show_x = TRUE) {
  ggplot(
    panel_data,
    aes(
      x = year,
      y = value,
      fill = cooling_type
    )
  ) +
    geom_col(
      width = 0.78,
      position = position_stack(reverse = TRUE),
      colour = NA,
      alpha = 0.95
    ) +
    scale_fill_manual(
      values = cooling_palette,
      breaks = cooling_levels,
      name = "Cooling type",
      drop = FALSE
    ) +
    scale_x_continuous(
      breaks = seq(2020, 2050, 5),
      limits = c(2017.5, 2050.5),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = seq(10, 50, 10),
      limits = c(0, 50),
      expand = c(0, 0)
    ) +
    labs(
      title = panel_title,
      x = if (show_x) "Year" else NULL,
      y = if (show_y) "Cumulative FOAK SMRs" else NULL
    ) +
    common_theme +
    theme(
      axis.title.y = if (show_y) {
        element_text(
          size = AXIS_TITLE_SIZE,
          margin = margin(r = 4)
        )
      } else {
        element_blank()
      },
      axis.title.x = if (show_x) {
        element_text(
          size = AXIS_TITLE_SIZE,
          margin = margin(t = 4)
        )
      } else {
        element_blank()
      }
    )
}

# ============================================================
# LOAD AND PREPARE DATA
# ============================================================

df <- readxl::read_excel(FILE)

names(df) <- clean_names(names(df))

required_cols <- c(
  "year",
  "cooling_type",
  "baseline_p10",
  "baseline_p50",
  "baseline_p90",
  "accelerated_p10",
  "accelerated_p50",
  "accelerated_p90"
)

missing_cols <- setdiff(required_cols, names(df))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      "\nAvailable columns are: ",
      paste(names(df), collapse = ", ")
    )
  )
}

plot_df <- df %>%
  mutate(
    year = as.numeric(year),
    cooling_type = normalise_cooling(cooling_type)
  ) %>%
  filter(!is.na(cooling_type)) %>%
  select(
    year,
    cooling_type,
    baseline_p10,
    baseline_p50,
    baseline_p90,
    accelerated_p10,
    accelerated_p50,
    accelerated_p90
  ) %>%
  pivot_longer(
    cols = c(
      baseline_p10,
      baseline_p50,
      baseline_p90,
      accelerated_p10,
      accelerated_p50,
      accelerated_p90
    ),
    names_to = "scenario_quantile",
    values_to = "value"
  ) %>%
  mutate(
    scenario_quantile = stringr::str_to_lower(scenario_quantile),
    scenario = dplyr::case_when(
      stringr::str_detect(scenario_quantile, "^baseline") ~ "Baseline",
      stringr::str_detect(scenario_quantile, "^accelerated") ~ "Accelerated",
      TRUE ~ NA_character_
    ),
    quantile = dplyr::case_when(
      stringr::str_detect(scenario_quantile, "p10") ~ "P10",
      stringr::str_detect(scenario_quantile, "p50") ~ "P50",
      stringr::str_detect(scenario_quantile, "p90") ~ "P90",
      TRUE ~ NA_character_
    ),
    panel = paste(scenario, quantile),
    panel = factor(panel, levels = panel_levels),
    cooling_type = factor(cooling_type, levels = cooling_levels),
    value = as.numeric(value)
  ) %>%
  filter(!is.na(panel)) %>%
  group_by(
    panel,
    year,
    cooling_type
  ) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    panel = factor(panel_levels, levels = panel_levels),
    year = 2018:2050,
    cooling_type = factor(cooling_levels, levels = cooling_levels),
    fill = list(value = 0)
  ) %>%
  arrange(
    panel,
    year,
    cooling_type
  )

# ============================================================
# PANELS
# ============================================================

p_a <- make_cooling_stack(
  filter(plot_df, panel == "Baseline P10"),
  "Baseline P10",
  show_y = TRUE,
  show_x = FALSE
)

p_b <- make_cooling_stack(
  filter(plot_df, panel == "Baseline P50"),
  "Baseline P50",
  show_y = FALSE,
  show_x = FALSE
)

p_c <- make_cooling_stack(
  filter(plot_df, panel == "Baseline P90"),
  "Baseline P90",
  show_y = FALSE,
  show_x = FALSE
)

p_d <- make_cooling_stack(
  filter(plot_df, panel == "Accelerated P10"),
  "Accelerated P10",
  show_y = TRUE,
  show_x = TRUE
)

p_e <- make_cooling_stack(
  filter(plot_df, panel == "Accelerated P50"),
  "Accelerated P50",
  show_y = FALSE,
  show_x = TRUE
)

p_f <- make_cooling_stack(
  filter(plot_df, panel == "Accelerated P90"),
  "Accelerated P90",
  show_y = FALSE,
  show_x = TRUE
)

shared_legend <- cowplot::get_legend(p_a)

p_a <- p_a + theme(legend.position = "none")
p_b <- p_b + theme(legend.position = "none")
p_c <- p_c + theme(legend.position = "none")
p_d <- p_d + theme(legend.position = "none")
p_e <- p_e + theme(legend.position = "none")
p_f <- p_f + theme(legend.position = "none")

panel_grid <- plot_grid(
  p_a,
  p_b,
  p_c,
  p_d,
  p_e,
  p_f,
  ncol = 3,
  align = "hv",
  axis = "tblr",
  rel_widths = c(1, 1, 1),
  rel_heights = c(1, 1)
)

panel_grid <- ggdraw(panel_grid) +
  draw_plot_label(
    label = c("a", "b", "c", "d", "e", "f"),
    x = c(0.01, 0.345, 0.678, 0.01, 0.345, 0.678),
    y = c(0.99, 0.99, 0.99, 0.49, 0.49, 0.49),
    hjust = 0,
    vjust = 1,
    size = TAG_SIZE,
    fontface = "bold"
  )

final_plot <- plot_grid(
  panel_grid,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.08)
)

print(final_plot)

# ============================================================
# SAVE SUPPLEMENTARY FIGURE 7
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

final_plot
