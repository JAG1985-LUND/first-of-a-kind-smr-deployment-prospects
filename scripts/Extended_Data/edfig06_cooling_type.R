# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Extended Data Fig. 6: global FOAK SMR deployment by reactor
# cooling type.
#
# Panel a shows median cumulative FOAK SMR deployment by reactor cooling type
# under the Baseline scenario. Panel b shows median cumulative FOAK SMR
# deployment by reactor cooling type under the Accelerated scenario. Stacked
# bars distinguish water-, gas-, metal- and salt-cooled designs.
#
# Required input:
# data/Extended_Data/edfig06_source_data_cooling_type_2050.xlsx
#
# Outputs:
# outputs/Extended_Data/Figure_06/edfig06_cooling_type.png
# outputs/Extended_Data/Figure_06/edfig06_cooling_type.pdf

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
# EXTENDED DATA FIGURE 6
# GLOBAL FOAK SMR DEPLOYMENT BY REACTOR COOLING TYPE
# ============================================================

# ------------------------------------------------------------
# PATHS — GITHUB RUN
# ------------------------------------------------------------

FILE <- file.path(
  "data",
  "Extended_Data",
  "edfig06_source_data_cooling_type_2050.xlsx"
)

OUT_DIR <- file.path(
  "outputs",
  "Extended_Data",
  "Figure_06"
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
SAVE_H_MM <- 90
SAVE_DPI  <- 600

FILE_STUB <- "edfig06_cooling_type"

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

# ------------------------------------------------------------
# COMMON THEME
# ------------------------------------------------------------

common_theme <- theme_classic(base_size = BASE_SIZE) +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks = element_line(colour = "grey35", linewidth = 0.3),
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
    legend.key.width = unit(0.35, "cm"),
    legend.spacing.x = unit(0.16, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = LEGEND_TITLE_SIZE),
    legend.text = element_text(size = LEGEND_TEXT_SIZE),
    plot.margin = margin(t = 6, r = 8, b = 6, l = 6),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

# ============================================================
# LOAD AND PREPARE DATA
# ============================================================

df <- readxl::read_excel(FILE)

names(df) <- names(df) %>%
  stringr::str_trim() %>%
  stringr::str_replace_all("\\s+", "_") %>%
  stringr::str_replace_all("\\.+", "_") %>%
  stringr::str_replace_all("_+$", "") %>%
  stringr::str_to_lower()

required_cols <- c(
  "year",
  "cooling_type",
  "baseline_p50",
  "accelerated_p50"
)

missing_cols <- setdiff(required_cols, names(df))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  )
}

plot_df <- df %>%
  mutate(
    year = as.numeric(year),
    cooling_type_raw = stringr::str_to_lower(
      stringr::str_trim(as.character(cooling_type))
    ),
    cooling_type = dplyr::case_when(
      cooling_type_raw %in% c(
        "water",
        "water-cooled",
        "water cooled"
      ) ~ "Water-cooled",
      cooling_type_raw %in% c(
        "gas",
        "gas-cooled",
        "gas cooled"
      ) ~ "Gas-cooled",
      cooling_type_raw %in% c(
        "metal",
        "metal-cooled",
        "metal cooled"
      ) ~ "Metal-cooled",
      cooling_type_raw %in% c(
        "salt",
        "salt-cooled",
        "salt cooled",
        "molten salt",
        "molten-salt",
        "molten-salt-cooled",
        "molten salt cooled"
      ) ~ "Salt-cooled",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(cooling_type)) %>%
  select(
    year,
    cooling_type,
    baseline_p50,
    accelerated_p50
  ) %>%
  pivot_longer(
    cols = c(
      baseline_p50,
      accelerated_p50
    ),
    names_to = "scenario",
    values_to = "value"
  ) %>%
  mutate(
    scenario = dplyr::case_when(
      scenario == "baseline_p50" ~ "Baseline scenario",
      scenario == "accelerated_p50" ~ "Accelerated scenario",
      TRUE ~ NA_character_
    ),
    scenario = factor(
      scenario,
      levels = c(
        "Baseline scenario",
        "Accelerated scenario"
      )
    ),
    cooling_type = factor(
      cooling_type,
      levels = cooling_levels
    ),
    value = as.numeric(value)
  ) %>%
  group_by(
    scenario,
    year,
    cooling_type
  ) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    scenario,
    year = 2018:2050,
    cooling_type = factor(
      cooling_levels,
      levels = cooling_levels
    ),
    fill = list(value = 0)
  ) %>%
  arrange(
    scenario,
    year,
    cooling_type
  )

# ============================================================
# FIXED AXIS RANGE
# ============================================================

Y_MAX <- 40

# ============================================================
# PLOTTING FUNCTION
# ============================================================

make_cooling_stack <- function(panel_data, panel_title, show_y = TRUE) {
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
      name = "Reactor cooling type"
    ) +
    scale_x_continuous(
      breaks = seq(2020, 2050, 5),
      limits = c(2017.5, 2050.5),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = seq(10, Y_MAX, 10),
      limits = c(0, Y_MAX),
      expand = c(0, 0)
    ) +
    labs(
      title = panel_title,
      x = "Year",
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
      }
    )
}

# ============================================================
# PANELS
# ============================================================

p_a <- make_cooling_stack(
  filter(plot_df, scenario == "Baseline scenario"),
  "Baseline scenario",
  show_y = TRUE
)

p_b <- make_cooling_stack(
  filter(plot_df, scenario == "Accelerated scenario"),
  "Accelerated scenario",
  show_y = FALSE
)

shared_legend <- cowplot::get_legend(p_a)

p_a <- p_a + theme(legend.position = "none")
p_b <- p_b + theme(legend.position = "none")

panel_row <- plot_grid(
  p_a,
  p_b,
  nrow = 1,
  align = "hv",
  axis = "tblr",
  rel_widths = c(1, 1)
)

panel_row <- ggdraw(panel_row) +
  draw_plot_label(
    label = c("a", "b"),
    x = c(0.01, 0.51),
    y = c(0.99, 0.99),
    hjust = 0,
    vjust = 1,
    size = TAG_SIZE,
    fontface = "bold"
  )

final_plot <- plot_grid(
  panel_row,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.10)
)

print(final_plot)

# ============================================================
# SAVE EXTENDED DATA FIGURE 6
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
