# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Figure 6: regional FOAK SMR deployment trajectories and
# cooling-type composition by 2050.
#
# Panel a shows median cumulative FOAK SMR deployments by region under the
# Baseline scenario.
#
# Panel b shows median cumulative FOAK SMR deployments by region under the
# Accelerated scenario.
#
# Panel c shows the median number of SMR designs reaching FOAK operation by
# 2050 by region and reactor cooling type under the Baseline scenario,
# distinguishing water-, metal-, salt- and gas-cooled designs.
#
# Panel d shows the median number of SMR designs reaching FOAK operation by
# 2050 by region and reactor cooling type under the Accelerated scenario,
# distinguishing water-, metal-, salt- and gas-cooled designs.
#
# In panels a and b, stacked areas indicate median cumulative deployments
# over time.
#
# Required input:
# data/Main_Figures/fig06_source_data_regional_deployment_cooling_type.xlsx
#   - sheet: regional_deployment
#   - sheet: cooling_type
#
# Outputs:
# outputs/Figure_06/fig06_regional_cooling.png
# outputs/Figure_06/fig06_regional_cooling.pdf

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
# FIGURE 6 — REGIONAL COOLING
# ============================================================

# ------------------------------------------------------------
# PROJECT PATHS
# ------------------------------------------------------------

find_repo_root <- function(marker = file.path("data", "Main_Figures", "fig06_source_data_regional_deployment_cooling_type.xlsx")) {
  path <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  
  repeat {
    candidate <- file.path(path, marker)
    
    if (file.exists(candidate)) {
      return(path)
    }
    
    parent <- dirname(path)
    
    if (identical(parent, path)) {
      stop(
        "Could not find repository root containing ",
        marker,
        "\nCurrent working directory was: ",
        getwd()
      )
    }
    
    path <- parent
  }
}

REPO_ROOT <- find_repo_root()

DATA_DIR <- file.path(REPO_ROOT, "data", "Main_Figures")
OUT_DIR  <- file.path(REPO_ROOT, "outputs", "Figure_06")

FILE <- file.path(DATA_DIR, "fig06_source_data_regional_deployment_cooling_type.xlsx")

if (!file.exists(FILE)) {
  stop("Input file not found: ", FILE)
}

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

available_sheets <- readxl::excel_sheets(FILE)

required_sheets <- c(
  "regional_deployment",
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
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 180
SAVE_H_MM <- 180
SAVE_DPI  <- 600

FIGURE_STEM <- "fig06_regional_cooling"

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
# THEME
# ------------------------------------------------------------

common_theme <- theme_classic(base_size = BASE_SIZE) +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks  = element_line(colour = "grey35", linewidth = 0.3),
    axis.title.x = element_text(size = AXIS_TITLE_SIZE, margin = margin(t = 4)),
    axis.title.y = element_text(size = AXIS_TITLE_SIZE, margin = margin(r = 4)),
    axis.text.x = element_text(size = AXIS_TEXT_SIZE, colour = "grey20"),
    axis.text.y = element_text(size = AXIS_TEXT_SIZE, colour = "grey20"),
    plot.title = element_blank(),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key = element_rect(fill = NA, colour = NA),
    legend.key.height = unit(0.32, "cm"),
    legend.key.width  = unit(0.32, "cm"),
    legend.spacing.y  = unit(0.02, "cm"),
    legend.margin     = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = LEGEND_TITLE_SIZE),
    legend.text  = element_text(size = LEGEND_TEXT_SIZE),
    plot.margin = margin(t = 6, r = 8, b = 6, l = 6),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------

clean_names_local <- function(x) {
  x %>%
    str_trim() %>%
    str_replace_all("\\s+", "_") %>%
    str_replace_all("\\.+", "_") %>%
    str_replace_all("_+$", "") %>%
    str_to_lower()
}

standardise_region <- function(x) {
  case_when(
    str_to_lower(x) %in% c("north america", "north_america", "northamerica") ~ "North America",
    str_to_lower(x) %in% c("europe") ~ "Europe",
    str_to_lower(x) %in% c("oecd asia", "oecd_asia", "oecd-asia", "oecdasia") ~ "OECD-Asia",
    str_to_lower(x) %in% c("china") ~ "China",
    str_to_lower(x) %in% c("russia") ~ "Russia",
    str_to_lower(x) %in% c("row", "rest of world", "rest_of_world", "restofworld") ~ "RoW",
    TRUE ~ as.character(x)
  )
}

standardise_cooling_type <- function(x) {
  case_when(
    str_to_lower(x) %in% c("water-cooled", "water cooled", "water") ~ "Water-cooled",
    str_to_lower(x) %in% c("metal-cooled", "metal cooled", "metal") ~ "Metal-cooled",
    str_to_lower(x) %in% c(
      "molten-salt-cooled",
      "molten salt cooled",
      "molten-salt",
      "salt-cooled",
      "salt cooled",
      "salt"
    ) ~ "Salt-cooled",
    str_to_lower(x) %in% c("gas-cooled", "gas cooled", "gas") ~ "Gas-cooled",
    TRUE ~ as.character(x)
  )
}

add_panel_decorations <- function(plot_obj, tag, title_text) {
  ggdraw() +
    draw_plot(plot_obj, x = 0, y = 0, width = 1, height = 0.955) +
    draw_plot_label(
      tag,
      x = 0.01,
      y = 0.99,
      hjust = 0,
      vjust = 1,
      size = TAG_SIZE,
      fontface = "plain"
    ) +
    draw_label(
      title_text,
      x = 0.56,
      y = 0.985,
      hjust = 0.5,
      vjust = 1,
      size = TITLE_SIZE,
      fontface = "bold"
    )
}

# ============================================================
# PANELS A-B — REGIONAL CUMULATIVE DEPLOYMENT
# ============================================================

df_reg <- readxl::read_excel(FILE, sheet = "regional_deployment")
names(df_reg) <- clean_names_local(names(df_reg))

required_cols_reg <- c(
  "year",
  "region",
  "baseline",
  "accelerated"
)

missing_cols_reg <- required_cols_reg[!required_cols_reg %in% names(df_reg)]

if (length(missing_cols_reg) > 0) {
  stop(
    "Missing required columns in regional_deployment sheet:\n",
    paste(missing_cols_reg, collapse = "\n"),
    "\n\nAvailable columns are:\n",
    paste(names(df_reg), collapse = "\n")
  )
}

plot_df_reg <- df_reg %>%
  mutate(
    year = as.numeric(year),
    region = standardise_region(region)
  ) %>%
  pivot_longer(
    cols = c(baseline, accelerated),
    names_to = "scenario",
    values_to = "value"
  ) %>%
  mutate(
    scenario = factor(
      case_when(
        scenario == "baseline"    ~ "Baseline scenario",
        scenario == "accelerated" ~ "Accelerated scenario",
        TRUE ~ scenario
      ),
      levels = c("Baseline scenario", "Accelerated scenario")
    ),
    region = factor(
      region,
      levels = c("Russia", "China", "RoW", "North America", "Europe", "OECD-Asia")
    ),
    value = as.numeric(value)
  ) %>%
  arrange(scenario, year, region)

region_cols <- c(
  "Russia"        = "#B22222",
  "China"         = "#C45A32",
  "RoW"           = "#7A3DB8",
  "North America" = "#228B22",
  "Europe"        = "#0072B2",
  "OECD-Asia"     = "#2A9D8F"
)

make_stack_panel <- function(panel_data) {
  ggplot(panel_data, aes(x = year, y = value, fill = region, group = region)) +
    geom_area(
      position = position_stack(reverse = TRUE),
      alpha = 0.95,
      colour = NA
    ) +
    scale_fill_manual(
      values = region_cols,
      breaks = c("Russia", "China", "RoW", "North America", "Europe", "OECD-Asia"),
      name = "Region"
    ) +
    scale_x_continuous(
      breaks = c(2020, 2030, 2040, 2050),
      limits = c(2018, 2050),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = c(10, 20, 30, 40, 50),
      limits = c(0, 50),
      expand = c(0, 0)
    ) +
    labs(
      x = "Year",
      y = "Median cumulative FOAK SMRs"
    ) +
    common_theme
}

p_a_core <- make_stack_panel(
  filter(plot_df_reg, scenario == "Baseline scenario")
)

p_b_core <- make_stack_panel(
  filter(plot_df_reg, scenario == "Accelerated scenario")
)

p_a <- add_panel_decorations(p_a_core, "a", "Baseline scenario")
p_b <- add_panel_decorations(p_b_core, "b", "Accelerated scenario")

# ============================================================
# PANELS C-D — COOLING-TYPE COMPOSITION BY REGION IN 2050
# ============================================================

df_cool <- readxl::read_excel(FILE, sheet = "cooling_type")
names(df_cool) <- clean_names_local(names(df_cool))

required_cols_cool <- c(
  "year",
  "region",
  "cooling_type",
  "baseline",
  "accelerated"
)

missing_cols_cool <- required_cols_cool[!required_cols_cool %in% names(df_cool)]

if (length(missing_cols_cool) > 0) {
  stop(
    "Missing required columns in cooling_type sheet:\n",
    paste(missing_cols_cool, collapse = "\n"),
    "\n\nAvailable columns are:\n",
    paste(names(df_cool), collapse = "\n")
  )
}

plot_df_cool <- df_cool %>%
  mutate(
    year = as.numeric(year),
    region = standardise_region(region),
    cooling_type = standardise_cooling_type(cooling_type)
  ) %>%
  filter(year == 2050) %>%
  pivot_longer(
    cols = c(baseline, accelerated),
    names_to = "scenario",
    values_to = "value"
  ) %>%
  mutate(
    scenario = factor(
      case_when(
        scenario == "baseline"    ~ "Baseline scenario",
        scenario == "accelerated" ~ "Accelerated scenario",
        TRUE ~ scenario
      ),
      levels = c("Baseline scenario", "Accelerated scenario")
    ),
    region = factor(
      region,
      levels = c("Russia", "China", "RoW", "North America", "Europe", "OECD-Asia")
    ),
    cooling_type = factor(
      cooling_type,
      levels = c("Water-cooled", "Metal-cooled", "Salt-cooled", "Gas-cooled")
    ),
    value = as.numeric(value)
  ) %>%
  arrange(scenario, region, cooling_type)

cooling_palette <- c(
  "Water-cooled" = "#1f77b4",
  "Metal-cooled" = "#A9A9A9",
  "Salt-cooled"  = "#1b9e77",
  "Gas-cooled"   = "#d62728"
)

make_cooling_panel <- function(panel_data) {
  ggplot(panel_data, aes(x = region, y = value, fill = cooling_type)) +
    geom_col(
      width = 0.62,
      position = position_stack(reverse = TRUE)
    ) +
    scale_fill_manual(
      values = cooling_palette,
      breaks = c("Water-cooled", "Metal-cooled", "Salt-cooled", "Gas-cooled"),
      name = "Cooling type"
    ) +
    scale_y_continuous(
      breaks = c(5, 10, 15),
      limits = c(0, 15),
      expand = c(0, 0)
    ) +
    labs(
      x = "Region",
      y = "Median number of SMR designs"
    ) +
    common_theme +
    theme(
      axis.text.x = element_text(
        size = AXIS_TEXT_SIZE,
        angle = 30,
        hjust = 1,
        vjust = 1,
        colour = "grey20"
      )
    )
}

p_c_core <- make_cooling_panel(
  filter(plot_df_cool, scenario == "Baseline scenario")
)

p_d_core <- make_cooling_panel(
  filter(plot_df_cool, scenario == "Accelerated scenario")
)

p_c <- add_panel_decorations(p_c_core, "c", "Baseline scenario")
p_d <- add_panel_decorations(p_d_core, "d", "Accelerated scenario")

# ============================================================
# COMBINE PANELS
# ============================================================

top_row <- plot_grid(
  p_a,
  p_b,
  ncol = 2,
  rel_widths = c(1, 1),
  align = "hv",
  axis = "tblr"
)

bottom_row <- plot_grid(
  p_c,
  p_d,
  ncol = 2,
  rel_widths = c(1, 1),
  align = "hv",
  axis = "tblr"
)

fig06_regional_cooling <- plot_grid(
  top_row,
  bottom_row,
  ncol = 1,
  rel_heights = c(1, 1),
  align = "v",
  axis = "tblr"
)

print(fig06_regional_cooling)

# ============================================================
# EXPORT
# ============================================================

ggsave(
  filename = file.path(OUT_DIR, paste0(FIGURE_STEM, ".png")),
  plot = fig06_regional_cooling,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, paste0(FIGURE_STEM, ".pdf")),
  plot = fig06_regional_cooling,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  device = grDevices::cairo_pdf,
  bg = "white"
)

fig06_regional_cooling
