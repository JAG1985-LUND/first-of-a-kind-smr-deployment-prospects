# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Extended Data Fig. 10: projected SMR capacity in 2035 and 2050.
#
# Panel a shows SMR capacity projections for 2035 from studies published
# between 2014 and 2024. Panel b shows SMR capacity projections for 2050 from
# studies published between 2022 and 2025. Points reproduce values reported in
# the original studies. Red crosses denote low or conservative cases, black
# squares denote central, base-case or reference estimates, green circles
# denote high or optimistic cases, and blue squares denote pathways explicitly
# aligned with tripling global nuclear capacity. Where only a high and low
# scenario are reported, a grey line denotes the mid-point.
#
# Required input:
# data/Extended_Data/edfig10_source_data_capacity_projections.xlsx
#
# Outputs:
# outputs/Extended_Data/edfig10_capacity_projections.png
# outputs/Extended_Data/edfig10_capacity_projections.pdf

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
# EXTENDED DATA FIGURE 10
# PROJECTED SMR CAPACITY IN 2035 AND 2050
# SHARED LEGEND ONLY
# ============================================================

# ------------------------------------------------------------
# PATHS — GITHUB RUN
# ------------------------------------------------------------

FILE <- file.path(
  "data",
  "Extended_Data",
  "edfig10_source_data_capacity_projections.xlsx"
)

OUT_DIR <- file.path(
  "outputs",
  "Extended_Data"
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
SAVE_H_MM <- 190
SAVE_DPI  <- 600

# ------------------------------------------------------------
# TYPOGRAPHY / CONVENTIONS
# ------------------------------------------------------------

BASE_SIZE         <- 11
AXIS_TEXT_SIZE    <- 10
AXIS_TITLE_SIZE   <- 11
LEGEND_TEXT_SIZE  <- 10
LEGEND_TITLE_SIZE <- 10
TAG_SIZE          <- 11

shape_vals <- c(
  "Low" = 4,
  "Central" = 15,
  "High" = 16,
  "Tripling pathway" = 15
)

colour_vals <- c(
  "Low" = "#B22222",
  "Central" = "black",
  "High" = "#228B22",
  "Tripling pathway" = "#1f77b4"
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

standardise_projection_sheet <- function(df, target_year) {
  names(df) <- clean_names_local(names(df))
  
  if (target_year == 2035) {
    df %>%
      transmute(
        scenario   = str_squish(as.character(scenario)),
        source     = str_squish(as.character(source)),
        study_year = as.numeric(study_year),
        target     = as.numeric(target_2035)
      )
  } else {
    df %>%
      transmute(
        scenario   = str_squish(as.character(scenario)),
        source     = str_squish(as.character(source)),
        study_year = as.numeric(study_year),
        target     = as.numeric(target_2050)
      )
  }
}

add_panel_tag <- function(plot_obj, tag, tag_x = 0.015, tag_y = 0.985) {
  ggdraw() +
    draw_plot(
      plot_obj,
      x = 0,
      y = 0,
      width = 1,
      height = 1
    ) +
    draw_plot_label(
      label = tag,
      x = tag_x,
      y = tag_y,
      hjust = 0,
      vjust = 1,
      size = TAG_SIZE,
      fontface = "bold"
    )
}

theme_shared <- theme_classic(base_size = BASE_SIZE) +
  theme(
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks = element_line(colour = "grey35", linewidth = 0.3),
    axis.text = element_text(size = AXIS_TEXT_SIZE, colour = "black"),
    axis.title = element_text(size = AXIS_TITLE_SIZE, colour = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = LEGEND_TITLE_SIZE),
    legend.text = element_text(size = LEGEND_TEXT_SIZE),
    legend.key.height = unit(0.35, "cm"),
    legend.key.width = unit(0.75, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    plot.title = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

# ============================================================
# READ WORKBOOK
# ============================================================

df_2035 <- readxl::read_excel(
  FILE,
  sheet = "capacity_projections_2035"
) %>%
  standardise_projection_sheet(target_year = 2035) %>%
  mutate(
    target_2035 = target,
    estimate_type = case_when(
      str_detect(str_to_lower(scenario), "tripling nuclear target") ~ "Tripling pathway",
      source == "M&C"                                               ~ "Tripling pathway",
      str_detect(str_to_lower(scenario), "low")                     ~ "Low",
      str_detect(str_to_lower(scenario), "high")                    ~ "High",
      TRUE                                                          ~ "Central"
    ),
    estimate_type = factor(
      estimate_type,
      levels = c(
        "Low",
        "Central",
        "High",
        "Tripling pathway"
      )
    )
  )

df_2050 <- readxl::read_excel(
  FILE,
  sheet = "capacity_projections_2050"
) %>%
  standardise_projection_sheet(target_year = 2050) %>%
  mutate(
    target_2050 = target
  ) %>%
  group_by(
    source,
    study_year
  ) %>%
  arrange(
    target_2050,
    .by_group = TRUE
  ) %>%
  mutate(
    order_in_group = row_number(),
    n_group = n(),
    estimate_type = case_when(
      str_detect(str_to_lower(scenario), "1160 gw") |
        str_detect(str_to_lower(scenario), "tripling") ~ "Tripling pathway",
      str_detect(str_to_lower(scenario), "low")   ~ "Low",
      str_detect(str_to_lower(scenario), "high")  ~ "High",
      str_detect(str_to_lower(scenario), "base")  ~ "Central",
      str_detect(str_to_lower(scenario), "aps")   ~ "Central",
      str_detect(str_to_lower(scenario), "steps") ~ "Low",
      str_detect(str_to_lower(scenario), "nze")   ~ "High",
      str_detect(str_to_lower(scenario), "nzs")   ~ "High",
      str_detect(str_to_lower(scenario), "bcs")   ~ "Central",
      n_group == 1 ~ "Central",
      n_group == 2 & order_in_group == 1 ~ "Central",
      n_group == 2 & order_in_group == 2 ~ "High",
      n_group >= 3 & order_in_group == 1 ~ "Low",
      n_group >= 3 & order_in_group == n_group ~ "High",
      TRUE ~ "Central"
    )
  ) %>%
  ungroup() %>%
  mutate(
    estimate_type = factor(
      estimate_type,
      levels = c(
        "Low",
        "Central",
        "High",
        "Tripling pathway"
      )
    )
  )

# ============================================================
# PANEL A — 2035
# ============================================================

range_df_2035 <- df_2035 %>%
  group_by(
    source,
    study_year
  ) %>%
  summarise(
    ymin = min(target_2035, na.rm = TRUE),
    ymax = max(target_2035, na.rm = TRUE),
    midpoint = mean(target_2035, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(
    n > 1,
    ymax > ymin
  )

path_line_2035 <- df_2035 %>%
  filter(
    estimate_type == "Tripling pathway"
  ) %>%
  arrange(study_year) %>%
  summarise(
    x = min(study_year, na.rm = TRUE),
    xend = max(study_year, na.rm = TRUE),
    y = first(target_2035),
    yend = first(target_2035)
  )

label_df_2035 <- bind_rows(
  df_2035 %>%
    filter(source == "NNL") %>%
    arrange(target_2035) %>%
    mutate(
      label = "NNL",
      label_x = study_year + 0.16,
      label_y = c(67.2, 81.0),
      hjust = 0,
      vjust = 0.5
    ),
  df_2035 %>%
    filter(source == "NEA", study_year == 2016) %>%
    arrange(target_2035) %>%
    mutate(
      label = "NEA",
      label_x = study_year + 0.16,
      label_y = c(3.4, 19.1),
      hjust = 0,
      vjust = 0.5
    ),
  df_2035 %>%
    filter(source == "NuScale") %>%
    arrange(target_2035) %>%
    mutate(
      label = "NuScale",
      label_x = study_year + 0.16,
      label_y = c(57.8, 69.0),
      hjust = 0,
      vjust = 0.5
    ),
  df_2035 %>%
    filter(source == "M&C") %>%
    mutate(
      label = "M&C",
      label_x = study_year,
      label_y = target_2035 - 3.7,
      hjust = 0.5,
      vjust = 1
    ),
  df_2035 %>%
    filter(source == "INL") %>%
    mutate(
      label = "INL",
      label_x = study_year,
      label_y = target_2035 + 3.6,
      hjust = 0.5,
      vjust = 0
    ),
  df_2035 %>%
    filter(source == "NEA", study_year == 2022) %>%
    mutate(
      label = "NEA",
      label_x = study_year,
      label_y = target_2035 - 3.7,
      hjust = 0.5,
      vjust = 1
    ),
  df_2035 %>%
    filter(source == "NNWI") %>%
    mutate(
      label = "NNWI",
      label_x = study_year,
      label_y = target_2035 + 3.6,
      hjust = 0.5,
      vjust = 0
    ),
  df_2035 %>%
    filter(source == "IAEA", study_year == 2024) %>%
    arrange(target_2035) %>%
    mutate(
      label = "IAEA",
      label_x = study_year + 0.16,
      label_y = c(7.3, 46.0),
      hjust = 0,
      vjust = 0.5
    )
)

p2035_core <- ggplot(
  df_2035,
  aes(
    x = study_year,
    y = target_2035
  )
) +
  geom_segment(
    data = range_df_2035,
    aes(
      x = study_year,
      xend = study_year,
      y = ymin,
      yend = ymax
    ),
    inherit.aes = FALSE,
    linewidth = 0.45,
    colour = "grey55"
  ) +
  geom_segment(
    data = range_df_2035,
    aes(
      x = study_year - 0.12,
      xend = study_year + 0.12,
      y = midpoint,
      yend = midpoint
    ),
    inherit.aes = FALSE,
    linewidth = 0.5,
    colour = "black"
  ) +
  geom_segment(
    data = path_line_2035,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    inherit.aes = FALSE,
    linewidth = 0.5,
    colour = "#1f77b4",
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = 2020.0,
    y = 40.6,
    label = "Tripling global nuclear energy pathway",
    colour = "#1f77b4",
    size = 3.3,
    hjust = 0.5
  ) +
  geom_point(
    aes(
      shape = estimate_type,
      colour = estimate_type
    ),
    size = 3.4,
    stroke = 1.0
  ) +
  geom_text(
    data = label_df_2035,
    aes(
      x = label_x,
      y = label_y,
      label = label,
      hjust = hjust,
      vjust = vjust
    ),
    inherit.aes = FALSE,
    size = 3.4,
    colour = "black"
  ) +
  scale_shape_manual(
    name = "Projection type",
    values = shape_vals
  ) +
  scale_colour_manual(
    name = "Projection type",
    values = colour_vals
  ) +
  guides(
    shape = guide_legend(
      order = 1,
      override.aes = list(size = 3.8)
    ),
    colour = guide_legend(
      order = 1,
      override.aes = list(size = 3.8)
    )
  ) +
  scale_x_continuous(
    breaks = c(
      2014,
      2016,
      2018,
      2020,
      2022,
      2024
    ),
    limits = c(2013.45, 2025.20),
    expand = expansion(
      mult = c(0.005, 0.01)
    )
  ) +
  scale_y_continuous(
    breaks = c(
      0,
      20,
      40,
      60,
      80
    ),
    limits = c(-2, 90),
    expand = expansion(
      mult = c(0, 0.01)
    )
  ) +
  labs(
    x = "Year of projection",
    y = "Installed capacity (GW)"
  ) +
  theme_shared +
  theme(
    plot.margin = margin(2, 12, 1, 8)
  )

# ============================================================
# PANEL B — 2050
# ============================================================

year_offsets_2050 <- df_2050 %>%
  distinct(
    study_year,
    source
  ) %>%
  arrange(
    study_year,
    source
  ) %>%
  group_by(study_year) %>%
  mutate(
    n_year = n(),
    row_id = row_number(),
    offset = case_when(
      n_year == 1 ~ 0,
      n_year == 2 & row_id == 1 ~ -0.16,
      n_year == 2 & row_id == 2 ~  0.16,
      n_year == 3 & row_id == 1 ~ -0.20,
      n_year == 3 & row_id == 2 ~  0.00,
      n_year == 3 & row_id == 3 ~  0.20,
      TRUE ~ seq(-0.24, 0.24, length.out = n_year)[row_id]
    )
  ) %>%
  ungroup() %>%
  select(
    study_year,
    source,
    offset
  )

df_2050 <- df_2050 %>%
  left_join(
    year_offsets_2050,
    by = c(
      "study_year",
      "source"
    )
  ) %>%
  mutate(
    x_plot = study_year + offset,
    label = source
  )

range_df_2050 <- df_2050 %>%
  group_by(
    source,
    study_year,
    x_plot
  ) %>%
  summarise(
    ymin = min(target_2050, na.rm = TRUE),
    ymax = max(target_2050, na.rm = TRUE),
    midpoint = mean(target_2050, na.rm = TRUE),
    n_group = n(),
    .groups = "drop"
  ) %>%
  filter(
    n_group > 1,
    ymax > ymin
  )

midtick_df_2050 <- range_df_2050 %>%
  filter(
    n_group == 2
  )

label_df_2050 <- df_2050 %>%
  mutate(
    label_x = case_when(
      source == "IEA" ~ x_plot - 0.08,
      source == "NEA" & study_year == 2022 ~ x_plot,
      TRUE ~ x_plot + 0.08
    ),
    label_y = case_when(
      source == "NEA" & study_year == 2022 ~ target_2050 - 12,
      source == "NNWI" & order_in_group == 1 ~ target_2050 + 8,
      source == "NNWI" & order_in_group == 2 ~ target_2050 + 8,
      source == "NNWI" & order_in_group == 3 ~ target_2050 + 8,
      source == "IAEA" & order_in_group == 1 ~ target_2050 + 8,
      source == "IAEA" & order_in_group == 2 ~ target_2050 - 18,
      source == "Wood Mck." & order_in_group == 1 ~ target_2050 + 8,
      source == "Wood Mck." & order_in_group == 2 ~ target_2050 - 10,
      source == "IEA" & order_in_group == 1 ~ target_2050 + 8,
      source == "IEA" & order_in_group == 2 ~ target_2050 + 8,
      source == "IEA" & order_in_group == 3 ~ target_2050 - 10,
      TRUE ~ target_2050
    ),
    hjust = case_when(
      source == "IEA" ~ 1,
      source == "NEA" & study_year == 2022 ~ 0.5,
      TRUE ~ 0
    ),
    vjust = case_when(
      source == "NEA" & study_year == 2022 ~ 1,
      TRUE ~ 0.5
    )
  )

p2050_core <- ggplot(
  df_2050,
  aes(
    x = x_plot,
    y = target_2050
  )
) +
  geom_segment(
    data = range_df_2050,
    aes(
      x = x_plot,
      xend = x_plot,
      y = ymin,
      yend = ymax
    ),
    inherit.aes = FALSE,
    linewidth = 0.45,
    colour = "grey55"
  ) +
  geom_segment(
    data = midtick_df_2050,
    aes(
      x = x_plot - 0.08,
      xend = x_plot + 0.08,
      y = midpoint,
      yend = midpoint
    ),
    inherit.aes = FALSE,
    linewidth = 0.5,
    colour = "black"
  ) +
  geom_point(
    aes(
      shape = estimate_type,
      colour = estimate_type
    ),
    size = 3.6,
    stroke = 1.0
  ) +
  geom_text(
    data = label_df_2050,
    aes(
      x = label_x,
      y = label_y,
      label = label,
      hjust = hjust,
      vjust = vjust
    ),
    inherit.aes = FALSE,
    size = 3.4,
    colour = "black"
  ) +
  scale_shape_manual(
    name = "Projection type",
    values = shape_vals
  ) +
  scale_colour_manual(
    name = "Projection type",
    values = colour_vals
  ) +
  guides(
    shape = guide_legend(
      order = 1,
      override.aes = list(size = 3.8)
    ),
    colour = guide_legend(
      order = 1,
      override.aes = list(size = 3.8)
    )
  ) +
  scale_x_continuous(
    breaks = c(
      2022,
      2023,
      2024,
      2025
    ),
    limits = c(2021.65, 2025.55),
    expand = expansion(
      mult = c(0.005, 0.01)
    )
  ) +
  scale_y_continuous(
    breaks = c(
      0,
      100,
      200,
      300,
      400
    ),
    limits = c(0, 400),
    expand = expansion(
      mult = c(0, 0.01)
    )
  ) +
  labs(
    x = "Year of projection",
    y = "Installed capacity (GW)"
  ) +
  theme_shared +
  theme(
    plot.margin = margin(1, 12, 2, 8)
  )

# ============================================================
# SHARED LEGEND
# ============================================================

shared_legend <- cowplot::get_legend(
  p2050_core +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box.margin = margin(0, 0, 0, 0),
      legend.margin = margin(0, 0, 0, 0)
    )
)

# ============================================================
# REMOVE PANEL-LEVEL LEGENDS + ADD PANEL TAGS
# ============================================================

p2035_tagged <- add_panel_tag(
  p2035_core + theme(legend.position = "none"),
  tag = "a",
  tag_x = 0.015,
  tag_y = 0.985
)

p2050_tagged <- add_panel_tag(
  p2050_core + theme(legend.position = "none"),
  tag = "b",
  tag_x = 0.015,
  tag_y = 0.985
)

# ============================================================
# COMBINE
# ============================================================

panels_only <- plot_grid(
  p2035_tagged,
  p2050_tagged,
  ncol = 1,
  align = "v",
  rel_heights = c(1, 0.98)
)

combined_plot <- plot_grid(
  panels_only,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.08)
)

print(combined_plot)

# ============================================================
# SAVE EXTENDED DATA FIGURE 10
# ============================================================

ggsave(
  filename = file.path(
    OUT_DIR,
    "edfig10_capacity_projections.pdf"
  ),
  plot = combined_plot,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  device = grDevices::cairo_pdf,
  bg = "white"
)

ggsave(
  filename = file.path(
    OUT_DIR,
    "edfig10_capacity_projections.png"
  ),
  plot = combined_plot,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

combined_plot
