# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Extended Data Fig. 9: post-FOAK scaling window to 2050 across
# regional and national groupings and cooling types.
#
# Panel a shows regional and national groupings: Russia, China, Rest of the
# World (RoW), North America, OECD Asia, Europe, non-OECD and OECD. Panel b
# shows reactor cooling types: water-cooled, gas-cooled, metal-cooled and
# molten-salt-cooled designs. Squares and horizontal error bars show Baseline
# medians and P10-P90 intervals; diamonds and horizontal error bars show the
# corresponding Accelerated values.
#
# Required input:
# data/Extended_Data/edfig09_source_data_NOAK_scaling.xlsx
#
# Outputs:
# outputs/Extended_Data/Figure_09/edfig09_noak_scaling.png
# outputs/Extended_Data/Figure_09/edfig09_noak_scaling.pdf
# outputs/Extended_Data/Figure_09/edfig09_noak_scaling.rds

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
# EXTENDED DATA FIGURE 9
# POST-FOAK SCALING WINDOW TO 2050
# ============================================================

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

SOURCE_FILE <- file.path(
  "data",
  "Extended_Data",
  "edfig09_source_data_NOAK_scaling.xlsx"
)

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
        "Start directory was: ",
        normalizePath(start_dir, winslash = "/", mustWork = TRUE),
        "\n\nRun this script from inside the GitHub repository, or set the working directory to the repository root."
      )
    }
    
    current <- parent
  }
}

ROOT_DIR <- find_repo_root()

FILE <- file.path(ROOT_DIR, SOURCE_FILE)
OUT_DIR <- file.path(ROOT_DIR, "outputs", "Extended_Data", "Figure_09")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(FILE)) {
  stop("Input file not found: ", FILE)
}

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 180
SAVE_H_MM <- 150
SAVE_DPI  <- 600

FILE_STUB <- "edfig09_noak_scaling"

# ============================================================
# LOAD DATA
# ============================================================

df_raw <- readxl::read_excel(
  FILE,
  sheet = 1,
  col_names = TRUE
)

group_col <- if ("global_group" %in% names(df_raw)) {
  "global_group"
} else if ("group" %in% names(df_raw)) {
  "group"
} else {
  stop("Neither 'global_group' nor 'group' was found in the input file.")
}

# ============================================================
# PANEL A DATA — REGIONAL / NATIONAL GROUPINGS
# ============================================================

df_a <- df_raw %>%
  transmute(
    group   = as.character(.data[[group_col]]),
    bas_p10 = as.numeric(bas_p10),
    bas_p50 = as.numeric(bas_p50),
    bas_p90 = as.numeric(bas_p90),
    acc_p10 = as.numeric(acc_p10),
    acc_p50 = as.numeric(acc_p50),
    acc_p90 = as.numeric(acc_p90)
  ) %>%
  filter(
    group %in% c(
      "Russia",
      "China",
      "RoW",
      "North America",
      "OECD Asia",
      "Europe",
      "Non-OECD",
      "OECD"
    )
  ) %>%
  mutate(
    group = factor(
      group,
      levels = c(
        "Russia",
        "China",
        "RoW",
        "North America",
        "OECD Asia",
        "Europe",
        "Non-OECD",
        "OECD"
      )
    )
  )

y_map_a <- tibble(
  group = factor(
    c(
      "Russia",
      "China",
      "RoW",
      "North America",
      "OECD Asia",
      "Europe",
      "Non-OECD",
      "OECD",
      "Russia",
      "China",
      "RoW",
      "North America",
      "OECD Asia",
      "Europe",
      "Non-OECD",
      "OECD"
    ),
    levels = c(
      "Russia",
      "China",
      "RoW",
      "North America",
      "OECD Asia",
      "Europe",
      "Non-OECD",
      "OECD"
    )
  ),
  Scenario = factor(
    c(
      rep("Baseline", 8),
      rep("Accelerated", 8)
    ),
    levels = c(
      "Baseline",
      "Accelerated"
    )
  ),
  y = c(
    16, 14, 12, 10, 8, 6, 3, 1,
    15, 13, 11, 9, 7, 5, 2, 0
  )
)

plot_df_a <- bind_rows(
  df_a %>%
    transmute(
      group,
      Scenario = factor(
        "Baseline",
        levels = c(
          "Baseline",
          "Accelerated"
        )
      ),
      p10 = bas_p10,
      p50 = bas_p50,
      p90 = bas_p90
    ),
  df_a %>%
    transmute(
      group,
      Scenario = factor(
        "Accelerated",
        levels = c(
          "Baseline",
          "Accelerated"
        )
      ),
      p10 = acc_p10,
      p50 = acc_p50,
      p90 = acc_p90
    )
) %>%
  left_join(
    y_map_a,
    by = c(
      "group",
      "Scenario"
    )
  ) %>%
  arrange(
    group,
    Scenario
  )

label_df_a <- tibble(
  group = factor(
    c(
      "Russia",
      "China",
      "RoW",
      "North America",
      "OECD Asia",
      "Europe",
      "Non-OECD",
      "OECD"
    ),
    levels = c(
      "Russia",
      "China",
      "RoW",
      "North America",
      "OECD Asia",
      "Europe",
      "Non-OECD",
      "OECD"
    )
  ),
  y = c(
    15.5,
    13.5,
    11.5,
    9.5,
    7.5,
    5.5,
    2.5,
    0.5
  ),
  label = c(
    "Russia",
    "China",
    "RoW",
    "North America",
    "OECD Asia",
    "Europe",
    "Non-OECD",
    "OECD"
  )
)

p_a <- ggplot() +
  geom_vline(
    xintercept = 0,
    colour = "grey35",
    linewidth = 0.3
  ) +
  geom_segment(
    data = plot_df_a,
    aes(
      x = p10,
      xend = p90,
      y = y,
      yend = y,
      colour = Scenario
    ),
    linewidth = 0.8,
    lineend = "butt"
  ) +
  geom_segment(
    data = plot_df_a,
    aes(
      x = p10,
      xend = p10,
      y = y - 0.16,
      yend = y + 0.16
    ),
    colour = "black",
    linewidth = 0.45
  ) +
  geom_segment(
    data = plot_df_a,
    aes(
      x = p90,
      xend = p90,
      y = y - 0.16,
      yend = y + 0.16
    ),
    colour = "black",
    linewidth = 0.45
  ) +
  geom_point(
    data = plot_df_a %>%
      filter(Scenario == "Baseline"),
    aes(
      x = p50,
      y = y
    ),
    shape = 15,
    size = 2.9,
    colour = "grey45",
    stroke = 0
  ) +
  geom_point(
    data = plot_df_a %>%
      filter(Scenario == "Accelerated"),
    aes(
      x = p50,
      y = y
    ),
    shape = 18,
    size = 3.2,
    colour = "#228B22",
    stroke = 0
  ) +
  geom_text(
    data = label_df_a,
    aes(
      x = -0.60,
      y = y,
      label = label
    ),
    hjust = 1,
    size = 3.2
  ) +
  scale_colour_manual(
    values = c(
      "Baseline" = "grey45",
      "Accelerated" = "#228B22"
    ),
    breaks = c(
      "Baseline",
      "Accelerated"
    )
  ) +
  coord_cartesian(
    xlim = c(0, 30),
    ylim = c(-0.6, 16.6),
    clip = "off"
  ) +
  scale_x_continuous(
    breaks = c(
      0,
      5,
      10,
      15,
      20,
      25,
      30
    ),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = NULL,
    expand = c(0, 0)
  ) +
  labs(
    x = "Post-FOAK scaling window to 2050 (years)",
    y = NULL,
    colour = "Scenario"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(
      colour = "grey35",
      linewidth = 0.3
    ),
    axis.line.y = element_blank(),
    axis.ticks.x = element_line(
      colour = "grey35",
      linewidth = 0.3
    ),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    legend.position = "none",
    legend.direction = "horizontal",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.key.width = unit(6, "mm"),
    legend.key.height = unit(4, "mm"),
    plot.margin = margin(4, 10, 4, 120),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

# ============================================================
# PANEL B DATA — COOLING TYPE
# ============================================================

df_b <- df_raw %>%
  transmute(
    type_raw = as.character(.data[[group_col]]),
    bas_p10 = as.numeric(bas_p10),
    bas_p50 = as.numeric(bas_p50),
    bas_p90 = as.numeric(bas_p90),
    acc_p10 = as.numeric(acc_p10),
    acc_p50 = as.numeric(acc_p50),
    acc_p90 = as.numeric(acc_p90)
  ) %>%
  mutate(
    type = case_when(
      str_to_lower(str_squish(type_raw)) %in% c(
        "water-cooled",
        "water cooled",
        "water_cooled"
      ) ~ "Water-cooled",
      str_to_lower(str_squish(type_raw)) %in% c(
        "gas-cooled",
        "gas cooled",
        "gas_cooled"
      ) ~ "Gas-cooled",
      str_to_lower(str_squish(type_raw)) %in% c(
        "metal-cooled",
        "metal cooled",
        "metal_cooled"
      ) ~ "Metal-cooled",
      str_to_lower(str_squish(type_raw)) %in% c(
        "molten-salt-cooled",
        "molten salt cooled",
        "molten_salt_cooled",
        "salt",
        "molten salt"
      ) ~ "Molten-salt-cooled",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(type)) %>%
  mutate(
    type = factor(
      type,
      levels = c(
        "Water-cooled",
        "Gas-cooled",
        "Metal-cooled",
        "Molten-salt-cooled"
      )
    )
  ) %>%
  arrange(type)

y_map_b <- tibble(
  type = factor(
    c(
      "Water-cooled",
      "Gas-cooled",
      "Metal-cooled",
      "Molten-salt-cooled",
      "Water-cooled",
      "Gas-cooled",
      "Metal-cooled",
      "Molten-salt-cooled"
    ),
    levels = c(
      "Water-cooled",
      "Gas-cooled",
      "Metal-cooled",
      "Molten-salt-cooled"
    )
  ),
  Scenario = factor(
    c(
      "Baseline",
      "Baseline",
      "Baseline",
      "Baseline",
      "Accelerated",
      "Accelerated",
      "Accelerated",
      "Accelerated"
    ),
    levels = c(
      "Baseline",
      "Accelerated"
    )
  ),
  y = c(
    8,
    6,
    4,
    2,
    7,
    5,
    3,
    1
  )
)

plot_df_b <- bind_rows(
  df_b %>%
    transmute(
      type,
      Scenario = factor(
        "Baseline",
        levels = c(
          "Baseline",
          "Accelerated"
        )
      ),
      p10 = bas_p10,
      p50 = bas_p50,
      p90 = bas_p90
    ),
  df_b %>%
    transmute(
      type,
      Scenario = factor(
        "Accelerated",
        levels = c(
          "Baseline",
          "Accelerated"
        )
      ),
      p10 = acc_p10,
      p50 = acc_p50,
      p90 = acc_p90
    )
) %>%
  left_join(
    y_map_b,
    by = c(
      "type",
      "Scenario"
    )
  ) %>%
  arrange(
    type,
    Scenario
  )

label_df_b <- tibble(
  type = factor(
    c(
      "Water-cooled",
      "Gas-cooled",
      "Metal-cooled",
      "Molten-salt-cooled"
    ),
    levels = c(
      "Water-cooled",
      "Gas-cooled",
      "Metal-cooled",
      "Molten-salt-cooled"
    )
  ),
  y = c(
    7.5,
    5.5,
    3.5,
    1.5
  ),
  label = c(
    "Water-cooled",
    "Gas-cooled",
    "Metal-cooled",
    "Molten-salt-cooled"
  )
)

p_b <- ggplot() +
  geom_vline(
    xintercept = 0,
    colour = "grey35",
    linewidth = 0.3
  ) +
  geom_segment(
    data = plot_df_b,
    aes(
      x = p10,
      xend = p90,
      y = y,
      yend = y,
      colour = Scenario
    ),
    linewidth = 0.8,
    lineend = "butt"
  ) +
  geom_segment(
    data = plot_df_b,
    aes(
      x = p10,
      xend = p10,
      y = y - 0.16,
      yend = y + 0.16
    ),
    colour = "black",
    linewidth = 0.45
  ) +
  geom_segment(
    data = plot_df_b,
    aes(
      x = p90,
      xend = p90,
      y = y - 0.16,
      yend = y + 0.16
    ),
    colour = "black",
    linewidth = 0.45
  ) +
  geom_point(
    data = plot_df_b %>%
      filter(Scenario == "Baseline"),
    aes(
      x = p50,
      y = y
    ),
    shape = 15,
    size = 2.9,
    colour = "grey45",
    stroke = 0
  ) +
  geom_point(
    data = plot_df_b %>%
      filter(Scenario == "Accelerated"),
    aes(
      x = p50,
      y = y
    ),
    shape = 18,
    size = 3.2,
    colour = "#228B22",
    stroke = 0
  ) +
  geom_text(
    data = label_df_b,
    aes(
      x = -0.60,
      y = y,
      label = label
    ),
    hjust = 1,
    size = 3.2
  ) +
  scale_colour_manual(
    values = c(
      "Baseline" = "grey45",
      "Accelerated" = "#228B22"
    ),
    breaks = c(
      "Baseline",
      "Accelerated"
    )
  ) +
  coord_cartesian(
    xlim = c(0, 30),
    ylim = c(0.4, 8.6),
    clip = "off"
  ) +
  scale_x_continuous(
    breaks = c(
      0,
      5,
      10,
      15,
      20,
      25,
      30
    ),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = NULL,
    expand = c(0, 0)
  ) +
  labs(
    x = "Post-FOAK scaling window to 2050 (years)",
    y = NULL,
    colour = "Scenario"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(
      colour = "grey35",
      linewidth = 0.3
    ),
    axis.line.y = element_blank(),
    axis.ticks.x = element_line(
      colour = "grey35",
      linewidth = 0.3
    ),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.key.width = unit(6, "mm"),
    legend.key.height = unit(4, "mm"),
    plot.margin = margin(4, 10, 4, 120),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

# ============================================================
# COMBINE
# ============================================================

fig_ab <- plot_grid(
  p_a,
  p_b,
  ncol = 1,
  rel_heights = c(1.00, 0.78),
  align = "v",
  axis = "lr",
  labels = c("a", "b"),
  label_size = 11,
  label_fontface = "bold",
  label_x = 0.01,
  label_y = 0.99,
  hjust = 0,
  vjust = 1
)

print(fig_ab)

# ============================================================
# SAVE EXTENDED DATA FIGURE 9
# ============================================================

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".png")),
  plot = fig_ab,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".pdf")),
  plot = fig_ab,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  device = grDevices::cairo_pdf,
  bg = "white"
)

saveRDS(
  fig_ab,
  file = file.path(OUT_DIR, paste0(FILE_STUB, ".rds"))
)

fig_ab