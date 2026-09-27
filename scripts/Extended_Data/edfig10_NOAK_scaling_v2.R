# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Extended Data Fig. 10:
# Post-FOAK scaling window to 2050 across regional and national groupings
# and by reactor cooling type.
#
# NOTE ON RENUMBERING:
# This figure was Extended Data Fig. 9 in the previous manuscript version.
# Following insertion of the regional FOAK competition sensitivity as
# Extended Data Fig. 7, this figure is now Extended Data Fig. 10.
#
# Panel a:
# Post-FOAK scaling window to 2050 across regional and national groupings.
#
# Panel b:
# Post-FOAK scaling window to 2050 by cooling type.
#
# Manuscript-facing scenario terminology:
#   Baseline    -> Reference
#   Accelerated -> Proliferation
#
# IMPORTANT:
# The source workbook retains the original internal column names:
#   bas_p10 / bas_p50 / bas_p90
#   acc_p10 / acc_p50 / acc_p90
#
# These internal source-data names are intentionally unchanged.
#
# Required input:
# data/Extended_Data/edfig10_source_data_NOAK_scaling.xlsx
#
# Outputs:
# outputs/Extended_Data/Figure_10/edfig10_noak_scaling_v2.png
# outputs/Extended_Data/Figure_10/edfig10_noak_scaling_v2.pdf
#
# Run this script from the repository root.


# ============================================================
# 1. PACKAGES
# ============================================================

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
# 2. EXTENDED DATA FIGURE 10
# POST-FOAK SCALING WINDOW TO 2050
# ============================================================


# ============================================================
# 3. REPOSITORY PATHS
# ============================================================

FILE <- file.path(
  "data",
  "Extended_Data",
  "edfig10_source_data_NOAK_scaling.xlsx"
)

OUT_DIR <- file.path(
  "outputs",
  "Extended_Data",
  "Figure_10"
)

dir.create(
  OUT_DIR,
  showWarnings = FALSE,
  recursive = TRUE
)

if (!file.exists(FILE)) {
  stop(
    paste0(
      "Input file not found: ",
      FILE,
      "\nRun this script from the repository root."
    )
  )
}


# ============================================================
# 4. LOAD SOURCE DATA
# ============================================================

df_raw <- read_excel(
  FILE,
  sheet = 1,
  col_names = TRUE
)


# ------------------------------------------------------------
# Identify grouping column
# ------------------------------------------------------------

group_col <- if ("global_group" %in% names(df_raw)) {
  
  "global_group"
  
} else if ("group" %in% names(df_raw)) {
  
  "group"
  
} else {
  
  stop(
    "Neither 'global_group' nor 'group' was found in the input file."
  )
}


# ============================================================
# 5. PANEL A DATA
# REGIONAL / NATIONAL GROUPINGS
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


# ============================================================
# 6. PANEL A Y-POSITIONS
# ============================================================

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
      rep("Reference", 8),
      rep("Proliferation", 8)
    ),
    levels = c(
      "Reference",
      "Proliferation"
    )
  ),
  
  y = c(
    16, 14, 12, 10, 8, 6, 3, 1,
    15, 13, 11, 9, 7, 5, 2, 0
  )
)


# ============================================================
# 7. PANEL A PLOTTING DATA
# ============================================================

plot_df_a <- bind_rows(
  
  # ----------------------------------------------------------
  # Reference scenario
  # Source columns retain original "bas_*" naming.
  # ----------------------------------------------------------
  
  df_a %>%
    transmute(
      group,
      
      Scenario = factor(
        "Reference",
        levels = c(
          "Reference",
          "Proliferation"
        )
      ),
      
      p10 = bas_p10,
      p50 = bas_p50,
      p90 = bas_p90
    ),
  
  
  # ----------------------------------------------------------
  # Proliferation scenario
  # Source columns retain original "acc_*" naming.
  # ----------------------------------------------------------
  
  df_a %>%
    transmute(
      group,
      
      Scenario = factor(
        "Proliferation",
        levels = c(
          "Reference",
          "Proliferation"
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


# ============================================================
# 8. PANEL A LABEL DATA
# ============================================================

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


# ============================================================
# 9. PANEL A PLOT
# ============================================================

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
      filter(
        Scenario == "Reference"
      ),
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
      filter(
        Scenario == "Proliferation"
      ),
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
      "Reference" = "grey45",
      "Proliferation" = "#228B22"
    ),
    breaks = c(
      "Reference",
      "Proliferation"
    )
  ) +
  
  coord_cartesian(
    xlim = c(
      0,
      30
    ),
    ylim = c(
      -0.6,
      16.6
    ),
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
    expand = c(
      0,
      0
    )
  ) +
  
  scale_y_continuous(
    breaks = NULL,
    expand = c(
      0,
      0
    )
  ) +
  
  labs(
    x = "Post-FOAK scaling window to 2050 (years)",
    y = NULL,
    colour = "Scenario"
  ) +
  
  theme_minimal(
    base_size = 10
  ) +
  
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
    
    axis.text.x = element_text(
      size = 9
    ),
    
    axis.title.x = element_text(
      size = 9
    ),
    
    legend.position = "none",
    legend.direction = "horizontal",
    
    legend.title = element_text(
      size = 9
    ),
    
    legend.text = element_text(
      size = 9
    ),
    
    legend.key.width = unit(
      6,
      "mm"
    ),
    
    legend.key.height = unit(
      4,
      "mm"
    ),
    
    plot.margin = margin(
      4,
      10,
      4,
      120
    )
  )


# ============================================================
# 10. PANEL B DATA
# COOLING TYPE
# ============================================================

df_b <- df_raw %>%
  
  transmute(
    type_raw = as.character(.data[[group_col]]),
    bas_p10  = as.numeric(bas_p10),
    bas_p50  = as.numeric(bas_p50),
    bas_p90  = as.numeric(bas_p90),
    acc_p10  = as.numeric(acc_p10),
    acc_p50  = as.numeric(acc_p50),
    acc_p90  = as.numeric(acc_p90)
  ) %>%
  
  mutate(
    type = case_when(
      
      str_to_lower(
        str_squish(type_raw)
      ) %in% c(
        "water-cooled",
        "water cooled",
        "water_cooled"
      ) ~ "Water-cooled",
      
      str_to_lower(
        str_squish(type_raw)
      ) %in% c(
        "gas-cooled",
        "gas cooled",
        "gas_cooled"
      ) ~ "Gas-cooled",
      
      str_to_lower(
        str_squish(type_raw)
      ) %in% c(
        "metal-cooled",
        "metal cooled",
        "metal_cooled"
      ) ~ "Metal-cooled",
      
      str_to_lower(
        str_squish(type_raw)
      ) %in% c(
        "molten-salt-cooled",
        "molten salt cooled",
        "molten_salt_cooled",
        "salt",
        "molten salt"
      ) ~ "Molten-salt-cooled",
      
      TRUE ~ NA_character_
    )
  ) %>%
  
  filter(
    !is.na(type)
  ) %>%
  
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


# ============================================================
# 11. PANEL B Y-POSITIONS
# ============================================================

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
      "Reference",
      "Reference",
      "Reference",
      "Reference",
      "Proliferation",
      "Proliferation",
      "Proliferation",
      "Proliferation"
    ),
    levels = c(
      "Reference",
      "Proliferation"
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


# ============================================================
# 12. PANEL B PLOTTING DATA
# ============================================================

plot_df_b <- bind_rows(
  
  # ----------------------------------------------------------
  # Reference scenario
  # ----------------------------------------------------------
  
  df_b %>%
    transmute(
      type,
      
      Scenario = factor(
        "Reference",
        levels = c(
          "Reference",
          "Proliferation"
        )
      ),
      
      p10 = bas_p10,
      p50 = bas_p50,
      p90 = bas_p90
    ),
  
  
  # ----------------------------------------------------------
  # Proliferation scenario
  # ----------------------------------------------------------
  
  df_b %>%
    transmute(
      type,
      
      Scenario = factor(
        "Proliferation",
        levels = c(
          "Reference",
          "Proliferation"
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


# ============================================================
# 13. PANEL B LABEL DATA
# ============================================================

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


# ============================================================
# 14. PANEL B PLOT
# ============================================================

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
      filter(
        Scenario == "Reference"
      ),
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
      filter(
        Scenario == "Proliferation"
      ),
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
      "Reference" = "grey45",
      "Proliferation" = "#228B22"
    ),
    breaks = c(
      "Reference",
      "Proliferation"
    )
  ) +
  
  coord_cartesian(
    xlim = c(
      0,
      30
    ),
    ylim = c(
      0.4,
      8.6
    ),
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
    expand = c(
      0,
      0
    )
  ) +
  
  scale_y_continuous(
    breaks = NULL,
    expand = c(
      0,
      0
    )
  ) +
  
  labs(
    x = "Post-FOAK scaling window to 2050 (years)",
    y = NULL,
    colour = "Scenario"
  ) +
  
  theme_minimal(
    base_size = 10
  ) +
  
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
    
    axis.text.x = element_text(
      size = 9
    ),
    
    axis.title.x = element_text(
      size = 9
    ),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    
    legend.title = element_text(
      size = 9
    ),
    
    legend.text = element_text(
      size = 9
    ),
    
    legend.key.width = unit(
      6,
      "mm"
    ),
    
    legend.key.height = unit(
      4,
      "mm"
    ),
    
    plot.margin = margin(
      4,
      10,
      4,
      120
    )
  )


# ============================================================
# 15. COMBINE PANELS
# ============================================================

fig_ab <- plot_grid(
  p_a,
  p_b,
  ncol = 1,
  
  rel_heights = c(
    1.00,
    0.78
  ),
  
  align = "v",
  axis = "lr",
  
  labels = c(
    "a",
    "b"
  ),
  
  label_size = 11,
  label_fontface = "bold",
  label_x = 0.01,
  label_y = 0.99,
  hjust = 0,
  vjust = 1
)


# ============================================================
# 16. DISPLAY
# ============================================================

print(fig_ab)


# ============================================================
# 17. EXPORT PNG
# ============================================================

ggsave(
  filename = file.path(
    OUT_DIR,
    "edfig10_noak_scaling_v2.png"
  ),
  plot = fig_ab,
  width = 180,
  height = 150,
  units = "mm",
  dpi = 600,
  bg = "white"
)


# ============================================================
# 18. EXPORT PDF
# ============================================================

ggsave(
  filename = file.path(
    OUT_DIR,
    "edfig10_noak_scaling_v2.pdf"
  ),
  plot = fig_ab,
  width = 180,
  height = 150,
  units = "mm",
  bg = "white"
)


# ============================================================
# 19. CONFIRM OUTPUTS
# ============================================================

cat(
  "\nExtended Data Figure 10 v2 successfully exported:\n",
  file.path(
    OUT_DIR,
    "edfig10_noak_scaling_v2.png"
  ),
  "\n",
  file.path(
    OUT_DIR,
    "edfig10_noak_scaling_v2.pdf"
  ),
  "\n"
)


fig_ab


# ============================================================
# END
# ============================================================