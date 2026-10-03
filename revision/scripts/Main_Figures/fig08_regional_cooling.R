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
# FIGURE 8 — REGIONAL DEPLOYMENT AND REGIONAL COOLING
#
# GITHUB-REPRODUCIBLE VERSION
#
# a) Regional cumulative deployment — Reference scenario
# b) Regional cumulative deployment — Proliferation scenario
# c) Regional cooling composition — Reference scenario
# d) Regional cooling composition — Proliferation scenario
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
      "fig08_source_data_regional_cooling.xlsx"
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
          "fig08_source_data_regional_cooling.xlsx\n",
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
  "fig08_source_data_regional_cooling.xlsx"
)


OUT_DIR <- file.path(
  ROOT_DIR,
  "revision",
  "outputs",
  "Main_Figures"
)


if (!file.exists(FILE)) {
  
  stop(
    "Input file not found: ",
    FILE
  )
}


dir.create(
  OUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# SHEET HANDLING
# ------------------------------------------------------------

sheet_names <- excel_sheets(
  FILE
)


regional_sheet <- intersect(
  c(
    "regional_deployment",
    "regional deployment",
    "Regional Deployment"
  ),
  sheet_names
)[1]


cooling_sheet <- intersect(
  c(
    "cooling_type",
    "regional_cooling",
    "regional cooling",
    "Regional Cooling",
    "cooling type"
  ),
  sheet_names
)[1]


if (is.na(regional_sheet)) {
  
  stop(
    "Regional deployment sheet not found in: ",
    FILE
  )
}


if (is.na(cooling_sheet)) {
  
  stop(
    "Regional cooling sheet not found in: ",
    FILE
  )
}


# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 180
SAVE_H_MM <- 180
SAVE_DPI  <- 600

FILE_STUB <- "fig08_regional_cooling"


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
# COMMON THEME
# ------------------------------------------------------------

common_theme <- theme_classic(
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
      colour = "grey20"
    ),
    
    axis.text.y = element_text(
      size = AXIS_TEXT_SIZE,
      colour = "grey20"
    ),
    
    plot.title = element_blank(),
    
    legend.position = c(
      0.02,
      0.98
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
    
    legend.key.height = unit(
      0.32,
      "cm"
    ),
    
    legend.key.width = unit(
      0.32,
      "cm"
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
    
    legend.title = element_text(
      size = LEGEND_TITLE_SIZE
    ),
    
    legend.text = element_text(
      size = LEGEND_TEXT_SIZE
    ),
    
    plot.margin = margin(
      t = 6,
      r = 8,
      b = 6,
      l = 6
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
# PANEL DECORATION
# ------------------------------------------------------------

add_panel_decorations <- function(
    plot_obj,
    tag,
    title_text
) {
  
  ggdraw() +
    
    draw_plot(
      plot_obj,
      x = 0,
      y = 0,
      width = 1,
      height = 0.955
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
      x = 0.56,
      y = 0.985,
      hjust = 0.5,
      vjust = 1,
      size = TITLE_SIZE,
      fontface = "bold"
    )
}


# ------------------------------------------------------------
# CLEAN COLUMN NAMES
# ------------------------------------------------------------

clean_names <- function(x) {
  
  x %>%
    str_trim() %>%
    str_replace_all(
      "\\s+",
      "_"
    ) %>%
    str_replace_all(
      "\\.+",
      "_"
    ) %>%
    str_replace_all(
      "_+$",
      ""
    ) %>%
    str_to_lower()
}


# ============================================================
# PANELS A–B — REGIONAL CUMULATIVE DEPLOYMENT
# ============================================================

df_reg <- read_excel(
  FILE,
  sheet = regional_sheet
)


names(df_reg) <- clean_names(
  names(df_reg)
)


plot_df_reg <- df_reg %>%
  
  mutate(
    
    year =
      as.numeric(year),
    
    region = case_when(
      
      str_to_lower(region) %in%
        c(
          "north america",
          "north_america",
          "northamerica"
        ) ~ "North America",
      
      str_to_lower(region) %in%
        c(
          "europe"
        ) ~ "Europe",
      
      str_to_lower(region) %in%
        c(
          "oecd asia",
          "oecd_asia",
          "oecd-asia",
          "oecdasia"
        ) ~ "OECD-Asia",
      
      str_to_lower(region) %in%
        c(
          "china"
        ) ~ "China",
      
      str_to_lower(region) %in%
        c(
          "russia"
        ) ~ "Russia",
      
      str_to_lower(region) %in%
        c(
          "row",
          "rest of world",
          "rest_of_world",
          "restofworld"
        ) ~ "RoW",
      
      TRUE ~
        as.character(region)
    )
  ) %>%
  
  pivot_longer(
    cols = c(
      baseline,
      accelerated
    ),
    names_to = "scenario",
    values_to = "value"
  ) %>%
  
  mutate(
    
    scenario = factor(
      
      case_when(
        
        scenario == "baseline" ~
          "Reference scenario",
        
        scenario == "accelerated" ~
          "Proliferation scenario",
        
        TRUE ~
          scenario
      ),
      
      levels = c(
        "Reference scenario",
        "Proliferation scenario"
      )
    ),
    
    region = factor(
      region,
      levels = c(
        "Russia",
        "China",
        "RoW",
        "North America",
        "Europe",
        "OECD-Asia"
      )
    ),
    
    value =
      as.numeric(value)
  ) %>%
  
  arrange(
    scenario,
    year,
    region
  )


# ------------------------------------------------------------
# REGIONAL COLOURS
# ------------------------------------------------------------

region_cols <- c(
  "Russia"        = "#B22222",
  "China"         = "#C45A32",
  "RoW"           = "#7A3DB8",
  "North America" = "#228B22",
  "Europe"        = "#0072B2",
  "OECD-Asia"     = "#2A9D8F"
)


# ------------------------------------------------------------
# REGIONAL DEPLOYMENT PANEL FUNCTION
# ------------------------------------------------------------

make_stack_panel <- function(
    panel_data
) {
  
  ggplot(
    panel_data,
    aes(
      x = year,
      y = value,
      fill = region,
      group = region
    )
  ) +
    
    geom_area(
      position = position_stack(
        reverse = TRUE
      ),
      alpha = 0.95,
      colour = NA
    ) +
    
    scale_fill_manual(
      values = region_cols,
      breaks = c(
        "Russia",
        "China",
        "RoW",
        "North America",
        "Europe",
        "OECD-Asia"
      ),
      name = "Region"
    ) +
    
    scale_x_continuous(
      breaks = c(
        2020,
        2030,
        2040,
        2050
      ),
      limits = c(
        2018,
        2050
      ),
      expand = c(
        0,
        0
      )
    ) +
    
    scale_y_continuous(
      breaks = c(
        10,
        20,
        30,
        40,
        50
      ),
      limits = c(
        0,
        50
      ),
      expand = c(
        0,
        0
      )
    ) +
    
    labs(
      x = "Year",
      y = "Median cumulative FOAK SMRs"
    ) +
    
    common_theme
}


# ------------------------------------------------------------
# PANEL A — REFERENCE
# ------------------------------------------------------------

p_a_core <- make_stack_panel(
  filter(
    plot_df_reg,
    scenario ==
      "Reference scenario"
  )
)


# ------------------------------------------------------------
# PANEL B — PROLIFERATION
# ------------------------------------------------------------

p_b_core <- make_stack_panel(
  filter(
    plot_df_reg,
    scenario ==
      "Proliferation scenario"
  )
)


p_a <- add_panel_decorations(
  p_a_core,
  "a",
  "Reference scenario"
)


p_b <- add_panel_decorations(
  p_b_core,
  "b",
  "Proliferation scenario"
)


# ============================================================
# PANELS C–D — REGIONAL COOLING BY 2050
# ============================================================

df_cool <- read_excel(
  FILE,
  sheet = cooling_sheet
)


names(df_cool) <- clean_names(
  names(df_cool)
)


plot_df_cool <- df_cool %>%
  
  mutate(
    
    year =
      as.numeric(year),
    
    region = case_when(
      
      str_to_lower(region) %in%
        c(
          "north america",
          "north_america",
          "northamerica"
        ) ~ "North America",
      
      str_to_lower(region) %in%
        c(
          "europe"
        ) ~ "Europe",
      
      str_to_lower(region) %in%
        c(
          "oecd asia",
          "oecd_asia",
          "oecd-asia",
          "oecdasia"
        ) ~ "OECD-Asia",
      
      str_to_lower(region) %in%
        c(
          "china"
        ) ~ "China",
      
      str_to_lower(region) %in%
        c(
          "russia"
        ) ~ "Russia",
      
      str_to_lower(region) %in%
        c(
          "row",
          "rest of world",
          "rest_of_world",
          "restofworld"
        ) ~ "RoW",
      
      TRUE ~
        as.character(region)
    ),
    
    cooling_type = case_when(
      
      str_to_lower(cooling_type) %in%
        c(
          "water-cooled",
          "water cooled",
          "water"
        ) ~ "Water-cooled",
      
      str_to_lower(cooling_type) %in%
        c(
          "metal-cooled",
          "metal cooled",
          "metal"
        ) ~ "Metal-cooled",
      
      str_to_lower(cooling_type) %in%
        c(
          "molten-salt-cooled",
          "molten salt cooled",
          "molten-salt",
          "salt-cooled",
          "salt cooled",
          "salt"
        ) ~ "Salt-cooled",
      
      str_to_lower(cooling_type) %in%
        c(
          "gas-cooled",
          "gas cooled",
          "gas"
        ) ~ "Gas-cooled",
      
      TRUE ~
        as.character(cooling_type)
    )
  ) %>%
  
  filter(
    year == 2050
  ) %>%
  
  pivot_longer(
    cols = c(
      baseline,
      accelerated
    ),
    names_to = "scenario",
    values_to = "value"
  ) %>%
  
  mutate(
    
    scenario = factor(
      
      case_when(
        
        scenario == "baseline" ~
          "Reference scenario",
        
        scenario == "accelerated" ~
          "Proliferation scenario",
        
        TRUE ~
          scenario
      ),
      
      levels = c(
        "Reference scenario",
        "Proliferation scenario"
      )
    ),
    
    region = factor(
      region,
      levels = c(
        "Russia",
        "China",
        "RoW",
        "North America",
        "Europe",
        "OECD-Asia"
      )
    ),
    
    cooling_type = factor(
      cooling_type,
      levels = c(
        "Water-cooled",
        "Metal-cooled",
        "Salt-cooled",
        "Gas-cooled"
      )
    ),
    
    value =
      as.numeric(value)
  ) %>%
  
  arrange(
    scenario,
    region,
    cooling_type
  )


# ------------------------------------------------------------
# COOLING-TYPE COLOURS
# ------------------------------------------------------------

cooling_palette <- c(
  "Water-cooled" = "#1f77b4",
  "Metal-cooled" = "#A9A9A9",
  "Salt-cooled"  = "#1b9e77",
  "Gas-cooled"   = "#d62728"
)


# ------------------------------------------------------------
# REGIONAL COOLING PANEL FUNCTION
# ------------------------------------------------------------

make_cooling_panel <- function(
    panel_data
) {
  
  ggplot(
    panel_data,
    aes(
      x = region,
      y = value,
      fill = cooling_type
    )
  ) +
    
    geom_col(
      width = 0.62,
      position = position_stack(
        reverse = TRUE
      )
    ) +
    
    scale_fill_manual(
      values = cooling_palette,
      breaks = c(
        "Water-cooled",
        "Metal-cooled",
        "Salt-cooled",
        "Gas-cooled"
      ),
      name = "Cooling type"
    ) +
    
    scale_y_continuous(
      breaks = c(
        5,
        10,
        15
      ),
      limits = c(
        0,
        15
      ),
      expand = c(
        0,
        0
      )
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


# ------------------------------------------------------------
# PANEL C — REFERENCE
# ------------------------------------------------------------

p_c_core <- make_cooling_panel(
  filter(
    plot_df_cool,
    scenario ==
      "Reference scenario"
  )
)


# ------------------------------------------------------------
# PANEL D — PROLIFERATION
# ------------------------------------------------------------

p_d_core <- make_cooling_panel(
  filter(
    plot_df_cool,
    scenario ==
      "Proliferation scenario"
  )
)


p_c <- add_panel_decorations(
  p_c_core,
  "c",
  "Reference scenario"
)


p_d <- add_panel_decorations(
  p_d_core,
  "d",
  "Proliferation scenario"
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


fig08_regional_cooling <- plot_grid(
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
  fig08_regional_cooling
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
  plot = fig08_regional_cooling,
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
  plot = fig08_regional_cooling,
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
  "\nFigure 8 successfully generated.\n"
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


fig08_regional_cooling