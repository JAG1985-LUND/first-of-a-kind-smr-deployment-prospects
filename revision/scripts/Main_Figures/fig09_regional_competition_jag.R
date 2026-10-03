# ============================================================
# FIGURE 9
# REGIONAL FOAK COMPETITION SENSITIVITY
# JAG LOCAL VERSION
#
# a) Global cumulative FOAK deployment under regional competition
# b) Regional FOAK deployment by 2050 — Reference scenario
# c) Regional FOAK deployment by 2050 — Proliferation scenario
#
# Panel a:
#   Median cumulative FOAK deployment with P10-P90 ribbons
#
# Panels b-c:
#   Median regional FOAK deployment with P10-P90 error bars
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
# PATHS
# ============================================================

FILE <- paste0(
  "C:/Users/joelg/OneDrive/Nat. Comms v2/",
  "Revision 1_source_data_main/",
  "fig09_source_data_regional_competition.xlsx"
)


OUT_DIR <- paste0(
  "C:/Users/joelg/OneDrive/Nat. Comms v2/",
  "Revision 1_outputs_main"
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


# ============================================================
# REQUIRED SHEETS
# ============================================================

required_sheets <- c(
  "panel_a_combined",
  "panel_b_ref_reg",
  "panel_c_pro_reg"
)


available_sheets <- excel_sheets(
  FILE
)


missing_sheets <- required_sheets[
  !required_sheets %in% available_sheets
]


if (length(missing_sheets) > 0) {
  
  stop(
    "Missing required sheet(s): ",
    paste(
      missing_sheets,
      collapse = ", "
    )
  )
}


# ============================================================
# EXPORT SETTINGS
# ============================================================

SAVE_W_MM <- 180
SAVE_H_MM <- 170
SAVE_DPI  <- 600

FILE_STUB <- "fig09_regional_competition"


# ============================================================
# TYPOGRAPHY
# ============================================================

BASE_SIZE         <- 7
AXIS_TEXT_SIZE    <- 6.5
AXIS_TITLE_SIZE   <- 7
TITLE_SIZE        <- 7
TAG_SIZE          <- 11
LEGEND_TEXT_SIZE  <- 6.2
LEGEND_TITLE_SIZE <- 6.2


# ============================================================
# LINE / POINT SETTINGS
# ============================================================

LW_REF <- 1.05
LW_PRO <- 1.05
LW_ERR <- 0.45

POINT_SIZE <- 1.8


# ============================================================
# SCENARIO COLOURS
# ============================================================

COL_REF_LINE <- "grey35"
COL_REF_FILL <- "grey70"

COL_PRO_LINE <- "#228B22"
COL_PRO_FILL <- "#90EE90"

ALPHA_REF <- 0.50
ALPHA_PRO <- 0.40


# ============================================================
# REGIONAL COLOURS
# ============================================================

REGION_COLS <- c(
  "Russia"        = "#B22222",
  "China"         = "#C45A32",
  "RoW"           = "#7A3DB8",
  "North America" = "#228B22",
  "Europe"        = "#0072B2",
  "OECD-Asia"     = "#2A9D8F"
)


REGION_ORDER <- c(
  "Russia",
  "China",
  "RoW",
  "North America",
  "Europe",
  "OECD-Asia"
)


# ============================================================
# HELPERS
# ============================================================

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


# ============================================================
# COMMON THEME
# ============================================================

theme_locked <- theme_classic(
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
      margin = margin(
        t = 4
      )
    ),
    
    axis.title.y = element_text(
      size = AXIS_TITLE_SIZE,
      margin = margin(
        r = 4
      )
    ),
    
    plot.title = element_blank(),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    panel.background = element_rect(
      fill = "white",
      colour = NA
    )
  )


# ============================================================
# PANEL DECORATION
# ============================================================

add_panel_decorations <- function(
    plot_obj,
    tag,
    title_text,
    title_x = 0.54,
    title_y = 0.985
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
      x = title_x,
      y = title_y,
      hjust = 0.5,
      vjust = 1,
      size = TITLE_SIZE,
      fontface = "bold"
    )
}


# ============================================================
# READ PANEL A
# ============================================================

df_a <- read_excel(
  FILE,
  sheet = "panel_a_combined"
)


names(df_a) <- clean_names_local(
  names(df_a)
)


df_a <- df_a %>%
  
  transmute(
    
    scenario = str_squish(
      as.character(
        scenario
      )
    ),
    
    year = as.numeric(
      year
    ),
    
    p10 = as.numeric(
      p10
    ),
    
    p50 = as.numeric(
      p50
    ),
    
    p90 = as.numeric(
      p90
    )
  ) %>%
  
  mutate(
    
    scenario = case_when(
      
      str_detect(
        str_to_lower(scenario),
        "reference"
      ) ~
        "Reference scenario",
      
      str_detect(
        str_to_lower(scenario),
        "proliferation"
      ) ~
        "Proliferation scenario",
      
      TRUE ~ scenario
    ),
    
    scenario = factor(
      scenario,
      levels = c(
        "Reference scenario",
        "Proliferation scenario"
      )
    )
  ) %>%
  
  arrange(
    scenario,
    year
  )


# ============================================================
# PANEL A DATA
# ============================================================

df_a_ref <- df_a %>%
  
  filter(
    scenario ==
      "Reference scenario"
  )


df_a_pro <- df_a %>%
  
  filter(
    scenario ==
      "Proliferation scenario"
  )


# ============================================================
# PANEL A
# GLOBAL FOAK DEPLOYMENT UNDER REGIONAL COMPETITION
# ============================================================

p_a_core <- ggplot() +
  
  # Reference P10-P90 ribbon
  geom_ribbon(
    data = df_a_ref,
    aes(
      x = year,
      ymin = p10,
      ymax = p90
    ),
    fill = COL_REF_FILL,
    alpha = ALPHA_REF,
    colour = NA
  ) +
  
  # Proliferation P10-P90 ribbon
  geom_ribbon(
    data = df_a_pro,
    aes(
      x = year,
      ymin = p10,
      ymax = p90
    ),
    fill = COL_PRO_FILL,
    alpha = ALPHA_PRO,
    colour = NA
  ) +
  
  # Reference median
  geom_step(
    data = df_a_ref,
    aes(
      x = year,
      y = p50,
      colour = "Reference scenario"
    ),
    linewidth = LW_REF,
    direction = "hv",
    lineend = "round"
  ) +
  
  # Proliferation median
  geom_step(
    data = df_a_pro,
    aes(
      x = year,
      y = p50,
      colour = "Proliferation scenario"
    ),
    linewidth = LW_PRO,
    direction = "hv",
    lineend = "round"
  ) +
  
  scale_colour_manual(
    
    values = c(
      "Reference scenario" =
        COL_REF_LINE,
      
      "Proliferation scenario" =
        COL_PRO_LINE
    ),
    
    breaks = c(
      "Reference scenario",
      "Proliferation scenario"
    ),
    
    labels = c(
      "Reference (P10-P90)",
      "Proliferation (P10-P90)"
    ),
    
    name = "Scenario"
  ) +
  
  scale_x_continuous(
    
    limits = c(
      2018,
      2050
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
      45
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
    y = "Cumulative FOAK SMRs"
  ) +
  
  theme_locked +
  
  theme(
    
    legend.position = "bottom",
    
    legend.direction = "horizontal",
    
    legend.title = element_text(
      size = LEGEND_TITLE_SIZE
    ),
    
    legend.text = element_text(
      size = LEGEND_TEXT_SIZE
    ),
    
    legend.key.width = unit(
      0.65,
      "cm"
    ),
    
    legend.margin = margin(
      t = 2,
      r = 0,
      b = 0,
      l = 0
    ),
    
    plot.margin = margin(
      6,
      12,
      2,
      8
    )
  )


p_a <- add_panel_decorations(
  p_a_core,
  tag = "a",
  title_text =
    "Global FOAK deployment under regional competition",
  title_x = 0.52
)


# ============================================================
# READ REGIONAL PANELS
# ============================================================

df_b <- read_excel(
  FILE,
  sheet = "panel_b_ref_reg"
)


df_c <- read_excel(
  FILE,
  sheet = "panel_c_pro_reg"
)


names(df_b) <- clean_names_local(
  names(df_b)
)


names(df_c) <- clean_names_local(
  names(df_c)
)


# ============================================================
# STANDARDISE REGIONAL DATA
# ============================================================

standardise_region <- function(x) {
  
  x_clean <- str_to_lower(
    str_squish(
      as.character(x)
    )
  )
  
  case_when(
    
    x_clean == "russia" ~
      "Russia",
    
    x_clean == "china" ~
      "China",
    
    x_clean %in% c(
      "row",
      "rest of world"
    ) ~
      "RoW",
    
    x_clean %in% c(
      "north america",
      "north-america"
    ) ~
      "North America",
    
    x_clean == "europe" ~
      "Europe",
    
    x_clean %in% c(
      "oecd asia",
      "oecd-asia"
    ) ~
      "OECD-Asia",
    
    TRUE ~ as.character(x)
  )
}


prepare_regional <- function(df) {
  
  df %>%
    
    transmute(
      
      scenario = as.character(
        scenario
      ),
      
      region = standardise_region(
        region
      ),
      
      year = as.numeric(
        year
      ),
      
      p10 = as.numeric(
        p10
      ),
      
      p50 = as.numeric(
        p50
      ),
      
      p90 = as.numeric(
        p90
      )
    ) %>%
    
    filter(
      year == 2050
    ) %>%
    
    mutate(
      region = factor(
        region,
        levels = REGION_ORDER
      )
    ) %>%
    
    arrange(
      region
    )
}


df_b <- prepare_regional(
  df_b
)


df_c <- prepare_regional(
  df_c
)


# ============================================================
# REGION CHECK
# ============================================================

missing_b <- setdiff(
  REGION_ORDER,
  as.character(
    df_b$region
  )
)


missing_c <- setdiff(
  REGION_ORDER,
  as.character(
    df_c$region
  )
)


if (length(missing_b) > 0) {
  
  warning(
    "Panel b missing region(s): ",
    paste(
      missing_b,
      collapse = ", "
    )
  )
}


if (length(missing_c) > 0) {
  
  warning(
    "Panel c missing region(s): ",
    paste(
      missing_c,
      collapse = ", "
    )
  )
}


# ============================================================
# REGIONAL PANEL FUNCTION
#
# P50 = bar height
# P10-P90 = error bar
#
# Common 0-10.5 scale retained across both panels.
# Highest labelled tick = 10, with slight headroom above.
# ============================================================

make_regional_panel <- function(
    panel_data
) {
  
  ggplot(
    panel_data,
    aes(
      x = region,
      y = p50
    )
  ) +
    
    # Median bars
    geom_col(
      aes(
        fill = region
      ),
      width = 0.62,
      colour = NA
    ) +
    
    # P10-P90 intervals
    geom_errorbar(
      aes(
        ymin = p10,
        ymax = p90
      ),
      width = 0.18,
      linewidth = LW_ERR,
      colour = "grey20"
    ) +
    
    # P50 marker
    geom_point(
      size = POINT_SIZE,
      shape = 21,
      stroke = 0.45,
      fill = "white",
      colour = "grey15"
    ) +
    
    scale_fill_manual(
      values = REGION_COLS,
      breaks = REGION_ORDER,
      drop = FALSE
    ) +
    
    scale_x_discrete(
      limits = REGION_ORDER,
      drop = FALSE
    ) +
    
    scale_y_continuous(
      
      limits = c(
        0,
        10.5
      ),
      
      breaks = c(
        0,
        2,
        4,
        6,
        8,
        10
      ),
      
      labels = hide_zero,
      
      expand = c(
        0,
        0
      )
    ) +
    
    labs(
      x = "Region",
      y = "FOAK SMRs by 2050"
    ) +
    
    theme_locked +
    
    theme(
      
      legend.position = "none",
      
      axis.text.x = element_text(
        size = AXIS_TEXT_SIZE,
        angle = 30,
        hjust = 1,
        vjust = 1,
        colour = "grey20"
      ),
      
      plot.margin = margin(
        6,
        8,
        8,
        6
      )
    )
}


# ============================================================
# PANEL B
# REFERENCE SCENARIO
# ============================================================

p_b_core <- make_regional_panel(
  df_b
)


p_b <- add_panel_decorations(
  p_b_core,
  tag = "b",
  title_text = "Reference scenario"
)


# ============================================================
# PANEL C
# PROLIFERATION SCENARIO
# ============================================================

p_c_core <- make_regional_panel(
  df_c
)


p_c <- add_panel_decorations(
  p_c_core,
  tag = "c",
  title_text = "Proliferation scenario"
)


# ============================================================
# COMBINE PANELS B + C
# ============================================================

bottom_row <- plot_grid(
  p_b,
  p_c,
  ncol = 2,
  
  rel_widths = c(
    1,
    1
  ),
  
  align = "hv",
  axis = "tblr"
)


# ============================================================
# FINAL FIGURE
# PANEL A FULL WIDTH
# PANELS B + C BELOW
# ============================================================

final_plot <- plot_grid(
  p_a,
  bottom_row,
  ncol = 1,
  
  rel_heights = c(
    1.08,
    1
  ),
  
  align = "v",
  axis = "lr"
)


# ============================================================
# DISPLAY
# ============================================================

print(
  final_plot
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


ggsave(
  filename = PNG_FILE,
  plot = final_plot,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)


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
# CONFIRM OUTPUTS
# ============================================================

cat(
  "\nFigure 9 successfully exported:\n",
  PNG_FILE,
  "\n",
  PDF_FILE,
  "\n"
)


final_plot