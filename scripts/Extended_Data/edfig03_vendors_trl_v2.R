# ============================================================
# EXTENDED DATA FIGURE 3 — VENDOR AMBITION GAP MAPPED AGAINST TRL
# GITHUB-COMPATIBLE v2
#
# a) Vendor target year vs TRL
# b) Reference P50 FOAK deployment year vs TRL
# c) Proliferation P50 FOAK deployment year vs TRL
#
# Revised scenario terminology:
#   Baseline    -> Reference
#   Accelerated -> Proliferation
#
# IMPORTANT:
# The source workbook retains the original internal column names
# "baseline_median" and "accelerated_median".
# These are intentionally unchanged for provenance.
# Only manuscript-facing terminology is updated.
#
# Required input:
#   data/Extended_Data/edfig03_source_data_vendors_trl.xlsx
#
# Outputs:
#   outputs/Extended_Data/Figure_03/
#     edfig03_vendors_trl_v2.png
#     edfig03_vendors_trl_v2.pdf
# ============================================================


# ============================================================
# PACKAGES
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(cowplot)
  library(grid)
})

rm(list = ls())
graphics.off()


# ============================================================
# REPOSITORY-RELATIVE PATHS
# ============================================================

FILE_IN <- file.path(
  "data",
  "Extended_Data",
  "edfig03_source_data_vendors_trl.xlsx"
)

OUT_DIR <- file.path(
  "outputs",
  "Extended_Data",
  "Figure_03"
)

if (!file.exists(FILE_IN)) {
  stop(
    "Input file not found: ",
    FILE_IN,
    "\nRun this script from the repository root."
  )
}

dir.create(
  OUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# EXPORT
# ============================================================

SAVE_W_IN <- 10.9
SAVE_H_IN <- 5.35
SAVE_DPI  <- 600

FILE_STUB <- "edfig03_vendors_trl_v2"


# ============================================================
# TYPOGRAPHY
# ============================================================

BASE_SIZE         <- 13
AXIS_TEXT_SIZE    <- 10.0
AXIS_TITLE_SIZE   <- 11.0
LEGEND_TEXT_SIZE  <- 10.0
LEGEND_TITLE_SIZE <- 10.6
PANEL_TAG_SIZE    <- 12


# ============================================================
# COLOURS
# ============================================================

phase_cols <- c(
  "Conceptual design" = "#FDE0D2",
  "Basic design"      = "#F4A582",
  "Detailed design"   = "#D6604D",
  "Licensing"         = "#D1E5F0",
  "Construction"      = "#92C5DE"
)

phase_order <- c(
  "Conceptual design",
  "Basic design",
  "Detailed design",
  "Licensing",
  "Construction"
)

COL_VENDOR <- "black"

# Reference scenario
COL_REF <- "grey40"

# Proliferation scenario
COL_PRO <- "#228B22"


# ============================================================
# THEME
# ============================================================

theme_base <- theme_classic(
  base_size = BASE_SIZE
) +
  theme(
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
      colour = "grey10",
      margin = margin(t = 3)
    ),
    
    axis.title.y = element_text(
      size = AXIS_TITLE_SIZE,
      colour = "grey10",
      margin = margin(r = 4)
    ),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    legend.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    legend.box.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    plot.margin = margin(
      3,
      2,
      2,
      2
    )
  )


# ============================================================
# READ SOURCE DATA
#
# Internal baseline/accelerated column names are deliberately
# retained in the source workbook.
#
# baseline_median    = Reference scenario
# accelerated_median = Proliferation scenario
# ============================================================

kang_sample_32 <- read_excel(
  FILE_IN
) %>%
  mutate(
    Phase = trimws(
      as.character(Phase)
    ),
    
    TRL = as.numeric(
      TRL
    ),
    
    vendor_target = as.numeric(
      vendor_target
    ),
    
    baseline_median = as.numeric(
      baseline_median
    ),
    
    accelerated_median = as.numeric(
      accelerated_median
    )
  ) %>%
  filter(
    !is.na(TRL),
    !is.na(Phase),
    Phase %in% phase_order
  ) %>%
  mutate(
    Phase = factor(
      Phase,
      levels = phase_order
    )
  )


# ============================================================
# AXIS LIMITS
# ============================================================

x_min_BC <- 2026
x_max_BC <- 2051.2

x_breaks_BC <- c(
  2030,
  2035,
  2040,
  2045,
  2050
)

x_min_A <- floor(
  min(
    kang_sample_32$vendor_target,
    na.rm = TRUE
  )
)

x_max_A <- 2036.6

x_breaks_A <- seq(
  x_min_A,
  2036,
  by = 2
)


# ============================================================
# MEDIANS / QUARTILES
# ============================================================

med_vendor <- median(
  kang_sample_32$vendor_target,
  na.rm = TRUE
)

med_ref <- median(
  kang_sample_32$baseline_median,
  na.rm = TRUE
)

med_pro <- median(
  kang_sample_32$accelerated_median,
  na.rm = TRUE
)

q1_vendor <- 2029.95
q3_vendor <- 2033.25


# ============================================================
# POINT JITTER
# ============================================================

jit <- position_jitter(
  width = 0.10,
  height = 0.05,
  seed = 42
)


# ============================================================
# PANEL a — VENDOR TARGET YEAR
# ============================================================

p_a <- ggplot(
  filter(
    kang_sample_32,
    !is.na(vendor_target)
  ),
  aes(
    x = vendor_target,
    y = TRL,
    fill = Phase
  )
) +
  
  geom_vline(
    xintercept = q1_vendor,
    linetype = "solid",
    linewidth = 0.35,
    colour = COL_VENDOR,
    alpha = 0.9
  ) +
  
  geom_vline(
    xintercept = q3_vendor,
    linetype = "solid",
    linewidth = 0.35,
    colour = COL_VENDOR,
    alpha = 0.9
  ) +
  
  geom_vline(
    xintercept = med_vendor,
    linetype = "dashed",
    linewidth = 0.7,
    colour = COL_VENDOR
  ) +
  
  geom_point(
    shape = 21,
    size = 3.7,
    stroke = 0.3,
    colour = "grey30",
    alpha = 0.95,
    position = jit
  ) +
  
  scale_fill_manual(
    values = phase_cols,
    breaks = phase_order,
    drop = FALSE
  ) +
  
  scale_y_continuous(
    breaks = 1:6,
    limits = c(
      0.92,
      6.18
    ),
    expand = c(
      0,
      0
    )
  ) +
  
  scale_x_continuous(
    limits = c(
      x_min_A,
      x_max_A
    ),
    breaks = x_breaks_A,
    expand = expansion(
      mult = c(
        0.005,
        0.005
      )
    )
  ) +
  
  coord_cartesian(
    clip = "off"
  ) +
  
  labs(
    x = "Vendor target year",
    y = "Technology readiness level (TRL)"
  ) +
  
  theme_base +
  
  theme(
    legend.position = "none"
  )


p_a <- ggdraw(
  p_a
) +
  draw_plot_label(
    label = "a",
    x = 0.01,
    y = 0.99,
    hjust = 0,
    vjust = 1,
    size = PANEL_TAG_SIZE,
    fontface = "bold"
  )


# ============================================================
# PANEL b — REFERENCE SCENARIO
#
# Source column: baseline_median
# ============================================================

p_b <- ggplot(
  filter(
    kang_sample_32,
    !is.na(baseline_median)
  ),
  aes(
    x = baseline_median,
    y = TRL,
    fill = Phase
  )
) +
  
  geom_vline(
    xintercept = med_vendor,
    linetype = "dashed",
    linewidth = 0.7,
    colour = COL_VENDOR
  ) +
  
  geom_vline(
    xintercept = med_ref,
    linetype = "dashed",
    linewidth = 0.7,
    colour = COL_REF
  ) +
  
  geom_point(
    shape = 21,
    size = 3.7,
    stroke = 0.3,
    colour = "grey30",
    alpha = 0.95,
    position = jit
  ) +
  
  scale_fill_manual(
    values = phase_cols,
    breaks = phase_order,
    drop = FALSE
  ) +
  
  scale_y_continuous(
    breaks = 1:6,
    limits = c(
      0.92,
      6.18
    ),
    expand = c(
      0,
      0
    )
  ) +
  
  scale_x_continuous(
    limits = c(
      x_min_BC,
      x_max_BC
    ),
    breaks = x_breaks_BC,
    labels = c(
      "2030",
      "2035",
      "2040",
      "2045",
      "2050"
    ),
    expand = expansion(
      add = c(
        0.10,
        0.65
      )
    )
  ) +
  
  coord_cartesian(
    clip = "off"
  ) +
  
  labs(
    x = "Reference P50 FOAK deployment",
    y = "Technology readiness level (TRL)"
  ) +
  
  theme_base +
  
  theme(
    legend.position = "none"
  )


p_b <- ggdraw(
  p_b
) +
  draw_plot_label(
    label = "b",
    x = 0.01,
    y = 0.99,
    hjust = 0,
    vjust = 1,
    size = PANEL_TAG_SIZE,
    fontface = "bold"
  )


# ============================================================
# PANEL c — PROLIFERATION SCENARIO
#
# Source column: accelerated_median
# ============================================================

p_c <- ggplot(
  filter(
    kang_sample_32,
    !is.na(accelerated_median)
  ),
  aes(
    x = accelerated_median,
    y = TRL,
    fill = Phase
  )
) +
  
  geom_vline(
    xintercept = med_vendor,
    linetype = "dashed",
    linewidth = 0.7,
    colour = COL_VENDOR
  ) +
  
  geom_vline(
    xintercept = med_pro,
    linetype = "dashed",
    linewidth = 0.7,
    colour = COL_PRO
  ) +
  
  geom_point(
    shape = 21,
    size = 3.7,
    stroke = 0.3,
    colour = "grey30",
    alpha = 0.95,
    position = jit
  ) +
  
  scale_fill_manual(
    values = phase_cols,
    breaks = phase_order,
    drop = FALSE
  ) +
  
  scale_y_continuous(
    breaks = 1:6,
    limits = c(
      0.92,
      6.18
    ),
    expand = c(
      0,
      0
    )
  ) +
  
  scale_x_continuous(
    limits = c(
      x_min_BC,
      x_max_BC
    ),
    breaks = x_breaks_BC,
    labels = c(
      "2030",
      "2035",
      "2040",
      "2045",
      "2050"
    ),
    expand = expansion(
      add = c(
        0.10,
        0.65
      )
    )
  ) +
  
  coord_cartesian(
    clip = "off"
  ) +
  
  labs(
    x = "Proliferation P50 FOAK deployment",
    y = "Technology readiness level (TRL)"
  ) +
  
  theme_base +
  
  theme(
    legend.position = "none"
  )


p_c <- ggdraw(
  p_c
) +
  draw_plot_label(
    label = "c",
    x = 0.01,
    y = 0.99,
    hjust = 0,
    vjust = 1,
    size = PANEL_TAG_SIZE,
    fontface = "bold"
  )


# ============================================================
# SHARED PHASE LEGEND
# ============================================================

legend_plot <- ggplot(
  kang_sample_32,
  aes(
    x = vendor_target,
    y = TRL,
    fill = Phase
  )
) +
  
  geom_point(
    shape = 21,
    size = 4.0,
    stroke = 0.3,
    colour = "grey30"
  ) +
  
  scale_fill_manual(
    values = phase_cols,
    breaks = phase_order,
    drop = FALSE
  ) +
  
  guides(
    fill = guide_legend(
      title = "Phase",
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        shape = 21,
        size = 4.2,
        alpha = 1,
        colour = "grey30"
      )
    )
  ) +
  
  theme_void() +
  
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.justification = "center",
    
    legend.title = element_text(
      size = LEGEND_TITLE_SIZE
    ),
    
    legend.text = element_text(
      size = LEGEND_TEXT_SIZE
    ),
    
    legend.key.width = unit(
      0.88,
      "cm"
    ),
    
    legend.key.height = unit(
      0.44,
      "cm"
    ),
    
    legend.spacing.x = unit(
      0.14,
      "cm"
    ),
    
    legend.spacing.y = unit(
      0.02,
      "cm"
    ),
    
    legend.box.spacing = unit(
      0.03,
      "cm"
    ),
    
    legend.margin = margin(
      0,
      0,
      0,
      0
    ),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    legend.background = element_rect(
      fill = "white",
      colour = NA
    )
  )


legend_shared <- get_legend(
  legend_plot
)


# ============================================================
# COMBINE
# ============================================================

aligned_panels <- align_plots(
  p_a,
  p_b,
  p_c,
  align = "v",
  axis = "l"
)


panel_row <- plot_grid(
  aligned_panels[[1]],
  aligned_panels[[2]],
  aligned_panels[[3]],
  nrow = 1,
  rel_widths = c(
    1,
    1,
    1
  ),
  greedy = TRUE
)


final_plot <- plot_grid(
  panel_row,
  legend_shared,
  ncol = 1,
  rel_heights = c(
    1,
    0.10
  )
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
  width = SAVE_W_IN,
  height = SAVE_H_IN,
  units = "in",
  dpi = SAVE_DPI,
  bg = "white"
)


ggsave(
  filename = PDF_FILE,
  plot = final_plot,
  width = SAVE_W_IN,
  height = SAVE_H_IN,
  units = "in",
  device = grDevices::cairo_pdf,
  bg = "white"
)


cat(
  "\nExtended Data Figure 3 v2 successfully exported:\n",
  PNG_FILE,
  "\n",
  PDF_FILE,
  "\n"
)


final_plot