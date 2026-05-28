# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Rebuilds Figure 2: probabilistic framework for SMR project progression,
# pipeline evolution and FOAK deployment.
#
# This Marta rebuild script reads the panel-level RDS objects generated for
# panels a, b, c, d, and e-f, then assembles the final Figure 2.
#
# Required inputs:
# Framework_panel_A_duration_distributions_centred.rds
# fig02_panel_b_project_progression.rds
# fig02_panel_c_design_additions.rds
# fig02_panel_d_failure_risks.rds
# fig02_panels_e_f_future_pipeline.rds
#
# Outputs:
# fig02_foak_framework.png
# fig02_foak_framework.pdf
# fig02_foak_framework.rds

suppressPackageStartupMessages({
  library(ggplot2)
  library(cowplot)
  library(grid)
})

rm(list = ls())
graphics.off()

# ============================================================
# FIGURE 2 — MASTER A-F MARTA REBUILD
# ============================================================

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

OUT_DIR <- "."

FILE_A <- "Framework_panel_A_duration_distributions_centred.rds"

FILE_B <- "fig02_panel_b_project_progression.rds"

FILE_C <- "fig02_panel_c_design_additions.rds"

FILE_D <- "fig02_panel_d_failure_risks.rds"

FILE_EF <- "fig02_panels_e_f_future_pipeline.rds"

required_files <- c(
  FILE_A,
  FILE_B,
  FILE_C,
  FILE_D,
  FILE_EF
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Missing required Figure 2 intermediate RDS file(s):\n",
      paste(missing_files, collapse = "\n")
    )
  )
}

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 180
SAVE_H_MM <- 230
SAVE_DPI  <- 600

FILE_STUB <- "fig02_foak_framework"

# ------------------------------------------------------------
# COLOURS
# ------------------------------------------------------------

col_baseline_line <- "grey30"
col_accel_line    <- "#006400"

# ------------------------------------------------------------
# READ PANEL OBJECTS
# ------------------------------------------------------------

panel_a   <- readRDS(FILE_A)
panel_b   <- readRDS(FILE_B)
panel_c   <- readRDS(FILE_C)
panel_d   <- readRDS(FILE_D)
panels_ef <- readRDS(FILE_EF)

# ------------------------------------------------------------
# BUILD PANEL ROWS
# ------------------------------------------------------------

fig_ab <- plot_grid(
  panel_a,
  panel_b,
  ncol = 2,
  align = "hv",
  axis = "tblr",
  rel_widths = c(1, 1.18)
)

fig_cd <- plot_grid(
  panel_c,
  panel_d,
  ncol = 2,
  align = "hv",
  axis = "tblr",
  rel_widths = c(1, 1.18)
)

# ------------------------------------------------------------
# BUILD MASTER A-F
# ------------------------------------------------------------

fig_abcdef <- plot_grid(
  fig_ab,
  fig_cd,
  panels_ef,
  ncol = 1,
  align = "v",
  axis = "lr",
  rel_heights = c(72, 72, 86)
)

# ------------------------------------------------------------
# SCENARIO LEGEND ROW
# ------------------------------------------------------------

scenario_df <- data.frame(
  x = c(1, 2),
  xend = c(2, 3),
  y = c(1, 1),
  yend = c(1, 1),
  lab = factor(
    c("Baseline", "Accelerated"),
    levels = c("Baseline", "Accelerated")
  )
)

scenario_legend_plot <- ggplot(
  scenario_df,
  aes(
    x = x,
    xend = xend,
    y = y,
    yend = yend,
    colour = lab
  )
) +
  geom_segment(linewidth = 1) +
  scale_colour_manual(
    name = "Scenario",
    values = c(
      "Baseline" = col_baseline_line,
      "Accelerated" = col_accel_line
    ),
    breaks = c(
      "Baseline",
      "Accelerated"
    )
  ) +
  guides(
    colour = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        linewidth = 1.2
      )
    )
  ) +
  theme_void(base_size = 7) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(
      size = 6.5,
      face = "plain",
      colour = "black"
    ),
    legend.text = element_text(
      size = 6,
      colour = "black"
    ),
    legend.key.width = unit(8, "mm"),
    legend.key.height = unit(3.5, "mm"),
    legend.spacing.x = unit(2, "mm"),
    legend.box.spacing = unit(0, "mm"),
    legend.margin = margin(0, 0, 0, 0)
  )

legend_scenario <- cowplot::get_legend(scenario_legend_plot)

# ------------------------------------------------------------
# FINAL FIGURE
# ------------------------------------------------------------

fig_final <- plot_grid(
  fig_abcdef,
  legend_scenario,
  ncol = 1,
  rel_heights = c(1, 0.035)
)

print(fig_final)

# ------------------------------------------------------------
# SAVE FINAL FIGURE
# ------------------------------------------------------------

ggsave(
  filename = file.path(
    OUT_DIR,
    paste0(FILE_STUB, ".png")
  ),
  plot = fig_final,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white",
  limitsize = FALSE
)

ggsave(
  filename = file.path(
    OUT_DIR,
    paste0(FILE_STUB, ".pdf")
  ),
  plot = fig_final,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  bg = "white",
  limitsize = FALSE
)

saveRDS(
  fig_final,
  file = file.path(
    OUT_DIR,
    paste0(FILE_STUB, ".rds")
  )
)

fig_final
