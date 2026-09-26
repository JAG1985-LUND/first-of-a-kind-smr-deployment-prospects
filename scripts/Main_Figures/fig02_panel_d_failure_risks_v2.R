# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Figure 2d: phase-specific cumulative survival probabilities under
# the Baseline and Accelerated scenarios.
#
# Panel d shows cumulative survival across the SMR development pathway from
# conceptual design to FOAK operation. Coloured points identify the development
# stage reached, and shaded bands indicate credible intervals based on
# phase-specific failure-risk uncertainty.
#
# Required input:
# None. Failure-rate assumptions for this panel are defined directly below.
#
# Outputs:
# fig02_panel_d_failure_risks.png
# fig02_panel_d_failure_risks.pdf
# fig02_panel_d_failure_risks.rds
# fig02_panel_d_failure_risks_core.rds

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tibble)
  library(cowplot)
  library(grid)
})

rm(list = ls())
graphics.off()

# ============================================================
# FIGURE 2 — PANEL D ONLY
# Phase-specific cumulative survival probabilities
# ============================================================

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

OUT_DIR <- "."

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 95
SAVE_H_MM <- 72
SAVE_DPI  <- 600

FILE_STUB <- "fig02_panel_d_failure_risks"

# ------------------------------------------------------------
# TYPOGRAPHY
# ------------------------------------------------------------

BASE_SIZE        <- 7
AXIS_TEXT_SIZE   <- 6.5
AXIS_TITLE_SIZE  <- 7
PANEL_LABEL_SIZE <- 10

# ------------------------------------------------------------
# PANEL TAG GEOMETRY
# ------------------------------------------------------------

TAG_X <- 0.01
TAG_Y <- 0.99

# ------------------------------------------------------------
# COLOURS
# ------------------------------------------------------------

col_baseline_line <- "grey30"
col_baseline_fill <- "grey70"
col_accel_line    <- "#006400"
col_accel_fill    <- "#90EE90"

phase_cols <- c(
  "Conceptual design" = "#FDE0D2",
  "Basic design"      = "#F4A582",
  "Detailed design"   = "#D6604D",
  "Licensing"         = "#D1E5F0",
  "Construction"      = "#92C5DE",
  "Operational"       = "#4393C3"
)

# ============================================================
# PANEL D — PROJECT PROGRESSION UNDER PHASE-SPECIFIC FAILURE RISKS
# ============================================================

ph_bas <- tribble(
  ~Phase, ~Mean, ~Low,  ~High,
  "CD", 14.9,  6.9, 27.0,
  "BD", 15.4,  5.4, 32.5,
  "DD", 41.2, 17.4, 61.7
)

ph_acc <- tribble(
  ~Phase, ~Mean, ~Low,  ~High,
  "CD",  9.1,  3.2, 20.2,
  "BD", 12.0,  3.5, 28.7,
  "DD", 23.1,  3.6, 43.6
)

post_bas <- tribble(
  ~Phase, ~Mean, ~Low, ~High,
  "Licensing",    19.9, 13.9, 36.0,
  "Construction", 16.5,  7.5, 26.3
)

post_acc <- tribble(
  ~Phase, ~Mean, ~Low, ~High,
  "Licensing",    11.0,  5.0, 20.3,
  "Construction",  8.3,  3.0, 16.3
)

calc_surv <- function(df) {
  df %>%
    mutate(
      fr      = Mean / 100,
      fr_lo   = Low / 100,
      fr_hi   = High / 100,
      surv    = cumprod(1 - fr),
      surv_lo = cumprod(1 - fr_hi),
      surv_hi = cumprod(1 - fr_lo)
    )
}

calc_post <- function(df, s, slo, shi) {
  x <- calc_surv(df)
  
  tibble(
    surv    = s   * c(1, x$surv),
    surv_lo = slo * c(1, x$surv_lo),
    surv_hi = shi * c(1, x$surv_hi)
  )
}

bas_d <- calc_surv(ph_bas)
acc_d <- calc_surv(ph_acc)

bas_p <- calc_post(
  post_bas,
  dplyr::last(bas_d$surv),
  dplyr::last(bas_d$surv_lo),
  dplyr::last(bas_d$surv_hi)
)

acc_p <- calc_post(
  post_acc,
  dplyr::last(acc_d$surv),
  dplyr::last(acc_d$surv_lo),
  dplyr::last(acc_d$surv_hi)
)

curve <- bind_rows(
  tibble(
    x = 1:6,
    surv = c(1, bas_d$surv, bas_p$surv[-1]),
    lo   = c(1, bas_d$surv_lo, bas_p$surv_lo[-1]),
    hi   = c(1, bas_d$surv_hi, bas_p$surv_hi[-1]),
    Scenario = "Reference"
  ),
  tibble(
    x = 1:6,
    surv = c(1, acc_d$surv, acc_p$surv[-1]),
    lo   = c(1, acc_d$surv_lo, acc_p$surv_lo[-1]),
    hi   = c(1, acc_d$surv_hi, acc_p$surv_hi[-1]),
    Scenario = "Proliferation"
  )
)

curve <- curve %>%
  mutate(
    Scenario = factor(
      Scenario,
      levels = c("Reference", "Proliferation")
    ),
    PhaseLabel = factor(
      x,
      levels = 1:6,
      labels = c(
        "Conceptual design",
        "Basic design",
        "Detailed design",
        "Licensing",
        "Construction",
        "Operational"
      )
    )
  )

# ============================================================
# PANEL D CORE
# ============================================================

p_d_core <- ggplot(
  curve,
  aes(x = x, y = surv * 100, colour = Scenario, fill = Scenario)
) +
  geom_ribbon(
    aes(ymin = lo * 100, ymax = hi * 100),
    alpha = 0.20,
    colour = NA
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(
    aes(fill = PhaseLabel),
    shape = 21,
    colour = "grey25",
    stroke = 0.25,
    size = 2.3,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "Reference" = col_baseline_line,
      "Proliferation" = col_accel_line
    ),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Reference" = col_baseline_fill,
      "Proliferation" = col_accel_fill,
      phase_cols
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = 1:6,
    labels = c(
      "Conceptual\nDesign",
      "Basic\nDesign",
      "Detailed\nDesign",
      "Licensing",
      "Construction",
      "FOAK\noperation"
    ),
    limits = c(0.8, 6.4),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 102),
    breaks = c(25, 50, 75, 100),
    expand = c(0, 0)
  ) +
  labs(
    x = "Development phase",
    y = "Cumulative survival (%)"
  ) +
  coord_cartesian(clip = "off") +
  theme_classic(base_size = BASE_SIZE) +
  theme(
    axis.title.x = element_text(
      size = AXIS_TITLE_SIZE,
      colour = "black",
      margin = margin(t = 4)
    ),
    axis.title.y = element_text(
      size = AXIS_TITLE_SIZE,
      colour = "black",
      margin = margin(r = 4)
    ),
    axis.text.x = element_text(
      size = AXIS_TEXT_SIZE,
      colour = "grey20",
      margin = margin(t = 2)
    ),
    axis.text.y = element_text(
      size = AXIS_TEXT_SIZE,
      colour = "grey20"
    ),
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks.length = unit(2.5, "pt"),
    plot.margin = margin(t = 8, r = 8, b = 6, l = 8),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

# ============================================================
# PANEL D LABELLED
# ============================================================

p_d <- ggdraw(p_d_core) +
  draw_plot_label(
    "d",
    x = TAG_X,
    y = TAG_Y,
    hjust = 0,
    vjust = 1,
    size = PANEL_LABEL_SIZE,
    fontface = "bold"
  )

print(p_d)

# ============================================================
# SAVE PANEL D
# ============================================================

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".png")),
  plot = p_d,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".pdf")),
  plot = p_d,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  bg = "white",
  useDingbats = FALSE
)

saveRDS(
  p_d,
  file = file.path(OUT_DIR, paste0(FILE_STUB, ".rds"))
)

saveRDS(
  p_d_core,
  file = file.path(OUT_DIR, paste0(FILE_STUB, "_core.rds"))
)

p_d
