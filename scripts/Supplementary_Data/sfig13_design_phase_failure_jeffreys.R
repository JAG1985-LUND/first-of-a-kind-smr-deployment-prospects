# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Supplementary Fig. 13: phase-specific posterior distributions
# of design-phase failure probabilities under Baseline and Accelerated scenarios.
#
# Required input:
# sfig13_design_phase_jeffreys.xlsx
#
# Outputs:
# sfig13_cd_bd_dd_jeffreys.png
# sfig13_cd_bd_dd_jeffreys.pdf

rm(list = ls())
graphics.off()

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(cowplot)
})

# ============================================================
# SUPPLEMENTARY FIGURE 13 — DESIGN-PHASE JEFFREYS POSTERIORS
# Panels: a–c
# a: Conceptual design
# b: Basic design
# c: Detailed design
# ============================================================

# ------------------------
# PATHS
# ------------------------

FILE <- "sfig13_design_phase_jeffreys.xlsx"

OUT_DIR <- "."

OUT_BASE <- "sfig13_cd_bd_dd_jeffreys"

if (!file.exists(FILE)) {
  stop("Input file not found: ", FILE)
}

# ------------------------
# SETTINGS
# ------------------------

SAVE_W_IN <- 12
SAVE_H_IN <- 4
SAVE_DPI  <- 600

set.seed(350)

col_acc  <- "forestgreen"
col_base <- "grey35"

axis_line_col <- "grey35"
axis_line_lw  <- 0.3

# ------------------------
# HELPERS
# ------------------------

clean_names_local <- function(x) {
  x <- trimws(x)
  x <- gsub("[[:space:][:punct:]]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

# ------------------------
# READ SOURCE DATA
# ------------------------

df_counts <- read_excel(FILE, sheet = 1)
names(df_counts) <- clean_names_local(names(df_counts))

required_cols <- c(
  "phase",
  "scenario",
  "failures_k",
  "total_n"
)

missing_cols <- setdiff(required_cols, names(df_counts))

if (length(missing_cols) > 0) {
  stop(
    "Missing required column(s): ",
    paste(missing_cols, collapse = ", "),
    "\nAvailable columns are: ",
    paste(names(df_counts), collapse = ", ")
  )
}

df_counts <- df_counts %>%
  transmute(
    phase = str_to_lower(str_squish(as.character(phase))),
    scenario = str_to_lower(str_squish(as.character(scenario))),
    failures_k = as.numeric(failures_k),
    total_n = as.numeric(total_n)
  ) %>%
  mutate(
    phase = str_replace_all(phase, "\\s+", "_"),
    scenario = str_replace_all(scenario, "\\s+", "_")
  )

get_count <- function(phase_name, scenario_name, var_name) {
  
  out <- df_counts %>%
    filter(
      phase == phase_name,
      scenario == scenario_name
    ) %>%
    pull({{ var_name }})
  
  if (length(out) != 1 || is.na(out)) {
    stop(
      "Could not uniquely retrieve ",
      deparse(substitute(var_name)),
      " for phase = ",
      phase_name,
      ", scenario = ",
      scenario_name
    )
  }
  
  out
}

# ------------------------
# PLOT HELPER
# ------------------------

plot_phase <- function(phase_name, p_max, xlab, tag) {
  
  x_acc  <- get_count(phase_name, "accelerated", failures_k)
  n_acc  <- get_count(phase_name, "accelerated", total_n)
  x_base <- get_count(phase_name, "baseline", failures_k)
  n_base <- get_count(phase_name, "baseline", total_n)
  
  prior_a <- 0.5
  prior_b <- 0.5
  
  a_acc  <- x_acc  + prior_a
  b_acc  <- (n_acc  - x_acc)  + prior_b
  a_base <- x_base + prior_a
  b_base <- (n_base - x_base) + prior_b
  
  p_grid <- seq(0, p_max, by = 0.0005)
  
  dens_acc  <- dbeta(p_grid, a_acc, b_acc)
  dens_base <- dbeta(p_grid, a_base, b_base)
  
  med_acc  <- qbeta(0.5, a_acc, b_acc)
  med_base <- qbeta(0.5, a_base, b_base)
  
  ymax <- max(dens_acc, dens_base)
  
  p <- ggplot() +
    geom_line(
      aes(x = p_grid, y = dens_acc),
      colour = col_acc,
      linewidth = 1
    ) +
    geom_line(
      aes(x = p_grid, y = dens_base),
      colour = col_base,
      linewidth = 1
    ) +
    geom_vline(
      xintercept = med_acc,
      linetype = "dashed",
      colour = col_acc,
      linewidth = 0.6
    ) +
    geom_vline(
      xintercept = med_base,
      linetype = "dashed",
      colour = col_base,
      linewidth = 0.6
    ) +
    scale_x_continuous(
      limits = c(0, p_max),
      breaks = seq(0, p_max, by = 0.2),
      expand = c(0, 0)
    ) +
    coord_cartesian(
      ylim = c(0, ymax)
    ) +
    labs(
      x = xlab,
      y = "Posterior density"
    ) +
    theme_classic(base_size = 11) +
    theme(
      axis.line.x = element_line(colour = axis_line_col, linewidth = axis_line_lw),
      axis.line.y = element_line(colour = axis_line_col, linewidth = axis_line_lw),
      axis.ticks  = element_line(colour = axis_line_col, linewidth = axis_line_lw),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(5, 5, 5, 5)
    )
  
  ggdraw(p) +
    draw_plot_label(
      tag,
      x = 0.01,
      y = 0.99,
      hjust = 0,
      vjust = 1,
      fontface = "plain",
      size = 12
    )
}

# ------------------------
# BUILD PANELS
# ------------------------

p_cd <- plot_phase(
  phase_name = "conceptual_design",
  p_max = 0.6,
  xlab = "Conceptual design failure probability (p)",
  tag = "a"
)

p_bd <- plot_phase(
  phase_name = "basic_design",
  p_max = 0.8,
  xlab = "Basic design failure probability (p)",
  tag = "b"
)

p_dd <- plot_phase(
  phase_name = "detailed_design",
  p_max = 1.0,
  xlab = "Detailed design failure probability (p)",
  tag = "c"
)

# ------------------------
# COMBINE PANELS
# ------------------------

fig_combined <- plot_grid(
  p_cd,
  p_bd,
  p_dd,
  nrow = 1,
  align = "hv",
  axis = "tblr"
)

print(fig_combined)

# ------------------------
# SAVE SUPPLEMENTARY FIGURE 13
# ------------------------

ggsave(
  filename = file.path(OUT_DIR, paste0(OUT_BASE, ".png")),
  plot = fig_combined,
  width = SAVE_W_IN,
  height = SAVE_H_IN,
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, paste0(OUT_BASE, ".pdf")),
  plot = fig_combined,
  width = SAVE_W_IN,
  height = SAVE_H_IN,
  device = cairo_pdf,
  bg = "white"
)

fig_combined
