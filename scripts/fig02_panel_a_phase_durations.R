# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Figure 2a: phase-duration assumptions by development stage and
# scenario for the probabilistic framework of SMR project progression and
# pipeline expansion.
#
# Panel a shows scenario-specific phase-duration distributions for five SMR
# development stages: conceptual design (CD), basic design (BD), detailed design
# (DD), licensing (LIC) and construction (CON). Curves show Baseline and
# Accelerated scenario assumptions; vertical ticks indicate median durations.
#
# Required input:
# data/fig02_source_data_foak_framework.xlsx
#   - sheet: phase_durations
#
# Outputs:
# outputs/Figure_02/intermediate/Framework_panel_A_duration_distributions_centred.png
# outputs/Figure_02/intermediate/Framework_panel_A_duration_distributions_centred.pdf
# outputs/Figure_02/intermediate/Framework_panel_A_duration_distributions_centred.rds
# outputs/Figure_02/intermediate/Framework_panel_A_duration_distributions_centred_core.rds


suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
  library(grid)
  library(stringr)
  library(tibble)
})

rm(list = ls())
graphics.off()

# ============================================================
# FIGURE 2 — PANEL A ONLY
# Phase-duration distributions from source workbook
# - Reads phase_durations sheet directly
# - Builds p_a_core
# - Builds labelled p_a
# - Saves PNG/PDF and RDS/core RDS
# ============================================================

# ------------------------------------------------------------
# FILE PATHS
# ------------------------------------------------------------
FILE_CANDIDATES <- c(
  "C:/Users/jo3151go/OneDrive/Nature_Energy_SMRs_2026/fig02_source_data_foak_framework.xlsx",
  "C:/Users/jo3151go/OneDrive/Nature_Energy_SMRs_2026/Main Data/fig02_source_data_foak_framework.xlsx",
  "C:/Users/jo3151go/OneDrive/Nature_Energy_SMRs_2026/Main Data/Main_R_Files/fig02_source_data_foak_framework.xlsx",
  "C:/Users/jo3151go/OneDrive/Nature_Energy_SMRs_2026/Main Data/Main_Data_Files/fig02_source_data_foak_framework.xlsx"
)

FILE <- FILE_CANDIDATES[file.exists(FILE_CANDIDATES)][1]

if (is.na(FILE)) {
  stop(
    paste0(
      "Could not find fig02_source_data_foak_framework.xlsx in any candidate path:\n",
      paste(FILE_CANDIDATES, collapse = "\n")
    )
  )
}

OUT_DIR <- "C:/Users/jo3151go/OneDrive/Nature_Energy_SMRs_2026/Framework Figure FInalised"
OUT_DIR_FIG02 <- "C:/Users/jo3151go/OneDrive/Nature_Energy_SMRs_2026/Figure_02"

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR_FIG02, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------
SAVE_W_MM <- 88
SAVE_H_MM <- 76
SAVE_DPI  <- 600

FILE_STUB <- "Framework_panel_A_duration_distributions_centred"

# ------------------------------------------------------------
# TYPOGRAPHY
# ------------------------------------------------------------
BASE_SIZE        <- 7
AXIS_TEXT_SIZE   <- 6.5
AXIS_TITLE_SIZE  <- 7
PANEL_LABEL_SIZE <- 12

# ------------------------------------------------------------
# COLOURS
# ------------------------------------------------------------
phase_colors <- c(
  "Conceptual design" = "#FDE0D2",
  "Basic design"      = "#F4A582",
  "Detailed design"   = "#D6604D",
  "Licensing"         = "#D1E5F0",
  "Construction"      = "#92C5DE"
)

scenario_line_colors <- c(
  "Baseline"    = "grey30",
  "Accelerated" = "#006400"
)

# ------------------------------------------------------------
# PANEL LABEL POSITION
# ------------------------------------------------------------
TAG_X <- 0.02
TAG_Y <- 0.98

# ------------------------------------------------------------
# GEOMETRY
# ------------------------------------------------------------
phase_center <- c(
  "Conceptual design" = 5,
  "Basic design"      = 4,
  "Detailed design"   = 3,
  "Licensing"         = 2,
  "Construction"      = 1
)

ridge_height <- 0.24
pair_gap     <- 0.18
x_step       <- 0.02
X_MIN        <- 0
X_MAX        <- 15

# ============================================================
# HELPERS
# ============================================================
clean_names_local <- function(x) {
  x <- trimws(x)
  x <- gsub("[[:space:][:punct:]]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

trunc_lognorm_density <- function(x, meanlog, sdlog, lower, upper) {
  denom <- plnorm(upper, meanlog, sdlog) - plnorm(lower, meanlog, sdlog)
  d <- dlnorm(x, meanlog, sdlog) / denom
  d[x < lower | x > upper] <- NA_real_
  d
}

trunc_lognorm_quantile <- function(p, meanlog, sdlog, lower, upper) {
  p_lower <- plnorm(lower, meanlog, sdlog)
  p_upper <- plnorm(upper, meanlog, sdlog)
  qlnorm(p_lower + p * (p_upper - p_lower), meanlog, sdlog)
}

map_phase <- function(x) {
  x2 <- tolower(trimws(as.character(x)))
  case_when(
    str_detect(x2, "concept")  | str_detect(x2, "\\bcd\\b") ~ "Conceptual design",
    str_detect(x2, "basic")    | str_detect(x2, "\\bbd\\b") ~ "Basic design",
    str_detect(x2, "detailed") | str_detect(x2, "\\bdd\\b") ~ "Detailed design",
    str_detect(x2, "licens")                               ~ "Licensing",
    str_detect(x2, "construct")                            ~ "Construction",
    TRUE                                                   ~ NA_character_
  )
}

map_scenario <- function(x) {
  x2 <- tolower(trimws(as.character(x)))
  case_when(
    str_detect(x2, "accelerated") | str_detect(x2, "^acc") ~ "Accelerated",
    str_detect(x2, "baseline")    | str_detect(x2, "^bas") ~ "Baseline",
    TRUE                                                   ~ NA_character_
  )
}

# ============================================================
# READ + STANDARDISE
# ============================================================
sheet_name <- readxl::excel_sheets(FILE)
sheet_name <- sheet_name[str_detect(tolower(sheet_name), "phase_durations")][1]

if (is.na(sheet_name)) {
  stop("Could not find a sheet matching 'phase_durations'.")
}

raw_a <- readxl::read_excel(FILE, sheet = sheet_name)
names(raw_a) <- clean_names_local(names(raw_a))

required_cols <- c("phase", "scenario", "mu_log", "sigma_log", "lower", "upper")
missing_cols <- setdiff(required_cols, names(raw_a))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "Missing required columns in phase_durations sheet:\n",
      paste(missing_cols, collapse = ", ")
    )
  )
}

dat_a <- raw_a %>%
  transmute(
    phase = map_phase(phase),
    scenario = map_scenario(scenario),
    mu_log = suppressWarnings(as.numeric(mu_log)),
    sigma_log = suppressWarnings(as.numeric(sigma_log)),
    lower = suppressWarnings(as.numeric(lower)),
    upper = suppressWarnings(as.numeric(upper))
  ) %>%
  mutate(
    p50 = trunc_lognorm_quantile(0.5, mu_log, sigma_log, lower, upper)
  ) %>%
  filter(
    !is.na(phase),
    !is.na(scenario),
    !is.na(mu_log),
    !is.na(sigma_log),
    !is.na(lower),
    !is.na(upper),
    !is.na(p50)
  ) %>%
  distinct() %>%
  arrange(
    factor(phase, levels = names(phase_center)),
    factor(scenario, levels = c("Baseline", "Accelerated"))
  )

expected_rows <- expand.grid(
  phase = names(phase_center),
  scenario = c("Baseline", "Accelerated"),
  stringsAsFactors = FALSE
)

missing_rows <- anti_join(expected_rows, dat_a, by = c("phase", "scenario"))

if (nrow(missing_rows) > 0) {
  stop(
    paste0(
      "Missing one or more phase-scenario combinations:\n",
      paste(paste(missing_rows$phase, missing_rows$scenario), collapse = "\n")
    )
  )
}

# ============================================================
# BUILD DENSITY DATA
# ============================================================
density_list <- lapply(seq_len(nrow(dat_a)), function(i) {
  row_i <- dat_a[i, ]
  
  x_seq <- seq(
    from = max(row_i$lower, X_MIN),
    to   = min(row_i$upper, X_MAX),
    by   = x_step
  )
  
  dens <- trunc_lognorm_density(
    x = x_seq,
    meanlog = row_i$mu_log,
    sdlog   = row_i$sigma_log,
    lower   = row_i$lower,
    upper   = row_i$upper
  )
  
  dens_scaled <- dens / max(dens, na.rm = TRUE) * ridge_height
  centre <- unname(phase_center[row_i$phase])
  
  if (row_i$scenario == "Baseline") {
    y_bottom <- centre + pair_gap / 2
    y_top    <- y_bottom + dens_scaled
  } else {
    y_bottom <- centre - pair_gap / 2 - ridge_height
    y_top    <- y_bottom + dens_scaled
  }
  
  tibble(
    phase    = row_i$phase,
    scenario = row_i$scenario,
    x        = x_seq,
    density  = dens,
    y_bottom = y_bottom,
    y_top    = y_top,
    centre   = centre
  )
})

density_df <- bind_rows(density_list)

median_df <- density_df %>%
  group_by(phase, scenario, centre) %>%
  summarise(
    p50 = dat_a$p50[match(paste(phase, scenario), paste(dat_a$phase, dat_a$scenario))][1],
    y_bottom = approx(x, y_bottom, xout = p50, rule = 2)$y,
    y_top    = approx(x, y_top,    xout = p50, rule = 2)$y,
    .groups = "drop"
  )

# ============================================================
# PANEL A CORE
# ============================================================
y_pad <- ridge_height + pair_gap / 2 + 0.08

p_a_core <- ggplot() +
  geom_ribbon(
    data = density_df,
    aes(
      x = x,
      ymin = y_bottom,
      ymax = y_top,
      fill = phase,
      group = interaction(phase, scenario)
    ),
    alpha = 0.80
  ) +
  geom_line(
    data = density_df,
    aes(
      x = x,
      y = y_top,
      colour = scenario,
      group = interaction(phase, scenario)
    ),
    linewidth = 0.72
  ) +
  geom_segment(
    data = median_df,
    aes(
      x = p50,
      xend = p50,
      y = y_bottom,
      yend = y_top
    ),
    colour = "black",
    linewidth = 0.72
  ) +
  scale_fill_manual(values = phase_colors, guide = "none") +
  scale_colour_manual(values = scenario_line_colors, guide = "none") +
  scale_x_continuous(
    limits = c(X_MIN, X_MAX),
    breaks = seq(0, 14, by = 2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(min(phase_center) - y_pad, max(phase_center) + y_pad),
    breaks = c(5, 4, 3, 2, 1),
    labels = c("Conceptual design", "Basic design", "Detailed design", "Licensing", "Construction"),
    expand = c(0, 0)
  ) +
  labs(
    x = "Phase duration (years)",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_classic(base_size = BASE_SIZE) +
  theme(
    axis.title.x = element_text(size = AXIS_TITLE_SIZE, colour = "black"),
    axis.title.y = element_blank(),
    axis.text.x  = element_text(size = AXIS_TEXT_SIZE, colour = "grey20"),
    axis.text.y  = element_text(size = AXIS_TEXT_SIZE, colour = "grey20"),
    axis.line.x  = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y  = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks.y = element_blank(),
    axis.ticks.length = unit(2.5, "pt"),
    plot.margin = margin(t = 8, r = 8, b = 8, l = 8)
  )

# ============================================================
# PANEL A LABELLED
# ============================================================
p_a <- ggdraw(p_a_core) +
  draw_plot_label(
    "a",
    x = TAG_X,
    y = TAG_Y,
    hjust = 0,
    vjust = 1,
    size = PANEL_LABEL_SIZE,
    fontface = "bold"
  )

print(p_a)

# ============================================================
# SAVE EXPORTS
# ============================================================
for (this_dir in c(OUT_DIR, OUT_DIR_FIG02)) {
  ggsave(
    filename = file.path(this_dir, paste0(FILE_STUB, ".png")),
    plot = p_a,
    width = SAVE_W_MM,
    height = SAVE_H_MM,
    units = "mm",
    dpi = SAVE_DPI,
    bg = "white"
  )
  
  ggsave(
    filename = file.path(this_dir, paste0(FILE_STUB, ".pdf")),
    plot = p_a,
    width = SAVE_W_MM,
    height = SAVE_H_MM,
    units = "mm",
    bg = "white"
  )
  
  saveRDS(
    p_a,
    file = file.path(this_dir, paste0(FILE_STUB, ".rds"))
  )
  
  saveRDS(
    p_a_core,
    file = file.path(this_dir, paste0(FILE_STUB, "_core.rds"))
  )
}