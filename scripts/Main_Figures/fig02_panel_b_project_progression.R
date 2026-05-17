# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Figure 2b: illustrative project-progression pathways from
# conceptual design to FOAK operation under the Baseline and Accelerated
# scenarios.
#
# Panel b shows P10, P50 and P90 project-progression trajectories from a
# representative Monte Carlo simulation. Purple points denote conceptual-design
# start years. Grey triangles and green diamonds denote median FOAK operation
# under the Baseline and Accelerated scenarios, respectively, with horizontal
# whiskers showing P10-P90 intervals.
#
# Required input:
# data/Main_Figures/fig02_source_data_foak_framework.xlsx
#   - sheet: project_progression
#
# Outputs:
# outputs/Figure_02/intermediate/fig02_panel_b_project_progression.png
# outputs/Figure_02/intermediate/fig02_panel_b_project_progression.pdf
# outputs/Figure_02/intermediate/fig02_panel_b_project_progression.rds
# outputs/Figure_02/intermediate/fig02_panel_b_project_progression_core.rds

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
# FIGURE 2 — PANEL B ONLY
# Project-progression pathways from source workbook
# ============================================================

# ------------------------------------------------------------
# FILE PATHS
# ------------------------------------------------------------

DATA_DIR <- file.path("data", "Main_Figures")

FILE <- file.path(
  DATA_DIR,
  "fig02_source_data_foak_framework.xlsx"
)

OUT_DIR <- file.path(
  "outputs",
  "Figure_02",
  "intermediate"
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

SAVE_DPI    <- 600
SAVE_W_MM_B <- 110
SAVE_H_MM_B <- 62

FILE_STUB_B <- "fig02_panel_b_project_progression"

# ------------------------------------------------------------
# TYPOGRAPHY
# ------------------------------------------------------------

BASE_SIZE        <- 7
AXIS_TEXT_SIZE   <- 6.5
AXIS_TITLE_SIZE  <- 7
PANEL_LABEL_SIZE <- 12
SCEN_LABEL_SIZE  <- 3.0

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

col_start <- "#9E6BB1"
col_base  <- "grey40"
col_acc   <- "#228B22"

CUT_2050 <- 2050

SH_BASE <- 17
SH_ACC  <- 18

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

# ============================================================
# READ DATA
# ============================================================

sheet_name <- readxl::excel_sheets(FILE)
sheet_name <- sheet_name[str_detect(tolower(sheet_name), "project_progression")][1]

if (is.na(sheet_name)) {
  stop("Could not find a sheet matching 'project_progression'.")
}

raw_b <- readxl::read_excel(FILE, sheet = sheet_name)
names(raw_b) <- clean_names_local(names(raw_b))

required_cols <- c(
  "smr_design",
  "cd_start",
  "bd_start",
  "dd_start",
  "lic_start",
  "con_start",
  "foak_operation_p10",
  "foak_operation_p50",
  "foak_operation_p90"
)

missing_cols <- setdiff(required_cols, names(raw_b))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "Missing required columns in project_progression sheet:\n",
      paste(missing_cols, collapse = ", ")
    )
  )
}

dat_b <- raw_b %>%
  transmute(
    Design   = trimws(as.character(smr_design)),
    CD       = suppressWarnings(as.numeric(cd_start)),
    BD       = suppressWarnings(as.numeric(bd_start)),
    DD       = suppressWarnings(as.numeric(dd_start)),
    Lic      = suppressWarnings(as.numeric(lic_start)),
    Con      = suppressWarnings(as.numeric(con_start)),
    FOAK_P10 = suppressWarnings(as.numeric(foak_operation_p10)),
    FOAK_P50 = suppressWarnings(as.numeric(foak_operation_p50)),
    FOAK_P90 = suppressWarnings(as.numeric(foak_operation_p90))
  ) %>%
  filter(!is.na(Design), Design != "", tolower(Design) != "na")

order_top_to_bottom <- c(
  "SMR-BAS-P10", "SMR-BAS-P50", "SMR-BAS-P90",
  "SMR-ACC-P10", "SMR-ACC-P50", "SMR-ACC-P90"
)

row_key <- tibble(
  Design = order_top_to_bottom,
  y = c(6, 5, 4, 3, 2, 1),
  RowLabel = c("P10", "P50", "P90", "P10", "P50", "P90"),
  ScenarioGroup = c("Baseline", "Baseline", "Baseline", "Accelerated", "Accelerated", "Accelerated")
)

dat_b <- dat_b %>%
  filter(Design %in% order_top_to_bottom) %>%
  left_join(row_key, by = "Design") %>%
  mutate(
    BD       = pmax(BD,  CD,       na.rm = TRUE),
    DD       = pmax(DD,  BD,       na.rm = TRUE),
    Lic      = pmax(Lic, DD,       na.rm = TRUE),
    Con      = pmax(Con, Lic,      na.rm = TRUE),
    FOAK_P10 = pmax(FOAK_P10, Con, na.rm = TRUE),
    FOAK_P50 = pmax(FOAK_P50, Con, na.rm = TRUE),
    FOAK_P90 = pmax(FOAK_P90, Con, na.rm = TRUE)
  ) %>%
  arrange(desc(y))

if (nrow(dat_b) != length(order_top_to_bottom)) {
  stop("Panel B data does not contain all six expected rows.")
}

# ============================================================
# PLOTTING DATA
# ============================================================

plot_df_b <- dat_b %>%
  transmute(
    y,
    CD_start  = CD,  CD_end  = BD,
    BD_start  = BD,  BD_end  = DD,
    DD_start  = DD,  DD_end  = Lic,
    Lic_start = Lic, Lic_end = Con,
    Con_start = Con, Con_end = FOAK_P50
  ) %>%
  pivot_longer(
    cols      = -y,
    names_to  = c("Phase", "pos"),
    names_sep = "_",
    values_to = "x"
  ) %>%
  pivot_wider(names_from = pos, values_from = x) %>%
  mutate(
    Phase = factor(
      Phase,
      levels = c("CD", "BD", "DD", "Lic", "Con"),
      labels = c("Conceptual design", "Basic design", "Detailed design", "Licensing", "Construction")
    )
  ) %>%
  filter(!is.na(start), !is.na(end))

milestone_start <- dat_b %>%
  transmute(
    y = y,
    x = CD
  )

milestone_foak_base <- dat_b %>%
  filter(ScenarioGroup == "Baseline") %>%
  transmute(
    y = y,
    x = FOAK_P50,
    xmin = FOAK_P10,
    xmax = FOAK_P90
  )

milestone_foak_acc <- dat_b %>%
  filter(ScenarioGroup == "Accelerated") %>%
  transmute(
    y = y,
    x = FOAK_P50,
    xmin = FOAK_P10,
    xmax = FOAK_P90
  )

cap_h   <- 0.12
B_X_MIN <- 2025
B_X_MAX <- max(dat_b$FOAK_P90, na.rm = TRUE) + 0.8

# ============================================================
# PANEL B CORE
# ============================================================

p_b_core <- ggplot() +
  geom_segment(
    data = plot_df_b,
    aes(y = y, x = start, xend = end, yend = y, colour = Phase),
    linewidth = 2.5,
    lineend = "butt"
  ) +
  geom_hline(
    yintercept = 3.5,
    linetype = "dotted",
    linewidth = 0.4,
    colour = "grey50"
  ) +
  geom_vline(
    xintercept = CUT_2050,
    linetype = "dashed",
    linewidth = 0.7,
    colour = "#cc0000"
  ) +
  annotate(
    "text",
    x = 2052.2,
    y = 5.0,
    label = "Baseline",
    hjust = 0,
    vjust = 0.5,
    size = SCEN_LABEL_SIZE,
    fontface = "bold",
    colour = col_base
  ) +
  annotate(
    "text",
    x = 2052.2,
    y = 2.0,
    label = "Accelerated",
    hjust = 0,
    vjust = 0.5,
    size = SCEN_LABEL_SIZE,
    fontface = "bold",
    colour = col_acc
  ) +
  geom_segment(
    data = milestone_foak_base,
    aes(x = xmin, xend = xmax, y = y, yend = y),
    linewidth = 0.70,
    colour = col_base
  ) +
  geom_segment(
    data = milestone_foak_base,
    aes(x = xmin, xend = xmin, y = y - cap_h, yend = y + cap_h),
    linewidth = 0.70,
    colour = col_base
  ) +
  geom_segment(
    data = milestone_foak_base,
    aes(x = xmax, xend = xmax, y = y - cap_h, yend = y + cap_h),
    linewidth = 0.70,
    colour = col_base
  ) +
  geom_segment(
    data = milestone_foak_acc,
    aes(x = xmin, xend = xmax, y = y, yend = y),
    linewidth = 0.68,
    colour = col_acc
  ) +
  geom_segment(
    data = milestone_foak_acc,
    aes(x = xmin, xend = xmin, y = y - cap_h, yend = y + cap_h),
    linewidth = 0.68,
    colour = col_acc
  ) +
  geom_segment(
    data = milestone_foak_acc,
    aes(x = xmax, xend = xmax, y = y - cap_h, yend = y + cap_h),
    linewidth = 0.68,
    colour = col_acc
  ) +
  geom_point(
    data = milestone_start,
    aes(x = x, y = y),
    colour = col_start,
    size = 2.1
  ) +
  geom_point(
    data = milestone_foak_base,
    aes(x = x, y = y),
    shape = SH_BASE,
    colour = col_base,
    size = 1.9
  ) +
  geom_point(
    data = milestone_foak_acc,
    aes(x = x, y = y),
    shape = SH_ACC,
    colour = col_acc,
    size = 2.1
  ) +
  scale_colour_manual(values = phase_colors, guide = "none") +
  scale_y_continuous(
    limits = c(0.65, 6.45),
    breaks = row_key$y,
    labels = row_key$RowLabel,
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(B_X_MIN, B_X_MAX),
    breaks = c(2030, 2040, 2050, 2060),
    expand = expansion(mult = 0, add = c(0.02, 0.05))
  ) +
  labs(
    x = "Year",
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
    axis.ticks   = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks.length = unit(2.5, "pt"),
    plot.margin = margin(t = 8, r = 8, b = 8, l = 12)
  )

# ============================================================
# PANEL B LABELLED
# ============================================================

p_b <- ggdraw(p_b_core) +
  draw_plot_label(
    "b",
    x = 0.02,
    y = 0.98,
    hjust = 0,
    vjust = 1,
    size = PANEL_LABEL_SIZE,
    fontface = "bold"
  )

print(p_b)

# ============================================================
# SAVE PANEL B
# ============================================================

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB_B, ".png")),
  plot = p_b,
  width = SAVE_W_MM_B,
  height = SAVE_H_MM_B,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB_B, ".pdf")),
  plot = p_b,
  width = SAVE_W_MM_B,
  height = SAVE_H_MM_B,
  units = "mm",
  bg = "white"
)

saveRDS(
  p_b,
  file = file.path(OUT_DIR, paste0(FILE_STUB_B, ".rds"))
)

saveRDS(
  p_b_core,
  file = file.path(OUT_DIR, paste0(FILE_STUB_B, "_core.rds"))
)
