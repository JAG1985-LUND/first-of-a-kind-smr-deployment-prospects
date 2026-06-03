# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Supplementary Fig. 11: relationship between SMR design-phase
# duration and conceptual design start year.
#
# Required input:
# data/Supplementary_Data/sfig11_source_data_design_phase_duration.xlsx
#
# Outputs:
# outputs/Supplementary_Figures/Figure_11/sfig11_design_phase_duration.png
# outputs/Supplementary_Figures/Figure_11/sfig11_design_phase_duration.pdf
# outputs/Supplementary_Figures/Figure_11/sfig11_design_phase_duration.rds

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(grid)
})

rm(list = ls())
graphics.off()

# ============================================================
# SUPPLEMENTARY FIGURE 11
# DESIGN-PHASE DURATION AND CONCEPTUAL DESIGN START YEAR
# ============================================================

# ------------------------------------------------------------
# FIND REPOSITORY ROOT
# ------------------------------------------------------------

find_repo_root <- function(start_dir = getwd()) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  
  repeat {
    has_git     <- dir.exists(file.path(current_dir, ".git"))
    has_data    <- dir.exists(file.path(current_dir, "data"))
    has_scripts <- dir.exists(file.path(current_dir, "scripts"))
    
    if (has_git && has_data && has_scripts) {
      return(current_dir)
    }
    
    parent_dir <- dirname(current_dir)
    
    if (identical(parent_dir, current_dir)) {
      stop(
        "Repository root not found. Run this script from inside the ",
        "first-of-a-kind-smr-deployment-prospects repository."
      )
    }
    
    current_dir <- parent_dir
  }
}

ROOT_DIR <- find_repo_root()

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

FILE_CANDIDATES <- c(
  "sfig11_source_data_design_phase_duration.xlsx",
  "sfig11_design_phase_duration.xlsx"
)

FILE_IN_CANDIDATES <- file.path(
  ROOT_DIR,
  "data",
  "Supplementary_Data",
  FILE_CANDIDATES
)

FILE_IN <- FILE_IN_CANDIDATES[file.exists(FILE_IN_CANDIDATES)][1]

if (is.na(FILE_IN)) {
  stop(
    "Input file not found. Checked: ",
    paste(FILE_IN_CANDIDATES, collapse = " | ")
  )
}

FILE <- basename(FILE_IN)

OUT_DIR <- file.path(
  ROOT_DIR,
  "outputs",
  "Supplementary_Figures",
  "Figure_11"
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 180
SAVE_H_MM <- 110
SAVE_DPI  <- 600

FILE_STUB <- "sfig11_design_phase_duration"

# ------------------------------------------------------------
# TYPOGRAPHY
# ------------------------------------------------------------

BASE_SIZE          <- 7
AXIS_TEXT_SIZE     <- 6.5
AXIS_TITLE_SIZE    <- 7
LEGEND_TEXT_SIZE   <- 6.5
LEGEND_TITLE_SIZE  <- 7

# ------------------------------------------------------------
# DESIGN ORDER, LABELS, COLOURS AND SHAPES
# ------------------------------------------------------------

design_order <- c(
  "CAREM",
  "BREST-OD-300",
  "SMART",
  "KLT-40S",
  "NuScale",
  "RITM-200",
  "HTR-PM",
  "ACP-100",
  "Xe-100",
  "BWRX-300"
)

legend_order <- c(
  "CAREM",
  "BREST-OD-300",
  "SMART",
  "KLT-40S (Operational)",
  "NuScale",
  "RITM-200 (Operational)",
  "HTR-PM (Operational)",
  "ACP-100",
  "Xe-100",
  "BWRX-300"
)

legend_cols <- c(
  "CAREM"                  = "#5B8DB8",
  "BREST-OD-300"           = "#000080",
  "SMART"                  = "#F08080",
  "KLT-40S (Operational)"  = "#FF9900",
  "NuScale"                = "#D00000",
  "RITM-200 (Operational)" = "#B8860B",
  "HTR-PM (Operational)"   = "#B22222",
  "ACP-100"                = "#32CD32",
  "Xe-100"                 = "#40CFC7",
  "BWRX-300"               = "#137A1F"
)

legend_shapes <- c(
  "CAREM"                  = 16,
  "BREST-OD-300"           = 16,
  "SMART"                  = 16,
  "KLT-40S (Operational)"  = 17,
  "NuScale"                = 16,
  "RITM-200 (Operational)" = 17,
  "HTR-PM (Operational)"   = 17,
  "ACP-100"                = 17,
  "Xe-100"                 = 16,
  "BWRX-300"               = 16
)

# ============================================================
# LOAD DATA
# ============================================================

raw <- readxl::read_excel(FILE_IN)

required_cols <- c(
  "design",
  "cd_start_year",
  "total_duration"
)

missing_cols <- setdiff(required_cols, names(raw))

if (length(missing_cols) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_cols, collapse = ", "),
    "\nAvailable columns are: ",
    paste(names(raw), collapse = ", ")
  )
}

plot_df <- raw %>%
  transmute(
    design = as.character(design),
    cd_start_year = suppressWarnings(as.numeric(cd_start_year)),
    total_duration = suppressWarnings(as.numeric(total_duration))
  ) %>%
  filter(
    !is.na(design),
    !is.na(cd_start_year),
    !is.na(total_duration)
  ) %>%
  mutate(
    design = factor(design, levels = design_order),
    smr_label = case_when(
      as.character(design) == "KLT-40S"  ~ "KLT-40S (Operational)",
      as.character(design) == "RITM-200" ~ "RITM-200 (Operational)",
      as.character(design) == "HTR-PM"   ~ "HTR-PM (Operational)",
      TRUE ~ as.character(design)
    ),
    smr_label = factor(smr_label, levels = legend_order)
  ) %>%
  arrange(design)

if (nrow(plot_df) == 0) {
  stop("No usable rows found in source data.")
}

# ============================================================
# PLOT
# ============================================================

p <- ggplot(
  plot_df,
  aes(
    x = cd_start_year,
    y = total_duration
  )
) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    colour = "black",
    fill = "grey70",
    linewidth = 0.8
  ) +
  geom_point(
    aes(
      colour = smr_label,
      shape = smr_label
    ),
    size = 2.7,
    stroke = 0.3
  ) +
  scale_colour_manual(
    values = legend_cols,
    breaks = legend_order,
    name = "SMR Design"
  ) +
  scale_shape_manual(
    values = legend_shapes,
    breaks = legend_order,
    name = "SMR Design"
  ) +
  scale_x_continuous(
    limits = c(1983.0, 2020.5),
    breaks = seq(1985, 2020, by = 5),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 35),
    breaks = seq(0, 35, by = 5),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = "Start year",
    y = "Duration of the design phase (years)"
  ) +
  theme_classic(base_size = BASE_SIZE) +
  theme(
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks  = element_line(colour = "grey35", linewidth = 0.3),
    axis.text.x = element_text(size = AXIS_TEXT_SIZE, colour = "grey20"),
    axis.text.y = element_text(size = AXIS_TEXT_SIZE, colour = "grey20"),
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
    legend.position = "right",
    legend.title = element_text(
      size = LEGEND_TITLE_SIZE,
      face = "bold",
      colour = "black"
    ),
    legend.text = element_text(
      size = LEGEND_TEXT_SIZE,
      colour = "black"
    ),
    legend.key = element_blank(),
    legend.key.height = unit(0.42, "cm"),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.box.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(t = 6, r = 8, b = 6, l = 6),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

print(p)

# ============================================================
# SAVE SUPPLEMENTARY FIGURE 11
# ============================================================

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".png")),
  plot = p,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".pdf")),
  plot = p,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  device = grDevices::cairo_pdf,
  bg = "white"
)

saveRDS(
  object = p,
  file = file.path(OUT_DIR, paste0(FILE_STUB, ".rds"))
)

p