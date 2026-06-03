# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Supplementary Fig. 4: SMR design portfolio by country
# and development phase.
#
# Required input:
# data/Supplementary_Data/sfig04__source_data_country_phase_counts.xlsx
#
# Outputs:
# outputs/Supplementary_Figures/Figure_04/sfig04_country_phase_counts.png
# outputs/Supplementary_Figures/Figure_04/sfig04_country_phase_counts.pdf
# outputs/Supplementary_Figures/Figure_04/sfig04_country_phase_counts.rds

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(grid)
})

rm(list = ls())
graphics.off()

# ============================================================
# SUPPLEMENTARY FIGURE 4
# COUNTRY PHASE COUNTS
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
        "Repository root not found. Please run this script from within ",
        "the first-of-a-kind-smr-deployment-prospects repository."
      )
    }
    
    current_dir <- parent_dir
  }
}

ROOT_DIR <- find_repo_root()

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

FILE <- "sfig04__source_data_country_phase_counts.xlsx"

FILE_IN <- file.path(
  ROOT_DIR,
  "data",
  "Supplementary_Data",
  FILE
)

OUT_DIR <- file.path(
  ROOT_DIR,
  "outputs",
  "Supplementary_Figures",
  "Figure_04"
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(FILE_IN)) {
  stop("Input file not found: ", FILE_IN)
}

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 180
SAVE_H_MM <- 110
SAVE_DPI  <- 600

FILE_STUB <- "sfig04_country_phase_counts"

# ------------------------------------------------------------
# STYLE
# ------------------------------------------------------------

BASE_SIZE <- 7

axis_line_col <- "grey35"
axis_line_lw  <- 0.3

phase_order <- c(
  "Conceptual design",
  "Basic design",
  "Detailed design",
  "Licensing",
  "Construction",
  "Operational"
)

phase_legend_order <- rev(phase_order)

phase_cols <- c(
  "Conceptual design" = "#FDE0D2",
  "Basic design"      = "#F4A582",
  "Detailed design"   = "#D6604D",
  "Licensing"         = "#D1E5F0",
  "Construction"      = "#92C5DE",
  "Operational"       = "#4393C3"
)

# ============================================================
# LOAD AND STANDARDISE DATA
# ============================================================

df <- read_excel(FILE_IN)

names(df) <- gsub("\\s+", "_", names(df))

if (!all(c("Country", "Phase") %in% names(df))) {
  stop(
    "Required columns not found. Available columns are: ",
    paste(names(df), collapse = ", ")
  )
}

plot_dat <- df %>%
  mutate(
    Country = trimws(as.character(Country)),
    Phase   = trimws(as.character(Phase))
  ) %>%
  filter(
    !is.na(Country),
    Country != "",
    !is.na(Phase),
    Phase %in% phase_order
  ) %>%
  count(Country, Phase, name = "n_designs")

country_order <- plot_dat %>%
  group_by(Country) %>%
  summarise(total = sum(n_designs), .groups = "drop") %>%
  arrange(desc(total), Country) %>%
  pull(Country)

plot_dat <- plot_dat %>%
  mutate(
    Country = factor(Country, levels = country_order),
    Phase   = factor(Phase, levels = rev(phase_order))
  )

# ============================================================
# PLOT
# ============================================================

p <- ggplot(plot_dat, aes(x = Country, y = n_designs, fill = Phase)) +
  geom_col(width = 0.7) +
  scale_fill_manual(
    values = phase_cols,
    breaks = phase_legend_order,
    name = "Phase"
  ) +
  scale_y_continuous(
    limits = c(0, 16),
    breaks = c(0, 4, 8, 12, 16),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = "Country",
    y = "Number of SMR designs"
  ) +
  theme_classic(base_size = BASE_SIZE) +
  theme(
    axis.line.x          = element_line(colour = axis_line_col, linewidth = axis_line_lw),
    axis.line.y          = element_line(colour = axis_line_col, linewidth = axis_line_lw),
    axis.ticks           = element_line(colour = axis_line_col, linewidth = axis_line_lw),
    axis.ticks.length    = unit(0.12, "cm"),
    axis.title           = element_text(size = BASE_SIZE),
    axis.text            = element_text(size = BASE_SIZE),
    axis.text.x          = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.title         = element_text(size = BASE_SIZE),
    legend.text          = element_text(size = BASE_SIZE),
    legend.position      = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background    = element_blank(),
    legend.key           = element_blank(),
    plot.margin          = margin(5.5, 5.5, 5.5, 5.5),
    plot.background      = element_rect(fill = "white", colour = NA),
    panel.background     = element_rect(fill = "white", colour = NA)
  )

print(p)

# ============================================================
# SAVE SUPPLEMENTARY FIGURE 4
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
  device = cairo_pdf,
  bg = "white"
)

saveRDS(
  object = p,
  file = file.path(OUT_DIR, paste0(FILE_STUB, ".rds"))
)

p