# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Extended Data Fig. 2: historical SMR design-entry trajectories
# by reactor cooling type and OECD membership.
#
# Required input:
# data/Extended_Data/edfig02_source_data_historical_cooling_oecd_trajectories.xlsx
#
# Outputs:
# outputs/Extended_Data/Figure_02/edfig02_historical_cooling_oecd_trajectories.png
# outputs/Extended_Data/Figure_02/edfig02_historical_cooling_oecd_trajectories.pdf

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
# EXTENDED DATA FIGURE 2
# HISTORICAL SMR DESIGN ENTRIES BY COOLING TYPE AND OECD MEMBERSHIP
# ============================================================

# ------------------------------------------------------------
# PROJECT PATHS
# ------------------------------------------------------------

find_repo_root <- function(marker = file.path("data", "Extended_Data", "edfig02_source_data_historical_cooling_oecd_trajectories.xlsx")) {
  path <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  
  repeat {
    candidate <- file.path(path, marker)
    
    if (file.exists(candidate)) {
      return(path)
    }
    
    parent <- dirname(path)
    
    if (identical(parent, path)) {
      stop(
        "Could not find repository root containing ",
        marker,
        "\nCurrent working directory was: ",
        getwd()
      )
    }
    
    path <- parent
  }
}

REPO_ROOT <- find_repo_root()

DATA_DIR <- file.path(
  REPO_ROOT,
  "data",
  "Extended_Data"
)

FILE <- file.path(
  DATA_DIR,
  "edfig02_source_data_historical_cooling_oecd_trajectories.xlsx"
)

OUT_DIR <- file.path(
  REPO_ROOT,
  "outputs",
  "Extended_Data",
  "Figure_02"
)

OUT_BASE <- "edfig02_historical_cooling_oecd_trajectories"

if (!file.exists(FILE)) {
  stop("Input file not found: ", FILE)
}

dir.create(
  OUT_DIR,
  showWarnings = FALSE,
  recursive = TRUE
)

# ============================================================
# EXPORT SETTINGS
# ============================================================

SAVE_W_MM <- 180
SAVE_H_MM <- 150
SAVE_DPI  <- 600

# ============================================================
# AXES AND STYLING
# ============================================================

YEAR_START_DATA <- 1985L
YEAR_START_AXIS <- 1983.9
YEAR_END        <- 2025L

x_breaks <- c(1985, 1995, 2005, 2015, 2025)

axis_line_col <- "grey35"
axis_line_lw  <- 0.3

cooling_palette_4 <- c(
  "Water-cooled" = "#1f77b4",
  "Metal-cooled" = "#A9A9A9",
  "Gas-cooled"   = "#d62728",
  "Salt-cooled"  = "#1b9e77"
)

cooling_palette_2 <- c(
  "Water-cooled"     = "#1f77b4",
  "Non-water-cooled" = "#e07b7b"
)

oecd_palette <- c(
  "OECD"     = "#B58900",
  "Non-OECD" = "#6A3D9A"
)

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

is_oecd_flag <- function(x) {
  toupper(trimws(as.character(x))) %in% c(
    "OECD",
    "YES",
    "Y",
    "1",
    "TRUE"
  )
}

recode_cooling <- function(x) {
  x <- trimws(as.character(x))
  
  dplyr::case_when(
    stringr::str_detect(x, stringr::regex("water", ignore_case = TRUE)) ~ "Water-cooled",
    stringr::str_detect(x, stringr::regex("metal", ignore_case = TRUE)) ~ "Metal-cooled",
    stringr::str_detect(x, stringr::regex("gas", ignore_case = TRUE))   ~ "Gas-cooled",
    stringr::str_detect(x, stringr::regex("salt", ignore_case = TRUE))  ~ "Salt-cooled",
    TRUE ~ NA_character_
  )
}

wrap_panel <- function(p, tag) {
  ggdraw() +
    draw_plot(p) +
    draw_plot_label(
      tag,
      x = 0.01,
      y = 0.99,
      hjust = 0,
      vjust = 1,
      size = 11,
      fontface = "bold"
    )
}

make_cumulative <- function(data, group_var) {
  levs <- unique(as.character(data[[group_var]]))
  
  out <- data %>%
    count(
      CD_Start,
      .data[[group_var]],
      name = "n"
    ) %>%
    complete(
      CD_Start = seq(YEAR_START_DATA, YEAR_END, by = 1),
      !!sym(group_var) := levs,
      fill = list(n = 0)
    ) %>%
    arrange(
      .data[[group_var]],
      CD_Start
    ) %>%
    group_by(
      .data[[group_var]]
    ) %>%
    mutate(
      cum_n = cumsum(n)
    ) %>%
    ungroup()
  
  out[[group_var]] <- factor(
    out[[group_var]],
    levels = levs
  )
  
  out
}

base_theme <- theme_classic(base_size = 7) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(
      colour = axis_line_col,
      linewidth = axis_line_lw
    ),
    axis.line.y = element_line(
      colour = axis_line_col,
      linewidth = axis_line_lw
    ),
    axis.ticks = element_line(
      colour = axis_line_col,
      linewidth = axis_line_lw
    ),
    axis.ticks.length = unit(0.12, "cm"),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 7),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.spacing.y = unit(0.03, "cm"),
    plot.margin = margin(5.5, 5.5, 5.5, 5.5),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

# ============================================================
# LOAD DATA
# ============================================================

df_raw <- readxl::read_excel(FILE)

names(df_raw) <- clean_names_local(names(df_raw))

required_cols <- c(
  "smr",
  "phase",
  "cooling_type",
  "oecd",
  "cd_start"
)

missing_cols <- required_cols[!required_cols %in% names(df_raw)]

if (length(missing_cols) > 0) {
  stop(
    "Missing required columns in Extended Data Fig. 2 source workbook:\n",
    paste(missing_cols, collapse = "\n"),
    "\n\nAvailable columns are:\n",
    paste(names(df_raw), collapse = "\n")
  )
}

df <- df_raw %>%
  mutate(
    SMR = as.character(smr),
    Phase = trimws(as.character(phase)),
    cooling_type = recode_cooling(cooling_type),
    oecd_group = if_else(
      is_oecd_flag(oecd),
      "OECD",
      "Non-OECD"
    ),
    water_group = if_else(
      cooling_type == "Water-cooled",
      "Water-cooled",
      "Non-water-cooled"
    ),
    CD_Start = suppressWarnings(
      as.integer(round(as.numeric(cd_start)))
    )
  ) %>%
  filter(
    !is.na(SMR),
    !is.na(CD_Start),
    CD_Start >= YEAR_START_DATA,
    CD_Start <= YEAR_END,
    !is.na(cooling_type),
    !str_detect(tolower(Phase), "failed")
  ) %>%
  distinct(
    SMR,
    .keep_all = TRUE
  )

# ============================================================
# PANEL A — WATER-COOLED OECD VS NON-OECD
# ============================================================

cum_a <- df %>%
  filter(water_group == "Water-cooled") %>%
  make_cumulative("oecd_group")

cum_a$oecd_group <- factor(
  cum_a$oecd_group,
  levels = c("OECD", "Non-OECD")
)

p_a <- ggplot(
  cum_a,
  aes(
    x = CD_Start,
    y = cum_n,
    colour = oecd_group
  )
) +
  geom_line(linewidth = 0.9) +
  annotate(
    "text",
    x = 2005,
    y = 33.0,
    label = "Water-cooled",
    size = 2.7,
    fontface = "plain",
    hjust = 0.5,
    vjust = 1
  ) +
  scale_colour_manual(
    values = oecd_palette,
    breaks = c("OECD", "Non-OECD"),
    name = "OECD membership"
  ) +
  scale_x_continuous(
    limits = c(YEAR_START_AXIS, YEAR_END + 1),
    breaks = x_breaks,
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 33.5),
    breaks = c(10, 20, 30),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Conceptual design start year",
    y = "Cumulative number of designs"
  ) +
  base_theme +
  theme(
    legend.position = c(0.02, 0.92),
    legend.justification = c(0, 1)
  )

# ============================================================
# PANEL B — NON-WATER-COOLED OECD VS NON-OECD
# ============================================================

cum_b <- df %>%
  filter(water_group == "Non-water-cooled") %>%
  make_cumulative("oecd_group")

cum_b$oecd_group <- factor(
  cum_b$oecd_group,
  levels = c("OECD", "Non-OECD")
)

p_b <- ggplot(
  cum_b,
  aes(
    x = CD_Start,
    y = cum_n,
    colour = oecd_group
  )
) +
  geom_line(linewidth = 0.9) +
  annotate(
    "text",
    x = 2005,
    y = 33.0,
    label = "Non-water-cooled",
    size = 2.7,
    fontface = "plain",
    hjust = 0.5,
    vjust = 1
  ) +
  scale_colour_manual(
    values = oecd_palette,
    breaks = c("OECD", "Non-OECD"),
    name = "OECD membership"
  ) +
  scale_x_continuous(
    limits = c(YEAR_START_AXIS, YEAR_END + 1),
    breaks = x_breaks,
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 33.5),
    breaks = c(10, 20, 30),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Conceptual design start year",
    y = "Cumulative number of designs"
  ) +
  base_theme +
  theme(
    legend.position = c(0.02, 0.92),
    legend.justification = c(0, 1)
  )

# ============================================================
# PANEL C — GLOBAL FOUR COOLING TYPES
# ============================================================

cum_c <- make_cumulative(
  df,
  "cooling_type"
)

cum_c$cooling_type <- factor(
  cum_c$cooling_type,
  levels = c(
    "Water-cooled",
    "Metal-cooled",
    "Gas-cooled",
    "Salt-cooled"
  )
)

p_c <- ggplot(
  cum_c,
  aes(
    x = CD_Start,
    y = cum_n,
    colour = cooling_type
  )
) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(
    values = cooling_palette_4,
    breaks = c(
      "Water-cooled",
      "Metal-cooled",
      "Gas-cooled",
      "Salt-cooled"
    ),
    name = "Cooling type"
  ) +
  scale_x_continuous(
    limits = c(YEAR_START_AXIS, YEAR_END + 1),
    breaks = x_breaks,
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 40),
    breaks = c(10, 20, 30, 40),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Conceptual design start year",
    y = "Cumulative number of designs"
  ) +
  base_theme +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1)
  )

# ============================================================
# PANEL D — GLOBAL WATER VS NON-WATER
# ============================================================

cum_d <- make_cumulative(
  df,
  "water_group"
)

cum_d$water_group <- factor(
  cum_d$water_group,
  levels = c(
    "Water-cooled",
    "Non-water-cooled"
  )
)

p_d <- ggplot(
  cum_d,
  aes(
    x = CD_Start,
    y = cum_n,
    colour = water_group
  )
) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(
    values = cooling_palette_2,
    breaks = c(
      "Water-cooled",
      "Non-water-cooled"
    ),
    name = "Cooling type"
  ) +
  scale_x_continuous(
    limits = c(YEAR_START_AXIS, YEAR_END + 1),
    breaks = x_breaks,
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 40),
    breaks = c(10, 20, 30, 40),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Conceptual design start year",
    y = "Cumulative number of designs"
  ) +
  base_theme +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1)
  )

# ============================================================
# WRAP AND ASSEMBLE
# ============================================================

pa <- wrap_panel(p_a, "a")
pb <- wrap_panel(p_b, "b")
pc <- wrap_panel(p_c, "c")
pd <- wrap_panel(p_d, "d")

fig <- plot_grid(
  pa,
  pb,
  pc,
  pd,
  ncol = 2,
  align = "hv",
  axis = "tblr"
)

print(fig)

# ============================================================
# SAVE EXTENDED DATA FIGURE 2
# ============================================================

ggsave(
  filename = file.path(OUT_DIR, paste0(OUT_BASE, ".png")),
  plot = fig,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, paste0(OUT_BASE, ".pdf")),
  plot = fig,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  bg = "white",
  useDingbats = FALSE
)

fig
