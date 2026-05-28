# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Extended Data Fig. 1: global SMR design-pipeline evolution
# before and after Fukushima, distinguishing cumulative entries, inactive
# designs and stalled designs.
#
# Required input:
# edfig01_source_data_pre_post_fukushima_pipeline.xlsx
#
# Outputs:
# edfig01_pre_post_fukushima_pipeline.png
# edfig01_pre_post_fukushima_pipeline.pdf

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(grid)
  library(tibble)
})

rm(list = ls())
graphics.off()

# ============================================================
# EXTENDED DATA FIGURE 1
# GLOBAL SMR DESIGN PIPELINE, 1984–2025
# ============================================================

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

FILE <- "edfig01_source_data_pre_post_fukushima_pipeline.xlsx"
OUT_DIR <- "."

if (!file.exists(FILE)) {
  stop("Input file not found: ", FILE)
}

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 180
SAVE_H_MM <- 120
SAVE_DPI  <- 600

FILE_STUB <- "edfig01_pre_post_fukushima_pipeline"

# ------------------------------------------------------------
# TYPOGRAPHY
# ------------------------------------------------------------

BASE_SIZE         <- 7
AXIS_TEXT_SIZE    <- 6.5
AXIS_TITLE_SIZE   <- 7
LEGEND_TEXT_SIZE  <- 6.0
LEGEND_TITLE_SIZE <- 7

# ------------------------------------------------------------
# PHASE SETTINGS
# ------------------------------------------------------------

phase_levels <- c(
  "Conceptual design phase",
  "Basic design phase",
  "Detailed design phase",
  "Licensing phase",
  "Construction phase"
)

phase_colors <- c(
  "Conceptual design phase" = "#FDE0D2",
  "Basic design phase"      = "#F4A582",
  "Detailed design phase"   = "#D6604D",
  "Licensing phase"         = "#D1E5F0",
  "Construction phase"      = "#92C5DE"
)

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------

clean_names_local <- function(x) {
  x <- trimws(x)
  x <- gsub("[[:space:]]+", "_", x)
  x <- gsub("[^A-Za-z0-9_]", "", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

normalise_event_phase <- function(x) {
  key <- tolower(trimws(as.character(x)))
  key <- gsub("[[:space:]-]+", "_", key)
  
  dplyr::case_when(
    grepl("conceptual", key) ~ "Conceptual design phase",
    grepl("basic", key)      ~ "Basic design phase",
    grepl("detailed", key)   ~ "Detailed design phase",
    grepl("licens", key)     ~ "Licensing phase",
    grepl("construct", key)  ~ "Construction phase",
    TRUE ~ NA_character_
  )
}

base_theme <- function() {
  theme_classic(base_size = BASE_SIZE) +
    theme(
      axis.text.x = element_text(
        size = AXIS_TEXT_SIZE,
        colour = "grey20",
        margin = margin(t = 10)
      ),
      axis.text.y = element_text(
        size = AXIS_TEXT_SIZE,
        colour = "grey20"
      ),
      axis.title.x = element_text(
        size = AXIS_TITLE_SIZE,
        colour = "black",
        margin = margin(t = 12)
      ),
      axis.title.y = element_text(
        size = AXIS_TITLE_SIZE,
        colour = "black"
      ),
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
      axis.ticks.length = unit(2.5, "pt"),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      legend.key.width = unit(7, "mm"),
      legend.key.height = unit(3.5, "mm"),
      legend.spacing.y = unit(0.5, "mm"),
      legend.box.spacing = unit(0, "mm"),
      legend.text = element_text(
        size = LEGEND_TEXT_SIZE,
        colour = "black"
      ),
      legend.title = element_text(
        size = LEGEND_TITLE_SIZE,
        colour = "black"
      ),
      plot.background = element_rect(
        fill = "white",
        colour = NA
      ),
      panel.background = element_rect(
        fill = "white",
        colour = NA
      )
    )
}

# ============================================================
# LOAD AND STANDARDISE DATA
# ============================================================

dat <- readxl::read_excel(
  FILE,
  na = c("n/a", "NA", "")
)

names(dat) <- clean_names_local(names(dat))

required_cols <- c(
  "smr",
  "cd_start",
  "failed",
  "year_of_failure",
  "failure_phase",
  "stalled_development",
  "year_stalled",
  "stalled_phase"
)

missing_cols <- required_cols[!required_cols %in% names(dat)]

if (length(missing_cols) > 0) {
  stop(
    "Missing required columns in Extended Data Fig. 1 source workbook:\n",
    paste(missing_cols, collapse = "\n"),
    "\n\nAvailable columns are:\n",
    paste(names(dat), collapse = "\n")
  )
}

plot_dat <- dat %>%
  mutate(
    smr = as.character(smr),
    cd_start = suppressWarnings(as.numeric(cd_start)),
    year_of_failure = suppressWarnings(as.numeric(year_of_failure)),
    year_stalled = suppressWarnings(as.numeric(year_stalled)),
    failed_flag_raw = toupper(trimws(as.character(failed))) == "TRUE",
    stalled_flag_raw = toupper(trimws(as.character(stalled_development))) == "TRUE",
    failure_phase = tolower(trimws(as.character(failure_phase))),
    stalled_phase = tolower(trimws(as.character(stalled_phase)))
  ) %>%
  mutate(
    failure_phase = gsub("[[:space:]-]+", "_", failure_phase),
    stalled_phase = gsub("[[:space:]-]+", "_", stalled_phase),
    failed_event_flag = failed_flag_raw |
      (
        !is.na(year_of_failure) &
          !is.na(failure_phase) &
          between(year_of_failure, 1984, 2025)
      ),
    stalled_event_flag = (
      (
        !is.na(year_stalled) &
          !is.na(stalled_phase) &
          between(year_stalled, 1984, 2025)
      ) |
        (
          stalled_flag_raw &
            !is.na(year_stalled) &
            !is.na(stalled_phase) &
            between(year_stalled, 1984, 2025)
        )
    ) & !failed_event_flag
  )

# ============================================================
# CUMULATIVE SERIES
# ============================================================

entries <- plot_dat %>%
  filter(
    !is.na(cd_start),
    between(cd_start, 1984, 2025)
  ) %>%
  distinct(smr, cd_start) %>%
  count(year = cd_start) %>%
  complete(
    year = 1984:2025,
    fill = list(n = 0)
  )

failures <- plot_dat %>%
  filter(
    failed_event_flag,
    !is.na(year_of_failure),
    between(year_of_failure, 1984, 2025)
  ) %>%
  distinct(smr, year_of_failure) %>%
  count(year = year_of_failure) %>%
  complete(
    year = 1984:2025,
    fill = list(n = 0)
  )

stalls <- plot_dat %>%
  filter(
    stalled_event_flag,
    !is.na(year_stalled),
    between(year_stalled, 1984, 2025)
  ) %>%
  distinct(smr, year_stalled) %>%
  count(year = year_stalled) %>%
  complete(
    year = 1984:2025,
    fill = list(n = 0)
  )

series <- entries %>%
  rename(n_entries = n) %>%
  left_join(
    failures %>% rename(n_failures = n),
    by = "year"
  ) %>%
  left_join(
    stalls %>% rename(n_stalled = n),
    by = "year"
  ) %>%
  mutate(
    n_failures = ifelse(is.na(n_failures), 0, n_failures),
    n_stalled = ifelse(is.na(n_stalled), 0, n_stalled),
    cumulative_entries = cumsum(n_entries),
    cumulative_active = cumulative_entries - cumsum(n_failures),
    cumulative_active_stalled = cumulative_active - cumsum(n_stalled)
  )

# ============================================================
# EVENT BARS
# ============================================================

event_bars <- bind_rows(
  plot_dat %>%
    filter(
      failed_event_flag,
      !is.na(year_of_failure),
      !is.na(failure_phase),
      between(year_of_failure, 1984, 2025)
    ) %>%
    transmute(
      year = year_of_failure,
      phase_group = normalise_event_phase(failure_phase)
    ),
  plot_dat %>%
    filter(
      stalled_event_flag,
      !is.na(year_stalled),
      !is.na(stalled_phase),
      between(year_stalled, 1984, 2025)
    ) %>%
    transmute(
      year = year_stalled,
      phase_group = normalise_event_phase(stalled_phase)
    )
) %>%
  filter(!is.na(phase_group)) %>%
  mutate(
    phase_group = factor(
      phase_group,
      levels = phase_levels
    )
  ) %>%
  arrange(year, phase_group) %>%
  group_by(year) %>%
  mutate(
    id = row_number(),
    ymin = -id,
    ymax = -(id - 1)
  ) %>%
  ungroup()

legend_fill_df <- tibble(
  x = NA_real_,
  y = NA_real_,
  phase_group = factor(
    phase_levels,
    levels = phase_levels
  )
)

# ============================================================
# PLOT
# ============================================================

p <- ggplot(series, aes(x = year)) +
  geom_rect(
    data = event_bars,
    aes(
      xmin = year - 0.4,
      xmax = year + 0.4,
      ymin = ymin,
      ymax = ymax,
      fill = phase_group
    ),
    inherit.aes = FALSE,
    colour = "grey85",
    linewidth = 0.25,
    show.legend = FALSE
  ) +
  geom_point(
    data = legend_fill_df,
    aes(
      x = x,
      y = y,
      fill = phase_group
    ),
    inherit.aes = FALSE,
    shape = 22,
    size = 3.2,
    stroke = 0.2,
    colour = "grey70",
    show.legend = TRUE
  ) +
  geom_hline(
    yintercept = 0,
    colour = "grey35",
    linewidth = 0.3
  ) +
  geom_step(
    aes(
      y = cumulative_active_stalled,
      colour = "Excluding inactive and stalled designs"
    ),
    linewidth = 0.7,
    direction = "hv"
  ) +
  geom_step(
    aes(
      y = cumulative_active,
      colour = "Excluding inactive designs"
    ),
    linewidth = 0.7,
    direction = "hv"
  ) +
  geom_step(
    aes(
      y = cumulative_entries,
      colour = "Cumulative SMR designs"
    ),
    linewidth = 0.7,
    direction = "hv"
  ) +
  geom_vline(
    xintercept = 2011,
    colour = "grey55",
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = 2004.3,
    y = 8.2,
    label = "Pre-Fukushima period",
    size = 2.45,
    colour = "black",
    hjust = 0.5
  ) +
  annotate(
    "text",
    x = 2015.4,
    y = 63.5,
    label = "Post-Fukushima period",
    size = 2.45,
    colour = "black",
    hjust = 0.5
  ) +
  scale_colour_manual(
    values = c(
      "Cumulative SMR designs" = "black",
      "Excluding inactive designs" = "#1F4E79",
      "Excluding inactive and stalled designs" = "grey55"
    ),
    breaks = c(
      "Cumulative SMR designs",
      "Excluding inactive designs",
      "Excluding inactive and stalled designs"
    ),
    name = "Cumulative design trajectories"
  ) +
  scale_fill_manual(
    values = phase_colors,
    breaks = phase_levels,
    name = "Inactive / stalled development"
  ) +
  guides(
    colour = guide_legend(
      order = 1,
      byrow = TRUE,
      override.aes = list(
        linewidth = 0.9,
        linetype = 1,
        shape = NA,
        fill = NA,
        alpha = 1
      )
    ),
    fill = guide_legend(
      order = 2,
      byrow = TRUE,
      ncol = 1,
      override.aes = list(
        shape = 22,
        size = 3.2,
        colour = "grey70",
        stroke = 0.2
      )
    )
  ) +
  scale_x_continuous(
    limits = c(1984, 2025.8),
    breaks = c(1985, 1995, 2005, 2015, 2025),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(-5.5, 75),
    breaks = c(-5, 0, 25, 50, 75),
    labels = c("-5", "0", "25", "50", "75"),
    expand = c(0, 0)
  ) +
  labs(
    x = "Year",
    y = "Cumulative SMR designs"
  ) +
  base_theme() +
  theme(
    legend.position = c(0.03, 0.98),
    legend.justification = c(0, 1)
  )

print(p)

# ============================================================
# SAVE EXTENDED DATA FIGURE 1
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
  bg = "white",
  useDingbats = FALSE
)

p
