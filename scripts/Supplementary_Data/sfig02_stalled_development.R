# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Supplementary Fig. 2: design-phase failure and stalled
# development across the SMR pipeline.
#
# Required input:
# data/Supplementary_Data/sfig02_source_data_stalled_development.xlsx
#
# Outputs:
# outputs/Supplementary_Data/sfig02_stalled_development.png
# outputs/Supplementary_Data/sfig02_stalled_development.pdf

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

rm(list = ls())
graphics.off()

# ============================================================
# SUPPLEMENTARY FIGURE 2
# DESIGN-PHASE FAILURE AND STALLED DEVELOPMENT
# ============================================================

# ------------------------------------------------------------
# PATHS — GITHUB RUN
# ------------------------------------------------------------

DATA_DIR <- file.path(
  "data",
  "Supplementary_Data"
)

FILE <- file.path(
  DATA_DIR,
  "sfig02_source_data_stalled_development.xlsx"
)

OUT_DIR <- file.path(
  "outputs",
  "Supplementary_Data"
)

if (!file.exists(FILE)) {
  stop("Input file not found: ", FILE)
}

dir.create(
  OUT_DIR,
  showWarnings = FALSE,
  recursive = TRUE
)

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 120
SAVE_H_MM <- 120
SAVE_DPI  <- 600

FILE_STUB <- "sfig02_stalled_development"

# ------------------------------------------------------------
# SETTINGS
# ------------------------------------------------------------

CENSOR_YEAR <- 2025
INACTIVE_SEG_YEARS <- 1
TIMEOUT_MARKER_OFFSET <- 1

phase_levels <- c(
  "Conceptual design",
  "Basic design",
  "Detailed design"
)

phase_index_map <- setNames(
  seq_along(phase_levels),
  phase_levels
)

phase_cols <- c(
  "Conceptual design" = "#FDE0D2",
  "Basic design"      = "#F4A582",
  "Detailed design"   = "#D6604D"
)

outcome_cols <- c(
  "Inactive design"     = "#D62728",
  "Stalled development" = "grey35"
)

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------

clean_numeric <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[tolower(x) %in% c("n/a", "na", "", "null", "-")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

clean_names_local <- function(x) {
  x <- tolower(x)
  x <- gsub("\\s+", "_", x)
  x <- gsub("\\.", "_", x)
  x
}

# ============================================================
# LOAD AND STANDARDISE DATA
# ============================================================

dat <- read_excel(FILE) %>%
  rename_with(clean_names_local)

required_cols <- c(
  "smr",
  "phase",
  "status",
  "stalled_year",
  "cd_start",
  "bd_start",
  "dd_start"
)

missing_cols <- setdiff(
  required_cols,
  names(dat)
)

if (length(missing_cols) > 0) {
  stop(
    "Missing required column(s): ",
    paste(missing_cols, collapse = ", "),
    "\nAvailable columns are: ",
    paste(names(dat), collapse = ", ")
  )
}

dat <- dat %>%
  mutate(
    smr          = as.character(smr),
    phase        = as.character(phase),
    status       = as.character(status),
    stalled_year = clean_numeric(stalled_year),
    cd_start     = clean_numeric(cd_start),
    bd_start     = clean_numeric(bd_start),
    dd_start     = clean_numeric(dd_start),
    status_clean = tolower(trimws(status)),
    phase_clean  = tolower(trimws(phase))
  ) %>%
  mutate(
    last_phase = case_when(
      grepl("conceptual", phase_clean) ~ "Conceptual design",
      grepl("basic",      phase_clean) ~ "Basic design",
      grepl("detailed",   phase_clean) ~ "Detailed design",
      TRUE ~ NA_character_
    ),
    last_phase_idx = unname(phase_index_map[last_phase]),
    Outcome = case_when(
      status_clean %in% c(
        "inactive",
        "failed",
        "failed design",
        "cancelled",
        "canceled",
        "suspended"
      ) ~ "Inactive design",
      status_clean %in% c(
        "stalled",
        "stalled development",
        "timeout",
        "timeout case"
      ) ~ "Stalled development",
      TRUE ~ NA_character_
    )
  )

# ============================================================
# LONG PHASE DATA
# ============================================================

phase_long <- dat %>%
  select(
    smr,
    status,
    Outcome,
    last_phase,
    last_phase_idx,
    cd_start,
    bd_start,
    dd_start
  ) %>%
  pivot_longer(
    cols = c(cd_start, bd_start, dd_start),
    names_to = "phase_key",
    values_to = "start_year"
  ) %>%
  filter(!is.na(start_year)) %>%
  mutate(
    Phase_name = recode(
      phase_key,
      "cd_start" = "Conceptual design",
      "bd_start" = "Basic design",
      "dd_start" = "Detailed design"
    ),
    Phase_name = factor(
      Phase_name,
      levels = phase_levels
    ),
    phase_idx = unname(
      phase_index_map[as.character(Phase_name)]
    )
  ) %>%
  filter(
    !is.na(last_phase_idx),
    phase_idx <= last_phase_idx
  )

# ============================================================
# PHASE SEGMENTS
# ============================================================

phase_seg <- phase_long %>%
  arrange(
    smr,
    phase_idx,
    start_year
  ) %>%
  group_by(smr) %>%
  mutate(
    end_year_raw = lead(start_year),
    end_year_raw = ifelse(
      is.na(end_year_raw),
      CENSOR_YEAR,
      end_year_raw
    ),
    end_year = case_when(
      Outcome == "Inactive design" &
        as.character(Phase_name) == last_phase ~
        pmin(start_year + INACTIVE_SEG_YEARS, CENSOR_YEAR),
      TRUE ~ end_year_raw
    )
  ) %>%
  ungroup()

# ============================================================
# OUTCOME MARKERS
# ============================================================

outcome_markers <- dat %>%
  filter(!is.na(Outcome)) %>%
  rowwise() %>%
  mutate(
    last_start = max(
      c(cd_start, bd_start, dd_start),
      na.rm = TRUE
    ),
    last_start = ifelse(
      is.infinite(last_start),
      NA_real_,
      last_start
    ),
    marker_x = case_when(
      Outcome == "Inactive design" ~
        pmin(last_start + INACTIVE_SEG_YEARS, CENSOR_YEAR),
      Outcome == "Stalled development" & !is.na(stalled_year) ~
        stalled_year,
      Outcome == "Stalled development" & is.na(stalled_year) ~
        CENSOR_YEAR - TIMEOUT_MARKER_OFFSET,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(marker_x))

# ============================================================
# ORDERING AND LABELS
# ============================================================

order_top <- dat %>%
  filter(!is.na(cd_start)) %>%
  arrange(cd_start) %>%
  pull(smr)

phase_seg <- phase_seg %>%
  mutate(
    smr = factor(
      smr,
      levels = rev(order_top)
    )
  )

outcome_markers <- outcome_markers %>%
  mutate(
    smr = factor(
      smr,
      levels = rev(order_top)
    )
  )

label_df <- dat %>%
  filter(smr %in% order_top) %>%
  mutate(
    smr = factor(
      smr,
      levels = rev(order_top)
    ),
    Outcome = factor(
      Outcome,
      levels = c(
        "Inactive design",
        "Stalled development"
      )
    )
  )

# ============================================================
# PLOT
# ============================================================

p <- ggplot(phase_seg, aes(y = smr)) +
  geom_segment(
    aes(
      x = start_year,
      xend = end_year,
      yend = smr,
      colour = Phase_name
    ),
    linewidth = 6,
    lineend = "butt"
  ) +
  geom_point(
    data = outcome_markers,
    aes(
      x = marker_x,
      y = smr,
      shape = Outcome,
      colour = Outcome
    ),
    size = 3.2,
    stroke = 1.1
  ) +
  geom_text(
    data = label_df,
    aes(
      x = 1984.05,
      y = smr,
      label = smr,
      colour = Outcome
    ),
    inherit.aes = FALSE,
    hjust = 1,
    size = 3.3
  ) +
  scale_colour_manual(
    values = c(
      phase_cols,
      outcome_cols
    ),
    breaks = phase_levels,
    name = "Phase"
  ) +
  scale_shape_manual(
    values = c(
      "Inactive design"     = 4,
      "Stalled development" = 2
    ),
    breaks = c(
      "Inactive design",
      "Stalled development"
    ),
    name = "Outcome"
  ) +
  scale_y_discrete(labels = NULL) +
  scale_x_continuous(
    breaks = c(1985, 1995, 2005, 2015, 2025),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  coord_cartesian(
    xlim = c(1984.6, 2025.8),
    clip = "off"
  ) +
  labs(
    x = "Year",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks = element_blank(),
    axis.text.x = element_text(colour = "grey35"),
    axis.text.y = element_blank(),
    axis.title.x = element_text(colour = "black"),
    legend.position = "right",
    legend.title = element_text(colour = "black"),
    legend.text = element_text(colour = "black"),
    plot.margin = margin(5.5, 5.5, 5.5, 95)
  ) +
  guides(
    colour = guide_legend(order = 1),
    shape = guide_legend(
      order = 2,
      override.aes = list(
        colour = c("#D62728", "grey35")
      )
    )
  )

print(p)

# ============================================================
# SAVE SUPPLEMENTARY FIGURE 2
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
  bg = "white"
)