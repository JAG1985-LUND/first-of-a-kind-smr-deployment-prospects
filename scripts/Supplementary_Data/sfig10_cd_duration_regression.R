# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Supplementary Fig. 10: conceptual design duration versus
# start year of the conceptual design phase.
#
# Required input:
# data/Supplementary_Data/sfig10_source_data_cd_duration_regression.xlsx
#
# Outputs:
# outputs/Supplementary_Data/sfig10_cd_duration_regression.png
# outputs/Supplementary_Data/sfig10_cd_duration_regression.pdf

rm(list = ls())
graphics.off()

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(grid)
})

# ============================================================
# SUPPLEMENTARY FIGURE 10 — CD DURATION REGRESSION
# Conceptual design duration versus conceptual-design start year
# Empirical, Baseline and Accelerated samples
# ============================================================

# --------------------------
# PATHS — GITHUB RUN
# --------------------------

DATA_DIR <- file.path(
  "data",
  "Supplementary_Data"
)

FILE_IN <- file.path(
  DATA_DIR,
  "sfig10_source_data_cd_duration_regression.xlsx"
)

OUT_DIR <- file.path(
  "outputs",
  "Supplementary_Data"
)

OUT_BASE <- "sfig10_cd_duration_regression"

if (!file.exists(FILE_IN)) {
  stop("Input file not found: ", FILE_IN)
}

dir.create(
  OUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

# --------------------------
# SETTINGS
# --------------------------

SAVE_W_MM <- 180
SAVE_H_MM <- 110
SAVE_DPI  <- 600

axis_line_col <- "grey35"
axis_line_lw  <- 0.3

col_emp  <- "black"
col_base <- "grey45"
col_acc  <- "forestgreen"

# --------------------------
# HELPERS
# --------------------------

clean_names_local <- function(x) {
  x <- trimws(x)
  x <- gsub("[[:space:][:punct:]]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

pick_col <- function(nms, exact = character(), patterns = character(), exclude = character()) {
  
  exact_hits <- intersect(exact, nms)
  
  if (length(exact_hits) > 0) {
    return(exact_hits[1])
  }
  
  for (p in patterns) {
    hits <- grep(p, nms, ignore.case = TRUE, value = TRUE)
    
    if (length(exclude) > 0) {
      for (ex in exclude) {
        hits <- hits[!grepl(ex, hits, ignore.case = TRUE)]
      }
    }
    
    if (length(hits) > 0) {
      return(hits[1])
    }
  }
  
  NA_character_
}

# --------------------------
# LOAD DATA
# --------------------------

raw <- read_excel(FILE_IN, sheet = 1)
names(raw) <- clean_names_local(names(raw))

cd_col <- pick_col(
  names(raw),
  exact = c("cd_start", "cd_start_year"),
  patterns = c("cd.*start", "start.*cd")
)

emp_col <- pick_col(
  names(raw),
  exact = c(
    "full_sample",
    "full_sampl",
    "empirical_sample",
    "empirical_duration",
    "cd_duration",
    "cd_duration_years"
  ),
  patterns = c(
    "full.*sample",
    "full.*sampl",
    "empirical.*duration",
    "empirical.*sample",
    "cd.*duration"
  )
)

base_col <- pick_col(
  names(raw),
  exact = c(
    "projected_baseline_sample",
    "projected_baseline_s",
    "baseline_sample",
    "baseline_s",
    "baseline_duration"
  ),
  patterns = c(
    "projected.*baseline",
    "baseline"
  ),
  exclude = c("accelerated")
)

acc_col <- pick_col(
  names(raw),
  exact = c(
    "accelerated_sample_q3_cut",
    "accelerated_sample",
    "accelerated_duration"
  ),
  patterns = c("accelerated")
)

if (any(is.na(c(cd_col, emp_col, base_col, acc_col)))) {
  stop(
    "Could not identify required columns.\n",
    "cd_col: ", cd_col, "\n",
    "emp_col: ", emp_col, "\n",
    "base_col: ", base_col, "\n",
    "acc_col: ", acc_col, "\n",
    "Available columns: ", paste(names(raw), collapse = ", ")
  )
}

df <- raw %>%
  transmute(
    SMR = if ("smr" %in% names(raw)) as.character(.data$smr) else as.character(row_number()),
    CD_Start = as.numeric(.data[[cd_col]]),
    Empirical_duration = as.numeric(.data[[emp_col]]),
    Baseline_duration = as.numeric(.data[[base_col]]),
    Accelerated_duration = as.numeric(.data[[acc_col]])
  ) %>%
  filter(!is.na(CD_Start))

# --------------------------
# BUILD SAMPLES
# --------------------------

empirical_df <- df %>%
  filter(!is.na(Empirical_duration)) %>%
  transmute(
    CD_Start,
    Duration = Empirical_duration
  )

baseline_df <- df %>%
  filter(!is.na(Baseline_duration)) %>%
  transmute(
    CD_Start,
    Duration = Baseline_duration
  )

accelerated_df <- df %>%
  filter(!is.na(Accelerated_duration)) %>%
  transmute(
    CD_Start,
    Duration = Accelerated_duration
  )

n_emp  <- nrow(empirical_df)
n_base <- nrow(baseline_df)
n_acc  <- nrow(accelerated_df)

lab_emp  <- paste0("Empirical (n = ", n_emp, ")")
lab_base <- paste0("Baseline (n = ", n_base, ")")
lab_acc  <- paste0("Accelerated (n = ", n_acc, ")")

empirical_df <- empirical_df %>%
  mutate(Sample = lab_emp)

baseline_df <- baseline_df %>%
  mutate(Sample = lab_base)

accelerated_df <- accelerated_df %>%
  mutate(Sample = lab_acc)

point_df <- df %>%
  mutate(
    Point_group = case_when(
      !is.na(Accelerated_duration) ~ "Accelerated",
      is.na(Accelerated_duration) & !is.na(Baseline_duration) ~ "Baseline only",
      is.na(Accelerated_duration) & is.na(Baseline_duration) & !is.na(Empirical_duration) ~ "Empirical only",
      TRUE ~ NA_character_
    ),
    Duration = case_when(
      Point_group == "Accelerated" ~ Accelerated_duration,
      Point_group == "Baseline only" ~ Baseline_duration,
      Point_group == "Empirical only" ~ Empirical_duration,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(Point_group), !is.na(Duration))

# --------------------------
# PLOT
# --------------------------

p <- ggplot() +
  geom_smooth(
    data = empirical_df,
    aes(x = CD_Start, y = Duration, colour = Sample),
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    fill = "grey80",
    alpha = 1,
    linewidth = 0.9
  ) +
  geom_smooth(
    data = baseline_df,
    aes(x = CD_Start, y = Duration, colour = Sample),
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linewidth = 0.9
  ) +
  geom_smooth(
    data = accelerated_df,
    aes(x = CD_Start, y = Duration, colour = Sample),
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linewidth = 0.9
  ) +
  geom_point(
    data = point_df,
    aes(x = CD_Start, y = Duration, colour = Point_group),
    size = 2.3,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    name = "Sample",
    values = c(
      setNames(col_emp, lab_emp),
      setNames(col_base, lab_base),
      setNames(col_acc, lab_acc),
      "Empirical only" = col_emp,
      "Baseline only"  = "grey60",
      "Accelerated"    = col_acc
    ),
    breaks = c(lab_emp, lab_base, lab_acc)
  ) +
  scale_x_continuous(
    limits = c(1984.5, 2027),
    breaks = c(1985, 1995, 2005, 2015, 2025),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 35),
    breaks = c(5, 15, 25, 35),
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Start of CD phase",
    y = "CD-duration (years)"
  ) +
  theme_classic(base_size = 11) +
  theme(
    axis.line.x          = element_line(colour = axis_line_col, linewidth = axis_line_lw),
    axis.line.y          = element_line(colour = axis_line_col, linewidth = axis_line_lw),
    axis.ticks           = element_line(colour = axis_line_col, linewidth = axis_line_lw),
    axis.ticks.length    = unit(0.12, "cm"),
    axis.title           = element_text(size = 11),
    axis.text            = element_text(size = 11),
    legend.position      = c(0.73, 0.82),
    legend.justification = c(0, 1),
    legend.direction     = "vertical",
    legend.title         = element_text(size = 11, face = "bold"),
    legend.text          = element_text(size = 11),
    legend.key.width     = unit(1.4, "cm"),
    legend.key.height    = unit(0.5, "cm"),
    legend.margin        = margin(0, 0, 0, 0),
    legend.box.margin    = margin(0, 0, 0, 0),
    legend.background    = element_blank(),
    legend.key           = element_blank(),
    plot.margin          = margin(5.5, 45, 5.5, 5.5),
    plot.background      = element_rect(fill = "white", colour = NA),
    panel.background     = element_rect(fill = "white", colour = NA)
  ) +
  guides(
    colour = guide_legend(
      override.aes = list(
        linewidth = 1.2,
        shape = NA
      )
    )
  )

print(p)

# --------------------------
# EXPORT
# --------------------------

ggsave(
  filename = file.path(OUT_DIR, paste0(OUT_BASE, ".png")),
  plot = p,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, paste0(OUT_BASE, ".pdf")),
  plot = p,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  device = cairo_pdf,
  bg = "white"
)

p