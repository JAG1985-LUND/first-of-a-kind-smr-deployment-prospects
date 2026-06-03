# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Extended Data Fig. 3: vendor-announced FOAK target years and
# modelled FOAK deployment years mapped against technology readiness levels.
#
# Required input:
# data/Extended_Data/edfig03_source_data_vendors_trl.xlsx
#
# Outputs:
# outputs/Extended_Data/Figure_03/edfig03_vendors_trl.png
# outputs/Extended_Data/Figure_03/edfig03_vendors_trl.pdf
# outputs/Extended_Data/Figure_03/edfig03_vendors_trl.rds

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(cowplot)
  library(grid)
})

rm(list = ls())
graphics.off()

# ============================================================
# EXTENDED DATA FIGURE 3
# VENDOR AMBITION GAP MAPPED AGAINST TECHNOLOGY READINESS LEVEL
# ============================================================

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

SOURCE_FILE <- file.path(
  "data",
  "Extended_Data",
  "edfig03_source_data_vendors_trl.xlsx"
)

find_repo_root <- function(start_dir = getwd(), target_file = SOURCE_FILE) {
  current <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  
  repeat {
    if (file.exists(file.path(current, target_file))) {
      return(current)
    }
    
    parent <- dirname(current)
    
    if (identical(parent, current)) {
      stop(
        "Could not locate repository root.\n",
        "Expected to find: ", target_file, "\n",
        "Start directory was: ",
        normalizePath(start_dir, winslash = "/", mustWork = TRUE),
        "\n\nRun this script from inside the GitHub repository, or set the working directory to the repository root."
      )
    }
    
    current <- parent
  }
}

ROOT_DIR <- find_repo_root()

FILE_IN <- file.path(ROOT_DIR, SOURCE_FILE)
OUT_DIR <- file.path(ROOT_DIR, "outputs", "Extended_Data", "Figure_03")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(FILE_IN)) {
  stop("Input file not found: ", FILE_IN)
}

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_IN <- 10.9
SAVE_H_IN <- 5.35
SAVE_DPI  <- 600

FILE_STUB <- "edfig03_vendors_trl"

# ------------------------------------------------------------
# TYPOGRAPHY
# ------------------------------------------------------------

BASE_SIZE         <- 13
AXIS_TEXT_SIZE    <- 10.0
AXIS_TITLE_SIZE   <- 11.0
LEGEND_TEXT_SIZE  <- 10.0
LEGEND_TITLE_SIZE <- 10.6
PANEL_TAG_SIZE    <- 12

# ------------------------------------------------------------
# COLOURS
# ------------------------------------------------------------

phase_cols <- c(
  "Conceptual design" = "#FDE0D2",
  "Basic design"      = "#F4A582",
  "Detailed design"   = "#D6604D",
  "Licensing"         = "#D1E5F0",
  "Construction"      = "#92C5DE"
)

phase_order <- c(
  "Conceptual design",
  "Basic design",
  "Detailed design",
  "Licensing",
  "Construction"
)

COL_VENDOR <- "black"
COL_BASE   <- "grey40"
COL_ACC    <- "#228B22"

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

normalise_phase <- function(x) {
  key <- tolower(trimws(as.character(x)))
  key <- gsub("[[:space:]-]+", "_", key)
  
  dplyr::case_when(
    grepl("conceptual", key) ~ "Conceptual design",
    grepl("basic", key)      ~ "Basic design",
    grepl("detailed", key)   ~ "Detailed design",
    grepl("licens", key)     ~ "Licensing",
    grepl("construct", key)  ~ "Construction",
    TRUE ~ as.character(x)
  )
}

# ------------------------------------------------------------
# THEME
# ------------------------------------------------------------

theme_base <- theme_classic(base_size = BASE_SIZE) +
  theme(
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
    axis.text.x = element_text(
      size = AXIS_TEXT_SIZE,
      colour = "grey20"
    ),
    axis.text.y = element_text(
      size = AXIS_TEXT_SIZE,
      colour = "grey20"
    ),
    axis.title.x = element_text(
      size = AXIS_TITLE_SIZE,
      colour = "grey10",
      margin = margin(t = 3)
    ),
    axis.title.y = element_text(
      size = AXIS_TITLE_SIZE,
      colour = "grey10",
      margin = margin(r = 4)
    ),
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    legend.background = element_rect(
      fill = "white",
      colour = NA
    ),
    legend.box.background = element_rect(
      fill = "white",
      colour = NA
    ),
    plot.margin = margin(3, 2, 2, 2)
  )

# ============================================================
# READ DATA
# ============================================================

kang_sample_32 <- readxl::read_excel(FILE_IN)

names(kang_sample_32) <- clean_names_local(names(kang_sample_32))

required_cols <- c(
  "phase",
  "trl",
  "vendor_target",
  "baseline_median",
  "accelerated_median"
)

missing_cols <- required_cols[!required_cols %in% names(kang_sample_32)]

if (length(missing_cols) > 0) {
  stop(
    "Missing required columns in Extended Data Fig. 3 source workbook:\n",
    paste(missing_cols, collapse = "\n"),
    "\n\nAvailable columns are:\n",
    paste(names(kang_sample_32), collapse = "\n")
  )
}

kang_sample_32 <- kang_sample_32 %>%
  mutate(
    Phase = normalise_phase(phase),
    TRL = as.numeric(trl),
    vendor_target = as.numeric(vendor_target),
    baseline_median = as.numeric(baseline_median),
    accelerated_median = as.numeric(accelerated_median)
  ) %>%
  filter(
    !is.na(TRL),
    !is.na(Phase),
    Phase %in% phase_order
  ) %>%
  mutate(
    Phase = factor(
      Phase,
      levels = phase_order
    )
  )

# ============================================================
# AXIS LIMITS AND SUMMARY VALUES
# ============================================================

x_min_BC <- 2026
x_max_BC <- 2051.2
x_breaks_BC <- c(2030, 2035, 2040, 2045, 2050)

x_min_A <- floor(
  min(kang_sample_32$vendor_target, na.rm = TRUE)
)

x_max_A <- 2036.6

x_breaks_A <- seq(
  x_min_A,
  2036,
  by = 2
)

med_vendor <- median(
  kang_sample_32$vendor_target,
  na.rm = TRUE
)

med_base <- median(
  kang_sample_32$baseline_median,
  na.rm = TRUE
)

med_acc <- median(
  kang_sample_32$accelerated_median,
  na.rm = TRUE
)

q1_vendor <- 2029.95
q3_vendor <- 2033.25

jit <- position_jitter(
  width = 0.10,
  height = 0.05,
  seed = 42
)

# ============================================================
# PANEL A — VENDOR TARGET YEAR VS TRL
# ============================================================

p_a <- ggplot(
  filter(kang_sample_32, !is.na(vendor_target)),
  aes(
    x = vendor_target,
    y = TRL,
    fill = Phase
  )
) +
  geom_vline(
    xintercept = q1_vendor,
    linetype = "solid",
    linewidth = 0.35,
    colour = COL_VENDOR,
    alpha = 0.9
  ) +
  geom_vline(
    xintercept = q3_vendor,
    linetype = "solid",
    linewidth = 0.35,
    colour = COL_VENDOR,
    alpha = 0.9
  ) +
  geom_vline(
    xintercept = med_vendor,
    linetype = "dashed",
    linewidth = 0.7,
    colour = COL_VENDOR
  ) +
  geom_point(
    shape = 21,
    size = 3.7,
    stroke = 0.3,
    colour = "grey30",
    alpha = 0.95,
    position = jit
  ) +
  scale_fill_manual(
    values = phase_cols,
    breaks = phase_order,
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = 1:6,
    limits = c(0.92, 6.18),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(x_min_A, x_max_A),
    breaks = x_breaks_A,
    expand = expansion(mult = c(0.005, 0.005))
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    x = "Vendor target year",
    y = "Technology readiness level (TRL)"
  ) +
  theme_base +
  theme(
    legend.position = "none"
  )

p_a <- ggdraw(p_a) +
  draw_plot_label(
    label = "a",
    x = 0.01,
    y = 0.99,
    hjust = 0,
    vjust = 1,
    size = PANEL_TAG_SIZE,
    fontface = "bold"
  )

# ============================================================
# PANEL B — BASELINE P50 FOAK DEPLOYMENT YEAR VS TRL
# ============================================================

p_b <- ggplot(
  filter(kang_sample_32, !is.na(baseline_median)),
  aes(
    x = baseline_median,
    y = TRL,
    fill = Phase
  )
) +
  geom_vline(
    xintercept = med_vendor,
    linetype = "dashed",
    linewidth = 0.7,
    colour = COL_VENDOR
  ) +
  geom_vline(
    xintercept = med_base,
    linetype = "dashed",
    linewidth = 0.7,
    colour = COL_BASE
  ) +
  geom_point(
    shape = 21,
    size = 3.7,
    stroke = 0.3,
    colour = "grey30",
    alpha = 0.95,
    position = jit
  ) +
  scale_fill_manual(
    values = phase_cols,
    breaks = phase_order,
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = 1:6,
    limits = c(0.92, 6.18),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(x_min_BC, x_max_BC),
    breaks = x_breaks_BC,
    labels = c("2030", "2035", "2040", "2045", "2050"),
    expand = expansion(add = c(0.10, 0.65))
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    x = "Baseline P50 FOAK deployment",
    y = "Technology readiness level (TRL)"
  ) +
  theme_base +
  theme(
    legend.position = "none"
  )

p_b <- ggdraw(p_b) +
  draw_plot_label(
    label = "b",
    x = 0.01,
    y = 0.99,
    hjust = 0,
    vjust = 1,
    size = PANEL_TAG_SIZE,
    fontface = "bold"
  )

# ============================================================
# PANEL C — ACCELERATED P50 FOAK DEPLOYMENT YEAR VS TRL
# ============================================================

p_c <- ggplot(
  filter(kang_sample_32, !is.na(accelerated_median)),
  aes(
    x = accelerated_median,
    y = TRL,
    fill = Phase
  )
) +
  geom_vline(
    xintercept = med_vendor,
    linetype = "dashed",
    linewidth = 0.7,
    colour = COL_VENDOR
  ) +
  geom_vline(
    xintercept = med_acc,
    linetype = "dashed",
    linewidth = 0.7,
    colour = COL_ACC
  ) +
  geom_point(
    shape = 21,
    size = 3.7,
    stroke = 0.3,
    colour = "grey30",
    alpha = 0.95,
    position = jit
  ) +
  scale_fill_manual(
    values = phase_cols,
    breaks = phase_order,
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = 1:6,
    limits = c(0.92, 6.18),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(x_min_BC, x_max_BC),
    breaks = x_breaks_BC,
    labels = c("2030", "2035", "2040", "2045", "2050"),
    expand = expansion(add = c(0.10, 0.65))
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    x = "Accelerated P50 FOAK deployment",
    y = "Technology readiness level (TRL)"
  ) +
  theme_base +
  theme(
    legend.position = "none"
  )

p_c <- ggdraw(p_c) +
  draw_plot_label(
    label = "c",
    x = 0.01,
    y = 0.99,
    hjust = 0,
    vjust = 1,
    size = PANEL_TAG_SIZE,
    fontface = "bold"
  )

# ============================================================
# SHARED LEGEND
# ============================================================

legend_plot <- ggplot(
  kang_sample_32,
  aes(
    x = vendor_target,
    y = TRL,
    fill = Phase
  )
) +
  geom_point(
    shape = 21,
    size = 4.0,
    stroke = 0.3,
    colour = "grey30"
  ) +
  scale_fill_manual(
    values = phase_cols,
    breaks = phase_order,
    drop = FALSE
  ) +
  guides(
    fill = guide_legend(
      title = "Phase",
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        shape = 21,
        size = 4.2,
        alpha = 1,
        colour = "grey30"
      )
    )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.title = element_text(
      size = LEGEND_TITLE_SIZE
    ),
    legend.text = element_text(
      size = LEGEND_TEXT_SIZE
    ),
    legend.key.width = unit(0.88, "cm"),
    legend.key.height = unit(0.44, "cm"),
    legend.spacing.x = unit(0.14, "cm"),
    legend.spacing.y = unit(0.02, "cm"),
    legend.box.spacing = unit(0.03, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    legend.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

legend_shared <- cowplot::get_legend(legend_plot)

# ============================================================
# COMBINE PANELS
# ============================================================

aligned_panels <- align_plots(
  p_a,
  p_b,
  p_c,
  align = "v",
  axis = "l"
)

panel_row <- plot_grid(
  aligned_panels[[1]],
  aligned_panels[[2]],
  aligned_panels[[3]],
  nrow = 1,
  rel_widths = c(1, 1, 1),
  greedy = TRUE
)

final_plot <- plot_grid(
  panel_row,
  legend_shared,
  ncol = 1,
  rel_heights = c(1, 0.10)
)

print(final_plot)

# ============================================================
# SAVE EXTENDED DATA FIGURE 3
# ============================================================

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".png")),
  plot = final_plot,
  width = SAVE_W_IN,
  height = SAVE_H_IN,
  units = "in",
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".pdf")),
  plot = final_plot,
  width = SAVE_W_IN,
  height = SAVE_H_IN,
  units = "in",
  bg = "white",
  useDingbats = FALSE
)

saveRDS(
  final_plot,
  file = file.path(OUT_DIR, paste0(FILE_STUB, ".rds"))
)

final_plot