# ============================================================
# PIPELINE EVOLUTION MONTE CARLO
# GITHUB / SUBMISSION MC25 TEST VERSION
# FAIL-GATED • LOGNORMAL EVERYWHERE
#
# Code for the manuscript:
# Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Module:
# 02_pipeline_evolution
#
# GitHub input workbook:
# data/Simulation_Inputs/pipeline_evolution_module_inputs.xlsx
#
# Required sheets:
# 1) smr_design_inputs
# 2) phase_rules
# 3) design_phase_failures
# 4) failure_rates_smrs
#
# GitHub test output folder:
# outputs/Simulation_Outputs/02_pipeline_evolution/pipeline_evolution_MC25_test_outputs/
#
# Files written:
# 1) pipeline_evolution_MC25_test.xlsx
# 2) pipeline_evolution_master_summary_sweep_MC25_test.xlsx
# 3) pipeline_evolution_bundle_MC25_test.rds
# 4) README.md
#
# Smoke-test settings:
# SIMS <- 25L
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(writexl)
  library(tidyr)
})

rm(list = ls())
graphics.off()

# -------------------- PATH HELPERS --------------------

normalise_path <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  gsub("\\\\", "/", normalizePath(x, winslash = "/", mustWork = FALSE))
}

normalise_path_vec <- function(x) {
  vapply(x, normalise_path, character(1))
}

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  
  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg[[1]])
    return(dirname(normalise_path(script_path)))
  }
  
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    ctx <- tryCatch(
      rstudioapi::getActiveDocumentContext(),
      error = function(e) NULL
    )
    
    if (!is.null(ctx) && !is.null(ctx$path) && nzchar(ctx$path)) {
      return(dirname(normalise_path(ctx$path)))
    }
  }
  
  normalise_path(getwd())
}

parent_dirs <- function(start_dir) {
  dirs <- character()
  d <- normalise_path(start_dir)
  
  repeat {
    dirs <- c(dirs, d)
    parent <- dirname(d)
    
    if (identical(parent, d)) break
    
    d <- parent
  }
  
  unique(dirs)
}

find_repo_root <- function(start_dirs) {
  start_dirs <- unique(start_dirs)
  
  for (start_dir in start_dirs) {
    for (d in parent_dirs(start_dir)) {
      candidate <- file.path(
        d,
        "data",
        "Simulation_Inputs",
        "pipeline_evolution_module.xlsx"
      )
      
      if (file.exists(candidate)) {
        return(d)
      }
    }
  }
  
  NA_character_
}

workbook_has_required_sheets <- function(path, required_sheets) {
  if (!file.exists(path)) return(FALSE)
  
  sheets <- tryCatch(
    readxl::excel_sheets(path),
    error = function(e) character()
  )
  
  all(required_sheets %in% sheets)
}

choose_input_workbook <- function(repo_root, script_dir, required_sheets) {
  candidate_name <- "pipeline_evolution_module.xlsx"
  
  candidate_paths <- character()
  candidate_modes <- character()
  
  if (!is.na(repo_root)) {
    candidate_paths <- c(
      candidate_paths,
      file.path(repo_root, "data", "Simulation_Inputs", candidate_name)
    )
    
    candidate_modes <- c(
      candidate_modes,
      "github_repo_layout"
    )
  }
  
  candidate_paths <- c(
    candidate_paths,
    file.path(getwd(), candidate_name),
    file.path(script_dir, candidate_name)
  )
  
  candidate_modes <- c(
    candidate_modes,
    "local_working_directory",
    "local_script_directory"
  )
  
  candidate_paths <- normalise_path_vec(candidate_paths)
  
  keep <- !duplicated(candidate_paths)
  candidate_paths <- candidate_paths[keep]
  candidate_modes <- candidate_modes[keep]
  
  for (i in seq_along(candidate_paths)) {
    if (workbook_has_required_sheets(candidate_paths[[i]], required_sheets)) {
      return(
        list(
          path = candidate_paths[[i]],
          mode = candidate_modes[[i]]
        )
      )
    }
  }
  
  existing <- candidate_paths[file.exists(candidate_paths)]
  
  stop(
    paste0(
      "No valid input workbook found.\n\n",
      "The script looked for:\n",
      paste0("- ", candidate_paths, collapse = "\n"),
      "\n\n",
      "A valid workbook must contain these sheets:\n",
      paste0("- ", required_sheets, collapse = "\n"),
      "\n\n",
      "Existing workbook candidates found:\n",
      ifelse(length(existing) == 0, "none", paste0(existing, collapse = "\n"))
    ),
    call. = FALSE
  )
}

# -------------------- PATHS --------------------

required_sheets <- c(
  "smr_design_inputs",
  "phase_rules",
  "design_phase_failures",
  "failure_rates_smrs"
)

SCRIPT_DIR <- get_script_dir()
REPO_ROOT  <- find_repo_root(c(getwd(), SCRIPT_DIR))

input_choice <- choose_input_workbook(
  repo_root = REPO_ROOT,
  script_dir = SCRIPT_DIR,
  required_sheets = required_sheets
)

P_WORKBOOK <- normalise_path(input_choice$path)
PATH_MODE  <- input_choice$mode

if (!is.na(REPO_ROOT) && PATH_MODE == "github_repo_layout") {
  OUTDIR <- file.path(
    REPO_ROOT,
    "outputs",
    "Simulation_Outputs",
    "02_pipeline_evolution",
    "pipeline_evolution_MC25_test_outputs"
  )
} else {
  OUTDIR <- file.path(
    dirname(P_WORKBOOK),
    "pipeline_evolution_MC25_test_outputs"
  )
}

OUTDIR <- normalise_path(OUTDIR)

if (!file.exists(P_WORKBOOK)) {
  stop("Input workbook not found: ", P_WORKBOOK, call. = FALSE)
}

if (!dir.exists(OUTDIR)) {
  dir.create(OUTDIR, recursive = TRUE)
}

# -------------------- GLOBALS --------------------

RUN_TYPE   <- "MC25_test"
CUT_YEAR   <- 2025L
NEXT_FLOOR <- 2026.0
SIMS       <- 25L
SEED       <- 42
YEARS      <- 2026:2050

set.seed(SEED)

run_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

available_sheets <- readxl::excel_sheets(P_WORKBOOK)
missing_sheets <- setdiff(required_sheets, available_sheets)

if (length(missing_sheets) > 0) {
  stop(
    "Missing required sheet(s): ",
    paste(missing_sheets, collapse = ", "),
    call. = FALSE
  )
}

# -------------------- STANDARDISERS --------------------

as_phase <- function(x) {
  stringr::str_to_lower(
    stringr::str_replace_all(
      stringr::str_trim(as.character(x)),
      "[ -]",
      "_"
    )
  )
}

as_scen <- function(x) {
  stringr::str_to_lower(stringr::str_trim(as.character(x)))
}

norm_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x)
  x <- ifelse(is.na(x), NA_character_, stringr::str_trim(x))
  x <- ifelse(tolower(x) %in% c("na", "n/a", "", "null"), NA_character_, x)
  x
}

key_ps <- function(phase, scenario) {
  paste0(phase, "__", scenario)
}

extract_year <- function(x) {
  y <- suppressWarnings(as.integer(stringr::str_extract(as.character(x), "\\d{4}")))
  if (is.na(y)) NA_integer_ else y
}

clamp01 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  pmin(1, pmax(0, x))
}

safe_q <- function(x, p) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, type = 7))
}

safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 1) return(NA_real_)
  stats::sd(x)
}

safe_min <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  min(x)
}

safe_max <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}

require_columns <- function(df, cols, sheet_name) {
  missing_cols <- setdiff(cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s) in sheet `",
      sheet_name,
      "`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
}

get_chr <- function(df, nm) {
  if (nm %in% names(df)) {
    as.character(df[[nm]])
  } else {
    rep(NA_character_, nrow(df))
  }
}

# -------------------- LOAD DESIGN INPUTS --------------------

smr_raw <- readxl::read_excel(P_WORKBOOK, sheet = "smr_design_inputs")

require_columns(
  smr_raw,
  c(
    "design",
    "phase",
    "country",
    "region",
    "oecd",
    "national_champion",
    "size",
    "cooling_type",
    "water_cooled",
    "spectrum",
    "uranium_enrichment",
    "cd_start",
    "bd_start",
    "dd_start",
    "licensing_start",
    "construction_start",
    "operational_start"
  ),
  "smr_design_inputs"
)

smr_specs <- tibble::tibble(
  design = stringr::str_trim(as.character(smr_raw$design)),
  phase  = as_phase(smr_raw$phase),
  configuration = get_chr(smr_raw, "configuration"),
  country = as.character(smr_raw$country),
  region  = as.character(smr_raw$region),
  oecd = as.logical(smr_raw$oecd),
  national_champion = as.logical(smr_raw$national_champion),
  size = suppressWarnings(as.numeric(smr_raw$size)),
  cooling_type = as.character(smr_raw$cooling_type),
  water_cooled = as.logical(smr_raw$water_cooled),
  spectrum = as.character(smr_raw$spectrum),
  fuel_binary = get_chr(smr_raw, "fuel_binary"),
  uranium_enrichment = as.character(smr_raw$uranium_enrichment),
  cd_start = suppressWarnings(as.numeric(smr_raw$cd_start)),
  bd_start = suppressWarnings(as.numeric(smr_raw$bd_start)),
  dd_start = suppressWarnings(as.numeric(smr_raw$dd_start)),
  licensing_start = suppressWarnings(as.numeric(smr_raw$licensing_start)),
  construction_start = suppressWarnings(as.numeric(smr_raw$construction_start)),
  operational_start = suppressWarnings(as.numeric(smr_raw$operational_start))
)

stopifnot(nrow(smr_specs) == dplyr::n_distinct(smr_specs$design))

# -------------------- LOAD PHASE RULES --------------------

phase_rules_raw <- readxl::read_excel(P_WORKBOOK, sheet = "phase_rules")

require_columns(
  phase_rules_raw,
  c(
    "design",
    "baseline_condition",
    "accelerated_condition",
    "phase_rule_baseline_licensing",
    "phase_rule_accelerated_licensing"
  ),
  "phase_rules"
)

phase_rules <- phase_rules_raw %>%
  dplyr::transmute(
    design = stringr::str_trim(as.character(design)),
    baseline_condition = norm_na(baseline_condition),
    accelerated_condition = norm_na(accelerated_condition),
    phase_rule_baseline_licensing = norm_na(phase_rule_baseline_licensing),
    phase_rule_accelerated_licensing = suppressWarnings(as.numeric(phase_rule_accelerated_licensing))
  ) %>%
  dplyr::mutate(
    accelerated_condition = dplyr::if_else(
      design %in% c("ACP-100", "BREST-OD-300") &
        stringr::str_detect(accelerated_condition, "^force_operational_\\d{4}$"),
      NA_character_,
      accelerated_condition
    )
  )

# -------------------- LOAD FAILURE RATES --------------------

fail_rates_raw <- readxl::read_excel(P_WORKBOOK, sheet = "failure_rates_smrs")

require_columns(
  fail_rates_raw,
  c(
    "design",
    "licensing_failure_baseline",
    "licensing_failure_accelerated",
    "construction_failure_baseline",
    "construction_failure_accelerated"
  ),
  "failure_rates_smrs"
)

fail_rates <- fail_rates_raw %>%
  dplyr::transmute(
    design = stringr::str_trim(as.character(design)),
    licensing_failure_baseline       = clamp01(licensing_failure_baseline),
    licensing_failure_accelerated    = clamp01(licensing_failure_accelerated),
    construction_failure_baseline    = clamp01(construction_failure_baseline),
    construction_failure_accelerated = clamp01(construction_failure_accelerated)
  ) %>%
  dplyr::mutate(
    lic_surv_baseline = clamp01(1 - licensing_failure_baseline),
    lic_surv_accel    = clamp01(1 - licensing_failure_accelerated),
    con_surv_baseline = clamp01(1 - construction_failure_baseline),
    con_surv_accel    = clamp01(1 - construction_failure_accelerated)
  ) %>%
  dplyr::select(
    design,
    lic_surv_baseline,
    lic_surv_accel,
    con_surv_baseline,
    con_surv_accel
  )

# -------------------- LOAD DESIGN-PHASE FAILURES --------------------

dpf_raw <- readxl::read_excel(P_WORKBOOK, sheet = "design_phase_failures")

require_columns(
  dpf_raw,
  c(
    "phase",
    "scenario",
    "failures_k",
    "total_N"
  ),
  "design_phase_failures"
)

dpf <- dpf_raw %>%
  dplyr::transmute(
    phase = as_phase(phase),
    scenario = as_scen(scenario),
    k = suppressWarnings(as.numeric(failures_k)),
    N = suppressWarnings(as.numeric(total_N))
  ) %>%
  dplyr::mutate(
    p_fail_mean = (k + 0.5) / (N + 1),
    p_surv_mean = clamp01(1 - p_fail_mean)
  )

req_phases <- c("conceptual_design", "basic_design", "detailed_design")
req_scens  <- c("baseline", "accelerated")

miss_sc <- setdiff(req_scens, unique(dpf$scenario))

if (length(miss_sc) > 0) {
  stop(
    "Missing scenarios in design_phase_failures: ",
    paste(miss_sc, collapse = ", "),
    call. = FALSE
  )
}

for (sc in req_scens) {
  missing_phase <- setdiff(
    req_phases,
    dpf %>%
      dplyr::filter(scenario == sc) %>%
      dplyr::pull(phase) %>%
      unique()
  )
  
  if (length(missing_phase) > 0) {
    stop(
      "Missing phases in design_phase_failures for ",
      sc,
      ": ",
      paste(missing_phase, collapse = ", "),
      call. = FALSE
    )
  }
}

jeffreys_survival <- split(dpf, dpf$scenario) %>%
  purrr::map(~ setNames(.x$p_surv_mean, .x$phase))

get_surv_phase <- function(phase, scenario, row) {
  phase <- as_phase(phase)
  scenario <- as_scen(scenario)
  
  if (phase %in% c("conceptual_design", "basic_design", "detailed_design")) {
    p <- jeffreys_survival[[scenario]][[phase]]
    return(clamp01(p))
  }
  
  if (phase == "licensing") {
    p <- if (scenario == "baseline") row$lic_surv_baseline else row$lic_surv_accel
    return(clamp01(p))
  }
  
  if (phase == "construction") {
    p <- if (scenario == "baseline") row$con_surv_baseline else row$con_surv_accel
    return(clamp01(p))
  }
  
  1.0
}

# -------------------- BUILD MASTER --------------------

smr_master <- smr_specs %>%
  dplyr::left_join(phase_rules, by = "design") %>%
  dplyr::left_join(fail_rates, by = "design")

miss_fail <- smr_master %>%
  dplyr::filter(
    is.na(lic_surv_baseline) |
      is.na(lic_surv_accel) |
      is.na(con_surv_baseline) |
      is.na(con_surv_accel)
  ) %>%
  dplyr::pull(design)

if (length(miss_fail) > 0) {
  stop(
    "Missing failure rates for designs: ",
    paste(unique(miss_fail), collapse = ", "),
    call. = FALSE
  )
}

# -------------------- PHASE PARAMETERS --------------------
# conceptual/basic/detailed mean/sd are arithmetic mean/sd.
# licensing/construction mean/sd are log-space meanlog/sdlog.

phase_params <- tibble::tribble(
  ~phase,              ~scenario,       ~mean,     ~sd,     ~min,  ~max,
  "conceptual_design", "baseline",       6.66,     3.46,    2.0,   15.0,
  "conceptual_design", "accelerated",    5.11,     1.95,    2.0,   10.0,
  "basic_design",      "baseline",       4.76,     3.10,    2.0,   14.0,
  "basic_design",      "accelerated",    3.39,     1.03,    2.0,   5.0,
  "detailed_design",   "baseline",       4.16,     2.09,    2.0,   9.0,
  "detailed_design",   "accelerated",    3.36,     1.15,    2.0,   5.0,
  "licensing",         "baseline",       1.46879,  0.2867,  2.0,   8.0,
  "licensing",         "accelerated",    0.8405,   0.1432,  1.0,   3.0,
  "construction",      "baseline",       1.64149,  0.2064,  3.0,   7.5,
  "construction",      "accelerated",    1.3768,   0.1242,  3.0,   5.0
) %>%
  dplyr::mutate(
    phase = as_phase(phase),
    scenario = as_scen(scenario)
  )

param_map <- phase_params %>%
  dplyr::mutate(key = key_ps(phase, scenario)) %>%
  split(.$key) %>%
  purrr::map(
    ~ list(
      mean = .x$mean[[1]],
      sd   = .x$sd[[1]],
      min  = .x$min[[1]],
      max  = .x$max[[1]]
    )
  )

get_p <- function(phase, scenario) {
  k <- key_ps(as_phase(phase), as_scen(scenario))
  
  if (!k %in% names(param_map)) {
    stop("Missing phase params for: ", k, call. = FALSE)
  }
  
  param_map[[k]]
}

# -------------------- DISTRIBUTION HELPERS --------------------

lognorm_from_mean_sd <- function(mean, sd) {
  v <- log(1 + (sd^2) / (mean^2))
  
  list(
    meanlog = log(mean) - 0.5 * v,
    sdlog = sqrt(v)
  )
}

r_lnorm_trunc_cond_total <- function(elapsed, a, b, meanlog, sdlog) {
  elapsed <- max(0, elapsed)
  lo <- max(a, elapsed)
  
  if (lo >= b) return(lo)
  
  Fa <- plnorm(lo, meanlog, sdlog)
  Fb <- plnorm(b, meanlog, sdlog)
  
  if (is.na(Fa) || is.na(Fb) || Fb <= Fa) return(lo)
  
  qlnorm(runif(1, Fa, Fb), meanlog, sdlog)
}

draw_total_duration <- function(phase, scenario, elapsed) {
  phase <- as_phase(phase)
  scenario <- as_scen(scenario)
  p <- get_p(phase, scenario)
  
  if (!is.na(elapsed) && elapsed >= p$max) return(elapsed)
  
  if (phase %in% c("licensing", "construction")) {
    return(r_lnorm_trunc_cond_total(elapsed, p$min, p$max, p$mean, p$sd))
  }
  
  lp <- lognorm_from_mean_sd(p$mean, p$sd)
  
  r_lnorm_trunc_cond_total(elapsed, p$min, p$max, lp$meanlog, lp$sdlog)
}

# -------------------- CONDITION OVERRIDES --------------------

apply_next_override <- function(next_phase, cond) {
  cond <- norm_na(cond)
  
  if (is.na(cond)) return(NA_real_)
  
  if (next_phase == "basic_design" && cond == "force_bd_start_2026") {
    return(2026.0)
  }
  
  if (next_phase == "detailed_design" && cond == "force_dd_start_2026") {
    return(2026.0)
  }
  
  if (next_phase == "construction" && grepl("^construction_start_\\d{4}$", cond)) {
    y <- extract_year(cond)
    if (!is.na(y)) return(as.numeric(y))
  }
  
  if (next_phase == "operational" && grepl("^force_operational_\\d{4}$", cond)) {
    y <- extract_year(cond)
    if (!is.na(y)) return(as.numeric(y))
  }
  
  NA_real_
}

# -------------------- CN/RU LICENSING RULE --------------------

licensing_duration_with_rules <- function(row, scenario, elapsed) {
  scenario <- as_scen(scenario)
  ctry <- as.character(row$country)
  
  base_rule <- norm_na(row$phase_rule_baseline_licensing)
  acc_rule  <- suppressWarnings(as.numeric(row$phase_rule_accelerated_licensing))
  
  is_cnru <- ctry %in% c("CN", "RU")
  
  if (scenario == "accelerated") {
    if (is_cnru || (!is.na(acc_rule) && acc_rule == 1)) {
      return(1.0)
    }
    
    return(draw_total_duration("licensing", "accelerated", elapsed))
  }
  
  if (is_cnru) {
    return(draw_total_duration("licensing", "accelerated", elapsed))
  }
  
  if (!is.na(base_rule) && as_scen(base_rule) == "accelerated") {
    return(draw_total_duration("licensing", "accelerated", elapsed))
  }
  
  draw_total_duration("licensing", "baseline", elapsed)
}

# -------------------- LICENSING FAILURE EXEMPTIONS --------------------

licensing_failure_exempt_designs <- c("BWRX-300")

# -------------------- SIMULATE ONE DESIGN TO FOAK --------------------

simulate_to_foak <- function(row, scenario) {
  scenario <- as_scen(scenario)
  
  phases <- c(
    "conceptual_design",
    "basic_design",
    "detailed_design",
    "licensing",
    "construction",
    "operational"
  )
  
  cur <- row$phase
  
  if (is.na(cur) || !cur %in% phases) {
    return(
      tibble(
        design = row$design,
        scenario = scenario,
        status = "bad_phase",
        fail_phase = NA_character_,
        cd_start = row$cd_start,
        bd_start = row$bd_start,
        dd_start = row$dd_start,
        licensing_start = row$licensing_start,
        construction_start = row$construction_start,
        operational_start = row$operational_start,
        foak_year = NA_real_
      )
    )
  }
  
  cond <- if (scenario == "baseline") {
    row$baseline_condition
  } else {
    row$accelerated_condition
  }
  
  cond <- norm_na(cond)
  
  cd_s  <- row$cd_start
  bd_s  <- row$bd_start
  dd_s  <- row$dd_start
  lic_s <- row$licensing_start
  con_s <- row$construction_start
  op_s  <- row$operational_start
  
  cd_e <- bd_e <- dd_e <- lic_e <- con_e <- NA_real_
  
  if (cur == "operational") {
    return(
      tibble(
        design = row$design,
        scenario = scenario,
        status = "already_operational",
        fail_phase = NA_character_,
        cd_start = cd_s,
        bd_start = bd_s,
        dd_start = dd_s,
        licensing_start = lic_s,
        construction_start = con_s,
        operational_start = op_s,
        foak_year = op_s
      )
    )
  }
  
  if (row$design == "CAREM" && cur == "construction") {
    p_con <- get_p("construction", scenario)
    
    total_con <- r_lnorm_trunc_cond_total(
      elapsed = 0,
      a = p_con$min,
      b = p_con$max,
      meanlog = p_con$mean,
      sdlog = p_con$sd
    )
    
    frac <- if (scenario == "accelerated") {
      runif(1, 0.40, 0.50)
    } else {
      runif(1, 0.30, 0.40)
    }
    
    remain <- (1 - frac) * total_con
    
    con_s <- 2026.0
    con_e <- 2026.0 + remain
    op_s  <- con_e
    
    ov_op <- apply_next_override("operational", cond)
    forced_op <- !is.na(ov_op)
    
    if (forced_op) {
      op_s  <- ov_op
      con_e <- op_s
    }
    
    if (!forced_op) {
      p_surv_con <- get_surv_phase("construction", scenario, row)
      
      if (runif(1) > p_surv_con) {
        return(
          tibble(
            design = row$design,
            scenario = scenario,
            status = "failed_construction",
            fail_phase = "construction",
            cd_start = cd_s,
            bd_start = bd_s,
            dd_start = dd_s,
            licensing_start = lic_s,
            construction_start = con_s,
            operational_start = NA_real_,
            foak_year = NA_real_
          )
        )
      }
    }
    
    return(
      tibble(
        design = row$design,
        scenario = scenario,
        status = "simulated",
        fail_phase = NA_character_,
        cd_start = cd_s,
        bd_start = bd_s,
        dd_start = dd_s,
        licensing_start = lic_s,
        construction_start = con_s,
        operational_start = op_s,
        foak_year = op_s
      )
    )
  }
  
  get_start <- function(phase) {
    if (phase == "conceptual_design") return(cd_s)
    if (phase == "basic_design")      return(bd_s)
    if (phase == "detailed_design")   return(dd_s)
    if (phase == "licensing")         return(lic_s)
    if (phase == "construction")      return(con_s)
    if (phase == "operational")       return(op_s)
    NA_real_
  }
  
  set_start <- function(phase, val) {
    if (phase == "basic_design")      bd_s  <<- val
    if (phase == "detailed_design")   dd_s  <<- val
    if (phase == "licensing")         lic_s <<- val
    if (phase == "construction")      con_s <<- val
    if (phase == "operational")       op_s  <<- val
  }
  
  set_end <- function(phase, val) {
    if (phase == "conceptual_design") cd_e  <<- val
    if (phase == "basic_design")      bd_e  <<- val
    if (phase == "detailed_design")   dd_e  <<- val
    if (phase == "licensing")         lic_e <<- val
    if (phase == "construction")      con_e <<- val
  }
  
  i0 <- match(cur, phases)
  
  for (i in i0:(length(phases) - 1)) {
    ph <- phases[i]
    st <- get_start(ph)
    
    if (is.na(st)) {
      st <- ifelse(ph == cur, CUT_YEAR, NEXT_FLOOR)
    }
    
    elapsed <- if (ph == cur && st <= CUT_YEAR) {
      max(0, CUT_YEAR - st + 1)
    } else {
      0
    }
    
    if (ph == "licensing") {
      tot <- licensing_duration_with_rules(row, scenario, elapsed)
      en_draw <- st + tot
      
      if (elapsed > 0 && en_draw < CUT_YEAR) {
        en_draw <- CUT_YEAR
      }
      
      lic_exempt <- row$design %in% licensing_failure_exempt_designs
      
      if (!lic_exempt) {
        p_surv <- get_surv_phase("licensing", scenario, row)
        
        if (runif(1) > p_surv) {
          return(
            tibble(
              design = row$design,
              scenario = scenario,
              status = "failed_licensing",
              fail_phase = "licensing",
              cd_start = cd_s,
              bd_start = bd_s,
              dd_start = dd_s,
              licensing_start = lic_s,
              construction_start = con_s,
              operational_start = NA_real_,
              foak_year = NA_real_
            )
          )
        }
      }
      
      nxt <- "construction"
      nxt_start <- max(NEXT_FLOOR, en_draw)
      
      ov <- apply_next_override(nxt, cond)
      
      if (!is.na(ov)) {
        nxt_start <- ov
      }
      
      lic_e <- nxt_start
      con_s <- nxt_start
      
      next
    }
    
    tot <- draw_total_duration(ph, scenario, elapsed)
    en <- st + tot
    
    if (elapsed > 0 && en < CUT_YEAR) {
      en <- CUT_YEAR
    }
    
    nxt <- phases[i + 1]
    nxt_start <- max(NEXT_FLOOR, en)
    
    ov <- apply_next_override(nxt, cond)
    forced_transition <- !is.na(ov)
    
    if (forced_transition) {
      nxt_start <- ov
      en <- nxt_start
    }
    
    set_end(ph, en)
    
    skip_failure <- forced_transition
    
    if (!skip_failure) {
      p_surv <- get_surv_phase(ph, scenario, row)
      
      if (runif(1) > p_surv) {
        return(
          tibble(
            design = row$design,
            scenario = scenario,
            status = paste0("failed_", ph),
            fail_phase = ph,
            cd_start = cd_s,
            bd_start = bd_s,
            dd_start = dd_s,
            licensing_start = lic_s,
            construction_start = con_s,
            operational_start = NA_real_,
            foak_year = NA_real_
          )
        )
      }
    }
    
    if (nxt == "operational") {
      ov2 <- apply_next_override("operational", cond)
      
      if (!is.na(ov2)) {
        nxt_start <- ov2
        en <- nxt_start
        set_end("construction", en)
      }
      
      op_s <- nxt_start
      break
    }
    
    if (is.na(get_start(nxt))) {
      set_start(nxt, nxt_start)
    }
  }
  
  tibble(
    design = row$design,
    scenario = scenario,
    status = "simulated",
    fail_phase = NA_character_,
    cd_start = cd_s,
    bd_start = bd_s,
    dd_start = dd_s,
    licensing_start = lic_s,
    construction_start = con_s,
    operational_start = op_s,
    foak_year = op_s
  )
}

# -------------------- MONTE CARLO RUNNER --------------------

design_list <- split(smr_master, smr_master$design)

run_mc <- function(scenario, sims) {
  purrr::map_dfr(seq_len(sims), function(s) {
    purrr::map_dfr(design_list, ~ simulate_to_foak(.x[1, ], scenario)) %>%
      dplyr::mutate(sim = s) %>%
      dplyr::select(
        sim,
        design,
        scenario,
        status,
        fail_phase,
        cd_start,
        bd_start,
        dd_start,
        licensing_start,
        construction_start,
        operational_start,
        foak_year
      )
  })
}

# -------------------- RUN MONTE CARLO --------------------

set.seed(SEED)

mc_baseline_pipeline_evolution <- run_mc("baseline", SIMS)
mc_accelerated_pipeline_evolution <- run_mc("accelerated", SIMS)

# -------------------- SUMMARY TABLES --------------------

summ_baseline <- mc_baseline_pipeline_evolution %>%
  dplyr::group_by(design) %>%
  dplyr::summarise(
    n_total = dplyr::n(),
    n_success = sum(!is.na(foak_year)),
    success_rate = n_success / n_total,
    P10 = safe_q(foak_year, 0.10),
    P50 = safe_q(foak_year, 0.50),
    P90 = safe_q(foak_year, 0.90),
    .groups = "drop"
  ) %>%
  dplyr::mutate(scenario = "baseline")

summ_accelerated <- mc_accelerated_pipeline_evolution %>%
  dplyr::group_by(design) %>%
  dplyr::summarise(
    n_total = dplyr::n(),
    n_success = sum(!is.na(foak_year)),
    success_rate = n_success / n_total,
    P10 = safe_q(foak_year, 0.10),
    P50 = safe_q(foak_year, 0.50),
    P90 = safe_q(foak_year, 0.90),
    .groups = "drop"
  ) %>%
  dplyr::mutate(scenario = "accelerated")

summary_quantiles <- dplyr::bind_rows(
  summ_baseline,
  summ_accelerated
) %>%
  dplyr::select(
    design,
    scenario,
    n_total,
    n_success,
    success_rate,
    P10,
    P50,
    P90
  ) %>%
  dplyr::arrange(scenario, design)

make_descriptives <- function(df, scenario_name) {
  df %>%
    dplyr::group_by(design) %>%
    dplyr::summarise(
      n    = sum(is.finite(foak_year)),
      mean = safe_mean(foak_year),
      sd   = safe_sd(foak_year),
      min  = safe_min(foak_year),
      q1   = safe_q(foak_year, 0.25),
      p10  = safe_q(foak_year, 0.10),
      p50  = safe_q(foak_year, 0.50),
      q3   = safe_q(foak_year, 0.75),
      p90  = safe_q(foak_year, 0.90),
      max  = safe_max(foak_year),
      .groups = "drop"
    ) %>%
    dplyr::mutate(scenario = scenario_name) %>%
    dplyr::select(
      design,
      scenario,
      n,
      mean,
      sd,
      min,
      q1,
      p10,
      p50,
      q3,
      p90,
      max
    )
}

summary_descriptives <- dplyr::bind_rows(
  make_descriptives(mc_baseline_pipeline_evolution, "baseline"),
  make_descriptives(mc_accelerated_pipeline_evolution, "accelerated")
) %>%
  dplyr::arrange(scenario, design)

# -------------------- RUN METADATA --------------------

run_metadata <- tibble::tibble(
  item = c(
    "script_name",
    "module",
    "run_type",
    "path_mode",
    "repo_root",
    "script_directory",
    "run_timestamp",
    "seed",
    "sims",
    "cut_year",
    "next_floor",
    "working_directory",
    "input_workbook",
    "input_sheet_designs",
    "input_sheet_rules",
    "input_sheet_design_phase_failures",
    "input_sheet_failure_rates",
    "output_directory",
    "model_type",
    "r_version",
    "platform"
  ),
  value = c(
    "pipeline_evolution_MC25_test_GitHub.R",
    "02_pipeline_evolution",
    RUN_TYPE,
    PATH_MODE,
    ifelse(is.na(REPO_ROOT), NA_character_, REPO_ROOT),
    SCRIPT_DIR,
    run_timestamp,
    as.character(SEED),
    as.character(SIMS),
    as.character(CUT_YEAR),
    as.character(NEXT_FLOOR),
    normalise_path(getwd()),
    P_WORKBOOK,
    "smr_design_inputs",
    "phase_rules",
    "design_phase_failures",
    "failure_rates_smrs",
    OUTDIR,
    "FAILGATED_LOGNORMAL_CURRENT_PIPELINE",
    paste(R.version$major, R.version$minor, sep = "."),
    R.version$platform
  )
)

# -------------------- EXPORT MAIN EXCEL OUTPUT --------------------

xlsx_path <- file.path(
  OUTDIR,
  "pipeline_evolution_MC25_test.xlsx"
)

xlsx_contents <- list(
  baseline_draws       = mc_baseline_pipeline_evolution,
  accelerated_draws    = mc_accelerated_pipeline_evolution,
  summary_quantiles    = summary_quantiles,
  summary_descriptives = summary_descriptives,
  run_metadata         = run_metadata
)

writexl::write_xlsx(xlsx_contents, path = xlsx_path)

# -------------------- POST-RUN SUMMARY SWEEP --------------------

mc_baseline <- mc_baseline_pipeline_evolution %>%
  dplyr::left_join(smr_specs, by = "design")

mc_accelerated <- mc_accelerated_pipeline_evolution %>%
  dplyr::left_join(smr_specs, by = "design")

design_summary <- summary_quantiles

build_ts <- function(mc_obj, grouping_vars = NULL) {
  df <- mc_obj %>%
    dplyr::mutate(
      foak_year_int = floor(foak_year),
      success = !is.na(foak_year_int)
    )
  
  results <- lapply(YEARS, function(y) {
    tmp <- df %>%
      dplyr::mutate(active_by_year = success & foak_year_int <= y)
    
    if (!is.null(grouping_vars)) {
      tmp <- tmp %>%
        dplyr::group_by(sim, dplyr::across(dplyr::all_of(grouping_vars))) %>%
        dplyr::summarise(
          total = sum(active_by_year, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      tmp <- tmp %>%
        dplyr::group_by(sim) %>%
        dplyr::summarise(
          total = sum(active_by_year, na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    tmp$year <- y
    tmp
  })
  
  ts_full <- dplyr::bind_rows(results)
  
  if (!is.null(grouping_vars)) {
    ts_full %>%
      dplyr::group_by(year, dplyr::across(dplyr::all_of(grouping_vars))) %>%
      dplyr::summarise(
        P10  = safe_q(total, 0.10),
        P50  = safe_q(total, 0.50),
        P90  = safe_q(total, 0.90),
        mean = safe_mean(total),
        .groups = "drop"
      )
  } else {
    ts_full %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(
        P10  = safe_q(total, 0.10),
        P50  = safe_q(total, 0.50),
        P90  = safe_q(total, 0.90),
        mean = safe_mean(total),
        .groups = "drop"
      )
  }
}

compute_2050 <- function(mc_obj, grouping_vars = NULL) {
  df <- mc_obj %>%
    dplyr::mutate(success_2050 = !is.na(foak_year) & foak_year <= 2050)
  
  if (!is.null(grouping_vars)) {
    df <- df %>%
      dplyr::group_by(sim, dplyr::across(dplyr::all_of(grouping_vars))) %>%
      dplyr::summarise(
        total = sum(success_2050, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    df <- df %>%
      dplyr::group_by(sim) %>%
      dplyr::summarise(
        total = sum(success_2050, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  if (!is.null(grouping_vars)) {
    df %>%
      dplyr::group_by(dplyr::across(-c(sim, total))) %>%
      dplyr::summarise(
        P10  = safe_q(total, 0.10),
        P50  = safe_q(total, 0.50),
        P90  = safe_q(total, 0.90),
        mean = safe_mean(total),
        .groups = "drop"
      )
  } else {
    df %>%
      dplyr::summarise(
        P10  = safe_q(total, 0.10),
        P50  = safe_q(total, 0.50),
        P90  = safe_q(total, 0.90),
        mean = safe_mean(total)
      )
  }
}

baseline_global     <- build_ts(mc_baseline)
accelerated_global  <- build_ts(mc_accelerated)

baseline_oecd       <- build_ts(mc_baseline, "oecd")
accelerated_oecd    <- build_ts(mc_accelerated, "oecd")

baseline_water      <- build_ts(mc_baseline, "water_cooled")
accelerated_water   <- build_ts(mc_accelerated, "water_cooled")

baseline_cooling    <- build_ts(mc_baseline, "cooling_type")
accelerated_cooling <- build_ts(mc_accelerated, "cooling_type")

baseline_region     <- build_ts(mc_baseline, "region")
accelerated_region  <- build_ts(mc_accelerated, "region")

baseline_2050_global    <- compute_2050(mc_baseline)
accelerated_2050_global <- compute_2050(mc_accelerated)

baseline_2050_oecd      <- compute_2050(mc_baseline, "oecd")
accelerated_2050_oecd   <- compute_2050(mc_accelerated, "oecd")

baseline_2050_region    <- compute_2050(mc_baseline, "region")
accelerated_2050_region <- compute_2050(mc_accelerated, "region")

master_sweep_path <- file.path(
  OUTDIR,
  "pipeline_evolution_master_summary_sweep_MC25_test.xlsx"
)

writexl::write_xlsx(
  list(
    design_level_summary = design_summary,
    baseline_global = baseline_global,
    accelerated_global = accelerated_global,
    baseline_oecd = baseline_oecd,
    accelerated_oecd = accelerated_oecd,
    baseline_water = baseline_water,
    accelerated_water = accelerated_water,
    baseline_cooling = baseline_cooling,
    accelerated_cooling = accelerated_cooling,
    baseline_region = baseline_region,
    accelerated_region = accelerated_region,
    baseline_2050_global = baseline_2050_global,
    accelerated_2050_global = accelerated_2050_global,
    baseline_2050_oecd = baseline_2050_oecd,
    accelerated_2050_oecd = accelerated_2050_oecd,
    baseline_2050_region = baseline_2050_region,
    accelerated_2050_region = accelerated_2050_region
  ),
  master_sweep_path
)

# -------------------- EXPORT BUNDLE RDS --------------------

model_bundle <- list(
  mc_baseline_pipeline_evolution = mc_baseline_pipeline_evolution,
  mc_accelerated_pipeline_evolution = mc_accelerated_pipeline_evolution,
  summary_quantiles = summary_quantiles,
  summary_descriptives = summary_descriptives,
  smr_specs = smr_specs,
  phase_rules = phase_rules,
  fail_rates = fail_rates,
  dpf = dpf,
  smr_master = smr_master,
  phase_params = phase_params,
  param_map = param_map,
  design_list = design_list,
  baseline_global = baseline_global,
  accelerated_global = accelerated_global,
  baseline_oecd = baseline_oecd,
  accelerated_oecd = accelerated_oecd,
  baseline_water = baseline_water,
  accelerated_water = accelerated_water,
  baseline_cooling = baseline_cooling,
  accelerated_cooling = accelerated_cooling,
  baseline_region = baseline_region,
  accelerated_region = accelerated_region,
  baseline_2050_global = baseline_2050_global,
  accelerated_2050_global = accelerated_2050_global,
  baseline_2050_oecd = baseline_2050_oecd,
  accelerated_2050_oecd = accelerated_2050_oecd,
  baseline_2050_region = baseline_2050_region,
  accelerated_2050_region = accelerated_2050_region,
  run_metadata = run_metadata
)

bundle_path <- file.path(
  OUTDIR,
  "pipeline_evolution_bundle_MC25_test.rds"
)

saveRDS(model_bundle, file = bundle_path)

# -------------------- README --------------------

readme_path <- file.path(OUTDIR, "README.md")

readme_lines <- c(
  "# Pipeline evolution Monte Carlo MC25 test outputs",
  "",
  "This folder contains GitHub/submission smoke-test outputs from the fail-gated pipeline evolution Monte Carlo module.",
  "",
  "The test uses 25 simulations per design and is intended only to validate script logic, repository paths, workbook loading and output naming before the final 10,000-simulation version.",
  "",
  "## Input workbook",
  "",
  paste0("- `", P_WORKBOOK, "`"),
  "",
  "## Input sheets",
  "",
  "- `smr_design_inputs`",
  "- `phase_rules`",
  "- `design_phase_failures`",
  "- `failure_rates_smrs`",
  "",
  "## Output files",
  "",
  "- `pipeline_evolution_MC25_test.xlsx`: raw baseline and accelerated draws plus design-level summaries.",
  "- `pipeline_evolution_master_summary_sweep_MC25_test.xlsx`: cumulative FOAK summaries by year and grouping, plus 2050 summaries.",
  "- `pipeline_evolution_bundle_MC25_test.rds`: compact R bundle containing outputs and model objects.",
  "- `README.md`: run description and metadata.",
  "",
  "## Run settings",
  "",
  paste0("- Module: 02_pipeline_evolution"),
  paste0("- Model type: FAILGATED_LOGNORMAL_CURRENT_PIPELINE"),
  paste0("- Run type: ", RUN_TYPE),
  paste0("- Path mode: ", PATH_MODE),
  paste0("- Simulations per design: ", SIMS),
  paste0("- Seed: ", SEED),
  paste0("- Cut year: ", CUT_YEAR),
  paste0("- Next phase floor: ", NEXT_FLOOR),
  paste0("- Run timestamp: ", run_timestamp)
)

writeLines(readme_lines, readme_path)

# -------------------- CONSOLE OUTPUT --------------------

cat("\n==================== PIPELINE EVOLUTION MC25 TEST COMPLETE ====================\n")
cat("Module: 02_pipeline_evolution\n")
cat("Run type:", RUN_TYPE, "\n")
cat("Path mode:", PATH_MODE, "\n")
cat("Input workbook:\n", P_WORKBOOK, "\n")
cat("Baseline rows:", nrow(mc_baseline_pipeline_evolution), "\n")
cat("Accelerated rows:", nrow(mc_accelerated_pipeline_evolution), "\n")
cat("Number of designs:", dplyr::n_distinct(smr_master$design), "\n")
cat("Simulations per design:", SIMS, "\n")
cat("Output directory:\n", OUTDIR, "\n")
cat("Main Excel saved:\n", xlsx_path, "\n")
cat("Master summary sweep saved:\n", master_sweep_path, "\n")
cat("Bundle RDS saved:\n", bundle_path, "\n")
cat("README saved:\n", readme_path, "\n")
cat("================================================================================\n\n")

print(xlsx_path)
print(master_sweep_path)
print(bundle_path)
print(readme_path)
