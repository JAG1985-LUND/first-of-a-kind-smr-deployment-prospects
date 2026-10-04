# ============================================================
# INTEGRATED FOAK COMPETITION SENSITIVITY
# ACTIVE PIPELINE + POST-2025 DESIGN ADDITIONS
# FULL 1,000-RUN FOAK-PROLIFERATION SENSITIVITY
# FAIL-GATED • LOGNORMAL EVERYWHERE
#
# Code for the manuscript:
# Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Module:
# 01_project_progression
#
# GitHub input workbook location:
# data/Simulation_Inputs/
#
# Accepted input workbook names:
# 1) pipeline_evolution_module.xlsx
# 2) project_progression_inputs.xlsx
#
# Required workbook sheets:
# 1) smr_design_inputs
# 2) phase_rules
# 3) design_phase_failures
# 4) failure_rates_smrs
#
# Optional workbook sheets:
# - README
#
# GitHub output folder:
# outputs/Simulation_Outputs/01_project_progression/project_progression_output_final/
#
# Files written:
# 1) project_progression_output_final.xlsx
# 2) project_progression_master_summary_sweep_final.xlsx
# 3) project_progression_2050_snapshot_final.xlsx
# 4) project_progression_output_bundle_final.rds
# 5) README.md

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
      if (
        dir.exists(file.path(d, "data", "Simulation_Inputs")) &&
        dir.exists(file.path(d, "simulation_codes"))
      ) {
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
  candidate_names <- c(
    "pipeline_evolution_module.xlsx",
    "project_progression_inputs.xlsx"
  )
  
  candidate_paths <- character()
  candidate_modes <- character()
  
  if (!is.na(repo_root)) {
    candidate_paths <- c(
      candidate_paths,
      file.path(repo_root, "data", "Simulation_Inputs", candidate_names)
    )
    
    candidate_modes <- c(
      candidate_modes,
      rep("github_repo_layout", length(candidate_names))
    )
  }
  
  candidate_paths <- c(
    candidate_paths,
    file.path(getwd(), candidate_names),
    file.path(script_dir, candidate_names)
  )
  
  candidate_modes <- c(
    candidate_modes,
    rep("local_working_directory", length(candidate_names)),
    rep("local_script_directory", length(candidate_names))
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

normalise_path_vec <- function(x) {
  vapply(x, normalise_path, character(1))
}

# -------------------- PATHS --------------------

required_sheets <- c(
  "smr_design_inputs",
  "phase_rules",
  "design_phase_failures",
  "failure_rates_smrs"
)

# Known local input workbook used by the original model.
# Hard-coded deliberately for this validation build to avoid repository-path discovery.
P_WORKBOOK <- "C:/Users/jo3151go/OneDrive/FOAK SMRs/pipeline_evolution_module_inputs.xlsx"
PATH_MODE  <- "local_hardcoded"

if (!file.exists(P_WORKBOOK)) {
  stop("Main input workbook not found: ", P_WORKBOOK, call. = FALSE)
}

OUTDIR <- "C:/Users/jo3151go/OneDrive/FOAK SMRs"

P_WORKBOOK <- normalise_path(P_WORKBOOK)
OUTDIR     <- normalise_path(OUTDIR)

if (!dir.exists(OUTDIR)) {
  dir.create(OUTDIR, recursive = TRUE)
}

# -------------------- GLOBALS --------------------

RUN_TYPE   <- "foak_competition_validation"
CUT_YEAR   <- 2025L
NEXT_FLOOR <- 2026.0
SIMS       <- 1000L
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

# -------------------- LOAD DESIGN INPUTS --------------------

smr_specs <- readxl::read_excel(P_WORKBOOK, sheet = "smr_design_inputs") %>%
  dplyr::transmute(
    design = stringr::str_trim(as.character(design)),
    phase  = as_phase(phase),
    country = as.character(country),
    region  = as.character(region),
    oecd = as.logical(oecd),
    national_champion = as.logical(national_champion),
    size = suppressWarnings(as.numeric(size)),
    cooling_type = as.character(cooling_type),
    water_cooled = as.logical(water_cooled),
    spectrum = as.character(spectrum),
    uranium_enrichment = as.character(uranium_enrichment),
    cd_start = suppressWarnings(as.numeric(cd_start)),
    bd_start = suppressWarnings(as.numeric(bd_start)),
    dd_start = suppressWarnings(as.numeric(dd_start)),
    licensing_start = suppressWarnings(as.numeric(licensing_start)),
    construction_start = suppressWarnings(as.numeric(construction_start)),
    operational_start = suppressWarnings(as.numeric(operational_start))
  )

stopifnot(nrow(smr_specs) == dplyr::n_distinct(smr_specs$design))

# -------------------- LOAD PHASE RULES --------------------

phase_rules <- readxl::read_excel(P_WORKBOOK, sheet = "phase_rules") %>%
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

fail_rates <- readxl::read_excel(P_WORKBOOK, sheet = "failure_rates_smrs") %>%
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

dpf <- readxl::read_excel(P_WORKBOOK, sheet = "design_phase_failures") %>%
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
  mm <- setdiff(
    req_phases,
    dpf %>%
      dplyr::filter(scenario == sc) %>%
      dplyr::pull(phase) %>%
      unique()
  )
  
  if (length(mm) > 0) {
    stop(
      "Missing phases in design_phase_failures for ",
      sc,
      ": ",
      paste(mm, collapse = ", "),
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


# ============================================================
# FOAK COMPETITION SENSITIVITY — COUPLED CHRONOLOGICAL RUNNER
# ============================================================
#
# Competition mechanism:
#   Phase-specific additive failure penalties are read from
#   foak_competition_rules.xlsx for every pre-operational phase:
#   conceptual_design, basic_design, detailed_design, licensing, construction.
#   operational should be present with a penalty of 0.
#
# Penalties are:
#   - additive to the ORIGINAL phase-specific failure probability;
#   - recalculated at each phase transition;
#   - based only on FOAKs already achieved in the same competition group
#     earlier in the same Monte Carlo simulation;
#   - capped at max_failure_probability (default 1);
#   - absent for N/A / NA / NONE competition groups.
#
# Historical FOAKs do not trigger penalties in this validation build unless
# historical_foaks_trigger is set TRUE in foak_competition_settings.xlsx.
#
# IMPORTANT:
# This replaces the independent-design Monte Carlo runner with a coupled
# chronological event simulation. The original duration, failure, licensing,
# override and CAREM rules above are otherwise retained.
# ============================================================

# -------------------- COMPETITION INPUT PATHS --------------------

COMP_DIR <- dirname(P_WORKBOOK)

find_comp_file <- function(filename) {
  candidates <- unique(c(
    file.path(COMP_DIR, filename),
    file.path(getwd(), filename),
    file.path(SCRIPT_DIR, filename)
  ))
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) {
    stop("Competition input file not found: ", filename,
         "\nLooked in:\n", paste(candidates, collapse = "\n"),
         call. = FALSE)
  }
  normalise_path(hit[[1]])
}

P_COMP_MAP       <- "C:/Users/jo3151go/OneDrive/FOAK SMRs/foak_competition_pipeline.xlsx"
P_COMP_UNIVERSAL <- "C:/Users/jo3151go/OneDrive/FOAK SMRs/foak_competition_universal.xlsx"
P_FUTURE_INPUT   <- "C:/Users/jo3151go/OneDrive/FOAK SMRs/post_2025_design_additions_inputs.xlsx"

competition_files <- c(P_COMP_MAP, P_COMP_UNIVERSAL, P_FUTURE_INPUT)
missing_competition_files <- competition_files[!file.exists(competition_files)]
if (length(missing_competition_files) > 0) {
  stop(
    "Competition input file(s) not found: ",
    paste(missing_competition_files, collapse = "; "),
    call. = FALSE
  )
}

pick_col <- function(df, candidates, required = TRUE) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) > 0) return(hit[[1]])
  if (required) {
    stop("Missing column; tried: ", paste(candidates, collapse = ", "),
         call. = FALSE)
  }
  NA_character_
}

comp_map_raw <- readxl::read_excel(P_COMP_MAP)
comp_rules_raw <- readxl::read_excel(P_COMP_UNIVERSAL, sheet = "phase_penalties")
comp_settings_raw <- readxl::read_excel(P_COMP_UNIVERSAL, sheet = "competition_settings")

# -------------------- NORMALISE COMPETITION MAP --------------------

map_design_col <- pick_col(comp_map_raw, c("smr_design", "design", "Design"))
map_group_col <- pick_col(
  comp_map_raw,
  c("competition", "competition_group", "group", "competition_region")
)

comp_map <- comp_map_raw %>%
  dplyr::transmute(
    design = stringr::str_trim(as.character(.data[[map_design_col]])),
    competition_group = stringr::str_trim(as.character(.data[[map_group_col]]))
  ) %>%
  dplyr::mutate(
    competition_group = dplyr::if_else(
      is.na(competition_group) |
        competition_group == "" |
        toupper(competition_group) %in% c("N/A", "NA", "NONE"),
      NA_character_,
      competition_group
    )
  )

missing_map <- setdiff(smr_master$design, comp_map$design)
extra_map   <- setdiff(comp_map$design, smr_master$design)
dups_map    <- unique(comp_map$design[duplicated(comp_map$design)])

if (length(missing_map) > 0) {
  stop("Missing competition mappings: ",
       paste(missing_map, collapse = ", "), call. = FALSE)
}
if (length(dups_map) > 0) {
  stop("Duplicate competition mappings: ",
       paste(dups_map, collapse = ", "), call. = FALSE)
}

# -------------------- NORMALISE COMPETITION RULES --------------------

rules_phase_col <- pick_col(
  comp_rules_raw,
  c("phase", "development_phase")
)
rules_penalty_col <- pick_col(
  comp_rules_raw,
  c("competition_penalty", "penalty", "penalty_per_foak",
    "failure_penalty", "increment", "value")
)

comp_rules <- comp_rules_raw %>%
  dplyr::transmute(
    phase = as_phase(.data[[rules_phase_col]]),
    competition_penalty = suppressWarnings(as.numeric(.data[[rules_penalty_col]]))
  ) %>%
  dplyr::filter(!is.na(phase), !is.na(competition_penalty))

required_comp_phases <- c(
  "conceptual_design",
  "basic_design",
  "detailed_design",
  "licensing",
  "construction"
)

missing_comp_rules <- setdiff(required_comp_phases, comp_rules$phase)
if (length(missing_comp_rules) > 0) {
  stop("Missing competition rules for: ",
       paste(missing_comp_rules, collapse = ", "), call. = FALSE)
}

# Operational is not a failure-gated phase, but require an explicit zero row
# so the competition workbook fully documents the intended cascade.
if (!"operational" %in% comp_rules$phase) {
  stop("Missing competition rule for: operational", call. = FALSE)
}
operational_penalty <- comp_rules$competition_penalty[
  comp_rules$phase == "operational"
][[1]]
if (!isTRUE(all.equal(as.numeric(operational_penalty), 0))) {
  stop("Operational competition penalty must be 0.", call. = FALSE)
}

get_comp_penalty <- function(phase) {
  phase <- as_phase(phase)
  z <- comp_rules$competition_penalty[comp_rules$phase == phase]
  if (length(z) == 0) return(0)
  as.numeric(z[[1]])
}

# -------------------- NORMALISE SETTINGS --------------------

settings_name_col <- pick_col(
  comp_settings_raw,
  c("parameter", "setting", "name")
)
settings_value_col <- pick_col(
  comp_settings_raw,
  c("value", "setting_value")
)

comp_settings <- comp_settings_raw %>%
  dplyr::transmute(
    parameter = stringr::str_trim(as.character(.data[[settings_name_col]])),
    value = as.character(.data[[settings_value_col]])
  )

setting <- function(name, default = NA_character_) {
  z <- comp_settings$value[comp_settings$parameter == name]
  if (length(z) == 0 || is.na(z[[1]]) || !nzchar(z[[1]])) return(default)
  z[[1]]
}

competition_start_year <- suppressWarnings(
  as.numeric(setting("competition_start_year", "2026"))
)
historical_foaks_trigger <- toupper(
  setting("historical_foaks_trigger", "FALSE")
) %in% c("TRUE", "T", "1", "YES")
penalty_type <- tolower(setting("penalty_type", "additive"))
max_failure_probability <- suppressWarnings(
  as.numeric(setting("max_failure_probability", "1"))
)

if (penalty_type != "additive") {
  stop("This validation build expects penalty_type = additive.", call. = FALSE)
}
if (!is.finite(max_failure_probability)) max_failure_probability <- 1
max_failure_probability <- clamp01(max_failure_probability)

# -------------------- COUPLED MASTER --------------------

smr_comp <- smr_master %>%
  dplyr::left_join(comp_map, by = "design") %>%
  dplyr::mutate(cohort = "active_2025")

if (any(is.na(smr_comp$competition_group) &
        !is.na(comp_map$competition_group[match(smr_comp$design, comp_map$design)]))) {
  stop("Competition mapping join failed.", call. = FALSE)
}

# ============================================================
# POST-2025 DESIGN ADDITIONS — INPUTS FOR INTEGRATED COMPETITION RUN
# ============================================================
# The original additions model is retained: Poisson design entry, country
# allocation, cooling-type allocation, country-specific licensing/construction
# risks, cooling penalties, and scenario-specific phase durations. The only new
# feature is that future entrants now share the SAME chronological state as the
# active 2025 cohort, so prior realised FOAKs can raise their subsequent phase
# failure probabilities through the universal competition mechanism.

clean_names_simple <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

std_country <- function(x) {
  x <- trimws(as.character(x))
  dplyr::case_when(
    x %in% c("US", "U.S.", "U.S.A.", "USA", "United States of America") ~ "United States",
    x %in% c("Russian Federation") ~ "Russia",
    x %in% c("ROW", "RoW", "Rest of World", "Rest-of-World") ~ "RoW",
    TRUE ~ x
  )
}

future_region_from_country <- function(country) {
  country <- std_country(country)
  dplyr::case_when(
    country %in% c("Canada", "United States") ~ "North America",
    country == "Europe" ~ "Europe",
    country == "OECD Asia" ~ "OECD Asia",
    country == "China" ~ "China",
    country == "Russia" ~ "Russia",
    country == "RoW" ~ "RoW",
    TRUE ~ NA_character_
  )
}

future_competition_group <- function(country) {
  country <- std_country(country)
  dplyr::case_when(
    country %in% c("Canada", "United States") ~ "North America",
    country == "Europe" ~ "Europe",
    country == "OECD Asia" ~ "OECD-Asia",
    country == "China" ~ "China",
    country == "Russia" ~ "Russia",
    TRUE ~ NA_character_   # RoW remains outside the regional competition penalty
  )
}

future_oecd_from_country <- function(country) {
  std_country(country) %in% c("Canada", "United States", "Europe", "OECD Asia")
}

std_cooling <- function(x) {
  x <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    x %in% c("gas", "gas_cooled", "gas-cooled") ~ "gas",
    x %in% c("metal", "metal_cooled", "metal-cooled", "lead", "sodium",
             "lead_bismuth", "lead-bismuth", "liquid_metal", "liquid-metal") ~ "metal",
    x %in% c("salt", "salt_cooled", "salt-cooled", "molten_salt",
             "molten-salt", "molten salt") ~ "salt",
    x %in% c("water", "water_cooled", "water-cooled") ~ "water",
    TRUE ~ x
  )
}

future_required_sheets <- c(
  "phase_rules_additions", "additions_region_country", "design_additions_cooling",
  "design_phase_failures", "failures_region_country", "cooling_type_failure"
)
future_available_sheets <- readxl::excel_sheets(P_FUTURE_INPUT)
future_missing_sheets <- setdiff(future_required_sheets, future_available_sheets)
if (length(future_missing_sheets) > 0) {
  stop("Missing future-additions sheet(s): ", paste(future_missing_sheets, collapse = ", "), call. = FALSE)
}

future_dpf <- readxl::read_excel(P_FUTURE_INPUT, sheet = "design_phase_failures")
names(future_dpf) <- clean_names_simple(names(future_dpf))
future_dpf <- future_dpf %>%
  dplyr::transmute(
    phase = as_phase(phase), scenario = as_scen(scenario),
    k = suppressWarnings(as.numeric(failures_k)), N = suppressWarnings(as.numeric(total_n))
  ) %>%
  dplyr::mutate(p_surv_mean = clamp01(1 - ((k + 0.5) / (N + 1))))
future_jeffreys_survival <- split(future_dpf, future_dpf$scenario) %>%
  purrr::map(~ setNames(.x$p_surv_mean, .x$phase))

future_fail_rates <- readxl::read_excel(P_FUTURE_INPUT, sheet = "failures_region_country")
names(future_fail_rates) <- clean_names_simple(names(future_fail_rates))
future_fail_rates <- future_fail_rates %>%
  dplyr::transmute(
    country = std_country(country),
    oecd = if ("oecd" %in% names(.)) as.logical(oecd) else future_oecd_from_country(country),
    licensing_failure_baseline = clamp01(baseline_licensing_failure),
    licensing_failure_accelerated = clamp01(accelerated_licensing_failure),
    construction_failure_baseline = clamp01(baseline_construction_failure),
    construction_failure_accelerated = clamp01(accelerated_construction_failure)
  ) %>%
  dplyr::mutate(
    region = future_region_from_country(country),
    lic_surv_baseline = clamp01(1 - licensing_failure_baseline),
    lic_surv_accel = clamp01(1 - licensing_failure_accelerated),
    con_surv_baseline = clamp01(1 - construction_failure_baseline),
    con_surv_accel = clamp01(1 - construction_failure_accelerated)
  ) %>%
  dplyr::select(country, region, oecd, lic_surv_baseline, lic_surv_accel,
                con_surv_baseline, con_surv_accel)

future_country_rates <- readxl::read_excel(P_FUTURE_INPUT, sheet = "additions_region_country")
names(future_country_rates) <- clean_names_simple(names(future_country_rates))
future_country_rates <- future_country_rates %>%
  dplyr::transmute(
    country = std_country(country),
    oecd = if ("oecd" %in% names(.)) as.logical(oecd) else future_oecd_from_country(country),
    baseline = suppressWarnings(as.numeric(baseline)),
    accelerated = suppressWarnings(as.numeric(accelerated))
  ) %>%
  dplyr::filter(!is.na(country), !country %in% c("Global", "OECD", "Non-OECD")) %>%
  dplyr::mutate(region = future_region_from_country(country))

future_valid_countries <- intersect(future_country_rates$country, future_fail_rates$country)
future_country_rates <- future_country_rates %>% dplyr::filter(country %in% future_valid_countries)
future_fail_rates <- future_fail_rates %>% dplyr::filter(country %in% future_valid_countries)

future_rules_add <- readxl::read_excel(P_FUTURE_INPUT, sheet = "phase_rules_additions")
names(future_rules_add) <- clean_names_simple(names(future_rules_add))
future_rules_add <- future_rules_add %>%
  dplyr::transmute(
    scenario = as_scen(scenario),
    start_year = suppressWarnings(as.integer(start_year)),
    pivot_year = suppressWarnings(as.integer(pivot_year)),
    end_year = suppressWarnings(as.integer(end_year)),
    lambda_start = suppressWarnings(as.numeric(lambda_start)),
    lambda_2050 = suppressWarnings(as.numeric(lambda_2050))
  ) %>% dplyr::filter(scenario %in% c("baseline", "accelerated"))

future_cooling_shares <- readxl::read_excel(P_FUTURE_INPUT, sheet = "design_additions_cooling")
names(future_cooling_shares) <- clean_names_simple(names(future_cooling_shares))
future_cooling_shares <- future_cooling_shares %>%
  dplyr::transmute(
    country = std_country(country),
    period = as_scen(period),
    cooling_type = std_cooling(cooling),
    jeffreys_share = suppressWarnings(as.numeric(jeffreys_share))
  ) %>%
  dplyr::filter(country %in% future_valid_countries, period %in% c("baseline", "accelerated")) %>%
  dplyr::group_by(country, period, cooling_type) %>%
  dplyr::summarise(jeffreys_share = dplyr::first(jeffreys_share), .groups = "drop") %>%
  dplyr::group_by(country, period) %>%
  dplyr::mutate(prob = jeffreys_share / sum(jeffreys_share, na.rm = TRUE)) %>%
  dplyr::ungroup()

future_cooling_fail <- readxl::read_excel(P_FUTURE_INPUT, sheet = "cooling_type_failure")
names(future_cooling_fail) <- clean_names_simple(names(future_cooling_fail))
future_cooling_fail <- future_cooling_fail %>%
  dplyr::transmute(
    cooling_type = std_cooling(if ("cooling_type" %in% names(.)) cooling_type else cooling),
    baseline_licensing_penalty = clamp01(if ("baseline_licensing_failure" %in% names(.)) baseline_licensing_failure else 0),
    accelerated_licensing_penalty = clamp01(if ("accelerated_licensing_failure" %in% names(.)) accelerated_licensing_failure else 0),
    baseline_construction_penalty = clamp01(if ("baseline_construction_failure" %in% names(.)) baseline_construction_failure else 0),
    accelerated_construction_penalty = clamp01(if ("accelerated_construction_failure" %in% names(.)) accelerated_construction_failure else 0)
  ) %>%
  dplyr::group_by(cooling_type) %>%
  dplyr::summarise(dplyr::across(dplyr::everything(), dplyr::first), .groups = "drop")

future_apply_cooling_penalty <- function(base_surv, cooling_type, phase, scenario) {
  z <- future_cooling_fail %>% dplyr::filter(.data$cooling_type == std_cooling(.env$cooling_type))
  if (nrow(z) == 0) return(clamp01(base_surv))
  penalty <- 0
  if (phase == "licensing" && scenario == "baseline") penalty <- z$baseline_licensing_penalty[[1]]
  if (phase == "licensing" && scenario == "accelerated") penalty <- z$accelerated_licensing_penalty[[1]]
  if (phase == "construction" && scenario == "baseline") penalty <- z$baseline_construction_penalty[[1]]
  if (phase == "construction" && scenario == "accelerated") penalty <- z$accelerated_construction_penalty[[1]]
  clamp01(1 - clamp01((1 - base_surv) + penalty))
}

future_country_weight_tbl <- dplyr::bind_rows(
  future_country_rates %>% dplyr::transmute(country, region, oecd, scenario = "baseline", weight = baseline),
  future_country_rates %>% dplyr::transmute(country, region, oecd, scenario = "accelerated", weight = accelerated)
) %>% dplyr::group_by(scenario) %>%
  dplyr::mutate(prob = weight / sum(weight, na.rm = TRUE)) %>% dplyr::ungroup()

future_get_cooling_probs <- function(country, scenario) {
  out <- future_cooling_shares %>%
    dplyr::filter(.data$country == .env$country, .data$period == as_scen(.env$scenario)) %>%
    dplyr::arrange(cooling_type)
  if (nrow(out) == 0) {
    out <- tibble::tibble(cooling_type = c("gas", "metal", "salt", "water"), prob = rep(0.25, 4))
  }
  out
}

future_lambda_path <- function(scenario, years_vec) {
  rr <- future_rules_add %>% dplyr::filter(.data$scenario == as_scen(.env$scenario))
  if (nrow(rr) != 1) stop("Expected exactly one future-additions rule for scenario: ", scenario)
  lambda_start <- rr$lambda_start[[1]]; lambda_2050 <- rr$lambda_2050[[1]]; pivot <- rr$pivot_year[[1]]
  k <- -log(lambda_2050 / lambda_start) / (2050 - pivot)
  vapply(years_vec, function(y) if (y <= pivot) lambda_start else lambda_start * exp(-k * (y - pivot)), numeric(1))
}

future_phase_params <- tibble::tribble(
  ~phase, ~scenario, ~mean, ~sd, ~min, ~max,
  "conceptual_design", "baseline", 6.66, 3.46, 2.0, 15.0,
  "conceptual_design", "accelerated", 5.11, 1.95, 2.0, 10.0,
  "basic_design", "baseline", 4.76, 3.10, 2.0, 14.0,
  "basic_design", "accelerated", 3.39, 1.03, 2.0, 5.0,
  "detailed_design", "baseline", 4.22, 2.13, 2.0, 9.0,
  "detailed_design", "accelerated", 3.36, 1.15, 2.0, 5.0,
  "licensing", "baseline", 1.46879, 0.2867, 2.0, 8.0,
  "licensing", "accelerated", 0.8405, 0.1432, 1.0, 3.0,
  "construction", "baseline", 1.64149, 0.2064, 3.0, 7.5,
  "construction", "accelerated", 1.3768, 0.1242, 3.0, 5.0
) %>% dplyr::mutate(phase = as_phase(phase), scenario = as_scen(scenario))

future_get_p <- function(phase, scenario) {
  z <- future_phase_params %>% dplyr::filter(.data$phase == as_phase(.env$phase), .data$scenario == as_scen(.env$scenario))
  if (nrow(z) != 1) stop("Missing future phase parameter: ", phase, " / ", scenario)
  list(mean=z$mean[[1]], sd=z$sd[[1]], min=z$min[[1]], max=z$max[[1]])
}

future_draw_total_duration <- function(phase, scenario, elapsed = 0) {
  p <- future_get_p(phase, scenario)
  if (elapsed >= p$max) return(elapsed)
  if (as_phase(phase) %in% c("licensing", "construction")) {
    return(r_lnorm_trunc_cond_total(elapsed, p$min, p$max, p$mean, p$sd))
  }
  lp <- lognorm_from_mean_sd(p$mean, p$sd)
  r_lnorm_trunc_cond_total(elapsed, p$min, p$max, lp$meanlog, lp$sdlog)
}

future_licensing_duration <- function(row, scenario, elapsed = 0) {
  is_cnru <- row$country %in% c("China", "Russia")
  if (as_scen(scenario) == "accelerated") {
    if (is_cnru) return(1.0)
    return(future_draw_total_duration("licensing", "accelerated", elapsed))
  }
  if (is_cnru) return(future_draw_total_duration("licensing", "accelerated", elapsed))
  future_draw_total_duration("licensing", "baseline", elapsed)
}

future_generate_additions <- function(scenario, sim_id) {
  years_vec <- 2026:2050
  lambda <- future_lambda_path(scenario, years_vec)
  ctry_probs <- future_country_weight_tbl %>% dplyr::filter(.data$scenario == as_scen(.env$scenario))
  out <- list(); kk <- 0L
  for (j in seq_along(years_vec)) {
    y <- years_vec[[j]]; n_new <- stats::rpois(1, lambda[[j]])
    if (n_new == 0) next
    ii <- sample(seq_len(nrow(ctry_probs)), n_new, replace = TRUE, prob = ctry_probs$prob)
    countries <- ctry_probs$country[ii]
    cool <- purrr::map_chr(countries, function(cc) {
      zz <- future_get_cooling_probs(cc, scenario)
      sample(zz$cooling_type, 1, prob = zz$prob)
    })
    tmp <- tibble::tibble(
      design = sprintf("FUT_%s_%05d_%04d_%03d", toupper(substr(scenario,1,3)), sim_id, y, seq_len(n_new)),
      phase = "conceptual_design", country = countries,
      region = future_region_from_country(countries),
      oecd = future_oecd_from_country(countries), national_champion = FALSE,
      size = NA_real_, cooling_type = cool, water_cooled = cool == "water",
      spectrum = NA_character_, uranium_enrichment = NA_character_,
      cd_start = as.numeric(y), bd_start = NA_real_, dd_start = NA_real_,
      licensing_start = NA_real_, construction_start = NA_real_, operational_start = NA_real_,
      baseline_condition = NA_character_, accelerated_condition = NA_character_,
      phase_rule_baseline_licensing = NA_character_, phase_rule_accelerated_licensing = NA_real_,
      competition_group = future_competition_group(countries), cohort = "post_2025_addition"
    ) %>% dplyr::left_join(future_fail_rates, by = c("country", "region", "oecd"))
    # Apply the original future-additions cooling modifiers to licensing/construction survival.
    for (i in seq_len(nrow(tmp))) {
      tmp$lic_surv_baseline[i] <- future_apply_cooling_penalty(tmp$lic_surv_baseline[i], tmp$cooling_type[i], "licensing", "baseline")
      tmp$lic_surv_accel[i] <- future_apply_cooling_penalty(tmp$lic_surv_accel[i], tmp$cooling_type[i], "licensing", "accelerated")
      tmp$con_surv_baseline[i] <- future_apply_cooling_penalty(tmp$con_surv_baseline[i], tmp$cooling_type[i], "construction", "baseline")
      tmp$con_surv_accel[i] <- future_apply_cooling_penalty(tmp$con_surv_accel[i], tmp$cooling_type[i], "construction", "accelerated")
    }
    kk <- kk + 1L; out[[kk]] <- tmp
  }
  dplyr::bind_rows(out)
}

# Cohort-aware base survival: active designs retain the active-pipeline failure
# structure; future entrants retain the original post-2025 additions structure.
get_surv_phase <- function(phase, scenario, row) {
  phase <- as_phase(phase); scenario <- as_scen(scenario)
  is_future <- !is.null(row$cohort) && !is.na(row$cohort) && row$cohort == "post_2025_addition"
  if (phase %in% c("conceptual_design", "basic_design", "detailed_design")) {
    p <- if (is_future) future_jeffreys_survival[[scenario]][[phase]] else jeffreys_survival[[scenario]][[phase]]
    return(clamp01(p))
  }
  if (phase == "licensing") return(clamp01(if (scenario == "baseline") row$lic_surv_baseline else row$lic_surv_accel))
  if (phase == "construction") return(clamp01(if (scenario == "baseline") row$con_surv_baseline else row$con_surv_accel))
  1.0
}

cohort_duration <- function(row, phase, scenario, elapsed = 0) {
  is_future <- !is.na(row$cohort) && row$cohort == "post_2025_addition"
  if (is_future) {
    if (as_phase(phase) == "licensing") return(future_licensing_duration(row, scenario, elapsed))
    return(future_draw_total_duration(phase, scenario, elapsed))
  }
  if (as_phase(phase) == "licensing") return(licensing_duration_with_rules(row, scenario, elapsed))
  draw_total_duration(phase, scenario, elapsed)
}

# -------------------- STATE HELPERS --------------------

phase_order <- c(
  "conceptual_design",
  "basic_design",
  "detailed_design",
  "licensing",
  "construction",
  "operational"
)

get_row_condition <- function(row, scenario) {
  if (scenario == "baseline") {
    norm_na(row$baseline_condition)
  } else {
    norm_na(row$accelerated_condition)
  }
}

get_phase_start_from_state <- function(state_row, phase) {
  switch(
    phase,
    conceptual_design = state_row$cd_start,
    basic_design = state_row$bd_start,
    detailed_design = state_row$dd_start,
    licensing = state_row$licensing_start,
    construction = state_row$construction_start,
    operational = state_row$operational_start,
    NA_real_
  )
}

set_phase_start_in_state <- function(state, idx, phase, value) {
  col <- switch(
    phase,
    conceptual_design = "cd_start",
    basic_design = "bd_start",
    detailed_design = "dd_start",
    licensing = "licensing_start",
    construction = "construction_start",
    operational = "operational_start",
    NA_character_
  )
  if (!is.na(col)) state[[col]][idx] <- value
  state
}

count_prior_foaks <- function(state, group, event_year, exclude_design) {
  if (is.na(group) || !nzchar(group)) return(0L)

  eligible <- state$design != exclude_design &
    state$competition_group == group &
    !is.na(state$foak_year) &
    state$foak_year < event_year

  # Historical FOAKs can be deliberately excluded.
  if (!historical_foaks_trigger) {
    eligible <- eligible & state$foak_year >= competition_start_year
  }

  sum(eligible, na.rm = TRUE)
}

competition_adjusted_failure <- function(
  state, row, phase, scenario, event_year
) {
  p_surv_base <- get_surv_phase(phase, scenario, row)
  p_fail_base <- clamp01(1 - p_surv_base)

  group <- row$competition_group
  n_prior <- 0L
  per_foak <- 0

  if (
    phase %in% required_comp_phases &&
    !is.na(group) &&
    event_year >= competition_start_year
  ) {
    n_prior <- count_prior_foaks(
      state = state,
      group = group,
      event_year = event_year,
      exclude_design = row$design
    )
    per_foak <- get_comp_penalty(phase)
  }

  penalty <- n_prior * per_foak
  p_fail_total <- min(
    max_failure_probability,
    p_fail_base + penalty
  )

  list(
    p_fail_base = p_fail_base,
    n_prior_foaks = n_prior,
    penalty_per_foak = per_foak,
    competition_penalty = penalty,
    p_fail_total = p_fail_total,
    p_surv_total = clamp01(1 - p_fail_total)
  )
}

# -------------------- INITIALISE ONE SIMULATION --------------------

initialise_state <- function(scenario, sim_id) {
  scenario <- as_scen(scenario)

  future_additions <- future_generate_additions(scenario, sim_id)
  combined_master <- dplyr::bind_rows(smr_comp, future_additions)

  state <- combined_master %>%
    dplyr::mutate(
      scenario = scenario,
      status = dplyr::if_else(
        phase == "operational",
        "already_operational",
        "active"
      ),
      fail_phase = NA_character_,
      foak_year = dplyr::if_else(
        phase == "operational",
        operational_start,
        NA_real_
      ),
      current_phase = phase,
      next_event_year = NA_real_,
      done = phase == "operational"
    )

  # Schedule the first completion/transition event for every active design.
  for (idx in which(!state$done)) {
    row <- state[idx, ]
    ph <- row$current_phase

    # CAREM retains the original special construction treatment.
    if (row$design == "CAREM" && ph == "construction") {
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
      state$construction_start[idx] <- 2026.0
      state$next_event_year[idx] <- 2026.0 + (1 - frac) * total_con
      next
    }

    st <- get_phase_start_from_state(row, ph)
    if (is.na(st)) st <- ifelse(ph == row$phase, CUT_YEAR, NEXT_FLOOR)

    elapsed <- if (ph == row$phase && st <= CUT_YEAR) {
      max(0, CUT_YEAR - st + 1)
    } else {
      0
    }

    tot <- cohort_duration(row, ph, scenario, elapsed)

    en <- st + tot
    if (elapsed > 0 && en < CUT_YEAR) en <- CUT_YEAR

    nxt <- phase_order[match(ph, phase_order) + 1]
    cond <- get_row_condition(row, scenario)
    ov <- apply_next_override(nxt, cond)

    if (!is.na(ov)) en <- ov

    state$next_event_year[idx] <- max(NEXT_FLOOR, en)
  }

  state
}

# -------------------- PROCESS ONE EVENT --------------------

process_event <- function(state, idx, scenario, sim_id, event_id) {
  row <- state[idx, ]
  ph <- row$current_phase
  event_year <- row$next_event_year
  cond <- get_row_condition(row, scenario)

  nxt <- phase_order[match(ph, phase_order) + 1]
  ov <- apply_next_override(nxt, cond)
  forced_transition <- !is.na(ov)

  # Preserve BWRX-300 licensing failure exemption.
  licensing_exempt <- (
    ph == "licensing" &&
    row$design %in% licensing_failure_exempt_designs
  )

  # Preserve original CAREM construction logic and forced-transition logic.
  skip_failure <- forced_transition || licensing_exempt

  # Competition applies to all five pre-operational phases using the Excel rules.
  adj <- competition_adjusted_failure(
    state = state,
    row = row,
    phase = ph,
    scenario = scenario,
    event_year = event_year
  )

  u <- NA_real_
  survived <- TRUE

  if (!skip_failure) {
    u <- runif(1)
    survived <- u > adj$p_fail_total
  }

  diag <- tibble::tibble(
    sim = sim_id,
    event_id = event_id,
    scenario = scenario,
    event_year = event_year,
    design = row$design,
    competition_group = row$competition_group,
    phase = ph,
    next_phase = nxt,
    forced_transition = forced_transition,
    licensing_failure_exempt = licensing_exempt,
    n_prior_foaks = adj$n_prior_foaks,
    base_failure_probability = adj$p_fail_base,
    penalty_per_prior_foak = adj$penalty_per_foak,
    competition_penalty = adj$competition_penalty,
    total_failure_probability = adj$p_fail_total,
    random_draw = u,
    survived = survived
  )

  if (!survived) {
    state$status[idx] <- paste0("failed_", ph)
    state$fail_phase[idx] <- ph
    state$done[idx] <- TRUE
    state$next_event_year[idx] <- NA_real_
    return(list(state = state, diagnostic = diag))
  }

  # Successful construction completion = FOAK.
  if (nxt == "operational") {
    ov_op <- apply_next_override("operational", cond)
    foak <- if (!is.na(ov_op)) ov_op else event_year

    state$operational_start[idx] <- foak
    state$foak_year[idx] <- foak
    state$current_phase[idx] <- "operational"
    state$status[idx] <- "simulated"
    state$done[idx] <- TRUE
    state$next_event_year[idx] <- NA_real_

    return(list(state = state, diagnostic = diag))
  }

  # Successful transition to next phase.
  nxt_start <- max(NEXT_FLOOR, event_year)
  ov_next <- apply_next_override(nxt, cond)
  if (!is.na(ov_next)) nxt_start <- ov_next

  state <- set_phase_start_in_state(state, idx, nxt, nxt_start)
  state$current_phase[idx] <- nxt

  # Draw duration for the newly entered phase.
  row2 <- state[idx, ]
  elapsed2 <- 0

  tot2 <- cohort_duration(row2, nxt, scenario, elapsed2)

  en2 <- nxt_start + tot2
  nxt2 <- phase_order[match(nxt, phase_order) + 1]
  ov2 <- apply_next_override(nxt2, cond)
  if (!is.na(ov2)) en2 <- ov2

  state$next_event_year[idx] <- en2

  list(state = state, diagnostic = diag)
}

# -------------------- RUN ONE COUPLED SIMULATION --------------------

run_one_competition_sim <- function(scenario, sim_id) {
  scenario <- as_scen(scenario)
  state <- initialise_state(scenario, sim_id)

  diagnostics <- list()
  event_id <- 0L

  while (any(!state$done & is.finite(state$next_event_year))) {
    active_idx <- which(!state$done & is.finite(state$next_event_year))

    # Chronological event ordering. Random tie-break prevents row-order bias
    # where multiple events occur at exactly the same simulated time.
    min_year <- min(state$next_event_year[active_idx], na.rm = TRUE)
    tied <- active_idx[
      abs(state$next_event_year[active_idx] - min_year) < 1e-10
    ]
    idx <- if (length(tied) == 1) tied else sample(tied, 1)

    event_id <- event_id + 1L

    z <- process_event(
      state = state,
      idx = idx,
      scenario = scenario,
      sim_id = sim_id,
      event_id = event_id
    )

    state <- z$state
    diagnostics[[event_id]] <- z$diagnostic
  }

  draws <- state %>%
    dplyr::transmute(
      sim = sim_id,
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
      foak_year,
      competition_group,
      cohort,
      country,
      region,
      cooling_type
    )

  diagnostics <- dplyr::bind_rows(diagnostics)

  list(draws = draws, diagnostics = diagnostics)
}

# -------------------- MONTE CARLO RUNNER --------------------

run_competition_mc <- function(scenario, sims) {
  res <- vector("list", sims)

  for (s in seq_len(sims)) {
    res[[s]] <- run_one_competition_sim(scenario, s)
    if (s %% 5 == 0 || s == sims) {
      cat("Completed", scenario, "simulation", s, "of", sims, "\n")
    }
  }

  list(
    draws = dplyr::bind_rows(lapply(res, `[[`, "draws")),
    diagnostics = dplyr::bind_rows(lapply(res, `[[`, "diagnostics"))
  )
}

# -------------------- RUN 50-SIM FOAK-PROLIFERATION INTEGRATED TEST --------------------

set.seed(SEED)
proliferation_run <- run_competition_mc("accelerated", SIMS)
mc_proliferation_integrated <- proliferation_run$draws
competition_diagnostics <- proliferation_run$diagnostics

# -------------------- SUMMARIES --------------------

global_2050 <- mc_proliferation_integrated %>%
  dplyr::mutate(success_2050 = !is.na(foak_year) & foak_year <= 2050) %>%
  dplyr::group_by(sim) %>%
  dplyr::summarise(total = sum(success_2050), .groups = "drop") %>%
  dplyr::summarise(
    scenario = "FOAK-Proliferation", P10 = safe_q(total, 0.10), P50 = safe_q(total, 0.50),
    P90 = safe_q(total, 0.90), mean = safe_mean(total)
  )

cohort_2050 <- mc_proliferation_integrated %>%
  dplyr::mutate(success_2050 = !is.na(foak_year) & foak_year <= 2050) %>%
  dplyr::group_by(sim, cohort) %>%
  dplyr::summarise(total = sum(success_2050), .groups = "drop") %>%
  tidyr::complete(sim = seq_len(SIMS), cohort = c("active_2025", "post_2025_addition"), fill = list(total = 0)) %>%
  dplyr::group_by(cohort) %>%
  dplyr::summarise(P10=safe_q(total,.10), P50=safe_q(total,.50), P90=safe_q(total,.90), mean=safe_mean(total), .groups="drop")

regional_2050 <- mc_proliferation_integrated %>%
  dplyr::mutate(success_2050 = !is.na(foak_year) & foak_year <= 2050,
                reporting_region = dplyr::coalesce(competition_group, "RoW / no competition group")) %>%
  dplyr::group_by(sim, reporting_region) %>%
  dplyr::summarise(total = sum(success_2050), .groups="drop") %>%
  dplyr::group_by(reporting_region) %>%
  dplyr::summarise(P10=safe_q(total,.10), P50=safe_q(total,.50), P90=safe_q(total,.90), mean=safe_mean(total), .groups="drop")

future_designs_generated <- mc_proliferation_integrated %>%
  dplyr::filter(cohort == "post_2025_addition") %>%
  dplyr::group_by(sim) %>%
  dplyr::summarise(generated=dplyr::n(), foak_2050=sum(!is.na(foak_year) & foak_year <= 2050), .groups="drop") %>%
  dplyr::summarise(
    generated_P10=safe_q(generated,.10), generated_P50=safe_q(generated,.50), generated_P90=safe_q(generated,.90), generated_mean=safe_mean(generated),
    foak_P10=safe_q(foak_2050,.10), foak_P50=safe_q(foak_2050,.50), foak_P90=safe_q(foak_2050,.90), foak_mean=safe_mean(foak_2050)
  )

penalty_summary <- competition_diagnostics %>%
  dplyr::group_by(competition_group, phase) %>%
  dplyr::summarise(
    events=dplyr::n(), events_with_prior_foak=sum(n_prior_foaks > 0),
    max_prior_foaks=max(n_prior_foaks, na.rm=TRUE),
    mean_competition_penalty=mean(competition_penalty, na.rm=TRUE),
    max_competition_penalty=max(competition_penalty, na.rm=TRUE),
    mean_total_failure_probability=mean(total_failure_probability, na.rm=TRUE),
    .groups="drop"
  )

# -------------------- EXPORT --------------------
# Keep the Excel workbook compact. Full draws and event diagnostics are stored
# as RDS files so the 1,048,576-row Excel limit can never destroy the run.

TEST_OUTDIR <- file.path(OUTDIR, "foak_competition_integrated_test_output")
if (!dir.exists(TEST_OUTDIR)) dir.create(TEST_OUTDIR, recursive = TRUE)

summary_xlsx <- file.path(TEST_OUTDIR, "foak_competition_integrated_proliferation_50runs_summary.xlsx")
draws_rds <- file.path(TEST_OUTDIR, "foak_competition_integrated_proliferation_50runs_draws.rds")
diag_rds <- file.path(TEST_OUTDIR, "foak_competition_integrated_proliferation_50runs_diagnostics.rds")

writexl::write_xlsx(
  list(
    global_2050 = global_2050,
    cohort_2050 = cohort_2050,
    regional_2050 = regional_2050,
    future_additions = future_designs_generated,
    penalty_summary = penalty_summary
  ),
  path = summary_xlsx
)
saveRDS(mc_proliferation_integrated, draws_rds)
saveRDS(competition_diagnostics, diag_rds)

cat("\n============================================================\n")
cat("INTEGRATED FOAK-PROLIFERATION 50-RUN TEST COMPLETE\n")
cat("============================================================\n\n")
cat("GLOBAL FOAKs BY 2050:\n"); print(global_2050)
cat("\nBY COHORT:\n"); print(cohort_2050)
cat("\nBY REGION / COMPETITION GROUP:\n"); print(regional_2050)
cat("\nFUTURE ADDITIONS GENERATED AND REACHING FOAK BY 2050:\n"); print(future_designs_generated)
cat("\nSummary workbook:\n", summary_xlsx, "\n")
cat("Full draws RDS:\n", draws_rds, "\n")
cat("Diagnostics RDS:\n", diag_rds, "\n")
cat("============================================================\n")



x <- readRDS(
  "C:/Users/jo3151go/OneDrive/FOAK SMRs/foak_competition_integrated_test_output/foak_competition_integrated_proliferation_50runs_draws.rds"
)

class(x)
names(x)
str(x, max.level = 2)


length(unique(x$sim))
range(x$sim)
table(x$cohort)


library(dplyr)
library(tidyr)
library(openxlsx)

years <- 2026:2050
sims  <- sort(unique(x$sim))

# ------------------------------------------------------------
# 1. GLOBAL ANNUAL CUMULATIVE FOAK COUNTS
# ------------------------------------------------------------

global_by_sim <- expand_grid(
  sim  = sims,
  year = years
) %>%
  left_join(
    x %>%
      filter(!is.na(foak_year)) %>%
      select(sim, foak_year),
    by = "sim",
    relationship = "many-to-many"
  ) %>%
  group_by(sim, year) %>%
  summarise(
    cumulative_foak = sum(foak_year <= year, na.rm = TRUE),
    .groups = "drop"
  )

global_annual <- global_by_sim %>%
  group_by(year) %>%
  summarise(
    P10  = as.numeric(quantile(cumulative_foak, 0.10)),
    P50  = as.numeric(quantile(cumulative_foak, 0.50)),
    P90  = as.numeric(quantile(cumulative_foak, 0.90)),
    mean = mean(cumulative_foak),
    .groups = "drop"
  ) %>%
  mutate(
    scenario = "Proliferation competition",
    .before = 1
  )


# ------------------------------------------------------------
# 2. REGIONAL ANNUAL CUMULATIVE FOAK COUNTS
# ------------------------------------------------------------

regions <- sort(unique(x$region))

regional_by_sim <- expand_grid(
  sim    = sims,
  region = regions,
  year   = years
) %>%
  left_join(
    x %>%
      filter(!is.na(foak_year)) %>%
      select(sim, region, foak_year),
    by = c("sim", "region"),
    relationship = "many-to-many"
  ) %>%
  group_by(sim, region, year) %>%
  summarise(
    cumulative_foak = sum(foak_year <= year, na.rm = TRUE),
    .groups = "drop"
  )

regional_annual <- regional_by_sim %>%
  group_by(region, year) %>%
  summarise(
    P10  = as.numeric(quantile(cumulative_foak, 0.10)),
    P50  = as.numeric(quantile(cumulative_foak, 0.50)),
    P90  = as.numeric(quantile(cumulative_foak, 0.90)),
    mean = mean(cumulative_foak),
    .groups = "drop"
  ) %>%
  mutate(
    scenario = "Proliferation competition",
    .before = 1
  )


# ------------------------------------------------------------
# 3. 2050 REGIONAL SUMMARY
# ------------------------------------------------------------

regional_2050 <- regional_annual %>%
  filter(year == 2050)


# ------------------------------------------------------------
# 4. COHORT CONTRIBUTIONS THROUGH TIME
#    active 2025 vs post-2025 additions
# ------------------------------------------------------------

cohorts <- sort(unique(x$cohort))

cohort_by_sim <- expand_grid(
  sim    = sims,
  cohort = cohorts,
  year   = years
) %>%
  left_join(
    x %>%
      filter(!is.na(foak_year)) %>%
      select(sim, cohort, foak_year),
    by = c("sim", "cohort"),
    relationship = "many-to-many"
  ) %>%
  group_by(sim, cohort, year) %>%
  summarise(
    cumulative_foak = sum(foak_year <= year, na.rm = TRUE),
    .groups = "drop"
  )

cohort_annual <- cohort_by_sim %>%
  group_by(cohort, year) %>%
  summarise(
    P10  = as.numeric(quantile(cumulative_foak, 0.10)),
    P50  = as.numeric(quantile(cumulative_foak, 0.50)),
    P90  = as.numeric(quantile(cumulative_foak, 0.90)),
    mean = mean(cumulative_foak),
    .groups = "drop"
  ) %>%
  mutate(
    scenario = "Proliferation competition",
    .before = 1
  )


# ------------------------------------------------------------
# 5. README
# ------------------------------------------------------------

readme <- data.frame(
  item = c(
    "Scenario",
    "Monte Carlo simulations",
    "Years",
    "Active 2025 observations",
    "Post-2025 design-addition observations",
    "Source"
  ),
  value = c(
    "Proliferation with regional FOAK competition",
    length(sims),
    "2026-2050",
    sum(x$cohort == "active_2025"),
    sum(x$cohort == "post_2025_addition"),
    "Integrated FOAK competition simulation draws"
  )
)


# ------------------------------------------------------------
# 6. EXPORT
# ------------------------------------------------------------

out_file <- paste0(
  "C:/Users/jo3151go/OneDrive/Nat. Comms v2/figures_data/",
  "extended_data_fig07_foak_competition_proliferation.xlsx"
)

write.xlsx(
  list(
    global_annual   = global_annual,
    regional_annual = regional_annual,
    regional_2050   = regional_2050,
    cohort_annual   = cohort_annual,
    README           = readme
  ),
  file = out_file,
  overwrite = TRUE
)

cat("\nSaved to:\n", out_file, "\n")

# Useful verification
print(tail(global_annual, 5))
print(regional_2050)



# Harmonise regional labels in the simulation draws
x_clean <- x %>%
  mutate(
    region = case_when(
      region %in% c("OECD Asia", "OECD-Asia") ~ "OECD Asia",
      TRUE ~ region
    )
  )

regions <- sort(unique(x_clean$region))

regional_by_sim <- expand_grid(
  sim    = sims,
  region = regions,
  year   = years
) %>%
  left_join(
    x_clean %>%
      filter(!is.na(foak_year)) %>%
      select(sim, region, foak_year),
    by = c("sim", "region"),
    relationship = "many-to-many"
  ) %>%
  group_by(sim, region, year) %>%
  summarise(
    cumulative_foak = sum(foak_year <= year, na.rm = TRUE),
    .groups = "drop"
  )

regional_annual <- regional_by_sim %>%
  group_by(region, year) %>%
  summarise(
    P10  = as.numeric(quantile(cumulative_foak, 0.10)),
    P50  = as.numeric(quantile(cumulative_foak, 0.50)),
    P90  = as.numeric(quantile(cumulative_foak, 0.90)),
    mean = mean(cumulative_foak),
    .groups = "drop"
  ) %>%
  mutate(
    scenario = "Proliferation competition",
    .before = 1
  )

regional_2050 <- regional_annual %>%
  filter(year == 2050)

print(regional_2050)



write.xlsx(
  list(
    global_annual   = global_annual,
    regional_annual = regional_annual,
    regional_2050   = regional_2050,
    cohort_annual   = cohort_annual,
    README           = readme
  ),
  file = out_file,
  overwrite = TRUE
)

cat("\nCorrected workbook saved to:\n", out_file, "\n")