# ============================================================
# SMR55 — PROJECT PROGRESSION MONTE CARLO
# CONSTRUCTION CASCADE SENSITIVITY
# FAIL-GATED • FULL CLEAN SCRIPT • LOGNORMAL EVERYWHERE
#
# GITHUB VERSION:
#   • 10,000 simulations for the full manuscript-scale run
#   • For a quick repository smoke test, set SIMS <- 25L
#
# INPUT WORKBOOK:
#   data/Simulation_Inputs/construction_cascade_sensitivity_inputs.xlsx
#
# REQUIRED SHEETS:
#   1) smr_design_inputs
#   2) phase_rules
#   3) design_phase_failures
#   4) construction_cascade_failures
#
# OUTPUT:
#   outputs/Simulation_Outputs/06_construction_cascade/construction_cascade_output_final/
#
# NOTE:
#   • LOGNORMAL EVERYWHERE for duration sampling
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

# -------------------- REPOSITORY ROOT --------------------

normalise_path <- function(x) {
  normalizePath(x, winslash = "/", mustWork = FALSE)
}

get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  hit <- grep(file_arg, cmd_args, value = TRUE)
  
  if (length(hit) > 0) {
    return(normalise_path(sub(file_arg, "", hit[1])))
  }
  
  for (frame in sys.frames()) {
    if (!is.null(frame$ofile)) {
      return(normalise_path(frame$ofile))
    }
  }
  
  NA_character_
}

parent_dirs <- function(start_dir) {
  dirs <- character()
  d <- normalise_path(start_dir)
  
  repeat {
    dirs <- c(dirs, d)
    parent <- dirname(d)
    
    if (identical(parent, d)) {
      break
    }
    
    d <- parent
  }
  
  unique(dirs)
}

find_repo_root <- function(start_dirs = getwd()) {
  target_file <- file.path(
    "data",
    "Simulation_Inputs",
    "construction_cascade_sensitivity_inputs.xlsx"
  )
  
  start_dirs <- unique(start_dirs)
  start_dirs <- start_dirs[!is.na(start_dirs)]
  
  for (start_dir in start_dirs) {
    if (!dir.exists(start_dir)) {
      next
    }
    
    for (d in parent_dirs(start_dir)) {
      if (file.exists(file.path(d, target_file))) {
        return(normalise_path(d))
      }
    }
  }
  
  stop(
    paste0(
      "Repository root not found.\n\n",
      "Expected to find:\n",
      target_file, "\n\n",
      "Current working directory:\n",
      getwd()
    ),
    call. = FALSE
  )
}

SCRIPT_PATH <- get_script_path()
SCRIPT_DIR  <- if (!is.na(SCRIPT_PATH)) dirname(SCRIPT_PATH) else getwd()

ROOT_DIR <- find_repo_root(c(getwd(), SCRIPT_DIR))

PATH_MODE <- "github_repo_layout"

# -------------------- PATHS --------------------

P_INPUT <- file.path(
  ROOT_DIR,
  "data",
  "Simulation_Inputs",
  "construction_cascade_sensitivity_inputs.xlsx"
)

OUTDIR <- file.path(
  ROOT_DIR,
  "outputs",
  "Simulation_Outputs",
  "06_construction_cascade",
  "construction_cascade_output_final"
)

P_INPUT <- normalise_path(P_INPUT)
OUTDIR  <- normalise_path(OUTDIR)

# -------------------- GLOBALS --------------------
CUT_YEAR   <- 2025L
NEXT_FLOOR <- 2026.0
SIMS       <- 10000L
SEED       <- 42L
YEARS      <- 2026:2050

MODEL_TAG  <- "CONSTRUCTION_CASCADE"
RUN_TAG    <- paste0("MC", SIMS)

set.seed(SEED)

if (!file.exists(P_INPUT)) {
  stop("Input workbook not found: ", P_INPUT, call. = FALSE)
}

required_sheets <- c(
  "smr_design_inputs",
  "phase_rules",
  "design_phase_failures",
  "construction_cascade_failures"
)

available_sheets <- readxl::excel_sheets(P_INPUT)
missing_sheets <- setdiff(required_sheets, available_sheets)

if (length(missing_sheets) > 0) {
  stop(
    "Missing required sheet(s) in input workbook: ",
    paste(missing_sheets, collapse = ", "),
    call. = FALSE
  )
}

dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)

stamp <- format(Sys.time(), "%Y%m%d_%H%M")

RUN_DIR    <- file.path(OUTDIR, paste0("run_", stamp))
LATEST_DIR <- file.path(OUTDIR, "latest")

dir.create(RUN_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LATEST_DIR, recursive = TRUE, showWarnings = FALSE)

# -------------------- STANDARDISERS --------------------
clean_names_simple <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

as_phase <- function(x) {
  str_to_lower(str_replace_all(str_trim(as.character(x)), "[ -]", "_"))
}

as_scen <- function(x) {
  str_to_lower(str_trim(as.character(x)))
}

norm_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x)
  x <- ifelse(is.na(x), NA_character_, str_trim(x))
  x <- ifelse(tolower(x) %in% c("na", "n/a", "", "null"), NA_character_, x)
  x
}

key_ps <- function(phase, scenario) {
  paste0(phase, "__", scenario)
}

extract_year <- function(x) {
  y <- suppressWarnings(as.integer(str_extract(as.character(x), "\\d{4}")))
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

as_bool_any <- function(x) {
  if (is.logical(x)) return(x)
  x_chr <- str_trim(tolower(as.character(x)))
  dplyr::case_when(
    x_chr %in% c("true", "t", "1", "yes", "y") ~ TRUE,
    x_chr %in% c("false", "f", "0", "no", "n") ~ FALSE,
    TRUE ~ NA
  )
}

# -------------------- LOAD 55 SPECS --------------------
smr55_raw <- readxl::read_excel(P_INPUT, sheet = "smr_design_inputs")
names(smr55_raw) <- clean_names_simple(names(smr55_raw))

smr55_specs <- smr55_raw %>%
  dplyr::transmute(
    design = str_trim(as.character(design)),
    phase = as_phase(phase),
    country = as.character(country),
    region = as.character(region),
    oecd = as_bool_any(oecd),
    national_champion = as_bool_any(national_champion),
    size = suppressWarnings(as.numeric(size)),
    cooling_type = as.character(cooling_type),
    water_cooled = as_bool_any(water_cooled),
    spectrum = as.character(spectrum),
    uranium_enrichment = as.character(uranium_enrichment),
    cd_start = suppressWarnings(as.numeric(cd_start)),
    bd_start = suppressWarnings(as.numeric(bd_start)),
    dd_start = suppressWarnings(as.numeric(dd_start)),
    licensing_start = suppressWarnings(as.numeric(licensing_start)),
    construction_start = suppressWarnings(as.numeric(construction_start)),
    operational_start = suppressWarnings(as.numeric(operational_start))
  )

stopifnot(nrow(smr55_specs) == dplyr::n_distinct(smr55_specs$design))

# -------------------- LOAD PHASE RULES --------------------
phase_rules_raw <- readxl::read_excel(P_INPUT, sheet = "phase_rules")
names(phase_rules_raw) <- clean_names_simple(names(phase_rules_raw))

phase_rules <- phase_rules_raw %>%
  dplyr::transmute(
    design = str_trim(as.character(design)),
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
fail_rates_raw <- readxl::read_excel(P_INPUT, sheet = "construction_cascade_failures")
names(fail_rates_raw) <- clean_names_simple(names(fail_rates_raw))

fail_rates <- fail_rates_raw %>%
  dplyr::transmute(
    design = str_trim(as.character(design)),
    licensing_failure_baseline = clamp01(licensing_failure_baseline),
    licensing_failure_accelerated = clamp01(licensing_failure_accelerated),
    construction_failure_baseline = clamp01(construction_failure_baseline),
    construction_failure_accelerated = clamp01(construction_failure_accelerated)
  ) %>%
  dplyr::mutate(
    lic_surv_baseline = clamp01(1 - licensing_failure_baseline),
    lic_surv_accel = clamp01(1 - licensing_failure_accelerated),
    con_surv_baseline = clamp01(1 - construction_failure_baseline),
    con_surv_accel = clamp01(1 - construction_failure_accelerated)
  ) %>%
  dplyr::select(
    design,
    lic_surv_baseline,
    lic_surv_accel,
    con_surv_baseline,
    con_surv_accel
  )

# -------------------- LOAD DESIGN-PHASE FAILURES --------------------
dpf_raw <- readxl::read_excel(P_INPUT, sheet = "design_phase_failures")
names(dpf_raw) <- clean_names_simple(names(dpf_raw))

dpf <- dpf_raw %>%
  dplyr::transmute(
    phase = as_phase(phase),
    scenario = as_scen(scenario),
    k = suppressWarnings(as.numeric(failures_k)),
    N = suppressWarnings(as.numeric(total_n))
  ) %>%
  dplyr::mutate(
    p_fail_mean = (k + 0.5) / (N + 1),
    p_surv_mean = clamp01(1 - p_fail_mean)
  )

req_phases <- c("conceptual_design", "basic_design", "detailed_design")
req_scens  <- c("baseline", "accelerated")

miss_sc <- setdiff(req_scens, unique(dpf$scenario))

if (length(miss_sc) > 0) {
  stop("Missing scenarios in design_phase_failures: ", paste(miss_sc, collapse = ", "))
}

for (sc in req_scens) {
  mm <- setdiff(req_phases, dpf %>% dplyr::filter(scenario == sc) %>% dplyr::pull(phase) %>% unique())
  if (length(mm) > 0) {
    stop("Missing phases in design_phase_failures for ", sc, ": ", paste(mm, collapse = ", "))
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
smr_master <- smr55_specs %>%
  dplyr::left_join(phase_rules, by = "design") %>%
  dplyr::left_join(fail_rates, by = "design")

miss_fail <- smr_master %>%
  dplyr::filter(is.na(lic_surv_baseline) | is.na(lic_surv_accel) | is.na(con_surv_baseline) | is.na(con_surv_accel)) %>%
  dplyr::pull(design)

if (length(miss_fail) > 0) {
  stop("Missing failure rates for designs: ", paste(unique(miss_fail), collapse = ", "))
}

# -------------------- HARDCODED PHASE PARAMETERS --------------------
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
  purrr::map(~ list(
    mean = .x$mean[[1]],
    sd = .x$sd[[1]],
    min = .x$min[[1]],
    max = .x$max[[1]]
  ))

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
  list(meanlog = log(mean) - 0.5 * v, sdlog = sqrt(v))
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
  
  if (next_phase == "basic_design" && cond == "force_bd_start_2026") return(2026.0)
  if (next_phase == "detailed_design" && cond == "force_dd_start_2026") return(2026.0)
  
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
    if (is_cnru || (!is.na(acc_rule) && acc_rule == 1)) return(1.0)
    return(draw_total_duration("licensing", "accelerated", elapsed))
  }
  
  if (is_cnru) return(draw_total_duration("licensing", "accelerated", elapsed))
  
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
  phases <- c("conceptual_design", "basic_design", "detailed_design", "licensing", "construction", "operational")
  
  cur <- row$phase
  
  if (is.na(cur) || !cur %in% phases) {
    return(tibble(
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
    ))
  }
  
  cond <- if (scenario == "baseline") row$baseline_condition else row$accelerated_condition
  cond <- norm_na(cond)
  
  cd_s  <- row$cd_start
  bd_s  <- row$bd_start
  dd_s  <- row$dd_start
  lic_s <- row$licensing_start
  con_s <- row$construction_start
  op_s  <- row$operational_start
  
  cd_e <- bd_e <- dd_e <- lic_e <- con_e <- NA_real_
  
  if (cur == "operational") {
    return(tibble(
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
    ))
  }
  
  if (row$design == "CAREM" && cur == "construction") {
    p_con <- get_p("construction", scenario)
    total_con <- r_lnorm_trunc_cond_total(0, p_con$min, p_con$max, p_con$mean, p_con$sd)
    frac <- if (scenario == "accelerated") runif(1, 0.40, 0.50) else runif(1, 0.30, 0.40)
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
        return(tibble(
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
        ))
      }
    }
    
    return(tibble(
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
    ))
  }
  
  get_start <- function(phase) {
    if (phase == "conceptual_design") return(cd_s)
    if (phase == "basic_design") return(bd_s)
    if (phase == "detailed_design") return(dd_s)
    if (phase == "licensing") return(lic_s)
    if (phase == "construction") return(con_s)
    if (phase == "operational") return(op_s)
    NA_real_
  }
  
  set_start <- function(phase, val) {
    if (phase == "basic_design") bd_s <<- val
    if (phase == "detailed_design") dd_s <<- val
    if (phase == "licensing") lic_s <<- val
    if (phase == "construction") con_s <<- val
    if (phase == "operational") op_s <<- val
  }
  
  set_end <- function(phase, val) {
    if (phase == "conceptual_design") cd_e <<- val
    if (phase == "basic_design") bd_e <<- val
    if (phase == "detailed_design") dd_e <<- val
    if (phase == "licensing") lic_e <<- val
    if (phase == "construction") con_e <<- val
  }
  
  i0 <- match(cur, phases)
  
  for (i in i0:(length(phases) - 1)) {
    
    ph <- phases[i]
    st <- get_start(ph)
    
    if (is.na(st)) {
      st <- ifelse(ph == cur, CUT_YEAR, NEXT_FLOOR)
    }
    
    elapsed <- if (ph == cur && st <= CUT_YEAR) max(0, CUT_YEAR - st + 1) else 0
    
    if (ph == "licensing") {
      
      tot <- licensing_duration_with_rules(row, scenario, elapsed)
      en_draw <- st + tot
      
      if (elapsed > 0 && en_draw < CUT_YEAR) en_draw <- CUT_YEAR
      
      lic_exempt <- row$design %in% licensing_failure_exempt_designs
      
      if (!lic_exempt) {
        p_surv <- get_surv_phase("licensing", scenario, row)
        if (runif(1) > p_surv) {
          return(tibble(
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
          ))
        }
      }
      
      nxt <- "construction"
      nxt_start <- max(NEXT_FLOOR, en_draw)
      
      ov <- apply_next_override(nxt, cond)
      if (!is.na(ov)) nxt_start <- ov
      
      lic_e <- nxt_start
      con_s <- nxt_start
      
      next
    }
    
    tot <- draw_total_duration(ph, scenario, elapsed)
    en <- st + tot
    
    if (elapsed > 0 && en < CUT_YEAR) en <- CUT_YEAR
    
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
        return(tibble(
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
        ))
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
    
    if (is.na(get_start(nxt))) set_start(nxt, nxt_start)
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

mc_baseline_55 <- run_mc("baseline", SIMS)
mc_accel_55    <- run_mc("accelerated", SIMS)

# -------------------- SUMMARY TABLES --------------------
summ_bas <- mc_baseline_55 %>%
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

summ_acc <- mc_accel_55 %>%
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

summary_quantiles <- dplyr::bind_rows(summ_bas, summ_acc) %>%
  dplyr::select(design, scenario, n_total, n_success, success_rate, P10, P50, P90) %>%
  dplyr::arrange(scenario, design)

make_descriptives <- function(df, scenario_name) {
  df %>%
    dplyr::group_by(design) %>%
    dplyr::summarise(
      n = sum(is.finite(foak_year)),
      mean = safe_mean(foak_year),
      sd = safe_sd(foak_year),
      min = safe_min(foak_year),
      q1 = safe_q(foak_year, 0.25),
      p10 = safe_q(foak_year, 0.10),
      p50 = safe_q(foak_year, 0.50),
      q3 = safe_q(foak_year, 0.75),
      p90 = safe_q(foak_year, 0.90),
      max = safe_max(foak_year),
      .groups = "drop"
    ) %>%
    dplyr::mutate(scenario = scenario_name) %>%
    dplyr::select(design, scenario, n, mean, sd, min, q1, p10, p50, q3, p90, max)
}

summary_descriptives <- dplyr::bind_rows(
  make_descriptives(mc_baseline_55, "baseline"),
  make_descriptives(mc_accel_55, "accelerated")
) %>%
  dplyr::arrange(scenario, design)

run_metadata <- tibble::tibble(
  item = c(
    "timestamp",
    "seed",
    "sims",
    "cut_year",
    "next_floor",
    "input_workbook",
    "sheet_smr_design_inputs",
    "sheet_phase_rules",
    "sheet_design_phase_failures",
    "sheet_construction_cascade_failures",
    "run_directory",
    "latest_directory",
    "model_type",
    "repository_note"
  ),
  value = c(
    stamp,
    as.character(SEED),
    as.character(SIMS),
    as.character(CUT_YEAR),
    as.character(NEXT_FLOOR),
    P_INPUT,
    "smr_design_inputs",
    "phase_rules",
    "design_phase_failures",
    "construction_cascade_failures",
    RUN_DIR,
    LATEST_DIR,
    MODEL_TAG,
    "GitHub/manuscript-scale run uses SIMS <- 10000L. For a quick smoke test, temporarily set SIMS <- 25L."
  )
)

run_metadata <- dplyr::bind_rows(
  run_metadata,
  tibble::tibble(
    item = c(
      "path_mode",
      "repository_root",
      "script_path",
      "output_directory"
    ),
    value = c(
      PATH_MODE,
      ROOT_DIR,
      ifelse(is.na(SCRIPT_PATH), NA_character_, SCRIPT_PATH),
      OUTDIR
    )
  )
)

# -------------------- EXPORT EXCEL --------------------
xlsx_path_run    <- file.path(RUN_DIR, "mc_outputs.xlsx")
xlsx_path_latest <- file.path(LATEST_DIR, "mc_outputs_latest.xlsx")

xlsx_contents <- list(
  baseline_draws = mc_baseline_55,
  accelerated_draws = mc_accel_55,
  summary_quantiles = summary_quantiles,
  summary_descriptives = summary_descriptives,
  run_metadata = run_metadata
)

writexl::write_xlsx(xlsx_contents, path = xlsx_path_run)
writexl::write_xlsx(xlsx_contents, path = xlsx_path_latest)

# -------------------- SAVE R OBJECTS --------------------
saveRDS(
  mc_baseline_55,
  file = file.path(RUN_DIR, "mc_baseline_55.rds")
)

saveRDS(
  mc_accel_55,
  file = file.path(RUN_DIR, "mc_accel_55.rds")
)

saveRDS(
  summary_quantiles,
  file = file.path(RUN_DIR, "summary_quantiles.rds")
)

saveRDS(
  summary_descriptives,
  file = file.path(RUN_DIR, "summary_descriptives.rds")
)

saveRDS(
  smr_master,
  file = file.path(RUN_DIR, "smr_master.rds")
)

saveRDS(
  phase_params,
  file = file.path(RUN_DIR, "phase_params.rds")
)

saveRDS(
  run_metadata,
  file = file.path(RUN_DIR, "run_metadata.rds")
)

saveRDS(mc_baseline_55,       file = file.path(LATEST_DIR, "mc_baseline_55_latest.rds"))
saveRDS(mc_accel_55,          file = file.path(LATEST_DIR, "mc_accel_55_latest.rds"))
saveRDS(summary_quantiles,    file = file.path(LATEST_DIR, "summary_quantiles_latest.rds"))
saveRDS(summary_descriptives, file = file.path(LATEST_DIR, "summary_descriptives_latest.rds"))
saveRDS(smr_master,           file = file.path(LATEST_DIR, "smr_master_latest.rds"))
saveRDS(phase_params,         file = file.path(LATEST_DIR, "phase_params_latest.rds"))
saveRDS(run_metadata,         file = file.path(LATEST_DIR, "run_metadata_latest.rds"))

model_bundle <- list(
  mc_baseline_55 = mc_baseline_55,
  mc_accel_55 = mc_accel_55,
  summary_quantiles = summary_quantiles,
  summary_descriptives = summary_descriptives,
  smr_master = smr_master,
  smr55_specs = smr55_specs,
  phase_rules = phase_rules,
  fail_rates = fail_rates,
  dpf = dpf,
  phase_params = phase_params,
  param_map = param_map,
  design_list = design_list,
  run_metadata = run_metadata
)

saveRDS(
  model_bundle,
  file = file.path(RUN_DIR, "bundle.rds")
)

saveRDS(
  model_bundle,
  file = file.path(LATEST_DIR, "bundle_latest.rds")
)

save(
  mc_baseline_55,
  mc_accel_55,
  summary_quantiles,
  summary_descriptives,
  smr55_specs,
  phase_rules,
  fail_rates,
  dpf,
  smr_master,
  phase_params,
  param_map,
  design_list,
  run_metadata,
  file = file.path(RUN_DIR, "workspace.RData")
)

save(
  mc_baseline_55,
  mc_accel_55,
  summary_quantiles,
  summary_descriptives,
  smr55_specs,
  phase_rules,
  fail_rates,
  dpf,
  smr_master,
  phase_params,
  param_map,
  design_list,
  run_metadata,
  file = file.path(LATEST_DIR, "workspace_latest.RData")
)

# -------------------- POST-RUN SUMMARY SWEEP --------------------
mc_baseline <- mc_baseline_55 %>% dplyr::left_join(smr55_specs, by = "design")
mc_accel    <- mc_accel_55    %>% dplyr::left_join(smr55_specs, by = "design")
design_summary <- summary_quantiles

build_ts <- function(mc_obj, grouping_vars = NULL) {
  
  df <- mc_obj %>%
    dplyr::mutate(
      foak_year_int = floor(foak_year),
      success = !is.na(foak_year_int)
    )
  
  results <- lapply(YEARS, function(y) {
    
    tmp <- df %>%
      dplyr::filter(success, foak_year_int <= y)
    
    if (!is.null(grouping_vars)) {
      tmp <- tmp %>%
        dplyr::group_by(sim, dplyr::across(dplyr::all_of(grouping_vars))) %>%
        dplyr::summarise(total = dplyr::n(), .groups = "drop")
    } else {
      tmp <- tmp %>%
        dplyr::group_by(sim) %>%
        dplyr::summarise(total = dplyr::n(), .groups = "drop")
    }
    
    tmp$year <- y
    tmp
  })
  
  ts_full <- dplyr::bind_rows(results)
  
  if (!is.null(grouping_vars)) {
    ts_full %>%
      dplyr::group_by(year, dplyr::across(dplyr::all_of(grouping_vars))) %>%
      dplyr::summarise(
        P10 = safe_q(total, 0.10),
        P50 = safe_q(total, 0.50),
        P90 = safe_q(total, 0.90),
        mean = safe_mean(total),
        .groups = "drop"
      )
  } else {
    ts_full %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(
        P10 = safe_q(total, 0.10),
        P50 = safe_q(total, 0.50),
        P90 = safe_q(total, 0.90),
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
      dplyr::summarise(total = sum(success_2050), .groups = "drop")
  } else {
    df <- df %>%
      dplyr::group_by(sim) %>%
      dplyr::summarise(total = sum(success_2050), .groups = "drop")
  }
  
  if (!is.null(grouping_vars)) {
    df %>%
      dplyr::group_by(dplyr::across(-c(sim, total))) %>%
      dplyr::summarise(
        P10 = safe_q(total, 0.10),
        P50 = safe_q(total, 0.50),
        P90 = safe_q(total, 0.90),
        mean = safe_mean(total),
        .groups = "drop"
      )
  } else {
    df %>%
      dplyr::summarise(
        P10 = safe_q(total, 0.10),
        P50 = safe_q(total, 0.50),
        P90 = safe_q(total, 0.90),
        mean = safe_mean(total)
      )
  }
}

baseline_global     <- build_ts(mc_baseline)
accelerated_global  <- build_ts(mc_accel)

baseline_oecd       <- build_ts(mc_baseline, "oecd")
accelerated_oecd    <- build_ts(mc_accel, "oecd")

baseline_water      <- build_ts(mc_baseline, "water_cooled")
accelerated_water   <- build_ts(mc_accel, "water_cooled")

baseline_cooling    <- build_ts(mc_baseline, "cooling_type")
accelerated_cooling <- build_ts(mc_accel, "cooling_type")

baseline_region     <- build_ts(mc_baseline, "region")
accelerated_region  <- build_ts(mc_accel, "region")

baseline_2050_global    <- compute_2050(mc_baseline)
accelerated_2050_global <- compute_2050(mc_accel)

baseline_2050_oecd      <- compute_2050(mc_baseline, "oecd")
accelerated_2050_oecd   <- compute_2050(mc_accel, "oecd")

baseline_2050_region    <- compute_2050(mc_baseline, "region")
accelerated_2050_region <- compute_2050(mc_accel, "region")

master_sweep_path_run    <- file.path(RUN_DIR, "master_summary_sweep.xlsx")
master_sweep_path_latest <- file.path(LATEST_DIR, "master_summary_sweep_latest.xlsx")
snapshot_2050_path_run   <- file.path(RUN_DIR, "snapshot_2050.xlsx")
snapshot_2050_path_latest <- file.path(LATEST_DIR, "snapshot_2050_latest.xlsx")

write_xlsx(
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
    accelerated_region = accelerated_region
  ),
  master_sweep_path_run
)

write_xlsx(
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
    accelerated_region = accelerated_region
  ),
  master_sweep_path_latest
)

write_xlsx(
  list(
    baseline_global_2050 = baseline_2050_global,
    accelerated_global_2050 = accelerated_2050_global,
    baseline_oecd_2050 = baseline_2050_oecd,
    accelerated_oecd_2050 = accelerated_2050_oecd,
    baseline_region_2050 = baseline_2050_region,
    accelerated_region_2050 = accelerated_2050_region
  ),
  snapshot_2050_path_run
)

write_xlsx(
  list(
    baseline_global_2050 = baseline_2050_global,
    accelerated_global_2050 = accelerated_2050_global,
    baseline_oecd_2050 = baseline_2050_oecd,
    accelerated_oecd_2050 = accelerated_2050_oecd,
    baseline_region_2050 = baseline_2050_region,
    accelerated_region_2050 = accelerated_2050_region
  ),
  snapshot_2050_path_latest
)

# -------------------- CONSOLE OUTPUT --------------------
cat("\n==================== ", RUN_TAG, " ", MODEL_TAG, " COMPLETE ====================\n", sep = "")
cat("Input workbook:", P_INPUT, "\n")
cat("Simulations:", SIMS, "\n")
cat("Baseline rows:", nrow(mc_baseline_55), "\n")
cat("Accelerated rows:", nrow(mc_accel_55), "\n")
cat("Excel saved (run):\n", xlsx_path_run, "\n")
cat("Excel saved (latest):\n", xlsx_path_latest, "\n")
cat("Master summary sweep (run):\n", master_sweep_path_run, "\n")
cat("Master summary sweep (latest):\n", master_sweep_path_latest, "\n")
cat("2050 snapshot (run):\n", snapshot_2050_path_run, "\n")
cat("2050 snapshot (latest):\n", snapshot_2050_path_latest, "\n")
cat("Bundle RDS saved:\n", file.path(LATEST_DIR, "bundle_latest.rds"), "\n")
cat("Workspace RData saved:\n", file.path(LATEST_DIR, "workspace_latest.RData"), "\n")
cat("For a quick smoke test, temporarily set SIMS <- 25L.\n")
cat("==========================================================================\n\n")

print(xlsx_path_run)
print(xlsx_path_latest)
print(master_sweep_path_run)
print(master_sweep_path_latest)
print(snapshot_2050_path_run)
print(snapshot_2050_path_latest)
print(file.path(LATEST_DIR, "bundle_latest.rds"))
print(file.path(LATEST_DIR, "workspace_latest.RData"))
