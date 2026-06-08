# ============================================================
# FUTURE DESIGN ADDITIONS — PROJECT PROGRESSION MONTE CARLO
# MODULE 14: POST-2025 CONSTRUCTION CASCADE SENSITIVITY
# BASELINE ONLY
# FAIL-GATED • FULL CLEAN SCRIPT • LOGNORMAL EVERYWHERE
#
# GitHub / submission version
#
# Required input workbook:
# data/Simulation_Inputs/post-2025_construction_cascade_baseline_sensitivity.xlsx
#
# Clean output directory:
# outputs/Simulation_Outputs/14_post2025_cascade_baseline/post2025_cascade_baseline_output_final/
#
# Construction cascade logic:
# Construction failure assumptions are read from the
# construction-cascade failure-rate inputs in the workbook.
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

# -------------------- REPOSITORY ROOT --------------------
normalise_path <- function(x) {
  normalizePath(x, winslash = "/", mustWork = FALSE)
}

get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  hit <- grep(file_arg, cmd_args, value = TRUE)
  
  if (length(hit) > 0) {
    return(normalise_path(sub(file_arg, "", hit[[1]])))
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
    
    if (identical(parent, d)) break
    
    d <- parent
  }
  
  unique(dirs)
}

find_repo_root <- function(start_dirs = getwd()) {
  target_file <- file.path(
    "data",
    "Simulation_Inputs",
    "post-2025_construction_cascade_baseline_sensitivity.xlsx"
  )
  
  start_dirs <- unique(start_dirs)
  start_dirs <- start_dirs[!is.na(start_dirs)]
  
  for (start_dir in start_dirs) {
    if (!dir.exists(start_dir)) next
    
    for (d in parent_dirs(start_dir)) {
      if (file.exists(file.path(d, target_file))) {
        return(normalise_path(d))
      }
    }
  }
  
  stop(
    paste0(
      "Repository root not found.\n",
      "Expected to find:\n",
      target_file,
      "\nCurrent working directory:\n",
      getwd()
    ),
    call. = FALSE
  )
}

SCRIPT_PATH <- get_script_path()
SCRIPT_DIR <- if (!is.na(SCRIPT_PATH)) dirname(SCRIPT_PATH) else getwd()

ROOT_DIR <- find_repo_root(c(getwd(), SCRIPT_DIR))
PATH_MODE <- "github_repo_layout"

# -------------------- PATHS --------------------
P_INPUT <- file.path(
  ROOT_DIR,
  "data",
  "Simulation_Inputs",
  "post-2025_construction_cascade_baseline_sensitivity.xlsx"
)

OUTDIR <- file.path(
  ROOT_DIR,
  "outputs",
  "Simulation_Outputs",
  "14_post2025_cascade_baseline",
  "post2025_cascade_baseline_output_final"
)

P_INPUT <- normalise_path(P_INPUT)
OUTDIR <- normalise_path(OUTDIR)

# -------------------- GLOBALS --------------------
SIMS       <- 10000L
SEED       <- 42L
SCENARIO   <- "baseline"
START_YEAR <- 2026L
END_YEAR   <- 2075L
YEARS      <- START_YEAR:END_YEAR

set.seed(SEED)

if (!file.exists(P_INPUT)) stop("Input workbook not found: ", P_INPUT)

if (!dir.exists(OUTDIR)) dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(OUTDIR)) stop("OUTDIR could not be created: ", OUTDIR)

stamp   <- format(Sys.time(), "%Y%m%d_%H%M")
RUN_TAG <- paste0("post2025_cascade_baseline_mc_", SIMS)

RUN_DIR <- file.path(
  OUTDIR,
  paste0("run_", SIMS, "_", stamp)
)

LATEST_DIR <- file.path(
  OUTDIR,
  "latest"
)

dir.create(RUN_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LATEST_DIR, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(RUN_DIR)) stop("RUN_DIR could not be created: ", RUN_DIR, call. = FALSE)
if (!dir.exists(LATEST_DIR)) stop("LATEST_DIR could not be created: ", LATEST_DIR, call. = FALSE)

# -------------------- HELPERS --------------------
clean_names_simple <- function(x){
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

as_phase <- function(x){
  str_to_lower(str_replace_all(str_trim(as.character(x)), "[ -]", "_"))
}

as_scen <- function(x){
  str_to_lower(str_trim(as.character(x)))
}

clamp01 <- function(x){
  x <- suppressWarnings(as.numeric(x))
  pmin(1, pmax(0, x))
}

safe_q <- function(x, p){
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, type = 7))
}

safe_mean <- function(x){
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

std_country <- function(x){
  x <- trimws(as.character(x))
  case_when(
    x %in% c("US", "U.S.", "U.S.A.", "USA", "United States of America") ~ "United States",
    x %in% c("Russian Federation") ~ "Russia",
    x %in% c("ROW", "RoW", "Rest of World", "Rest-of-World") ~ "RoW",
    TRUE ~ x
  )
}

region_from_country <- function(country){
  country <- std_country(country)
  case_when(
    country %in% c("Canada", "United States") ~ "North America",
    country == "Europe" ~ "Europe",
    country == "OECD Asia" ~ "OECD Asia",
    country == "China" ~ "China",
    country == "Russia" ~ "Russia",
    country == "RoW" ~ "RoW",
    TRUE ~ NA_character_
  )
}

oecd_from_country <- function(country){
  country <- std_country(country)
  country %in% c("Canada", "United States", "Europe", "OECD Asia")
}

std_cooling <- function(x){
  x <- tolower(trimws(as.character(x)))
  case_when(
    x %in% c("gas", "gas_cooled", "gas-cooled") ~ "gas",
    x %in% c("metal", "metal_cooled", "metal-cooled", "lead", "sodium", "lead_bismuth", "lead-bismuth", "liquid_metal", "liquid-metal") ~ "metal",
    x %in% c("salt", "salt_cooled", "salt-cooled", "molten_salt", "molten-salt", "molten salt") ~ "salt",
    x %in% c("water", "water_cooled", "water-cooled") ~ "water",
    TRUE ~ x
  )
}

water_from_cooling <- function(x){
  std_cooling(x) == "water"
}

water_class_from_flag <- function(x){
  ifelse(x, "water_cooled", "non_water_cooled")
}

empty_draw_tbl <- function(){
  tibble(
    sim = integer(),
    design = character(),
    scenario = character(),
    entry_year = numeric(),
    country = character(),
    region = character(),
    oecd = logical(),
    geopolitical_group = character(),
    cooling_type = character(),
    water_cooled = logical(),
    water_class = character(),
    status = character(),
    fail_phase = character(),
    cd_start = numeric(),
    bd_start = numeric(),
    dd_start = numeric(),
    licensing_start = numeric(),
    construction_start = numeric(),
    operational_start = numeric(),
    foak_year = numeric()
  )
}

# -------------------- LOAD DESIGN-PHASE FAILURES --------------------
dpf <- read_excel(P_INPUT, sheet = "design_phase_failures")
names(dpf) <- clean_names_simple(names(dpf))

dpf <- dpf %>%
  transmute(
    phase = as_phase(phase),
    scenario = as_scen(scenario),
    k = suppressWarnings(as.numeric(failures_k)),
    N = suppressWarnings(as.numeric(total_n))
  ) %>%
  filter(scenario == SCENARIO) %>%
  mutate(
    p_fail_mean = (k + 0.5) / (N + 1),
    p_surv_mean = clamp01(1 - p_fail_mean)
  )

req_phases <- c("conceptual_design", "basic_design", "detailed_design")
miss_phases <- setdiff(req_phases, unique(dpf$phase))
if (length(miss_phases) > 0) {
  stop("Missing phases in design_phase_failures for baseline: ", paste(miss_phases, collapse = ", "))
}

jeffreys_survival <- setNames(dpf$p_surv_mean, dpf$phase)

# -------------------- LOAD COUNTRY FAILURE RATES --------------------
fail_rates <- read_excel(P_INPUT, sheet = "failures_region_country")
names(fail_rates) <- clean_names_simple(names(fail_rates))

fail_rates <- fail_rates %>%
  transmute(
    country = std_country(country),
    oecd = if ("oecd" %in% names(.)) as.logical(oecd) else oecd_from_country(country),
    licensing_failure_baseline    = clamp01(baseline_licensing_failure),
    construction_failure_baseline = clamp01(baseline_construction_failure)
  ) %>%
  mutate(
    region = region_from_country(country),
    lic_surv_baseline = clamp01(1 - licensing_failure_baseline),
    con_surv_baseline = clamp01(1 - construction_failure_baseline)
  ) %>%
  select(country, region, oecd, lic_surv_baseline, con_surv_baseline)

if (any(is.na(fail_rates$country))) stop("Missing country values in failures_region_country sheet")
if (any(is.na(fail_rates$region))) stop("Missing derived region values in failures_region_country sheet")

# -------------------- LOAD COUNTRY ADDITIONS RATES --------------------
country_rates <- read_excel(P_INPUT, sheet = "additions_region_country")
names(country_rates) <- clean_names_simple(names(country_rates))

country_rates <- country_rates %>%
  transmute(
    country = std_country(country),
    oecd = if ("oecd" %in% names(.)) as.logical(oecd) else oecd_from_country(country),
    baseline = suppressWarnings(as.numeric(baseline))
  ) %>%
  filter(
    !is.na(country),
    !country %in% c("Global", "OECD", "Non-OECD")
  ) %>%
  mutate(region = region_from_country(country)) %>%
  select(country, region, oecd, baseline)

# -------------------- ALIGN COUNTRY SPACE --------------------
valid_countries <- intersect(country_rates$country, fail_rates$country)

country_rates <- country_rates %>% filter(country %in% valid_countries)
fail_rates    <- fail_rates %>% filter(country %in% valid_countries)

if (length(valid_countries) == 0) {
  stop("No overlapping country values across additions_region_country and failures_region_country sheets")
}

# -------------------- LOAD ADDITIONS PHASE RULES --------------------
rules_add <- read_excel(P_INPUT, sheet = "phase_rules_additions")
names(rules_add) <- clean_names_simple(names(rules_add))

rules_add <- rules_add %>%
  transmute(
    scenario = as_scen(scenario),
    phase = if ("phase" %in% names(.)) as.character(phase) else "additions",
    start_year = suppressWarnings(as.integer(start_year)),
    pivot_year = suppressWarnings(as.integer(pivot_year)),
    end_year = suppressWarnings(as.integer(end_year)),
    lambda_start = suppressWarnings(as.numeric(lambda_start)),
    lambda_2050 = suppressWarnings(as.numeric(lambda_2050)),
    retain_share_2050 = if ("retain_share_2050" %in% names(.)) suppressWarnings(as.numeric(retain_share_2050)) else NA_real_,
    rule = if ("rule" %in% names(.)) as.character(rule) else NA_character_
  ) %>%
  filter(scenario == SCENARIO)

if (nrow(rules_add) == 0) stop("No baseline row found in phase_rules_additions sheet")

# -------------------- LOAD COUNTRY COOLING SHARES --------------------
cooling_shares <- read_excel(P_INPUT, sheet = "design_additions_cooling")
names(cooling_shares) <- clean_names_simple(names(cooling_shares))

cooling_shares <- cooling_shares %>%
  transmute(
    country = std_country(country),
    oecd = if ("oecd" %in% names(.)) as.logical(oecd) else oecd_from_country(country),
    period = as_scen(period),
    cooling_type = std_cooling(cooling),
    jeffreys_share = suppressWarnings(as.numeric(jeffreys_share))
  ) %>%
  filter(
    country %in% valid_countries,
    period == SCENARIO
  ) %>%
  group_by(country, period, cooling_type) %>%
  summarise(
    oecd = first(oecd),
    jeffreys_share = first(jeffreys_share),
    .groups = "drop"
  ) %>%
  group_by(country, period) %>%
  mutate(prob = jeffreys_share / sum(jeffreys_share, na.rm = TRUE)) %>%
  ungroup()

# -------------------- LOAD COOLING-TYPE FAILURE PENALTIES --------------------
cooling_fail <- read_excel(P_INPUT, sheet = "cooling_type_failure")
names(cooling_fail) <- clean_names_simple(names(cooling_fail))

cooling_fail <- cooling_fail %>%
  transmute(
    cooling_type = std_cooling(if ("cooling_type" %in% names(.)) cooling_type else cooling),
    baseline_licensing_penalty    = clamp01(if ("baseline_licensing_failure" %in% names(.)) baseline_licensing_failure else 0),
    baseline_construction_penalty = clamp01(if ("baseline_construction_failure" %in% names(.)) baseline_construction_failure else 0)
  ) %>%
  group_by(cooling_type) %>%
  summarise(
    baseline_licensing_penalty    = first(baseline_licensing_penalty),
    baseline_construction_penalty = first(baseline_construction_penalty),
    .groups = "drop"
  )

req_cooling <- c("water", "gas", "metal", "salt")
miss_cooling <- setdiff(req_cooling, cooling_fail$cooling_type)
if (length(miss_cooling) > 0) {
  stop("Missing cooling types in cooling_type_failure sheet: ", paste(miss_cooling, collapse = ", "))
}

# -------------------- PHASE PARAMETERS --------------------
phase_params <- tibble::tribble(
  ~phase,              ~scenario,       ~mean,     ~sd,     ~min,  ~max,
  "conceptual_design", "baseline",       6.66,     3.46,    2.0,   15.0,
  "basic_design",      "baseline",       4.76,     3.10,    2.0,   14.0,
  "detailed_design",   "baseline",       4.22,     2.13,    2.0,   9.0,
  "licensing",         "baseline",       1.46879,  0.2867,  2.0,   8.0,
  "licensing",         "accelerated",    0.8405,   0.1432,  1.0,   3.0,
  "construction",      "baseline",       1.64149,  0.2064,  3.0,   7.5
) %>%
  mutate(
    phase = as_phase(phase),
    scenario = as_scen(scenario)
  )

param_map <- phase_params %>%
  mutate(key = paste0(phase, "__", scenario)) %>%
  split(.$key) %>%
  purrr::map(~ list(mean = .x$mean[[1]], sd = .x$sd[[1]], min = .x$min[[1]], max = .x$max[[1]]))

get_p <- function(phase, scenario){
  k <- paste0(as_phase(phase), "__", as_scen(scenario))
  if (!k %in% names(param_map)) stop("Missing phase params for: ", k, call. = FALSE)
  param_map[[k]]
}

# -------------------- DISTRIBUTION HELPERS --------------------
lognorm_from_mean_sd <- function(mean, sd){
  v <- log(1 + (sd^2) / (mean^2))
  list(meanlog = log(mean) - 0.5 * v, sdlog = sqrt(v))
}

r_lnorm_trunc_cond_total <- function(elapsed, a, b, meanlog, sdlog){
  elapsed <- max(0, elapsed)
  lo <- max(a, elapsed)
  if (lo >= b) return(lo)
  
  Fa <- plnorm(lo, meanlog, sdlog)
  Fb <- plnorm(b, meanlog, sdlog)
  
  if (is.na(Fa) || is.na(Fb) || Fb <= Fa) return(lo)
  
  qlnorm(runif(1, Fa, Fb), meanlog, sdlog)
}

draw_total_duration <- function(phase, scenario, elapsed = 0){
  phase <- as_phase(phase)
  scenario <- as_scen(scenario)
  p <- get_p(phase, scenario)
  
  if (!is.na(elapsed) && elapsed >= p$max) return(elapsed)
  
  if (phase %in% c("licensing", "construction")){
    return(r_lnorm_trunc_cond_total(elapsed, p$min, p$max, p$mean, p$sd))
  }
  
  lp <- lognorm_from_mean_sd(p$mean, p$sd)
  r_lnorm_trunc_cond_total(elapsed, p$min, p$max, lp$meanlog, lp$sdlog)
}

# -------------------- LAMBDA PATH --------------------
lambda_path_from_rule <- function(rule_row, years_vec){
  lambda_start <- as.numeric(rule_row$lambda_start[[1]])
  lambda_2050  <- as.numeric(rule_row$lambda_2050[[1]])
  pivot_year   <- as.integer(rule_row$pivot_year[[1]])
  
  years_decline <- 2050 - pivot_year
  k <- -log(lambda_2050 / lambda_start) / years_decline
  
  sapply(years_vec, function(y){
    if (y <= pivot_year) {
      lambda_start
    } else {
      lambda_start * exp(-k * (y - pivot_year))
    }
  })
}

lambda_path <- lambda_path_from_rule(rules_add, YEARS)

# -------------------- COUNTRY / COOLING WEIGHTS --------------------
country_weight_tbl <- country_rates %>%
  transmute(country, region, oecd, scenario = SCENARIO, weight = baseline) %>%
  group_by(scenario) %>%
  mutate(prob = weight / sum(weight, na.rm = TRUE)) %>%
  ungroup()

get_country_probs <- function(scenario){
  country_weight_tbl %>%
    filter(scenario == as_scen(scenario)) %>%
    arrange(country)
}

get_cooling_probs <- function(country, scenario){
  out <- cooling_shares %>%
    filter(country == !!country, period == as_scen(scenario)) %>%
    arrange(cooling_type)
  
  if (nrow(out) == 0){
    out <- tibble(
      country = country,
      oecd = oecd_from_country(country),
      period = as_scen(scenario),
      cooling_type = c("gas", "metal", "salt", "water"),
      jeffreys_share = c(0.25, 0.25, 0.25, 0.25),
      prob = c(0.25, 0.25, 0.25, 0.25)
    )
  }
  
  out
}

# -------------------- FAILURE LOOKUPS --------------------
apply_cooling_penalty_to_survival <- function(base_surv, cooling_type, phase, scenario){
  cooling_type <- std_cooling(cooling_type)
  phase <- as_phase(phase)
  scenario <- as_scen(scenario)
  
  pen_row <- cooling_fail %>% filter(cooling_type == !!cooling_type)
  if (nrow(pen_row) == 0) return(clamp01(base_surv))
  
  base_fail <- clamp01(1 - base_surv)
  
  penalty_fail <- 0
  if (phase == "licensing"    && scenario == "baseline") penalty_fail <- pen_row$baseline_licensing_penalty[[1]]
  if (phase == "construction" && scenario == "baseline") penalty_fail <- pen_row$baseline_construction_penalty[[1]]
  
  adj_fail <- clamp01(base_fail + penalty_fail)
  clamp01(1 - adj_fail)
}

get_surv_phase <- function(phase, row){
  phase <- as_phase(phase)
  
  if (phase %in% c("conceptual_design", "basic_design", "detailed_design")){
    return(clamp01(jeffreys_survival[[phase]]))
  }
  
  if (phase == "licensing"){
    return(apply_cooling_penalty_to_survival(row$lic_surv_baseline, row$cooling_type, phase, SCENARIO))
  }
  
  if (phase == "construction"){
    return(apply_cooling_penalty_to_survival(row$con_surv_baseline, row$cooling_type, phase, SCENARIO))
  }
  
  1.0
}

licensing_duration_with_country_rules <- function(row, elapsed = 0){
  ctry <- as.character(row$country)
  
  if (ctry %in% c("China", "Russia")){
    return(draw_total_duration("licensing", "accelerated", elapsed))
  }
  
  draw_total_duration("licensing", "baseline", elapsed)
}

# -------------------- GENERATE SYNTHETIC ADDITIONS --------------------
generate_additions_for_sim <- function(scenario, sim_id, years_vec, lambda_path){
  ctry_probs <- get_country_probs(scenario)
  
  out <- vector("list", length(years_vec))
  idx <- 1L
  
  for (j in seq_along(years_vec)){
    y <- years_vec[j]
    n_new <- rpois(1, lambda_path[j])
    
    if (n_new == 0) next
    
    draw_idx <- sample(
      seq_len(nrow(ctry_probs)),
      size = n_new,
      replace = TRUE,
      prob = ctry_probs$prob
    )
    
    draw_countries <- ctry_probs$country[draw_idx]
    draw_oecd      <- ctry_probs$oecd[draw_idx]
    draw_regions   <- ctry_probs$region[draw_idx]
    
    cooling_draws <- map_chr(draw_countries, function(cc){
      cool_tbl <- get_cooling_probs(cc, scenario)
      sample(cool_tbl$cooling_type, size = 1, prob = cool_tbl$prob)
    })
    
    tmp <- tibble(
      sim = sim_id,
      design = sprintf("BAS_CC_%05d_%04d_%03d", sim_id, y, seq_len(n_new)),
      scenario = scenario,
      entry_year = as.numeric(y),
      phase = "conceptual_design",
      country = draw_countries,
      region = draw_regions,
      oecd = draw_oecd,
      geopolitical_group = ifelse(draw_oecd, "OECD", "Non-OECD"),
      cooling_type = cooling_draws,
      water_cooled = water_from_cooling(cooling_draws),
      water_class = water_class_from_flag(water_from_cooling(cooling_draws)),
      cd_start = as.numeric(y),
      bd_start = NA_real_,
      dd_start = NA_real_,
      licensing_start = NA_real_,
      construction_start = NA_real_,
      operational_start = NA_real_
    ) %>%
      left_join(fail_rates, by = c("country", "region", "oecd"))
    
    out[[idx]] <- tmp
    idx <- idx + 1L
  }
  
  bind_rows(out)
}

# -------------------- SIMULATE ONE FUTURE DESIGN --------------------
simulate_future_to_foak <- function(row, scenario){
  phases <- c(
    "conceptual_design",
    "basic_design",
    "detailed_design",
    "licensing",
    "construction",
    "operational"
  )
  
  cd_s  <- row$cd_start
  bd_s  <- row$bd_start
  dd_s  <- row$dd_start
  lic_s <- row$licensing_start
  con_s <- row$construction_start
  op_s  <- row$operational_start
  
  get_start <- function(phase){
    if (phase == "conceptual_design") return(cd_s)
    if (phase == "basic_design")      return(bd_s)
    if (phase == "detailed_design")   return(dd_s)
    if (phase == "licensing")         return(lic_s)
    if (phase == "construction")      return(con_s)
    if (phase == "operational")       return(op_s)
    NA_real_
  }
  
  set_start <- function(phase, val){
    if (phase == "basic_design")      bd_s  <<- val
    if (phase == "detailed_design")   dd_s  <<- val
    if (phase == "licensing")         lic_s <<- val
    if (phase == "construction")      con_s <<- val
    if (phase == "operational")       op_s  <<- val
  }
  
  for (i in 1:(length(phases) - 1)){
    ph <- phases[i]
    st <- get_start(ph)
    
    if (is.na(st)) st <- row$entry_year
    
    if (ph == "licensing"){
      tot <- licensing_duration_with_country_rules(row, elapsed = 0)
    } else {
      tot <- draw_total_duration(ph, scenario, elapsed = 0)
    }
    
    en <- st + tot
    p_surv <- get_surv_phase(ph, row)
    
    if (runif(1) > p_surv){
      return(tibble(
        sim = row$sim,
        design = row$design,
        scenario = scenario,
        entry_year = row$entry_year,
        country = row$country,
        region = row$region,
        oecd = row$oecd,
        geopolitical_group = row$geopolitical_group,
        cooling_type = row$cooling_type,
        water_cooled = row$water_cooled,
        water_class = row$water_class,
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
    
    nxt <- phases[i + 1]
    
    if (nxt == "operational"){
      op_s <- en
      break
    } else {
      set_start(nxt, en)
    }
  }
  
  tibble(
    sim = row$sim,
    design = row$design,
    scenario = scenario,
    entry_year = row$entry_year,
    country = row$country,
    region = row$region,
    oecd = row$oecd,
    geopolitical_group = row$geopolitical_group,
    cooling_type = row$cooling_type,
    water_cooled = row$water_cooled,
    water_class = row$water_class,
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

# -------------------- RUN MONTE CARLO --------------------
run_future_mc <- function(scenario, sims, years_vec, lambda_path){
  map_dfr(seq_len(sims), function(s){
    adds <- generate_additions_for_sim(scenario, s, years_vec, lambda_path)
    
    if (nrow(adds) == 0){
      return(empty_draw_tbl())
    }
    
    map_dfr(seq_len(nrow(adds)), function(i){
      simulate_future_to_foak(adds[i, ], scenario)
    })
  })
}

set.seed(SEED)
mc_baseline <- run_future_mc(SCENARIO, SIMS, YEARS, lambda_path)
mc_all <- mc_baseline

# -------------------- SUMMARY HELPERS --------------------
compute_cutoff_counts <- function(df, cutoff_year, value = c("entry_year", "foak_year")){
  value <- match.arg(value)
  
  tmp <- df %>%
    mutate(
      ok = if (value == "entry_year") {
        !is.na(entry_year) & entry_year <= cutoff_year
      } else {
        !is.na(foak_year) & floor(foak_year) <= cutoff_year
      }
    ) %>%
    group_by(sim, scenario) %>%
    summarise(total = sum(ok), .groups = "drop")
  
  full_grid <- expand.grid(
    sim = seq_len(SIMS),
    scenario = SCENARIO,
    stringsAsFactors = FALSE
  )
  
  tmp <- full_grid %>%
    left_join(tmp, by = c("sim", "scenario")) %>%
    mutate(total = ifelse(is.na(total), 0, total))
  
  tmp %>%
    group_by(scenario) %>%
    summarise(
      Mean = safe_mean(total),
      P10  = safe_q(total, 0.10),
      P50  = safe_q(total, 0.50),
      P90  = safe_q(total, 0.90),
      .groups = "drop"
    ) %>%
    mutate(
      cutoff_year = cutoff_year,
      metric = ifelse(value == "entry_year", "cumulative_additions", "cumulative_foaks")
    ) %>%
    select(scenario, cutoff_year, metric, Mean, P10, P50, P90)
}

compute_cutoff_counts_by_group <- function(df, cutoff_year, value = c("entry_year", "foak_year"), group_vars, metric_name){
  value <- match.arg(value)
  group_vars <- as.character(group_vars)
  
  tmp <- df %>%
    mutate(
      ok = if (value == "entry_year") {
        !is.na(entry_year) & entry_year <= cutoff_year
      } else {
        !is.na(foak_year) & floor(foak_year) <= cutoff_year
      }
    ) %>%
    group_by(sim, scenario, across(all_of(group_vars))) %>%
    summarise(total = sum(ok), .groups = "drop")
  
  group_levels <- lapply(group_vars, function(g) sort(unique(df[[g]])))
  names(group_levels) <- group_vars
  
  full_grid <- expand.grid(
    c(list(sim = seq_len(SIMS), scenario = SCENARIO), group_levels),
    stringsAsFactors = FALSE
  )
  
  tmp <- full_grid %>%
    left_join(tmp, by = c("sim", "scenario", group_vars)) %>%
    mutate(total = ifelse(is.na(total), 0, total))
  
  tmp %>%
    group_by(scenario, across(all_of(group_vars))) %>%
    summarise(
      Mean = safe_mean(total),
      P10  = safe_q(total, 0.10),
      P50  = safe_q(total, 0.50),
      P90  = safe_q(total, 0.90),
      .groups = "drop"
    ) %>%
    mutate(
      cutoff_year = cutoff_year,
      metric = metric_name
    ) %>%
    select(scenario, cutoff_year, all_of(group_vars), metric, Mean, P10, P50, P90)
}

build_annual_summary <- function(df){
  bind_rows(lapply(YEARS, function(y){
    tmp <- df %>%
      group_by(sim, scenario) %>%
      summarise(
        cumulative_additions = sum(!is.na(entry_year) & entry_year <= y),
        cumulative_foaks = sum(!is.na(foak_year) & floor(foak_year) <= y),
        .groups = "drop"
      )
    
    full_grid <- expand.grid(
      sim = seq_len(SIMS),
      scenario = SCENARIO,
      stringsAsFactors = FALSE
    )
    
    full_grid %>%
      left_join(tmp, by = c("sim", "scenario")) %>%
      mutate(
        cumulative_additions = ifelse(is.na(cumulative_additions), 0, cumulative_additions),
        cumulative_foaks = ifelse(is.na(cumulative_foaks), 0, cumulative_foaks)
      ) %>%
      group_by(scenario) %>%
      summarise(
        additions_mean = safe_mean(cumulative_additions),
        additions_p10  = safe_q(cumulative_additions, 0.10),
        additions_p50  = safe_q(cumulative_additions, 0.50),
        additions_p90  = safe_q(cumulative_additions, 0.90),
        foaks_mean     = safe_mean(cumulative_foaks),
        foaks_p10      = safe_q(cumulative_foaks, 0.10),
        foaks_p50      = safe_q(cumulative_foaks, 0.50),
        foaks_p90      = safe_q(cumulative_foaks, 0.90),
        .groups = "drop"
      ) %>%
      mutate(year = y) %>%
      select(year, everything())
  }))
}

build_group_annual_foaks <- function(df, group_vars){
  group_vars <- as.character(group_vars)
  
  bind_rows(lapply(YEARS, function(y){
    tmp <- df %>%
      group_by(sim, scenario, across(all_of(group_vars))) %>%
      summarise(
        cumulative_foaks = sum(!is.na(foak_year) & floor(foak_year) <= y),
        .groups = "drop"
      )
    
    group_levels <- lapply(group_vars, function(g) sort(unique(df[[g]])))
    names(group_levels) <- group_vars
    
    full_grid <- expand.grid(
      c(list(sim = seq_len(SIMS), scenario = SCENARIO), group_levels),
      stringsAsFactors = FALSE
    )
    
    full_grid %>%
      left_join(tmp, by = c("sim", "scenario", group_vars)) %>%
      mutate(cumulative_foaks = ifelse(is.na(cumulative_foaks), 0, cumulative_foaks)) %>%
      group_by(scenario, across(all_of(group_vars))) %>%
      summarise(
        Mean = safe_mean(cumulative_foaks),
        P10  = safe_q(cumulative_foaks, 0.10),
        P50  = safe_q(cumulative_foaks, 0.50),
        P90  = safe_q(cumulative_foaks, 0.90),
        .groups = "drop"
      ) %>%
      mutate(year = y) %>%
      select(year, scenario, all_of(group_vars), Mean, P10, P50, P90)
  }))
}

# -------------------- SUMMARIES --------------------
summary_quantiles <- bind_rows(
  compute_cutoff_counts(mc_all, 2050, "entry_year"),
  compute_cutoff_counts(mc_all, 2050, "foak_year"),
  compute_cutoff_counts(mc_all, 2075, "entry_year"),
  compute_cutoff_counts(mc_all, 2075, "foak_year")
) %>%
  arrange(scenario, cutoff_year, metric)

summary_geo <- bind_rows(
  compute_cutoff_counts_by_group(mc_all, 2050, "entry_year", "geopolitical_group", "cumulative_additions"),
  compute_cutoff_counts_by_group(mc_all, 2050, "foak_year", "geopolitical_group", "cumulative_foaks"),
  compute_cutoff_counts_by_group(mc_all, 2075, "entry_year", "geopolitical_group", "cumulative_additions"),
  compute_cutoff_counts_by_group(mc_all, 2075, "foak_year", "geopolitical_group", "cumulative_foaks")
) %>%
  arrange(scenario, cutoff_year, geopolitical_group, metric)

summary_country <- bind_rows(
  compute_cutoff_counts_by_group(mc_all, 2050, "entry_year", "country", "cumulative_additions"),
  compute_cutoff_counts_by_group(mc_all, 2050, "foak_year", "country", "cumulative_foaks"),
  compute_cutoff_counts_by_group(mc_all, 2075, "entry_year", "country", "cumulative_additions"),
  compute_cutoff_counts_by_group(mc_all, 2075, "foak_year", "country", "cumulative_foaks")
) %>%
  arrange(scenario, cutoff_year, country, metric)

summary_region <- bind_rows(
  compute_cutoff_counts_by_group(mc_all, 2050, "entry_year", "region", "cumulative_additions"),
  compute_cutoff_counts_by_group(mc_all, 2050, "foak_year", "region", "cumulative_foaks"),
  compute_cutoff_counts_by_group(mc_all, 2075, "entry_year", "region", "cumulative_additions"),
  compute_cutoff_counts_by_group(mc_all, 2075, "foak_year", "region", "cumulative_foaks")
) %>%
  arrange(scenario, cutoff_year, region, metric)

summary_cooling <- bind_rows(
  compute_cutoff_counts_by_group(mc_all, 2050, "entry_year", "cooling_type", "cumulative_additions"),
  compute_cutoff_counts_by_group(mc_all, 2050, "foak_year", "cooling_type", "cumulative_foaks"),
  compute_cutoff_counts_by_group(mc_all, 2075, "entry_year", "cooling_type", "cumulative_additions"),
  compute_cutoff_counts_by_group(mc_all, 2075, "foak_year", "cooling_type", "cumulative_foaks")
) %>%
  arrange(scenario, cutoff_year, cooling_type, metric)

summary_water <- bind_rows(
  compute_cutoff_counts_by_group(mc_all, 2050, "entry_year", "water_class", "cumulative_additions"),
  compute_cutoff_counts_by_group(mc_all, 2050, "foak_year", "water_class", "cumulative_foaks"),
  compute_cutoff_counts_by_group(mc_all, 2075, "entry_year", "water_class", "cumulative_additions"),
  compute_cutoff_counts_by_group(mc_all, 2075, "foak_year", "water_class", "cumulative_foaks")
) %>%
  arrange(scenario, cutoff_year, water_class, metric)

summary_region_cooling <- bind_rows(
  compute_cutoff_counts_by_group(mc_all, 2050, "entry_year", c("region", "cooling_type"), "cumulative_additions"),
  compute_cutoff_counts_by_group(mc_all, 2050, "foak_year", c("region", "cooling_type"), "cumulative_foaks"),
  compute_cutoff_counts_by_group(mc_all, 2075, "entry_year", c("region", "cooling_type"), "cumulative_additions"),
  compute_cutoff_counts_by_group(mc_all, 2075, "foak_year", c("region", "cooling_type"), "cumulative_foaks")
) %>%
  arrange(scenario, cutoff_year, region, cooling_type, metric)

summary_descriptives <- build_annual_summary(mc_all) %>%
  arrange(scenario, year)

annual_geo <- build_group_annual_foaks(mc_all, "geopolitical_group") %>%
  arrange(scenario, geopolitical_group, year)

annual_country <- build_group_annual_foaks(mc_all, "country") %>%
  arrange(scenario, country, year)

annual_region <- build_group_annual_foaks(mc_all, "region") %>%
  arrange(scenario, region, year)

annual_cooling <- build_group_annual_foaks(mc_all, "cooling_type") %>%
  arrange(scenario, cooling_type, year)

annual_water <- build_group_annual_foaks(mc_all, "water_class") %>%
  arrange(scenario, water_class, year)

annual_region_cooling <- build_group_annual_foaks(mc_all, c("region", "cooling_type")) %>%
  arrange(scenario, region, cooling_type, year)

run_metadata <- tibble::tibble(
  item = c(
    "timestamp",
    "seed",
    "sims",
    "scenario",
    "start_year",
    "end_year",
    "input_workbook",
    "sheet_phase_rules_additions",
    "sheet_additions_region_country",
    "sheet_design_additions_cooling",
    "sheet_design_phase_failures",
    "sheet_failures_region_country",
    "sheet_cooling_type_failure",
    "run_directory",
    "latest_directory",
    "path_mode",
    "repository_root",
    "run_tag",
    "counterfactual_type"
  ),
  value = c(
    stamp,
    as.character(SEED),
    as.character(SIMS),
    SCENARIO,
    as.character(START_YEAR),
    as.character(END_YEAR),
    P_INPUT,
    "phase_rules_additions",
    "additions_region_country",
    "design_additions_cooling",
    "design_phase_failures",
    "failures_region_country",
    "cooling_type_failure",
    RUN_DIR,
    LATEST_DIR,
    PATH_MODE,
    ROOT_DIR,
    RUN_TAG,
    "construction_cascade"
  )
)

# -------------------- SAFE OUTPUT HELPERS --------------------
ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path)) {
    stop("Directory could not be created: ", path, call. = FALSE)
  }
  invisible(path)
}

ensure_parent_dir <- function(path) {
  ensure_dir(dirname(path))
}

safe_copy_file <- function(from, to) {
  ensure_parent_dir(to)
  ok <- file.copy(from, to, overwrite = TRUE)
  if (!ok || !file.exists(to)) {
    stop("File copy failed: ", to, call. = FALSE)
  }
  invisible(to)
}

safe_save_rds <- function(object, file) {
  ensure_parent_dir(file)
  saveRDS(object, file = file)
  if (!file.exists(file)) {
    stop("RDS save failed: ", file, call. = FALSE)
  }
  invisible(file)
}

ensure_dir(OUTDIR)
ensure_dir(RUN_DIR)
ensure_dir(LATEST_DIR)

# -------------------- EXPORT --------------------
xlsx_path_run <- file.path(
  RUN_DIR,
  paste0("post2025_cascade_baseline_mc_", SIMS, "_", stamp, ".xlsx")
)

xlsx_path_latest <- file.path(
  LATEST_DIR,
  "post2025_cascade_baseline_mc_latest.xlsx"
)

rds_path_run <- file.path(
  RUN_DIR,
  paste0("post2025_cascade_baseline_bundle_", SIMS, "_", stamp, ".rds")
)

rds_path_latest <- file.path(
  LATEST_DIR,
  "post2025_cascade_baseline_bundle_latest.rds"
)

xlsx_contents <- list(
  baseline_draws = mc_baseline,
  sum_quant      = summary_quantiles,
  sum_desc       = summary_descriptives,
  sum_geo        = summary_geo,
  sum_country    = summary_country,
  sum_region     = summary_region,
  sum_cooling    = summary_cooling,
  sum_water      = summary_water,
  sum_reg_cool   = summary_region_cooling,
  ann_geo        = annual_geo,
  ann_country    = annual_country,
  ann_region     = annual_region,
  ann_cooling    = annual_cooling,
  ann_water      = annual_water,
  ann_reg_cool   = annual_region_cooling,
  metadata       = run_metadata
)

tmp_xlsx <- tempfile(pattern = "post2025_bas_cc_", fileext = ".xlsx")
writexl::write_xlsx(xlsx_contents, path = tmp_xlsx)

safe_copy_file(tmp_xlsx, xlsx_path_run)
safe_copy_file(tmp_xlsx, xlsx_path_latest)

model_bundle <- list(
  mc_baseline = mc_baseline,
  summary_quantiles = summary_quantiles,
  summary_descriptives = summary_descriptives,
  summary_geo = summary_geo,
  summary_country = summary_country,
  summary_region = summary_region,
  summary_cooling = summary_cooling,
  summary_water = summary_water,
  summary_region_cooling = summary_region_cooling,
  annual_geo = annual_geo,
  annual_country = annual_country,
  annual_region = annual_region,
  annual_cooling = annual_cooling,
  annual_water = annual_water,
  annual_region_cooling = annual_region_cooling,
  dpf = dpf,
  jeffreys_survival = jeffreys_survival,
  fail_rates = fail_rates,
  country_rates = country_rates,
  rules_add = rules_add,
  cooling_shares = cooling_shares,
  cooling_fail = cooling_fail,
  phase_params = phase_params,
  lambda_path = lambda_path,
  run_metadata = run_metadata
)

safe_save_rds(model_bundle, rds_path_run)
safe_save_rds(model_bundle, rds_path_latest)

cat("\n================ POST-2025 BASELINE CONSTRUCTION CASCADE MC COMPLETE ================\n")
cat("Path mode:", PATH_MODE, "\n")
cat("Input workbook:", P_INPUT, "\n")
cat("Output directory:", OUTDIR, "\n")
cat("Simulations:", SIMS, "\n")
cat("Baseline rows:", nrow(mc_baseline), "\n")
cat("RUN_DIR:\n", RUN_DIR, "\n")
cat("LATEST_DIR:\n", LATEST_DIR, "\n")
cat("XLSX run:\n", xlsx_path_run, "\n")
cat("XLSX latest:\n", xlsx_path_latest, "\n")
cat("RDS run:\n", rds_path_run, "\n")
cat("RDS latest:\n", rds_path_latest, "\n")
cat("====================================================================================\n\n")

print(xlsx_path_run)
print(xlsx_path_latest)
print(rds_path_run)
print(rds_path_latest)
