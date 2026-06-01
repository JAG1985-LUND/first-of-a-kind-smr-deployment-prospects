# 11. Post-2025 design additions: current-phase completion sensitivity, Accelerated scenario

This folder contains the simulation code and input workbook used to run the post-2025 design additions current-phase completion sensitivity for the Accelerated scenario.

The sensitivity tests a counterfactual in which post-2025 design additions entering the early design pipeline are assumed to complete their current phase rather than failing during conceptual or basic design. In the Accelerated scenario, this means that conceptual design and basic design survival are forced to 1.0, while detailed design, licensing and construction failure risks remain active.

## Files

- `post_2025_current_phase_completion_accelerated_sensitivity.R`  
  Runs the Monte Carlo simulation for the Accelerated current-phase completion sensitivity.

- `post-2025_current_phase_completion_accelerated_sensitivity.xlsx`  
  Input workbook containing additions rates, phase rules, regional/country assumptions, cooling-type shares, phase failure rates and cooling-type failure penalties.

## Input workbook structure

The R script reads the following sheets from the input workbook:

- `phase_rules_additions`  
  Defines the post-2025 design additions rate pathway.

- `additions_region_country`  
  Defines regional and country-level weights for future design additions.

- `design_additions_cooling`  
  Defines cooling-type probabilities for future design additions.

- `design_phase_failures`  
  Defines phase-level failure counts and sample sizes for conceptual design, basic design and detailed design.

- `failures_region_country`  
  Defines licensing and construction failure rates by country or regional grouping.

- `cooling_type_failure`  
  Defines additional licensing and construction failure penalties by reactor cooling type.

## Scenario logic

This folder covers the Accelerated current-phase completion sensitivity.

The key current-phase completion overrides are:

```r
jeffreys_survival[["conceptual_design"]] <- 1.0
jeffreys_survival[["basic_design"]]      <- 1.0
