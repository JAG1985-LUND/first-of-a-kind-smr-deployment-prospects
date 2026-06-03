# 17. Post-2025: late-stage risk sensitivity, Accelerated scenario

This folder contains the simulation code and input workbook used to run the post-2025 late-stage risk sensitivity for the Accelerated scenario.

The sensitivity tests the effect of revised late-stage failure assumptions on future post-2025 additions. In the Accelerated scenario, licensing and construction failure assumptions are read from the late-stage-risk failure-rate inputs in the workbook, while the remaining design-phase, additions-rate and cooling-type assumptions follow the Accelerated input structure.

## Files

- `post_2025_late_stage_risk_accelerated_sensitivity.R`  
  Runs the Monte Carlo simulation for the Accelerated late-stage risk sensitivity.

- `post-2025_late_stage_risk_accelerated_sensitivity.xlsx`  
  Input workbook containing additions rates, phase rules, regional/country assumptions, cooling-type shares, design-phase failure rates, regional/country failure rates and cooling-type failure penalties.

## Input workbook structure

The R script reads the following sheets from the input workbook:

- `phase_rules_additions`  
  Defines the post-2025 additions rate pathway.

- `additions_region_country`  
  Defines regional and country-level weights for future additions.

- `design_additions_cooling`  
  Defines cooling-type probabilities for future additions.

- `design_phase_failures`  
  Defines phase-level failure counts and sample sizes for conceptual design, basic design and detailed design.

- `failures_region_country`  
  Defines licensing and construction failure rates by country or regional grouping.

- `cooling_type_failure`  
  Defines additional licensing and construction failure penalties by reactor cooling type.

## Scenario logic

This folder covers the Accelerated late-stage risk sensitivity.

Licensing and construction failure assumptions are read from the workbook inputs:

```r
licensing_failure_accelerated = clamp01(accelerated_licensing_failure)
construction_failure_accelerated = clamp01(accelerated_construction_failure)
