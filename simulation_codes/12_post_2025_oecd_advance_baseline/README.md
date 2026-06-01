# 12. Post-2025 design additions: OECD advance sensitivity, Baseline scenario

This folder contains the simulation code and input workbook used to run the post-2025 design additions OECD advance sensitivity for the Baseline scenario.

The sensitivity tests a counterfactual in which OECD countries are assumed to achieve licensing durations comparable to the accelerated licensing pathway used for China and Russia. In the Baseline scenario, this means that OECD countries plus China and Russia use the accelerated licensing duration distribution, while other non-OECD regions continue to use the standard Baseline licensing duration distribution.

## Files

- `post_2025_oecd_advance_baseline_sensitivity.R`  
  Runs the Monte Carlo simulation for the Baseline OECD advance sensitivity.

- `post-2025_oecd_advance_baseline_sensitivity.xlsx`  
  Input workbook containing additions rates, phase rules, regional/country assumptions, cooling-type shares, design-phase failure rates, regional/country failure rates and cooling-type failure penalties.

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

This folder covers the Baseline OECD advance sensitivity.

The key counterfactual rule is:

```r
if (ctry %in% c("China", "Russia") || is_oecd){
  return(draw_total_duration("licensing", "accelerated", elapsed))
}
