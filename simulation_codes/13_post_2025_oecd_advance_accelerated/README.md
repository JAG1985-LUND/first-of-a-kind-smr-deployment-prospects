# 13. Post-2025: OECD advance sensitivity, Accelerated scenario

This folder contains the simulation code and input workbook used to run the post-2025 OECD advance sensitivity for the Accelerated scenario.

The sensitivity tests a counterfactual in which OECD countries are assumed to achieve deterministic one-year licensing durations, matching the accelerated licensing treatment applied to China and Russia. In the Accelerated scenario, this means that OECD countries plus China and Russia use a deterministic one-year licensing duration, while other non-OECD regions continue to use the standard Accelerated licensing duration distribution.

## Files

- `post_2025_oecd_advance_accelerated_sensitivity.R`  
  Runs the Monte Carlo simulation for the Accelerated OECD advance sensitivity.

- `post-2025_oecd_advance_accelerated_sensitivity.xlsx`  
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

This folder covers the Accelerated OECD advance sensitivity.

The key counterfactual rule is:

```r
if (ctry %in% c("China", "Russia") || is_oecd){
  return(1.0)
}
