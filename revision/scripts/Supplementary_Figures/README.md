# Supplementary Figure Reproduction Scripts

This directory contains the R scripts used to reproduce Supplementary Figures S2–S22 accompanying the manuscript.

Supplementary Figure S1 is a conceptual schematic and is therefore not generated from an R script. Supplementary Figure S23 is not included in this reproduction-script set.

## Repository structure

Supplementary figure source data are stored in:

`revision/data/Supplementary/`

Supplementary figure outputs are stored in:

`revision/outputs/Supplementary/`

Reproduction scripts are stored in:

`revision/scripts/Supplementary_Figures/`

## Reproduction scripts

| Figure | Script |
|---|---|
| S2 | `sfig02_stalled_development.R` |
| S3 | `sfig03_historical_cooling_oecd.R` |
| S4 | `sfig04_country_phase_counts.R` |
| S5 | `sfig05_pipeline_composition.R` |
| S6 | `sfig06_historical_vendor_pipeline.R` |
| S7 | `sfig07_vendors_trl.R` |
| S8 | `sfig08_counterfactual_scenarios.R` |
| S9 | `sfig09_scenario_space.R` |
| S10 | `sfig10_cooling_type_2050.R` |
| S11 | `sfig11_regional_national_trajectories.R` |
| S12 | `sfig12_readiness_benchmarking.R` |
| S13 | `sfig13_duration_trl.R` |
| S14 | `sfig14_amr_late_stage_risk.R` |
| S15 | `sfig15_oecd_pipeline_timelines.R` |
| S16 | `sfig16_post_foak_scaling_window.R` |
| S17 | `sfig17_smr_capacity_projections.R` |
| S18 | `sfig18_conceptual_design_duration.R` |
| S19 | `sfig19_design_phase_duration.R` |
| S20 | `sfig20_construction_duration_fits.R` |
| S21 | `sfig21_failure_probability_posteriors.R` |
| S22 | `sfig22_readiness_across_phases.R` |

## Reproducibility

The scripts use repository-relative paths and are intended to be run from within the cloned repository.

Each script reads the corresponding source-data workbook from `revision/data/Supplementary/` and writes the reproduced figure output to `revision/outputs/Supplementary/`.

The analyses were developed using R 4.3.3. Required R packages are specified within the individual reproduction scripts.
