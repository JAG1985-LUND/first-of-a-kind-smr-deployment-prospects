# 20. AMR late-stage risk sensitivity

This folder contains the Monte Carlo simulation code for the advanced modular reactor (AMR) late-stage risk sensitivity analysis.

The script evaluates how adjusted licensing and construction failure risks for non-water-cooled SMR designs affect projected first-of-a-kind (FOAK) deployment outcomes through 2050. The sensitivity uses the same project-progression logic as the main FOAK simulation framework, but applies AMR-specific late-stage risk assumptions from the dedicated input workbook.

## Script

- `amrs_late_stage_risk_sensitivity.R`

## Input data

The script reads the following workbook:

- `../../data/Simulation_Inputs/AMRs_sensitivity_inputs.xlsx`

This workbook contains the design inputs, phase rules, design-phase failure rates and AMR-specific failure-rate assumptions required for the simulation.

## Outputs

When run, the script writes simulation outputs to the current folder, including:

- design-level Monte Carlo draws
- design-level summary quantiles
- annual FOAK deployment summaries through 2050
- 2050 deployment snapshots
- run metadata

Outputs are generated as Excel workbooks and R objects.

## Simulation settings

The GitHub version is configured to run:

- 10,000 Monte Carlo simulations per design
- accelerated project-progression assumptions
- annual deployment summaries from 2026 to 2050
- fail-gated project progression using lognormal phase-duration assumptions

## Purpose

This sensitivity is used to test whether AMR deployment projections are materially affected by alternative assumptions about late-stage licensing and construction risks.
