# 18. AMR late-stage risk sensitivity

This folder contains the Monte Carlo simulation code for the advanced modular reactor (AMR) late-stage risk sensitivity analysis.

The script evaluates how adjusted licensing and construction failure risks for non-water-cooled small modular reactor (SMR) designs affect projected first-of-a-kind (FOAK) deployment outcomes through 2050. The sensitivity uses the same project-progression logic as the main FOAK simulation framework, but applies AMR-specific late-stage risk assumptions from the dedicated input workbook.

## Files

- `amrs_late_stage_risk_sensitivity.R`  
  Runs the AMR late-stage risk sensitivity simulation.

## Input data

The script reads the following workbook:

- `../../data/Simulation_Inputs/AMRs_sensitivity_inputs.xlsx`

This workbook contains the design inputs, phase rules, design-phase failure rates and AMR-specific late-stage failure-rate assumptions required for the simulation.

## Input workbook structure

The R script reads the relevant sheets from the input workbook, including:

- design-level project inputs;
- phase-duration and phase-progression assumptions;
- design-phase failure rates;
- AMR-specific licensing and construction failure-rate assumptions;
- assumptions used to distinguish water-cooled and non-water-cooled designs.

## Scenario logic

This sensitivity tests whether projected AMR deployment outcomes are materially affected by alternative assumptions about late-stage licensing and construction risks.

The key logic is that non-water-cooled SMR designs are assigned adjusted late-stage failure risks during licensing and construction. These risks are then applied within the fail-gated project-progression simulation, so affected designs must survive each development phase before reaching FOAK operation.

The sensitivity therefore isolates the effect of AMR-specific late-stage risks while retaining the broader project-progression structure used in the main FOAK simulation framework.

## Simulation settings

The GitHub version is configured to run:

- 10,000 Monte Carlo simulations per design;
- accelerated project-progression assumptions;
- annual FOAK deployment summaries from 2026 to 2050;
- fail-gated progression through development, licensing and construction phases;
- lognormal phase-duration assumptions.

## Outputs

When run, the script writes simulation outputs to the current folder, including:

- design-level Monte Carlo draws;
- design-level summary quantiles;
- annual FOAK deployment summaries through 2050;
- 2050 deployment snapshots;
- run metadata.

Outputs are generated as Excel workbooks and R objects.

Large output files are not stored in this GitHub repository. They are generated locally by running the script and may be archived separately with the supplementary data materials.

## Reproducibility note

This script is part of the simulation code for the manuscript:

Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.  
First-of-a-Kind deployment prospects for small modular reactors.

The script is designed to reproduce the AMR late-stage risk sensitivity using the central input workbook stored in `data/Simulation_Inputs`.
