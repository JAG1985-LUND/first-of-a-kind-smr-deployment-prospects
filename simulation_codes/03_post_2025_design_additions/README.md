# Post-2025 design additions Monte Carlo

This folder contains the Monte Carlo code used to simulate post-2025 small modular reactor design additions.

This module complements the project-progression and pipeline-evolution modules by estimating the number and timing of future SMR designs entering the global pipeline after 2025 under Baseline and Accelerated assumptions.

## Source script

- `post_2025_design_additions_monte_carlo.R`

## Input workbook

- `data/Simulation_Inputs/pipeline_evolution_module_inputs.xlsx`

## Expected outputs

Running the script generates post-2025 design-addition outputs, including:

- simulated annual post-2025 design entries
- cumulative post-2025 design additions
- scenario-level summaries for Baseline and Accelerated assumptions
- run metadata documenting the seed, simulation count and input workbook

Full large Monte Carlo output files are deposited on Figshare because they exceed practical GitHub size limits. This repository contains the input workbook and source code needed to reproduce the outputs.

## Quick local testing

The final submission run uses:

- `SIMS <- 10000L`

For quick local testing, debugging or checking that the script runs correctly, users may temporarily reduce this to:

- `SIMS <- 25L`

The value should be restored to `SIMS <- 10000L` before reproducing the final manuscript outputs.
