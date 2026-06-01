# Simulation inputs

This folder contains input workbooks used by the Monte Carlo simulation modules.

These files provide the design-level pipeline inputs, phase-rule assumptions, design-phase failure rates, regional and country assumptions, post-2025 design-addition inputs and scenario-specific sensitivity assumptions used in the FOAK SMR deployment analysis.

## Files

- `project_progression_inputs.xlsx`  
  Input workbook for the project-progression Monte Carlo module. It contains the current SMR design pipeline and phase-rule assumptions used to simulate FOAK timing under Baseline and Accelerated assumptions.

- `pipeline_evolution_module_inputs.xlsx`  
  Input workbook for the pipeline-evolution module, including assumptions used to model future additions to the global SMR design pipeline.

- `post_2025_design_additions_inputs.xlsx`  
  Input workbook for the reference post-2025 design-additions module.

- `current_phase_completion_sensitivity_inputs.xlsx`  
  Input workbook for the active-pipeline current-phase completion sensitivity.

- `oecd_advance_sensitivity_inputs.xlsx`  
  Input workbook for the active-pipeline OECD advance sensitivity.

- `late_stage_risk_sensitivity_inputs.xlsx`  
  Input workbook for the active-pipeline late-stage risk sensitivity.

- `AMRs_sensitivity_inputs.xlsx`  
  Input workbook for the advanced modular reactor (AMR) late-stage risk sensitivity.

- `stalled_development_sensitivity_inputs_baseline.xlsx`  
  Input workbook for the Baseline stalled-development sensitivity.

- `stalled_development_sensitivity_inputs_accelerated.xlsx`  
  Input workbook for the Accelerated stalled-development sensitivity.

- `post-2025_current_phase_completion_baseline_sensitivity.xlsx`  
  Input workbook for the post-2025 current-phase completion sensitivity under the Baseline scenario.

- `post-2025_current_phase_completion_accelerated_sensitivity.xlsx`  
  Input workbook for the post-2025 current-phase completion sensitivity under the Accelerated scenario.

- `post-2025_oecd_advance_baseline_sensitivity.xlsx`  
  Input workbook for the post-2025 OECD advance sensitivity under the Baseline scenario.

- `post-2025_oecd_advance_accelerated_sensitivity.xlsx`  
  Input workbook for the post-2025 OECD advance sensitivity under the Accelerated scenario.

- `post-2025_construction_cascade_baseline_sensitivity.xlsx`  
  Input workbook for the post-2025 construction cascade sensitivity under the Baseline scenario.

- `post-2025_construction_cascade_accelerated_sensitivity.xlsx`  
  Input workbook for the post-2025 construction cascade sensitivity under the Accelerated scenario.

- `post-2025_late_stage_risk_baseline_sensitivity.xlsx`  
  Input workbook for the post-2025 late-stage risk sensitivity under the Baseline scenario.

- `post-2025_late_stage_risk_accelerated_sensitivity.xlsx`  
  Input workbook for the post-2025 late-stage risk sensitivity under the Accelerated scenario.

## Notes

The input workbooks are separated from the R scripts to make the simulation workflow reproducible and easier to inspect.

The post-2025 sensitivity workbooks are self-contained input files. Each workbook includes the relevant additions rates, phase rules, regional and country assumptions, cooling-type shares, design-phase failure rates, late-stage failure rates and cooling-type failure penalties needed by the corresponding simulation script.

Large simulation outputs are not stored in this folder. Generated outputs are written locally by the relevant scripts and may be archived separately with the supplementary data materials.
