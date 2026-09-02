# Package index

## All functions

- [`add_tracked_job_parent()`](https://hallquistlab.github.io/BrainGnomes/reference/add_tracked_job_parent.md)
  : Add parent/child id relationship to tracking database
- [`automask()`](https://hallquistlab.github.io/BrainGnomes/reference/automask.md)
  : Create an automatic brain mask from a NIfTI image (Rcpp
  implementation)
- [`butterworth_filter_4d()`](https://hallquistlab.github.io/BrainGnomes/reference/butterworth_filter_4d.md)
  : Apply a Butterworth Filter to a 4D NIfTI Image
- [`calculate_motion_outliers()`](https://hallquistlab.github.io/BrainGnomes/reference/calculate_motion_outliers.md)
  : Summarize framewise displacement outliers across runs
- [`cancel_project_run()`](https://hallquistlab.github.io/BrainGnomes/reference/cancel_project_run.md)
  : Cancel queued or running jobs from one run
- [`cluster_job_submit()`](https://hallquistlab.github.io/BrainGnomes/reference/cluster_job_submit.md)
  : This function submits a single script to a high-performance cluster
  using a scheduler (Slurm or TORQUE). It accepts a vector of arguments
  to be passed to the scheduler and a vector of environment variables
  that should be passed to the compute node at job execution.
- [`construct_bids_filename()`](https://hallquistlab.github.io/BrainGnomes/reference/construct_bids_filename.md)
  : Construct BIDS-Compatible Filenames from Extracted Entity Data
- [`construct_bids_regex()`](https://hallquistlab.github.io/BrainGnomes/reference/construct_bids_regex.md)
  : Construct a Regular Expression for Matching BIDS Filenames
- [`derive_reference_core()`](https://hallquistlab.github.io/BrainGnomes/reference/derive_reference_core.md)
  : Derive a conservative functional reference core from 4D BOLD data
- [`diagnose_pipeline()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_pipeline.md)
  : Deprecated interactive pipeline diagnosis
- [`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
  : Diagnose failed project work
- [`doctor()`](https://hallquistlab.github.io/BrainGnomes/reference/doctor.md)
  : Project preflight shorthand
- [`doctor_project()`](https://hallquistlab.github.io/BrainGnomes/reference/doctor_project.md)
  : Run non-mutating project and runtime preflight checks
- [`edit_project()`](https://hallquistlab.github.io/BrainGnomes/reference/edit_project.md)
  : Interactively edit a project configuration by field (field-guided)
- [`extract_bids_info()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_bids_info.md)
  : Extract fields from BIDS filenames
- [`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md)
  : Extract ROI timeseries and connectivity matrices
- [`filtfilt_cpp()`](https://hallquistlab.github.io/BrainGnomes/reference/filtfilt_cpp.md)
  : Zero-Phase IIR Filtering via Forward and Reverse Filtering
- [`find_run_logs()`](https://hallquistlab.github.io/BrainGnomes/reference/find_run_logs.md)
  : Find output and error logs for one run
- [`get_fmriprep_outputs()`](https://hallquistlab.github.io/BrainGnomes/reference/get_fmriprep_outputs.md)
  : Identify fMRIPrep-Derived Outputs for a NIfTI File
- [`get_postproc_output_files()`](https://hallquistlab.github.io/BrainGnomes/reference/get_postproc_output_files.md)
  : List postprocessed output files for paired input specifications
- [`get_project_runs()`](https://hallquistlab.github.io/BrainGnomes/reference/get_project_runs.md)
  : List submitted runs for a project
- [`get_project_status()`](https://hallquistlab.github.io/BrainGnomes/reference/get_project_status.md)
  : Get processing status for all subjects
- [`get_run_jobs()`](https://hallquistlab.github.io/BrainGnomes/reference/get_run_jobs.md)
  : Inspect the jobs submitted for one project run
- [`get_run_provenance()`](https://hallquistlab.github.io/BrainGnomes/reference/get_run_provenance.md)
  : Read the complete provenance record for a project run
- [`get_subject_status()`](https://hallquistlab.github.io/BrainGnomes/reference/get_subject_status.md)
  : Get processing status for a single subject
- [`get_tracked_job_status()`](https://hallquistlab.github.io/BrainGnomes/reference/get_tracked_job_status.md)
  : Query job status in tracking SQLite database
- [`getline()`](https://hallquistlab.github.io/BrainGnomes/reference/getline.md)
  : Read a Line of Input from the User in Both Interactive and
  Non-Interactive Sessions
- [`image_quantile()`](https://hallquistlab.github.io/BrainGnomes/reference/image_quantile.md)
  : Compute Quantiles from a 3D or 4D NIfTI Image
- [`initialize_project()`](https://hallquistlab.github.io/BrainGnomes/reference/initialize_project.md)
  : Initialize a BrainGnomes project interactively or from portable
  defaults
- [`insert_df_sqlite()`](https://hallquistlab.github.io/BrainGnomes/reference/insert_df_sqlite.md)
  : helper function to insert a keyed data.frame into the sqlite storage
  database
- [`insert_tracked_job()`](https://hallquistlab.github.io/BrainGnomes/reference/insert_tracked_job.md)
  : Internal helper function to insert a job into the tracking SQLite
  database
- [`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
  : Inspect current project progress
- [`lmfit_residuals_4d()`](https://hallquistlab.github.io/BrainGnomes/reference/lmfit_residuals_4d.md)
  : Apply Confound Regression to 4D fMRI Data Using Voxelwise Linear
  Models
- [`load_project()`](https://hallquistlab.github.io/BrainGnomes/reference/load_project.md)
  : Load a project configuration from a file
- [`natural_spline_4d()`](https://hallquistlab.github.io/BrainGnomes/reference/natural_spline_4d.md)
  : Interpolate fMRI Time Series with Cubic Splines in a NIfTI File
- [`natural_spline_interp()`](https://hallquistlab.github.io/BrainGnomes/reference/natural_spline_interp.md)
  : Cubic spline interpolation with natural spline and linear
  extrapolation
- [`parse_cli_args()`](https://hallquistlab.github.io/BrainGnomes/reference/parse_cli_args.md)
  : Parse CLI-style arguments into a nested list using args_to_df()
- [`plan_project()`](https://hallquistlab.github.io/BrainGnomes/reference/plan_project.md)
  : Inspect or persist the resolved project execution model
- [`postprocess_subject()`](https://hallquistlab.github.io/BrainGnomes/reference/postprocess_subject.md)
  : Postprocess a single fMRI BOLD image using a configured pipeline
- [`read_project_plan()`](https://hallquistlab.github.io/BrainGnomes/reference/read_project_plan.md)
  : Read a saved execution plan
- [`remove_nifti_volumes()`](https://hallquistlab.github.io/BrainGnomes/reference/remove_nifti_volumes.md)
  : Remove Specified Timepoints from a 4D NIfTI Image
- [`resample_template_to_img()`](https://hallquistlab.github.io/BrainGnomes/reference/resample_template_to_img.md)
  : Resample TemplateFlow Mask to fMRIPrep Image Using Python
- [`retry_project_run()`](https://hallquistlab.github.io/BrainGnomes/reference/retry_project_run.md)
  : Create a new run for failed work
- [`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md)
  : Run BIDS validation on the project BIDS directory
- [`run_fsl_command()`](https://hallquistlab.github.io/BrainGnomes/reference/run_fsl_command.md)
  : Run an FSL command with optional Singularity container support and
  structured logging
- [`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
  : Run the processing pipeline
- [`setup_project()`](https://hallquistlab.github.io/BrainGnomes/reference/setup_project.md)
  : Setup the processing pipeline for a new fMRI study
- [`submit_project_plan()`](https://hallquistlab.github.io/BrainGnomes/reference/submit_project_plan.md)
  : Submit a saved or in-memory execution plan
- [`summary(`*`<bg_project_cfg>`*`)`](https://hallquistlab.github.io/BrainGnomes/reference/summary.bg_project_cfg.md)
  : summary method for project configuration object
- [`summary(`*`<bg_status_df>`*`)`](https://hallquistlab.github.io/BrainGnomes/reference/summary.bg_status_df.md)
  : Summarize project status
- [`update_tracked_job_status()`](https://hallquistlab.github.io/BrainGnomes/reference/update_tracked_job_status.md)
  : Update Job Status in Tracking SQLite Database
- [`validate_project_config()`](https://hallquistlab.github.io/BrainGnomes/reference/validate_project_config.md)
  : Validate a BrainGnomes project configuration without changing it
- [`wait_for_job()`](https://hallquistlab.github.io/BrainGnomes/reference/wait_for_job.md)
  : This function pauses execution of an R script while a scheduled qsub
  job is not yet complete.
- [`write_project_config()`](https://hallquistlab.github.io/BrainGnomes/reference/write_project_config.md)
  : Write a project configuration without interactive prompts
- [`write_project_plan()`](https://hallquistlab.github.io/BrainGnomes/reference/write_project_plan.md)
  : Save an execution plan to YAML
