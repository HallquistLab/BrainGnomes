# Initialize a BrainGnomes project interactively or from portable defaults

Initialize a BrainGnomes project interactively or from portable defaults

## Usage

``` r
initialize_project(
  project_name,
  project_directory,
  template = NULL,
  interactive = base::interactive(),
  overwrite = FALSE
)
```

## Arguments

- project_name:

  Project label.

- project_directory:

  Project root directory.

- template:

  Optional configuration object or YAML file to use as a base.

- interactive:

  Launch the existing guided setup. When false, missing paths are
  populated beneath `project_directory` and all pipeline stages default
  to disabled.

- overwrite:

  Replace an existing `project_config.yaml` in non-interactive mode.

## Value

A `bg_project_cfg` object.
