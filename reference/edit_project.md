# Interactively edit a project configuration by field (field-guided)

Allows the user to interactively browse and edit individual fields
within the configuration object, grouped by domain. Field paths are
defined within the function to avoid relying on a complete `scfg`
structure.

## Usage

``` r
edit_project(input = getwd())
```

## Arguments

- input:

  A `bg_project_cfg` object, a YAML file path, or a project directory
  containing `project_config.yaml`. Defaults to the current working
  directory. If a directory is provided but the file is absent,
  `edit_project` will stop.

## Value

An updated `bg_project_cfg` object. The updated configuration is written
to `project_config.yaml` in the project directory unless the user
chooses not to overwrite an existing file.
