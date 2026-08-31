# Write a project configuration without interactive prompts

Write a project configuration without interactive prompts

## Usage

``` r
write_project_config(input, file = NULL, overwrite = FALSE)
```

## Arguments

- input:

  A `bg_project_cfg` object.

- file:

  Destination YAML path. Defaults to `project_config.yaml` beneath the
  configured project directory.

- overwrite:

  Replace an existing file.

## Value

The configuration, invisibly, with its `yaml_file` attribute set.
