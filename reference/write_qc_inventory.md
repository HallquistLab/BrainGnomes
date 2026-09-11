# Export a QC inventory as RDS, TSV tables, and a snapshot description

Export a QC inventory as RDS, TSV tables, and a snapshot description

## Usage

``` r
write_qc_inventory(x, output_dir)
```

## Arguments

- x:

  A `bg_qc_inventory` returned by
  [`collect_qc_inventory()`](https://hallquistlab.github.io/BrainGnomes/reference/collect_qc_inventory.md).

- output_dir:

  A new or empty output directory. Existing snapshots are protected from
  replacement; use a new directory when refreshing a report.

## Value

Invisibly returns the absolute path to the exported directory.

## Details

Each data-frame component becomes a TSV with quoted text and `NA` for
missing values. `inventory.rds` preserves the complete R object;
`snapshot.json` records schema, snapshot ID, collection time, and
project. IDs join tables within that snapshot. Reports contain
participant IDs and source paths and should be shared with the same care
as their source data.
