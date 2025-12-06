# Create a report summarising results of screen

Create a report summarising results of screen

## Usage

``` r
create_summary_report(
  optimisations,
  outdir = getwd(),
  prefix = "screen_results"
)
```

## Arguments

- optimisations:

  an
  [`OptimisationResultCollection`](https://selkamand.github.io/symbo/reference/OptimisationResultCollection.md)
  object produced by
  [`screen_molecules()`](https://selkamand.github.io/symbo/reference/screen_molecules.md)

## Value

invisibly returns NULL (this function is run for its side effects)
