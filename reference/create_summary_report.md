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

- outdir:

  Directory where the HTML report should be written. Defaults to the
  current working directory. The final report will be saved as
  `{prefix}.html` inside this folder.

- prefix:

  A character string used as the base filename for the report. The
  resulting output file will be named `{prefix}.html`. Defaults to
  `"screen_results"`.

## Value

invisibly returns NULL (this function is run for its side effects)
