# Collection of optimisation results

`OptimisationResultCollection` is an S7 container object that stores one
or more
[`OptimisationResult`](https://selkamand.github.io/symbo/reference/OptimisationResult.md)
objects, typically corresponding to different shape classes or binding
modes evaluated in a single alignment / docking run.

It provides:

- a list-like slot `@optimisations` containing the individual
  optimisation results; and

- a read-only `@shapeclasses` property that extracts the `shapeclass`
  field from each contained result.

A convenience
[`base::as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
method is provided so that key summary quantities (distance, angle,
rotations, slides, axes) can be inspected and compared in a tabular
form.

## Usage

``` r
OptimisationResultCollection(
  optimisations = list(),
  mol1_not_optimised,
  mol2_not_optimised
)
```

## Arguments

- optimisations:

  A list of
  [`OptimisationResult`](https://selkamand.github.io/symbo/reference/OptimisationResult.md)
  objects. Defaults to an empty list.

## Value

A new `OptimisationResultCollection` S7 object containing the supplied
optimisation results.

## Fields

The class has the following properties:

- optimisations:

  A list of
  [`OptimisationResult`](https://selkamand.github.io/symbo/reference/OptimisationResult.md)
  objects. The list may be empty. A validator enforces that every
  element in this list is a valid `OptimisationResult`; otherwise an
  error is raised.

- shapeclasses:

  Character vector (read-only) giving the `shapeclass` value of each
  element in `@optimisations`, in the same order. This is computed on
  the fly via the getter and cannot be set directly.

## Typical usage

Instances of `OptimisationResultCollection` are usually constructed by
higher-level routines that evaluate multiple shape classes. Each shape
class produces an
[`OptimisationResult`](https://selkamand.github.io/symbo/reference/OptimisationResult.md),
and these are collected into a single object for printing, summarising,
or coercion to a data frame.

## Examples

``` r
# Create a couple of dummy optimisation results
res1 <- OptimisationResult(shapeclass = "shapeA")
res2 <- OptimisationResult(shapeclass = "shapeB")

# Combine into a collection
coll <- OptimisationResultCollection(
  optimisations = list(res1, res2)
)
#> Error in OptimisationResultCollection(optimisations = list(res1, res2)): argument "mol1_not_optimised" is missing, with no default

# Print summary
coll
#> Error: object 'coll' not found

# Coerce to data.frame for further analysis
df <- as.data.frame(coll)
#> Error: object 'coll' not found
df$shapeclass
#> Error in df$shapeclass: object of type 'closure' is not subsettable
```
