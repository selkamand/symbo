# Optimisation result for two aligned molecules

`OptimisationResult` is an S7 object that stores the outcome of a
geometric optimisation used to align two
[`structures::Molecule3D`](https://rdrr.io/pkg/structures/man/Molecule3D.html)
objects. It records the optimised geometries of each molecule, the
optimisation objective, key geometric parameters (angles, slides and
axes) and bookkeeping information returned by the optimiser.

## Usage

``` r
OptimisationResult(
  shapeclass = "NotSpecified",
  mol1 = structures::Molecule3D(),
  mol2 = structures::Molecule3D(),
  min_sum_of_squared_distance = NaN,
  angle_between_dummy_binding_vectors = NaN,
  mol1_phi = NaN,
  mol2_phi = NaN,
  mol1_slide = NaN,
  mol2_slide = NaN,
  mol1_axis = c(NaN, NaN, NaN),
  mol2_axis = c(NaN, NaN, NaN),
  n_calls_to_fn = 0,
  n_calls_to_gr = 0,
  convergence = NA_real_,
  message = NA_character_,
  hessian = 2
)
```

## Arguments

- shapeclass:

  Name of the shape class being evaluated (character scalar), used to
  identify which supramolecular geometry this optimisation result
  corresponds to.

- mol1:

  A
  [`structures::Molecule3D`](https://rdrr.io/pkg/structures/man/Molecule3D.html)
  object giving the optimised coordinates of the first molecule.
  Defaults to an empty `Molecule3D()` instance.

- mol2:

  A
  [`structures::Molecule3D`](https://rdrr.io/pkg/structures/man/Molecule3D.html)
  object giving the optimised coordinates of the second molecule.
  Defaults to an empty `Molecule3D()` instance.

- min_sum_of_squared_distance:

  Numeric scalar giving the minimised sum of squared distances between
  the dummy atoms of each molecule and the opposing binding atom.
  Formally: \\d_1^2 + d_2^2\\, where \\d_1\\ is the distance from the
  mol1 dummy atom to the mol2 binding atom, and \\d_2\\ is the distance
  from the mol2 dummy atom to the mol1 binding atom, evaluated at the
  optimum.

- angle_between_dummy_binding_vectors:

  Numeric scalar (radians) giving the angle between the vector from mol1
  dummy \\\to\\ mol1 binding atom and the vector from mol2 dummy \\\to\\
  mol2 binding atom, computed for the optimised geometry. For a
  “perfect” solution this angle would be \\\pi\\ (180 degrees).

- mol1_phi:

  Numeric scalar (radians) giving the optimal rotation angle applied to
  `mol1` about `mol1_axis` to obtain the geometrically optimal
  configuration.

- mol2_phi:

  Numeric scalar (radians) giving the optimal rotation angle applied to
  `mol2` about `mol2_axis` in the optimised configuration.

- mol1_slide:

  Numeric scalar giving the optimal distance to translate `mol1` along
  `mol1_axis` (the “slide” along its symmetry axis) in the optimised
  configuration.

- mol2_slide:

  Numeric scalar giving the optimal distance to translate `mol2` along
  `mol2_axis`.

- mol1_axis:

  Numeric length-3 vector describing the Cartesian direction of the
  symmetry axis about which `mol1` is rotated and slid during
  optimisation.

- mol2_axis:

  Numeric length-3 vector describing the Cartesian direction of the
  symmetry axis about which `mol2` is rotated and slid during
  optimisation.

- n_calls_to_fn:

  Numeric scalar giving the number of calls to the objective function
  (`fn`) made by the optimiser.

- n_calls_to_gr:

  Numeric scalar giving the number of calls to the gradient function
  (`gr`) made by the optimiser (where applicable).

- convergence:

  Numeric scalar convergence code, typically matching the codes returned
  by [`stats::optim()`](https://rdrr.io/r/stats/optim.html) (e.g. `0`
  for successful completion, `1` for iteration limit reached, and higher
  codes for warnings or errors depending on the method).

- message:

  Character scalar giving any additional information returned by the
  optimiser (e.g. warnings, diagnostic messages). May be `NA_character_`
  if no message is available.

- hessian:

  Numeric object (typically a symmetric matrix) giving an estimate of
  the Hessian at the solution. If no Hessian is available, this is set
  to `NaN`. Passing `NULL` will also be stored internally as `NaN`.

## Value

A new `OptimisationResult` S7 object with the supplied molecules and all
other fields initialised to their type-appropriate defaults.

## Fields

The class has the following properties:

- mol1:

  A
  [`structures::Molecule3D`](https://rdrr.io/pkg/structures/man/Molecule3D.html)
  object representing the first input molecule in its geometrically
  optimised form (i.e., after applying the optimal rotation/translation
  that minimises `min_sum_of_squared_distance`).

- mol2:

  A
  [`structures::Molecule3D`](https://rdrr.io/pkg/structures/man/Molecule3D.html)
  object representing the second input molecule in its geometrically
  optimised form.

- min_sum_of_squared_distance:

  Numeric scalar giving the minimised sum of squared distances between
  the dummy atoms of each molecule and the opposing binding atom.
  Formally: \\d_1^2 + d_2^2\\, where \\d_1\\ is the distance from the
  mol1 dummy atom to the mol2 binding atom, and \\d_2\\ is the distance
  from the mol2 dummy atom to the mol1 binding atom, evaluated at the
  optimum.

- angle_between_dummy_binding_vectors:

  Numeric scalar (radians) giving the angle between the vector from mol1
  dummy \\\to\\ mol1 binding atom and the vector from mol2 dummy \\\to\\
  mol2 binding atom, computed for the optimised geometry. For a
  “perfect” solution this angle would be \\\pi\\ (180 degrees).

- mol1_phi:

  Numeric scalar (radians): the optimal rotation angle applied to `mol1`
  about `mol1_axis` to obtain the geometrically optimal configuration.

- mol2_phi:

  Numeric scalar (radians): the optimal rotation angle applied to `mol2`
  about `mol2_axis`.

- mol1_slide:

  Numeric scalar giving the optimal distance to translate `mol1` along
  `mol1_axis` (the “slide” along the axis) in the optimal configuration.

- mol2_slide:

  Numeric scalar giving the optimal distance to translate `mol2` along
  `mol2_axis`.

- mol1_axis:

  Numeric length-3 vector describing the Cartesian direction of the
  symmetry axis about which `mol1` was rotated and along which it was
  slid during optimisation.

- mol2_axis:

  Numeric length-3 vector describing the corresponding axis for `mol2`.

- n_calls_to_fn:

  Numeric scalar giving the number of calls to the objective function
  (`fn`) made by the optimiser.

- n_calls_to_gr:

  Numeric scalar giving the number of calls to the gradient function
  (`gr`) made by the optimiser (where applicable).

- convergence:

  Numeric scalar convergence code, typically matching the codes returned
  by [`stats::optim()`](https://rdrr.io/r/stats/optim.html). For
  example:

  - `0` – successful completion (method-dependent).

  - `1` – iteration limit (`maxit`) reached.

  - `10` – degeneracy of the Nelder–Mead simplex.

  - `51` – warning from method `"L-BFGS-B"` (see `message`).

  - `52` – error from method `"L-BFGS-B"` (see `message`).

  See the documentation of the optimisation routine for exact meanings.

- message:

  Character scalar giving any additional information returned by the
  optimiser (e.g. warnings, diagnostic messages). May be `NA_character_`
  if no message is available.

- hessian:

  Numeric object (typically a symmetric matrix) giving an estimate of
  the Hessian at the solution, if available. This is usually the Hessian
  of the *unconstrained* problem, even if box constraints are active. If
  the optimisation was run without requesting a Hessian, this property
  is set to `NaN`. Assigning `NULL` to `hessian` will also store `NaN`
  internally.

## Typical usage

Instances of `OptimisationResult` are usually created internally by
higher-level alignment / docking routines, and returned as a structured
record of the optimisation outcome for a given shape class or binding
mode.

## Examples

``` r
# (Pseudo-example; real usage would normally be through an optimiser)
res <- OptimisationResult(
  mol1 = structures::Molecule3D(),
  mol2 = structures::Molecule3D()
)
res@min_sum_of_squared_distance <- 0.12
res@n_calls_to_fn <- 35

print(res)
#> ================================
#> Optimisation Result
#> ================================
#> Shape Classes: NotSpecified
#> Minimised Sum of Squared Distance: 0.120000
#> Minimised Angle (pi = perfect): NaN
#> Convergence: NA
```
