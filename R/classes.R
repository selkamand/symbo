
# OptimisationResult ------------------------------------------------------



#' Optimisation result for two aligned molecules
#'
#' @description
#' `OptimisationResult` is an S7  object that stores the outcome of
#' a geometric optimisation used to align two [`structures::Molecule3D`]
#' objects. It records the optimised geometries of each molecule, the
#' optimisation objective, key geometric parameters (angles, slides and
#' axes) and bookkeeping information returned by the optimiser.
#'
#' @section Fields:
#'
#' The class has the following properties:
#'
#' \describe{
#'
#'   \item{mol1}{A [`structures::Molecule3D`] object representing
#'   the first input molecule in its geometrically optimised form (i.e.,
#'   after applying the optimal rotation/translation that minimises
#'   `min_sum_of_squared_distance`).}
#'
#'   \item{mol2}{A [`structures::Molecule3D`] object representing
#'   the second input molecule in its geometrically optimised form.}
#'
#'   \item{min_sum_of_squared_distance}{Numeric scalar giving the
#'   minimised sum of squared distances between the dummy atoms of each
#'   molecule and the opposing binding atom. Formally:
#'   \eqn{d_1^2 + d_2^2}, where \eqn{d_1} is the distance from the mol1
#'   dummy atom to the mol2 binding atom, and \eqn{d_2} is the distance
#'   from the mol2 dummy atom to the mol1 binding atom, evaluated at the
#'   optimum.}
#'
#'   \item{angle_between_dummy_binding_vectors}{Numeric scalar (radians)
#'   giving the angle between the vector from mol1 dummy \eqn{\to} mol1
#'   binding atom and the vector from mol2 dummy \eqn{\to} mol2 binding
#'   atom, computed for the optimised geometry. For a “perfect” solution
#'   this angle would be \eqn{\pi} (180 degrees).}
#'
#'   \item{mol1_phi}{Numeric scalar (radians): the optimal rotation angle
#'   applied to `mol1` about `mol1_axis` to obtain the geometrically
#'   optimal configuration.}
#'
#'   \item{mol2_phi}{Numeric scalar (radians): the optimal rotation angle
#'   applied to `mol2` about `mol2_axis`.}
#'
#'   \item{mol1_slide}{Numeric scalar giving the optimal distance to
#'   translate `mol1` along `mol1_axis` (the “slide” along the axis)
#'   in the optimal configuration.}
#'
#'   \item{mol2_slide}{Numeric scalar giving the optimal distance to
#'   translate `mol2` along `mol2_axis`.}
#'
#'   \item{mol1_axis}{Numeric length-3 vector describing the Cartesian
#'   direction of the symmetry axis about which `mol1` was rotated and
#'   along which it was slid during optimisation.}
#'
#'   \item{mol2_axis}{Numeric length-3 vector describing the corresponding
#'   axis for `mol2`.}
#'
#'   \item{n_calls_to_fn}{Numeric scalar giving the number of calls to
#'   the objective function (`fn`) made by the optimiser.}
#'
#'   \item{n_calls_to_gr}{Numeric scalar giving the number of calls to
#'   the gradient function (`gr`) made by the optimiser (where applicable).}
#'
#'   \item{convergence}{Numeric scalar convergence code, typically matching
#'   the codes returned by [stats::optim()]. For example:
#'   \itemize{
#'     \item `0` – successful completion (method-dependent).
#'     \item `1` – iteration limit (`maxit`) reached.
#'     \item `10` – degeneracy of the Nelder–Mead simplex.
#'     \item `51` – warning from method `"L-BFGS-B"` (see `message`).
#'     \item `52` – error from method `"L-BFGS-B"` (see `message`).
#'   }
#'   See the documentation of the optimisation routine for exact meanings.}
#'
#'   \item{message}{Character scalar giving any additional information
#'   returned by the optimiser (e.g. warnings, diagnostic messages). May be
#'   `NA_character_` if no message is available.}
#'
#'   \item{hessian}{Numeric object (typically a symmetric matrix) giving an
#'   estimate of the Hessian at the solution, if available. This is usually
#'   the Hessian of the *unconstrained* problem, even if box constraints are
#'   active. If the optimisation was run without requesting a Hessian, this
#'   property is set to `NaN`. Assigning `NULL` to `hessian` will also store
#'   `NaN` internally.}
#'
#' }
#'
#' @section Typical usage:
#'
#' Instances of `OptimisationResult` are usually created internally by
#' higher-level alignment / docking routines, and returned as a structured
#' record of the optimisation outcome for a given shape class or binding
#' mode.
#'
#' @param mol1 A [`structures::Molecule3D`] object giving the
#'   optimised coordinates of the first molecule. Defaults to an empty
#'   `Molecule3D()` instance.
#' @param mol2 A [`structures::Molecule3D`] object giving the
#'   optimised coordinates of the second molecule. Defaults to an empty
#'   `Molecule3D()` instance.
#'
#' @return
#' A new `OptimisationResult` S7 object with the supplied molecules and
#' all other fields initialised to their type-appropriate defaults.
#'
#' @examples
#' # (Pseudo-example; real usage would normally be through an optimiser)
#' res <- OptimisationResult(
#'   mol1 = Molecule3D(),
#'   mol2 = Molecule3D()
#' )
#' res@min_sum_of_squared_distance <- 0.12
#' res@n_calls_to_fn <- 35
#'
#' @export
OptimisationResult <- S7::new_class(
  name = "OptimisationResult",
  properties = list(
    shapeclass = S7::class_character,
    mol1 = S7::new_property(class = structures::Molecule3D),
    mol2 = S7::new_property(class = structures::Molecule3D),
    min_sum_of_squared_distance = S7::class_numeric,
    angle_between_dummy_binding_vectors = S7::class_numeric,
    mol1_phi = S7::class_numeric,
    mol2_phi = S7::class_numeric,
    mol1_slide = S7::class_numeric,
    mol2_slide = S7::class_numeric,
    mol1_axis = S7::class_numeric, # length-3 numeric
    mol2_axis = S7::class_numeric, # length-3 numeric
    n_calls_to_fn = S7::class_numeric,
    n_calls_to_gr = S7::class_numeric,
    convergence = S7::class_numeric,
    message = S7::new_property(
      class = S7::class_character,
      setter = function(self, value) {
        self@message <- if(is.null(value)) "" else value
        self
      }
    ),
    hessian = S7::new_property(
      class = S7::class_numeric,
      setter = function(self, value) {
       self@hessian <- if(is.null(value)) NaN else value
       self
      },
      validator = function(value){
        if(!is.numeric(value)) return(sprintf("@hessian must be numeric, not [%s]", toString(class(value))))
        return(NULL)
      }
    )
  ),
  constructor = function(
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
      hessian = 2) {
    S7::new_object(
      S7::S7_object(),
      shapeclass = shapeclass,
      mol1 = mol1,
      mol2 = mol2,
      min_sum_of_squared_distance = min_sum_of_squared_distance,
      angle_between_dummy_binding_vectors = angle_between_dummy_binding_vectors,
      mol1_phi = mol1_phi,
      mol2_phi = mol2_phi,
      mol1_slide = mol1_slide,
      mol2_slide = mol2_slide,
      mol1_axis = mol1_axis,
      mol2_axis = mol2_axis,
      n_calls_to_fn = n_calls_to_fn,
      n_calls_to_gr = n_calls_to_gr,
      convergence = convergence,
      message = message,
      hessian = hessian
    )
  }
)


## Generics ----------------------------------------------------------------

#' @export
S7::method(print, OptimisationResult) <- function(x, ...) {

  cat(sep = "\n",
      "================================",
      "Optimisation Result",
      "================================",
      sprintf("Shape Classes: %s", x@shapeclass),
      sprintf("Minimised Sum of Squared Distance: %f", x@min_sum_of_squared_distance),
      sprintf("Minimised Angle (pi = perfect): %f", x@angle_between_dummy_binding_vectors),
      sprintf("Convergence: %s", x@convergence)
  )
}


## Non-Generics  ---------------------------------------------------------------
is_optimisation_result <- function(x){
  inherits(x, "bondy::OptimisationResult")
}

get_optimistation_stats <- function(x){
  stats <- c(
    "Shape Class" = x@shapeclass,
    "Minimised Sum of Squared Distance" = x@min_sum_of_squared_distance,
    "Minimised Angle (pi = perfect)" = x@angle_between_dummy_binding_vectors,
    "Convergence" = x@convergence,
    "Calls to Optimisation Function" = x@n_calls_to_fn,
    "Messages/Warnings: " = x@message
  )

  df = data.frame(
    Property = names(stats),
    Values = as.character(stats)
  )
  rownames(df) <- NULL
  return(df)
}

# OptimisationResultCollection --------------------------------------------

#' Collection of optimisation results
#'
#' @description
#' `OptimisationResultCollection` is an S7 container object that stores one or
#' more [`bondy::OptimisationResult`] objects, typically corresponding to
#' different shape classes or binding modes evaluated in a single alignment /
#' docking run.
#'
#' It provides:
#' \itemize{
#'   \item a list-like slot \code{@optimisations} containing the individual
#'         optimisation results; and
#'   \item a read-only \code{@shapeclasses} property that extracts the
#'         \code{shapeclass} field from each contained result.
#' }
#'
#' A convenience [base::as.data.frame()] method is provided so that key
#' summary quantities (distance, angle, rotations, slides, axes) can be
#' inspected and compared in a tabular form.
#'
#' @section Fields:
#'
#' The class has the following properties:
#'
#' \describe{
#'
#'   \item{optimisations}{A list of [`bondy::OptimisationResult`] objects.
#'   The list may be empty. A validator enforces that every element in this
#'   list is a valid `OptimisationResult`; otherwise an error is raised.}
#'
#'   \item{shapeclasses}{Character vector (read-only) giving the
#'   \code{shapeclass} value of each element in \code{@optimisations}, in the
#'   same order. This is computed on the fly via the getter and cannot be set
#'   directly.}
#'
#' }
#'
#' @section Typical usage:
#'
#' Instances of `OptimisationResultCollection` are usually constructed by
#' higher-level routines that evaluate multiple shape classes. Each shape
#' class produces an [`bondy::OptimisationResult`], and these are collected
#' into a single object for printing, summarising, or coercion to a data frame.
#'
#' @param optimisations A list of [`bondy::OptimisationResult`] objects.
#'   Defaults to an empty list.
#'
#' @return
#' A new `OptimisationResultCollection` S7 object containing the supplied
#' optimisation results.
#'
#' @examples
#' # Create a couple of dummy optimisation results
#' res1 <- OptimisationResult(shapeclass = "shapeA")
#' res2 <- OptimisationResult(shapeclass = "shapeB")
#'
#' # Combine into a collection
#' coll <- OptimisationResultCollection(
#'   optimisations = list(res1, res2)
#' )
#'
#' # Print summary
#' coll
#'
#' # Coerce to data.frame for further analysis
#' df <- as.data.frame(coll)
#' df$shapeclass
#'
#' @export
OptimisationResultCollection <- S7::new_class(
  name = "OptimisationResultCollection",

  properties = list(

    mol1_not_optimised = S7::new_property(
      class = structures::Molecule3D
    ),
    mol2_not_optimised = S7::new_property(
      class = structures::Molecule3D
    ),

    optimisations = S7::new_property(
      class = S7::class_list,
      validator = function(value){
        for (val in value){
          if (!is_optimisation_result(val)) {
            return(sprintf("All @optimisations in OptimisationResultCollection must be an OptimisationResult object, not a [%s]", toString(class(val))))
          }
        }
      }
    ),
    # Shape Classes that were Evaluated
    shapeclasses = S7::new_property(
      class = S7::class_character,
      setter = function(self, value) { stop("@shapeclasses is a read only property") },
      getter = function(self) {
        vapply(X = self@optimisations, function(o){o@shapeclass}, FUN.VALUE = character(1))
      }
    )
  ),
  constructor = function(optimisations = list(), mol1_not_optimised, mol2_not_optimised){
    S7::new_object(
    S7::S7_object(),
    optimisations = optimisations,
    mol1_not_optimised = mol1_not_optimised,
    mol2_not_optimised = mol2_not_optimised
    )
  }
)


# Generics ----------------------------------------------------------------
#' @export
S7::method(print, OptimisationResultCollection) <- function(x, ...) {
  optimisations <- x@optimisations
  n_optimisations = length(optimisations)
  df <- as.data.frame(x)
  shapeclasses <- df$shapeclass

  df$summary_string <- with(
    df,
    {
      sprintf("-> %s (D: %f | A: %f)", shapeclass, min_sum_of_squared_distance, angle_between_dummy_binding_vectors)
    }
  )
  shapeclass_summary_string <- if(n_optimisations == 0) "" else paste0(df$summary_string, collapse = "\n")

  cat(sep = "\n",
    "================================",
    "Optimisation Result Collection",
    "================================",
    sprintf("Shape Classes Evaluated: %s", length(optimisations)),
    shapeclass_summary_string
  )
}

#' @export
S7::method(as.data.frame, OptimisationResultCollection) <- function(x, ...) {
  optimisations <- x@optimisations

  data.frame(
    shapeclass = vapply(X = optimisations, function(o) { o@shapeclass }, FUN.VALUE = character(1)),
    min_sum_of_squared_distance = vapply(X = optimisations, function(o) { o@min_sum_of_squared_distance }, FUN.VALUE = numeric(1)),
    angle_between_dummy_binding_vectors = vapply(X = optimisations, function(o) { o@angle_between_dummy_binding_vectors }, FUN.VALUE = numeric(1)),
    mol1_name = vapply(X = optimisations, function(o) { o@mol1@name }, FUN.VALUE = character(1)),
    mol2_name = vapply(X = optimisations, function(o) { o@mol2@name }, FUN.VALUE = character(1)),
    mol1_phi = vapply(X = optimisations, function(o) { o@mol1_phi }, FUN.VALUE = numeric(1)),
    mol2_phi = vapply(X = optimisations, function(o) { o@mol2_phi }, FUN.VALUE = numeric(1)),
    mol1_slide = vapply(X = optimisations, function(o) { o@mol1_slide }, FUN.VALUE = numeric(1)),
    mol2_slide = vapply(X = optimisations, function(o) { o@mol2_slide }, FUN.VALUE = numeric(1)),
    mol1_axis_x = vapply(X = optimisations, function(o) { o@mol1_axis[1] }, FUN.VALUE = numeric(1)),
    mol1_axis_y = vapply(X = optimisations, function(o) { o@mol1_axis[2] }, FUN.VALUE = numeric(1)),
    mol1_axis_z = vapply(X = optimisations, function(o) { o@mol1_axis[3] }, FUN.VALUE = numeric(1)),
    mol2_axis_x = vapply(X = optimisations, function(o) { o@mol2_axis[1] }, FUN.VALUE = numeric(1)),
    mol2_axis_y = vapply(X = optimisations, function(o) { o@mol2_axis[2] }, FUN.VALUE = numeric(1)),
    mol2_axis_z = vapply(X = optimisations, function(o) { o@mol2_axis[3] }, FUN.VALUE = numeric(1))
  )
}

## Non-Generics  ---------------------------------------------------------------

