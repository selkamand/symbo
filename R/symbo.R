


# Core Function -----------------------------------------------------------

#' Screen molecules for valid Shape classes
#'
#' Screens a pair of molecules to find their most geometrically optimal configuration to produce shapeclasses
#'
#' @param molecule1 a [structures::Molecule3D()] object with symmetry axes annotated and dummy atoms representing where the mol2_binding_atom might bind.
#' @param molecule2 a [structures::Molecule3D()] object with symmetry axes annotated and dummy atoms representing where the mol1_binding_atom might bind.
#' @param mol1_binding_atom the atom you expect will bind to molecule2 (integer representing element number a.k.a eleno).
#' @param mol2_binding_atom the atom you expect will bind to molecule1 (integer representing element number a.k.a eleno).
#' @param method,lower,upper,control,hessian optimisation algorith configuration. See [stats::optim()] for details.
#' @inherit find_optimal_position return
#' @export
#'
#' @returns [`OptimisationResultCollection`] object.
#'
screen_molecules <- function(molecule1, molecule2,
                             mol1_binding_atom, mol2_binding_atom,
                             method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
                                        "Brent"),
                             lower = -Inf, upper = Inf,
                             control = list(), hessian = FALSE
){
  method <- rlang::arg_match(method)

  assertions::assert_class(molecule1, class = "structures::Molecule3D")
  assertions::assert_class(molecule2, class = "structures::Molecule3D")

  # Fetch Assessable shapeclasses
  cli::cli_alert_info("Figuring out which shape classes are assessable")
  molecule1_name <- molecule1@name
  molecule2_name <- molecule2@name
  molecule1_proper_rotation_axes <- molecule1@symmetry_elements@unique_proper_axis_orders
  molecule2_proper_rotation_axes <- molecule2@symmetry_elements@unique_proper_axis_orders

  if(length(molecule1_proper_rotation_axes) == 0){
    cli::cli_abort("Molecule 1 [{molecule1_name}] has no proper rotation axes. Can NOT screen for valid shapeclasses. Please annotate molecule with a ProperRotationAxis using the [structures::add_symmetry_element_to_molecule()] function and try again")
  }
  if(length(molecule2_proper_rotation_axes) == 0){
    cli::cli_abort("Molecule 2 [{molecule2_name}] has no proper rotation axes. Can NOT screen for valid shapeclasses. Please annotate molecule with a ProperRotationAxis using the [structures::add_symmetry_element_to_molecule()] function and try again")
  }

  cli::cli_alert_info(sprintf("Molecule 1 [%s] has [%s]", molecule1_name, toString(paste0("C", molecule1_proper_rotation_axes))))
  cli::cli_alert_info(sprintf("Molecule 2 [%s] has [%s]", molecule2_name, toString(paste0("C", molecule2_proper_rotation_axes))))
  # cli::cli_alert_info("Molecule 2 [{molecule2_name}] has {molecule2_proper_rotation_axes}")

  df_assessable <- get_assessable_shapeclasses(
    molecule1_axes = molecule1_proper_rotation_axes,
    molecule2_axes = molecule2_proper_rotation_axes
  )

  n_assessable_shapeclasses <- nrow(df_assessable)
  if(n_assessable_shapeclasses == 0){
   cli::cli_abort("Molecules [{molecule1_name}] and [{molecule2_name}] can not be assessed for any shape class (no viable combination of annotated symmetry axes)")
  }

  assessable_shapeclass_string <- toString(df_assessable$ShapeClass)
  cli::cli_alert_info("Molecules [{molecule1_name}] and [{molecule2_name}] can be assessed for [{n_assessable_shapeclasses}] shape class{?es} ({assessable_shapeclass_string}).")


  # Assess feasiblity for each assessable shapeclass
  ls_optimisations <- lapply(seq_len(nrow(df_assessable)), FUN = function(i){
    shapeclass = df_assessable$ShapeClass[i]
    cli::cli_h1("{shapeclass}")

    flipped = df_assessable$flipped[i]
    mol1_axis_cn = df_assessable$mol1_axis[i]
    mol2_axis_cn = df_assessable$mol2_axis[i]

    target_axis1_position <- c(
      x = df_assessable$Axis1x[i],
      y = df_assessable$Axis1y[i],
      z = df_assessable$Axis1z[i]
    )

    target_axis2_position <- c(
      x = df_assessable$Axis2x[i],
      y = df_assessable$Axis2y[i],
      z = df_assessable$Axis2z[i]
    )

    # Get molecule order (flip if required)
    mol1 = if(!flipped) molecule1 else molecule2
    mol2 = if(!flipped)  molecule2 else molecule1

    # Get binding atom eleno (flip if required)
    mol1_binding_atom_flipped = if(!flipped) mol1_binding_atom else mol2_binding_atom
    mol2_binding_atom_flipped = if(!flipped) mol2_binding_atom else mol1_binding_atom

    # Fetch the proper rotation axis IDs
    mol1_axis_id <- structures::fetch_id_of_first_proper_rotation_axis_with_order(mol1, Cn = mol1_axis_cn)
    mol2_axis_id <- structures::fetch_id_of_first_proper_rotation_axis_with_order(mol2, Cn = mol2_axis_cn)

    # Fetch first dummy atoms from each molecule
    cli::cli_alert_info("Fetching the first dummy atom in each molecule")
    mol1_dummy_eleno <- head(structures::fetch_eleno_by_atom_type(mol1, atom_type = c("Du", "Du.C")), n=1)
    mol2_dummy_eleno <- head(structures::fetch_eleno_by_atom_type(mol2, atom_type = c("Du", "Du.C")), n=1)

    assertions::assert_length_greater_than(
      mol1_dummy_eleno,
      length = 0,
      msg = "Failed to find any dummy atoms in molecule [{mol1@name}]. Please ensure they are correctly labelled in your mol2 file (atom_type must be 'Du' or 'Du.C')"
    )
    assertions::assert_length_greater_than(
      mol2_dummy_eleno,
      length = 0,
      msg = "Failed to find any dummy atoms in molecule [{mol2@name}]. Please ensure they are correctly labelled in your mol2 file (atom_type must be 'Du' or 'Du.C')"
    )


    # Rotate Molecules So Symmetry axes align with targets
    cli::cli_alert_info("Rotating molecules so Symmetry Axes align with target vectors")

    mol1 <- structures::rotate_molecule_so_symmetry_axis_aligns_with_vector(
      mol1,
      symmetry_element_id = mol1_axis_id,
      target = target_axis1_position
    )

    mol2 <- structures::rotate_molecule_so_symmetry_axis_aligns_with_vector(
      mol2,
      symmetry_element_id = mol2_axis_id,
      target = target_axis2_position
    )

    # Extract Symmetry Axes
    cli::cli_alert_info("Extracting Symmetry Axes")
    mol1_proper_rotation_axis <- structures::fetch_symmetry_element_from_molecule(mol1, id = mol1_axis_id, error_if_missing = TRUE)
    mol2_proper_rotation_axis <- structures::fetch_symmetry_element_from_molecule(mol2, id = mol2_axis_id, error_if_missing = TRUE)

    # Translate to molecule so symmetry axis is in position
    mol1 <- mol1 |>
      structures::set_anchor_by_position(mol1_proper_rotation_axis@posA) |>
      structures::translate_molecule_to_position(c(0, 0, 0))

    mol2 <- mol2 |>
      structures::set_anchor_by_position(mol2_proper_rotation_axis@posA) |>
      structures::translate_molecule_to_position(c(0, 0, 0))


    cli::cli_alert_info("Searching for optimal arrangement of molecules")

    # Slide along axis distance between dummy atom of 1 molecule and binding atom of the other
    cli::cli_alert_info("Running optimisation (this may take a moment) ... ")
    optimisation <- find_optimal_position(
      mol1 = mol1,
      mol2 = mol2,
      mol1_axis = target_axis1_position,
      mol2_axis = target_axis2_position,
      mol1_dummy_eleno = mol1_dummy_eleno,
      mol2_dummy_eleno = mol2_dummy_eleno,
      mol1_binding_atom = mol1_binding_atom_flipped,
      mol2_binding_atom = mol2_binding_atom_flipped,
      method = method,
      lower = lower,
      upper = upper,
      control = control,
      hessian = hessian
    )

    optimisation@shapeclass <- shapeclass
    return(optimisation)
  })

  OptimisationResultCollection(
    optimisations = ls_optimisations,
    mol1_not_optimised = molecule1,
    mol2_not_optimised = molecule2
  )
}


# Optimisation ------------------------------------------------------------

#' Find Optimal Position of molecules
#'
#' @inheritParams screen_molecules
#' @param mol1 A [Molecule3D()] with a proper rotation axis that has been transformed such that it lies on mol1_axis.
#' @param mol2 A [Molecule3D()] with a proper rotation axis that has been transformed such that it lies on mol2_axis.
#' @param mol1_axis,mol2_axis symmetry axis about which to transform molecules when searching for a geometrically optimal solution
#' @param mol1_dummy_eleno,mol2_dummy_eleno eleno of dummy atom representing the binding atom of opposing molecule
#'
#' @returns A list including:
#' mol1_optimal: A [structures::Molecule3D()] object representing the input molecule geometrically optimised to minimiise min_sum_of_squared_distance
#' mol2_optimal: A [structures::Molecule3D()] object representing the input molecule geometrically optimised to minimiise min_sum_of_squared_distance
#' min_sum_of_squared_distance: Minimised sum of squared distance between the dummy atoms of each molecule and the opposing atom binding atom.
#'  Formally: d1^2 + d2^2 where d1 = distance from mol1 dummy to mol2 binding atom and d2 = distance from mol2 dummy to mol1 binding atom
#' angle_between_dummy_binding_vectors: The angle (in radians) between vectors from mol1 dummy atom -> mol1 bonding atom and mol2 dummy atom -> mol2 bonding atom. This value is calculated from the geometrically optimised solution, and if a perfect solution is found would be equal to pi (180 degrees)
#' mol1_phi: optimal angle (in radians) about which mol1 was rotated about mol1_axis to give geometrically optimal solution
#' mol2_phi: optimal angle (in radians) about which mol2 was rotated about mol2_axis to give geometrically optimal solution
#' mol1_slide: optimal distance to slide mol1 along mol1_axis to give geometrically optimal solution.
#' mol2_slide: optimal distance to slide mol2 along mol2_axis to give geometrically optimal solution.
#' mol1_axis: length-3 vector describing xyz direction of symmetry axis about which mol1 was transformed around
#' mol2_axis: length-3 vector describing xyz direction of symmetry axis about which mol2 was transformed around
#' n_calls_to_fn: number of calls to optimisation function (fn)
#' n_calls_to_gr: number of calls to gradient function (gr)
#' convergence: convergence value. 1 indicates that the iteration limit maxit had been reached. 10 indicates degeneracy of the Nelder–Mead simplex. 51 indicates a warning from the "L-BFGS-B" method; see component message for further details. 52 indicates an error from the "L-BFGS-B" method; see component message for further details.
#' message: A character string giving any additional information returned by the optimizer, or NULL.
#' hessian: Only if argument hessian is true. A symmetric matrix giving an estimate of the Hessian at the solution found. Note that this is the Hessian of the unconstrained problem even if the box constraints are active.
#'
find_optimal_position <- function(
    mol1, mol2,
    mol1_axis, mol2_axis,
    mol1_dummy_eleno, mol2_dummy_eleno,
    mol1_binding_atom, mol2_binding_atom,
    method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
               "Brent"),
    lower = -Inf, upper = Inf,
    control = list(), hessian = FALSE
){
  method = rlang::arg_match(method)
  mol1_dummy_eleno <- as.character(mol1_dummy_eleno)
  mol2_dummy_eleno <- as.character(mol2_dummy_eleno)
  mol1_binding_atom <- as.character(mol1_binding_atom)
  mol2_binding_atom <- as.character(mol2_binding_atom)

  # Prepare matrices of atom positions (faster to modify)
  mol1_atom_mx_all <- as.matrix(mol1)[,c("x", "y", "z"), drop=FALSE]
  mol2_atom_mx_all <- as.matrix(mol2)[,c("x", "y", "z"), drop=FALSE]

  # We do NOT need to slide and rotate the whole molecule to find the optimum positions.
  # Instead we can just slide/move the binding and dummy atoms (mol1_dummy_eleno, mol2_dummy_eleno, mol1_binding_atom, mol2_binding_atom)
  # This will make compute faster so we can optimise more samples. The code below drops unnecessary atoms from our matrix
  mol1_atom_mx <- mol1_atom_mx_all[c(mol1_dummy_eleno, mol1_binding_atom), , drop=FALSE]
  mol2_atom_mx <- mol2_atom_mx_all[c(mol2_dummy_eleno, mol2_binding_atom), , drop=FALSE]

  optimisation_results <- optim(c("mol1_phi" = 0, "mol1_slide" = 0, "mol2_phi" = 0, "mol2_slide" = 0), fn = function(x){
    mol1_phi=x[1]
    mol1_slide=x[2]
    mol2_phi=x[3]
    mol2_slide=x[4]

    # Rotate Molecule
    mol1_rotated_mx <- move::rotate_table_around_axis(mol1_atom_mx, rotation_axis = mol1_axis, angle = mol1_phi)
    mol2_rotated_mx <- move::rotate_table_around_axis(mol2_atom_mx, rotation_axis = mol2_axis, angle = mol2_phi)

    # Slide Molecule
    mol1_slid_mx <- move::translate_table_in_direction(mol1_rotated_mx, direction = mol1_axis, magnitude = mol1_slide)
    mol2_slid_mx <- move::translate_table_in_direction(mol2_rotated_mx, direction = mol2_axis, magnitude = mol2_slide)

    # Compute distance between dummy and binding atom
    pos_mol1_dummy = mol1_slid_mx[mol1_dummy_eleno, , drop=TRUE]
    pos_mol2_dummy = mol2_slid_mx[mol2_dummy_eleno, , drop=TRUE]
    pos_mol1_binding_atom = mol1_slid_mx[mol1_binding_atom, , drop=TRUE]
    pos_mol2_binding_atom = mol2_slid_mx[mol2_binding_atom, , drop=TRUE]

    d1 <- move::measure_distance_between_two_points(pos_mol1_dummy, pos_mol2_binding_atom)
    d2 <- move::measure_distance_between_two_points(pos_mol2_dummy, pos_mol1_binding_atom)

    # Our objective is to minimise the sum of square distance between the dummy atoms of each molecule and the opposing atom binding atom
    obj = d1^2 + d2^2
    return(obj)
  },
  method = method,
  upper = upper, lower = lower,
  control = control, hessian=hessian
  )

  # Apply optimal transformations to molecules
  cli::cli_alert_info("Optimisation finished! Compiling results ... ")
  optimal_mol1_phi <- optimisation_results$par["mol1_phi"]
  optimal_mol2_phi <- optimisation_results$par["mol2_phi"]
  optimal_mol1_slide <- optimisation_results$par["mol1_slide"]
  optimal_mol2_slide <- optimisation_results$par["mol2_slide"]

  # Return Molecule3D of optimised results
  mol1_optimal <- mol1 |>
    structures::rotate_molecule_around_vector(axis = mol1_axis, angle = optimal_mol1_phi) |>
    structures::translate_molecule_by_vector(move::normalise(mol1_axis) * optimal_mol1_slide)

  mol2_optimal <- mol2 |>
    structures::rotate_molecule_around_vector(axis = mol2_axis, angle = optimal_mol2_phi) |>
    structures::translate_molecule_by_vector(move::normalise(mol2_axis) * optimal_mol2_slide)


  # Compute angle created by vectors dummy -> binding atom for each molecule. We'd expect this to be zero
  mol1_pos_dummy <- structures::fetch_atom_position(mol1_optimal, eleno = mol1_dummy_eleno)
  mol1_pos_binding <- structures::fetch_atom_position(mol1_optimal, eleno = mol1_binding_atom)
  mol2_pos_dummy <- structures::fetch_atom_position(mol2_optimal, eleno = mol2_dummy_eleno)
  mol2_pos_binding <- structures::fetch_atom_position(mol2_optimal, eleno = mol2_binding_atom)
  v1 = move::create_vector_from_start_end(mol1_pos_dummy, mol1_pos_binding)
  v2 = move::create_vector_from_start_end(mol2_pos_dummy, mol2_pos_binding)
  angle_between_dummy_binding_vectors <- move::measure_angle_between_vectors(a = v1, b=v2, degrees = FALSE)

  OptimisationResult(
    mol1 = mol1_optimal,
    mol2 = mol2_optimal,
    min_sum_of_squared_distance = optimisation_results$value,
    angle_between_dummy_binding_vectors = angle_between_dummy_binding_vectors,
    mol1_phi = optimal_mol1_phi,
    mol2_phi = optimal_mol2_phi,
    mol1_slide = optimal_mol1_slide,
    mol2_slide = optimal_mol2_slide,
    mol1_axis = mol1_axis,
    mol2_axis = mol2_axis,
    n_calls_to_fn = optimisation_results$counts[1],
    n_calls_to_gr = optimisation_results$counts[2],
    convergence = optimisation_results$convergence,
    message = optimisation_results$message,
    hessian = optimisation_results$hessian
  )
  # return_object <- list(
  #     mol1 = mol1_optimal,
  #     mol2 = mol2_optimal,
  #     min_sum_of_squared_distance = optimisation_results$value,
  #     angle_between_dummy_binding_vectors = angle_between_dummy_binding_vectors,
  #     mol1_phi = optimal_mol1_phi,
  #     mol2_phi = optimal_mol2_phi,
  #     mol1_slide = optimal_mol1_slide,
  #     mol2_slide = optimal_mol2_slide,
  #     mol1_axis = mol1_axis,
  #     mol2_axis = mol2_axis,
  #     n_calls_to_fn = optimisation_results$counts[1],
  #     n_calls_to_gr = optimisation_results$counts[2],
  #     convergence = optimisation_results$convergence,
  #     message = optimisation_results$convergence,
  #     hessian = optimisation_results$hessian
  #   )
  #
  # return(return_object)
}



# Shapeclass utils --------------------------------------------------------


axes_to_shapeclass_reference <- function(){

  df <- read.csv(system.file(package = "symbo", "shapeclass_info.csv"))
  df$AxisComboKey <- paste(df$Axis1,df$Axis2)

  return(df)
}

list_all_shapeclasses <- function(){
  axes_to_shapeclass_reference()[["ShapeClass"]]
}


#' Get assessable polyhedral geometries from symmetry axis orders
#'
#' Given two sets of unique proper rotation axis orders for two molecules,
#' this function returns the subset of geometries that can be assessed
#' with those symmetries.
#'
#' Geometries are matched in two orientations:
#' \itemize{
#'   \item \strong{Forward:} \code{molecule1_axes} supply \code{Axis1_order}
#'         and \code{molecule2_axes} supply \code{Axis2_order}.
#'   \item \strong{Swapped:} \code{molecule2_axes} supply \code{Axis1_order}
#'         and \code{molecule1_axes} supply \code{Axis2_order}.
#' }
#'
#' For geometries that are only assessable in the swapped orientation,
#' \code{treat_molecule2_as_1} is set to \code{TRUE} to indicate that
#' downstream code should treat molecule 2 as molecule 1.
#'
#' @param molecule1_axes A numeric or character vector of unique proper
#'   rotation axis orders present in molecule 1 (e.g. \code{c(3, 4)}).
#' @param molecule2_axes A numeric or character vector of unique proper
#'   rotation axis orders present in molecule 2 (e.g. \code{c(2)}).
#' @param mapping A \code{data.frame} describing which symmetry-axis
#'   combinations correspond to which geometries. It must contain at least
#'   the columns \code{ShapeClass}, \code{Axis1_order}, and \code{Axis2_order};
#'   additional columns (e.g. \code{Notes}) are preserved in the output.
#'   Defaults to \code{symbo::ShapeClass_axis_map}.
#'
#' @return A \code{data.frame} containing all assessable geometries given
#'   the supplied axes. It includes all columns from \code{mapping}, plus:
#'   \itemize{
#'     \item \code{treat_molecule2_as_1}: logical; \code{FALSE} if the
#'       ShapeClass is assessable in the forward orientation
#'       (\code{mol1 -> Axis1}, \code{mol2 -> Axis2}),
#'       \code{TRUE} if only assessable when molecules are swapped.
#'   }
#'   If no geometries are assessable, an empty \code{data.frame} is returned.
#'
#' @examples
#' # Suppose molecule 1 has C3 and C4, molecule 2 has C2:
#' get_assessable_shapeclasses(molecule1_axes = c(3, 4),
#'                           molecule2_axes = 2)
#'
#' # If molecule 1 only has C2 and molecule 2 has C3, some geometries
#' # (e.g. edge-capped dodecahedron) are only assessable by swapping:
#' get_assessable_shapeclasses(molecule1_axes = 2,
#'                           molecule2_axes = 3)
#'
#' @export
get_assessable_shapeclasses <- function(molecule1_axes, molecule2_axes) {

  # Coerce to integer (in case user passes characters like "2", "3")
  mol1 <- as.integer(molecule1_axes)
  mol2 <- as.integer(molecule2_axes)

  df_combos <- create_combos(molecule1_axes, molecule2_axes)

  mapping <- axes_to_shapeclass_reference()

  df_combos_annotated <- df_combos |>
    dplyr::left_join(mapping, by = "AxisComboKey")

  df_combos_annotated$assessable <- !is.na(df_combos_annotated$ShapeClass)

  df_combos_annotated <- df_combos_annotated |>
    dplyr::filter(assessable) |>
    dplyr::slice_head(n=1, by = ShapeClass)

  df_combos_annotated$AxisComboKey <- NULL

  return(df_combos_annotated)
}

create_combos <- function(molecule1_axes, molecule2_axes){
  df_combos_unflipped = expand.grid(mol1_axis=molecule1_axes, mol2_axis = molecule2_axes)
  df_combos_unflipped$flipped <- FALSE
  df_combos_flipped <- expand.grid(mol1_axis=molecule2_axes, mol2_axis = molecule1_axes)
  df_combos_flipped$flipped <- TRUE
  df_combos <- rbind(df_combos_unflipped, df_combos_flipped)
  df_combos$AxisComboKey <- paste(df_combos$mol1, df_combos$mol2)

  return(df_combos)
}

bind_rows_into_mx <- function(ls){
  as.matrix(do.call(rbind, ls))
}
