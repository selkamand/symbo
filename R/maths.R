#'
#' # Set vector magnitude to 1 (divide each element by magnitude of vector)
#' normalise <- function(x) {x / sqrt(sum(x^2))}
#'
#'
#'
#' # Rotation ----------------------------------------------------------------
#'
#' #' Rotate a 3D vector to align with a target direction (Rodrigues' formula)
#' #'
#' #' Rotates a 3D vector \code{v} so that its \emph{direction} aligns with the
#' #' direction of \code{target}, using Rodrigues' rotation formula.
#' #' The returned vector preserves the original length of \code{v}.
#' #'
#' #' @details
#' #' Let \eqn{\mathbf{u} = \frac{\mathbf{v}}{\|\mathbf{v}\|}} and
#' #' \eqn{\mathbf{t} = \frac{\mathbf{target}}{\|\mathbf{target}\|}} be unit vectors.
#' #' Define \eqn{\theta = \arccos(\mathbf{u}\cdot\mathbf{t})} and rotation axis
#' #' \eqn{\mathbf{k} = \frac{\mathbf{u}\times\mathbf{t}}{\|\mathbf{u}\times\mathbf{t}\|}} (if not degenerate).
#' #'
#' #' Rodrigues' rotation formula rotates any vector \eqn{\mathbf{x}} about unit axis \eqn{\mathbf{k}}
#' #' by angle \eqn{\theta} via:
#' #' \deqn{
#' #'   \mathrm{Rot}_{\mathbf{k},\theta}(\mathbf{x}) =
#' #'   \mathbf{x}\cos\theta + (\mathbf{k}\times\mathbf{x})\sin\theta + \mathbf{k}\,(\mathbf{k}\cdot\mathbf{x})(1-\cos\theta).
#' #' }
#' #' This function applies the formula to \eqn{\mathbf{v}} with \eqn{\mathbf{k}} chosen to rotate
#' #' \eqn{\mathbf{u}} toward \eqn{\mathbf{t}} (right-hand rule).
#' #'
#' #' Edge cases handled:
#' #' \itemize{
#' #'   \item \strong{Already aligned} (\eqn{\theta \approx 0}): returns \code{v} unchanged.
#' #'   \item \strong{Opposite directions} (\eqn{\theta \approx \pi}): chooses a stable axis orthogonal to \code{v}
#' #'         and applies the \eqn{180^\circ} rotation (closed form).
#' #'   \item \strong{Numerical safety}: clamps the dot product to \code{[-1, 1]} before \code{acos}.
#' #' }
#' #'
#' #' @param v Numeric length-3 vector. The vector to rotate (need not be unit length).
#' #' @param target Numeric length-3 vector. Direction to align \code{v} with (need not be unit;
#' #'        only direction matters).
#' #' @param tol Numeric scalar. Tolerance for detecting degeneracies (parallel/opposite/zero norms).
#' #' @param return Character; either \code{"vprime"} (default) to return the rotated vector,
#' #'   or \code{"axis_plus_angle"} to return a list with rotation axis and angle.
#' #'
#' #' @returns A numeric length-3 vector \code{v_prime} representing \code{v} rotated so that its
#' #' direction aligns with \code{target}. The \emph{magnitude} of \code{v_prime} matches \code{||v||}.
#' #' If return = "axis_plus_angle" returns a unit-vector rotation axis and angle that can be applied in \code{rotate_vector_around_axis()}
#' #'
#' #' @section Potential pitfalls:
#' #' \itemize{
#' #'   \item \strong{Zero vectors:} If \code{v} or \code{target} has (near) zero norm,
#' #'         rotation is undefined;
#' #'   \item \strong{Axis normalization:} The rotation axis must be a \emph{unit} vector. We automatically scale non-unit target vectors
#' #'   \item \strong{180° case:} When \code{v} and \code{target} are opposite, \code{u × t = 0} and the axis
#' #'         is not defined. We choose a stable axis orthogonal to \code{v} automatically.
#' #' }
#' #'
#' #' @references
#' #' Rodrigues, O. (1840). \emph{Des lois géométriques qui régissent les déplacements d’un système solide dans l’espace}.
#' #'
#' #' @seealso
#' #' \code{rotations::genR()} (CRAN) for generating rotation matrices via Rodrigues’ formula.
#' #'
#' #' @examples
#' #' # 1) Simple 90° rotation around z: (1,0,0) -> (0,1,0) when target = (0,1,0)
#' #' rotate_vector_to_align_with_target(c(1,0,0), c(0,1,0))
#' #'
#' #' # 2) Align arbitrary v with (1,1,1): direction matches, length preserved
#' #' v      <- c(2, -3, 4)
#' #' target <- c(1, 1, 1)
#' #' v_rot  <- rotate_vector_to_align_with_target(v, target)
#' #' sqrt(sum(v^2))            # original length
#' #' sqrt(sum(v_rot^2))        # same length
#' #' cor(v_rot, target)        # ~ 1 (directions aligned)
#' #'
#' #' # 3) Opposite directions (180°): target is -v's direction
#' #' v      <- c(1, 2, 3)
#' #' target <- -v
#' #' rotate_vector_to_align_with_target(v, target)
#' #'
#' #' # 4) Input validation
#' #' \dontrun{
#' #' rotate_vector_to_align_with_target(c(0,0,0), c(1,0,0))  # error: zero-norm v
#' #' rotate_vector_to_align_with_target(c(1,0,0), c(0,0,0))  # error: zero-norm target
#' #' }
#' #'
#' #' @export
#' rotate_vector_to_align_with_target <- function(v, target, tol=1e-8, return = c("vprime", "axis_plus_angle")){
#'
#'   return <- rlang::arg_match(return)
#'
#'   if (!is.numeric(v) || length(v) != 3L) {
#'     stop("`v` must be a numeric vector of length 3.")
#'   }
#'   if (!is.numeric(target) || length(target) != 3L) {
#'     stop("`target` must be a numeric vector of length 3.")
#'   }
#'
#'   if(sum(abs(v)) < tol) {
#'     warning("Cannot rotate a zero vector")
#'     return(NA)
#'   }
#'
#'   # Unname inputs
#'   v <- unname(v); target <- unname(target)
#'
#'   # Ensure target is a unit vector
#'   target_unit <- normalise(target)
#'   v_unit <- normalise(v)
#'
#'   # Find the rotation axis
#'   n <- normalise(pracma::cross(v, target))
#'
#'   # Find the rotation angle
#'   costheta <- as.vector(v_unit %*% target_unit)
#'
#'   # Clamp result to -1 to 1. It should already be restricted but floating point errors can cause values very slightly outside this range
#'   costheta <- clamp(costheta, min = -1, max = 1)
#'
#'   # Rotation in radians
#'   theta = acos(costheta)
#'
#'   # If theta is 0 then they're parallel and we can return v as is
#'   if(theta == 0) return(v)
#'   if(theta == pi) {
#'     # If v and target are anti-parallel, rotate target by 90 degrees and use that as the new rotation axis
#'     n = normalise(pracma::cross(target_unit, c(1, 0, 0)))
#'   }
#'
#'   # Rotation in degrees
#'   # theta * 180/pi
#'
#'   sintheta = as.vector(sin(theta))
#'
#'   if(return == "axis_plus_angle") {
#'     return(list("axis" = n, "angle" = theta))
#'   }
#'
#'   # Apply rodrigues transformation
#'   vprime = v * costheta + (pracma::cross(n, v)) * sintheta + n * as.vector(n %*% v) * (1-costheta)
#'
#'   # Return vector with very small numbers zapped to zero
#'   zapsmall(vprime)
#' }
#'
#'
#' #' Rotate a vector around an axis by a given angle
#' #'
#' #' Rotates a 3D vector \code{v} about \code{rotation_axis} by \code{angle} radians
#' #' using Rodrigues' rotation formula. Positive angles follow the right-hand rule.
#' #'
#' #' @param v Numeric length-3 vector to rotate.
#' #' @param rotation_axis Numeric length-3 vector; rotation axis (will be normalized).
#' #' @param angle Numeric scalar (radians); rotation angle.
#' #' @param tol Numeric; tolerance for early return check (\code{sum(v) < tol}).
#' #'
#' #' @return Rotated vector of same length as \code{v}.
#' #'
#' #' @note
#' #' - Early return uses \code{sum(v) < tol}, which may skip rotation for some nonzero vectors.
#' #' - Depends on user-defined \code{normalise()} and \code{pracma::cross()}.
#' #' - No input validation; assumes 3-element numeric vectors and nonzero rotation axis.
#' #'
#' #' @examples
#' #' # Rotate (1,0,0) by 90° around z-axis -> (0,1,0)
#' #' v <- c(1,0,0); axis <- c(0,0,1)
#' #' rotate_vector_around_axis(v, axis, pi/2)
#' #'
#' #' @export
#' rotate_vector_around_axis_bondy <- function(v, rotation_axis, angle, tol=1e-8){
#'
#'   # Unname inputs
#'   v <- unname(v); rotation_axis <- unname(rotation_axis)
#'
#'   # If zero return untransformed
#'   if(sum(abs(v)) < tol) return(v)
#'
#'   # Get stats required for rodrigues rotation
#'   sintheta = as.vector(sin(angle))
#'   costheta = as.vector(cos(angle))
#'
#'   # normalise the rotation axis
#'   n = normalise(rotation_axis)
#'
#'   # Rotate vector
#'   vprime = v * costheta + (pracma::cross(n, v)) * sintheta + n * as.vector(n %*% v) * (1-costheta)
#'
#'   # Return vector with very small numbers zapped to zero
#'   zapsmall(vprime)
#' }
#'
#' clamp <- function(x, min = -1, max = 1){
#'    pmax(min, pmin(max, x))
#' }
#'
#' rotate_to_phi_equals_zero <- function(){
#'
#' }
#'
#' #' Compute the rotation needed to align a 3D vector with a plane
#' #'
#' #' Calculates the minimal rotation that moves vector `v` so it lies in the
#' #' plane with normal `plane_normal`. Returns a list containing the rotation
#' #' axis (unit vector) and rotation angle (radians).
#' #'
#' #' @param v numeric(3) Vector to rotate.
#' #' @param plane_normal numeric(3) Plane normal (not necessarily unit length).
#' #' @return list with elements `axis` (unit vector) and `angle` (radians).
#' #' @examples
#' #' rotate_vector_into_a_plane(c(1,2,3), c(0,1,-1))
#' rotate_vector_into_a_plane <- function(v, plane_normal) {
#'
#'   # Ensure both vectors are numeric, no names attached
#'   v <- unname(v)
#'   plane_normal <- unname(plane_normal)
#'
#'   # Normalise plane normal
#'   plane_normal_norm <- normalise(plane_normal)
#'
#'   # --- Compute in-plane component of v ---------------------------------------
#'   # Convert dot product to numeric scalar to avoid array recycling warning
#'   v_dot_n <- as.numeric(v %*% plane_normal_norm)
#'   v_inplane <- v - v_dot_n * plane_normal_norm
#'
#'   # --- Compute rotation axis -------------------------------------------------
#'   # Axis perpendicular to both v and its in-plane projection
#'   u <- pracma::cross(v, v_inplane)
#'   k <- normalise(u)                  # unit rotation axis
#'   u_l2 <- sqrt(sum(u^2))             # magnitude of u (for angle)
#'
#'   # --- Compute rotation angle (radians) --------------------------------------
#'   theta <- atan2(u_l2, as.numeric(v %*% v_inplane))
#'
#'   # Return rotation axis and angle
#'   list(axis = k, angle = theta)
#' }
#'
#'
#'
#'
#' # Translation -------------------------------------------------------------
#' translate_position_by_vector <- function(position, vector){
#'   if(length(position) != length(vector)) stop("To translate a position by a vector the position and translation vector must have the same number of elements")
#'   position + vector
#' }
#'
#' translate_position_in_direction <- function(position, direction, magnitude){
#'   translation_vector = normalise(direction) * magnitude
#'   position + translation_vector
#' }
#'
