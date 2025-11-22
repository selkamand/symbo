
# Visualise Optimisation Results ------------------------------------------
plot_optimisation_results <- function(optimisation){

}

summarise_optimisation_results <- function(optimisation){

}

plot_c34_helpers <- function(c3, c4, c3_colour="gold", c4_colour = "purple"){
  # Plot C3 and 4 vectors
  rgl::abclines3d(x=c(0, 0, 0), a=c3, color = c3_colour, lit=FALSE)
  rgl::abclines3d(x=c(0, 0, 0), a=c4, color = c4_colour, lit=FALSE)

  # Plot C3xC4 plane
  c34_plane <-  move::compute_plane_normal_from_vectors(c3, c4)
  rgl::planes3d(c34_plane, lit=FALSE, color="pink", alpha = 0.2)

  # Draw 2-Unit Cube
  cube_corners <- data.frame(
    x = c(1, 1, -1, -1, 1, 1, -1, -1),
    y = c(1, -1, 1, -1, 1, -1, 1, -1),
    z = c(1, 1, 1, 1, -1, -1, -1, -1)
  )

  cube_face <- data.frame(
    x = c(1, -1, 0, 0, 0, 0),
    y = c(0, 0, 1, -1, 0, 0),
    z = c(0, 0, 0, 0, 1, -1)
  )
  rgl::wire3d(rgl::cube3d(), color = "red", lit=FALSE)
  rgl::points3d(cube_face, col="red")
  rgl::spheres3d(cube_corners, color = "pink", radius=0.1)
}

pal_bondy <- function() {c(chemviewR::pal_atoms(), "Du" = "yellow")}
