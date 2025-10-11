enrich_bonds_with_xyz_position <- function(bonds, atoms, origin = "origin", target="target", atom_id="eleno"){
  df_atoms_minimal <- atoms[,c(atom_id, "x", "y", "z"),drop=FALSE]
  df_atoms_minimal_end <- df_atoms_minimal
  colnames(df_atoms_minimal_end) <- c(atom_id, "xend", "yend", "zend")

  bonds |>
    dplyr::left_join(df_atoms_minimal, by = list(x=origin, y = atom_id)) |>
    dplyr::left_join(df_atoms_minimal_end, by = list(x=target, y = atom_id))
}
