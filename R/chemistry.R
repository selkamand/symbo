fetch_atom_positions <- function(data, atom_ids){

  df_xyz <- data |>
    dplyr::filter(eleno %in% atom_ids) |>
    dplyr::select(eleno, x, y, z) |>
    dplyr::bind_rows()

  rownames(df_xyz) <- df_xyz[["eleno"]]
  df_xyz[["eleno"]] <- NULL
  return(df_xyz)
}
