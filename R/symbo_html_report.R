#' Create a report summarising results of screen
#'
#'  Create a report summarising results of screen
#'
#' @param optimisations an [`OptimisationResultCollection`] object produced by [screen_molecules()]
#'
#' @returns invisibly returns NULL (this function is run for its side effects)
#' @export
#'
create_summary_report <- function(optimisations, outdir = getwd(), prefix = "screen_results"){
  assertions::assert_class(optimisations, class = "symbo::OptimisationResultCollection")
  path_template <- system.file(package = "symbo", "template/template.Rmd")

  outpath = sprintf("%s/%s.html", outdir, prefix)
  cli::cli_alert_info("Saving report to {.path {outpath}}")
  rmarkdown::render(
    input = path_template,
    output_format = "html_document",
    output_file = prefix,
    output_dir = outdir,
    params = list(collection = optimisations),
    intermediates_dir = outdir
  )

  utils::browseURL(url = outpath)


  invisible(NULL)
}
