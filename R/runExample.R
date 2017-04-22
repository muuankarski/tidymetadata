#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "lisapp", package = "tidymetadata")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `tidymetadata`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
