#' Settings App for gpttools
#'
#'
#' @return This function has no return value.
#'
#' @export
launch_settings <- function() {
  run_app_file <- system.file("settings/app.R", package = "gpttools")
  shiny::runApp(run_app_file)
}
