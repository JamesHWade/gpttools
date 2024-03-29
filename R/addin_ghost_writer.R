#' Writing suggestions
#'
#'
#' @export
ghost_writer_addin <- function() {
  cli::cli_alert_info("Attempting to add suggestion")
  ghost_writer(
    service = getOption("gpttools.service", "openai"),
    stream  = TRUE,
    where   = "source"
  )
  cli::cli_alert_info("Done adding code suggestion")
}
