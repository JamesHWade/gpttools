#' Copilot-style Code Suggestions
#'
#'
#' @export
copilot_addin <- function() {
  cli::cli_alert_info("Attempting to add code suggestions")
  ghost_chat(
    service = getOption("gpttools.service", "openai"),
    stream  = TRUE,
    where   = "source"
  )
  cli::cli_alert_info("Done adding code suggestion")
}
