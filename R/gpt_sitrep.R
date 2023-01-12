#' GPT Sitrep
#'
#' Check the status of the OpenAI API and RStudio API, and inform
#' the user of the settings for gpttools.
#'
#' @return A message is returned to the user informing them of the status of the
#' OpenAI API, RStudio API, and gpttools settings.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' gpt_sitrep()
#' }
gpt_sitrep <- function() {
  cli_rule("OpenAI API Status")
  if (is_true(getOption("gpttools.valid_api"))) {
    api_key <- obscure_key(Sys.getenv("OPENAI_API_KEY"))
    cli_bullets(c(
      "v" = "OpenAI API has been check and is valid",
      "i" = "API key set to {api_key}"
    ))
  } else {
    cli_inform(c("x" = "OpenAI API not validated."))
    api_check <- usethis::ui_yeah("Would you like to perform an API check?")
    if (api_check) {
      check_api()
    }
  }
  cli_rule("RStudio API")
  if (rstudioapi::isAvailable()) {
    options(gpttools.valid_rstudioapi = TRUE)
    cli_inform(c("v" = "Addins likely with work, rstudioapi is available."))
  }
  cli_rule("Settings for gpttools")
  max_tokens <- getOption("gpttools.max_tokens")
  cli_inform(c("i" = "Max tokens set to {col_br_yellow(max_tokens)}"))
  clcode_style <- getOption("gpttools.code_style")
  cli_inform(c("i" = "Code style is set to {col_br_green(code_style)}"))
}
