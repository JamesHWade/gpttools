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
    cli_inform(c(
      "x" = "OpenAI API not validated.",
      "i" = "You can validate API by calling {.code gptstudio::check_api()}"
    ))
  }
  cli_rule("RStudio API")
  if (rstudioapi::isAvailable()) {
    options(gpttools.valid_rstudioapi = TRUE)
    cli_inform(c("v" = "Addins likely with work, rstudioapi is available."))
  }
  cli_rule("Settings for gpttools")
  max_tokens <- getOption("gpttools.max_tokens")
  cli_inform(c("i" = "Max tokens set to {col_br_yellow(max_tokens)}"))
  code_style <- getOption("gpttools.code_style")
  cli_inform(c("i" = "Code style is set to {col_br_green(code_style)}"))
}

obscure_key <- function(api_key) {
  if (nchar(api_key) == 0) {
    "no key provided"
  } else if (nchar(api_key) > 8) {
    api_start <- substr(api_key, 1, 4)
    api_mid <- paste0(rep("*", nchar(api_key) - 8), collapse = "")
    api_end <- substr(api_key, nchar(api_key) - 3, nchar(api_key))
    paste0(api_start, api_mid, api_end)
  } else {
    "<hidden> (too short to obscure)"
  }
}
