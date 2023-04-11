#' Convert Script to Function Addin
#'
#' "convert this R code into an R function"
#'
#' @export
#'
script_to_function_addin <- function() {
  instructions <- paste(
    "You are an expert code copilot. Convert this R code into an R function",
    "or a few R functions. Any text or annotations should be included as code",
    "comments. Do not use code blocks. The output goes into an R file. Do not",
    "provide tests or examples.",
    collapse = " "
  )
  gpt_chat(instructions)
}
