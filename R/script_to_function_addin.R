#' Convert Script to Function Addin
#'
#' This function converts a script into a function
#'
#' @export
#'
script_to_function_addin <- function() {
  gpt_edit(
    model = "code-davinci-edit-001",
    instruction = "convert this R code into an R function",
    temperature = 0.1,
    top_p = 1
  )
}
