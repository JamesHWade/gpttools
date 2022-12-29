#' Convert Script to Function Addin
#'
#' "convert this R code into an R function"
#'
#' @export
#'
script_to_function_addin <- function() {
  gpt_edit(
    model = "code-davinci-edit-001",
    instruction = "convert this R code into an R function or a few R functions",
    temperature = 0.3
  )
}
