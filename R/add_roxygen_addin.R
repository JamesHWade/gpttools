#' Add Roxygen documentation to a function
#'
#' This function uses the OpenAI API to generate a roxygen skeleton for the
#' current selection in RStudio. The roxygen skeleton is then inserted into the
#'  document.
#'
#' @return NULL (nothing is returned; the generated roxygen skeleton is inserted into the document).
#' @export
#'
add_roxygen_addin <- function() {
  gpt_insert(
    model = "text-davinci-003",
    prompt = "insert roxygen skeleton to document this function",
    temperature = 0.1,
    top_p = 1
  )
}
