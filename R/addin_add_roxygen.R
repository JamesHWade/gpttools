#' Add Roxygen documentation to a function
#'
#' This function uses the OpenAI API to generate a roxygen skeleton for the
#' current selection in RStudio. The roxygen skeleton is then inserted into the
#' document using prompt: "insert roxygen skeleton to document this function"
#'
#' @return NULL (nothing is returned; the generated roxygen skeleton is
#'  inserted into the document).
#' @export
#'
add_roxygen_addin <- function() {
  gptstudio::gpt_insert(
    model = "text-davinci-003",
    prompt = "Insert a roxygen skeleton to document this function:\n\n",
    temperature = 0.5
  )
}
