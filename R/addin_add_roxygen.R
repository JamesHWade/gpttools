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
  instructions <- paste(
    "You are an expert code copilot. Insert a roxygen skeleton to document",
    "this function. Any text or annotations should be included as code",
    "comments. Do not use code blocks. The output goes into an R file.",
    collapse = " "
  )
  gpt_chat(instructions)
}
