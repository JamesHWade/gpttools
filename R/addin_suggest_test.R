#' Suggest a unit test for a function
#'
#' Prompt: "Suggest a unit test for this function using the testthat package"
#'
#' @export
suggest_unit_test_addin <- function() {
  instructions <- paste(
    "You are an expert code copilot.",
    "Suggest a unit test for this function with R package testthat.",
    "Any text or annotations should be included as code",
    "comments. Do not use code blocks. The output goes into an R file. Do not",
    "provide tests or examples.",
    collapse = " "
  )
  gpt_chat(instructions)
}
