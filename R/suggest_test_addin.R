#' Suggest a unit test for a function
#'
#' Prompt: "Suggest a unit test for this function using the testthat package"
#'
#' @export
suggest_unit_test_addin <- function() {
  gpt_insert(
    model = "text-davinci-003",
    prompt = "Suggest a unit test for this function with R package testthat",
    temperature = 0.5,
    max_tokens = getOption("gpttools.max_tokens"),
    append_text = TRUE
  )
}
