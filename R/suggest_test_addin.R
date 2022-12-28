#' Suggest a unit test for a function
#'
#' Prompt: "Suggest a unit test for this function using the testthat package"
#'
#' @export
suggest_unit_test_addin <- function() {
  gpt_insert(
    model = "text-davinci-003",
    prompt = "Suggest a unit text for this function use the testthat package",
    temperature = 0.5,
    top_p = 1,
    max_tokens = 1000,
    append_text = TRUE
  )
}
