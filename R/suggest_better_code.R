#' Suggest code improvements
#'
#' Prompt: "I'm writing R code for use in a package. How can I improve this
#' code:"
#'
#' @export
suggest_code_improvements <- function() {
  gpt_insert(
    model = "text-davinci-003",
    prompt = glue::glue(
      "I'm writing R code for use in a package.",
      "\nHow can I improve this code:"
    ),
    temperature = 0.5,
    max_tokens = getOption("gpttools.max_tokens"),
    append_text = TRUE
  )
}
