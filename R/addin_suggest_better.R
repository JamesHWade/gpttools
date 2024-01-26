#' Suggest code improvements using a code copilot
#'
#' This function provides suggestions for improving the provided code using an
#' expert code copilot. The output is formatted for inclusion in an R file.
#'
#' @return A string containing suggestions for code improvements, including
#'   descriptions of the changes and any necessary annotations as code comments.
#'   The output is formatted for inclusion in an R file.
suggest_code_improvements <- function() {
  instructions <- paste(
    "You are an expert code copilot. How can the provided code be improved?",
    "Briefly describe why you made the change.",
    "Any text or annotations should be included as code",
    "comments. Do not use code blocks. The output goes into an R file. Do not",
    "provide tests or examples.",
    collapse = " "
  )
  gpt_chat(instructions)
}
