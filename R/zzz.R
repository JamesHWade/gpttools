.onLoad <- function(lib, pkg) {
  op <- options()
  op_gpttools <- list(
    gpttools.valid_api        = FALSE,
    gpttools.openai_key       = NULL,
    gpttools.max_tokens       = 500,
    gpttools.valid_rstudioapi = FALSE
  )

  toset <- !(names(op_gpttools) %in% names(op))
  if (any(toset)) options(op_gpttools[toset])
  invisible()
}


utils::globalVariables(".rs.invokeShinyPaneViewer")
