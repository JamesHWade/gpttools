.onLoad <- function(lib, pkg) {
  user_config <- set_user_config()

  if (rlang::is_false(user_config)) {
    op <- options()
    op_gpttools <- list(
      gpttools.service      = "openai",
      gpttools.model        = "gpt-4-1106-preview",
      gpttools.local_embed  = TRUE,
      gpttools.task         = "Permissive Chat",
      gpttools.k_context    = 4,
      gpttools.k_history    = 4,
      gpttools.save_history = FALSE,
      gpttools.sources      = "All"
    )

    toset <- !(names(op_gpttools) %in% names(op))
    if (any(toset)) options(op_gpttools[toset])
  }

  invisible()
}


utils::globalVariables(".rs.invokeShinyPaneViewer")
