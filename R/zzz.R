.onLoad <- function(lib, pkg) {
  op <- options()
  op.gpttools <- list(
    gpttools.valid_api = FALSE,
    gpttools.openai_key = NULL
  )

  toset <- !(names(op.gpttools) %in% names(op))
  if (any(toset)) options(op.gpttools[toset])


  invisible()
}

globalVariables(".rs.invokeShinyPaneViewer")


.onAttach <- function(lib, pkg) {
  packageStartupMessage(startup_message(), appendLF = FALSE)
}

startup_message <- function() {
  cli::cli_h1("Privacy Notice")
  cli::cli_h2("Please read this notice before using gpttools")
  cli::cli_text("These functions work by taking the text or code you have highlighted or selected with the cursor and send these to OpenAI as part of a prompt, they fall under their privacy notice, rules, or exceptions you agreed to with OpenAI when making an account. We do not know how secure these are when sent to OpenAI, we also do not know what OpenAI does with them. The code is designed to ONLY share the highlighted or selected text and no other elements of your R environment (i.e. data) unless you have highlighted it when running the addin. This may limit usability for now, but I do not want people to accidentally share sensitive data with OpenAI.")
  cli::cli_text()
  cli::cli_text("{.strong DO NOT HIGHLIGHT AND THEREFORE UPLOAD DATA, CODE, OR TEXT THAT SHOULD REMAIN PRIVATE}")
}