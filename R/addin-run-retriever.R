#' Run Chat with Retrieval
#'
#' Run the Chat with Retrieval shiny app
#'
#' @export
#'
#' @return This function has no return value.
#'
#' @examples
#' # Call the function as an RStudio addin
#' \dontrun{
#' chat_with_retrieval()
#' }
chat_with_retrieval <- function() {
  indices <- list_index()
  if (length(indices) == 0) {
    cli_abort(
      "No index was found. Create an index with {.code crawl(<url>)}."
    )
  }
  run_app_file <- system.file("scripts/run_retriever.R", package = "gpttools")
  rstudioapi::jobRunScript(path = run_app_file)
}
