#' Run Chat GPT with Retrieval
#'
#' Run the ChatGPT shiny app with semantic search and document retrieval
#'
#' @export
#'
#' @return This function has no return value.
#'
#' @examples
#' # Call the function as an RStudio addin
#' \dontrun{
#' addin_run_retriever()
#' }
addin_run_retriever <- function() {
  gptstudio::check_api()
  indices <- list_index()
  if (length(indices) == 0) {
    cli::cli_abort(
      "No index was found. Create an index with {.code crawl(<url>)}."
    )
  }
  run_app_file <- system.file("scripts/run_retriever.R", package = "gpttools")
  rstudioapi::jobRunScript(path = run_app_file)
}
