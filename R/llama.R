#' Create Llama Index
#'
#' Create a Llama index for a given domain
#'
#' @param domain character string specifying the domain to create an index for
#' @param index_type character string specifying the type of index to create
#' @param input_dir character string specifying input directory
#'
#' @return the index created
#'
#' @examples
#' \dontrun{
#' create_llama_index("movies")
#' create_llama_index("books", index_type = "faiss")
#' }
create_llama_index <- function(domain, input_dir = "text", index_type = "simple") {
  check_python_configuration()
  llama <- reticulate::import("llama_index")
  index_text <-
    switch(index_type,
      "simple"   = llama$GPTSimpleVectorIndex,
      "faiss"    = llama$GPTFaissIndex
    )
  simple_directory_reader <- llama$SimpleDirectoryReader
  docs_to_load <- simple_directory_reader(
    input_dir = glue::glue("{input_dir}/{domain}")
  )
  documents <- docs_to_load$load_data()
  index <- index_text(documents)
  index$save_to_disk(glue::glue("indices/{domain}.json"))
}

#' Load Llama Index
#'
#' This function loads a Llama index from disk.
#'
#' @param domain The domain to load the index from.
#'
#' @return A GPTSimpleVectorIndex object.
load_llama_index <- function(domain) {
  check_python_configuration()
  llama <- reticulate::import("llama_index")
  gpt_simple_vector_index <- llama$GPTSimpleVectorIndex
  gpt_simple_vector_index$load_from_disk(glue::glue("indices/{domain}.json"))
}

#' Query the Llama Index
#'
#' This function queries the Llama Index using a query string.
#'
#' @param index The Llama Index object.
#' @param query The query string.
#'
#' @return The result of the query.
#'
#' @export
query_llama_index <- function(index, query) {
  check_python_configuration()
  index$query(query, mode = "embedding")
}

#' Check Python Configuration
#'
#' Check if the required Python modules are installed.
#'
#' @return Returns TRUE if the modules are installed, otherwise throws an error.
#' @export
check_python_configuration <- function() {
  rlang::check_installed("reticulate")
  modules <- c("llama_index")
  purrr::walk(modules, \(mod) {
    if (!reticulate::py_module_available(mod)) {
      cli::cli_abort(
        c(
          "!" = "Python module {mod} is required but not found.",
          "i" = "See reticulate documentation for installation help:
          {.url https://rstudio.github.io/reticulate/}."
        )
      )
    }
  })
}
