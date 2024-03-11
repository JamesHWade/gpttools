#' Index All Scraped Data
#'
#' This function iterates through all the text files in a specified directory,
#' updating or creating indexes for each domain contained in the file names.
#' Allows customization of the indexing process through various parameters.
#'
#' @param overwrite A logical value determining whether to overwrite existing
#'   indexes.
#' @param local_embeddings A logical indicating whether to use local embeddings
#'   for indexing.
#' @param dont_ask A logical value that, if TRUE, disables interactive
#'   confirmation prompts during the indexing process.
#'
#' @details The function first retrieves a list of all text files in the
#'   targeted directory. For each file, it extracts the domain name from the
#'   filename, prints an informative message about the indexing process for that
#'   domain, and then proceeds to create or update the index for the domain
#'   based on the function arguments.
#'
#' @return Invisible NULL. The function is called for its side effects.
#'
#' @examples
#' # Index all scraped data without overwriting existing indexes, using local
#' # embeddings, and without interactive prompts.
#'
#' \dontrun{
#' gpttools_index_all_scraped_data(
#'   overwrite = FALSE,
#'   local_embeddings = TRUE,
#'   dont_ask = TRUE
#' )
#' }
#'
#' @export
gpttools_index_all_scraped_data <- function(overwrite = FALSE,
                                            local_embeddings = TRUE,
                                            dont_ask = TRUE) {
  text_files <- list_index("text", full_path = TRUE)

  walk(text_files, function(file_path) {
    domain <- tools::file_path_sans_ext(basename(file_path))
    cli_alert_info(glue("Creating/updating index for domain {domain}..."))
    create_index(
      domain = domain,
      overwrite = overwrite,
      dont_ask = dont_ask,
      local_embeddings = local_embeddings
    )
  })
}

#' Load Index Data for a Domain
#'
#' This function loads the index data for a given domain from a parquet file.
#'
#' @param domain A character string indicating the name of the domain.
#' @param local_embeddings A logical indicating whether to load the local
#' embeddings or the OpenAI embeddings. Defaults to FALSE.
#'
#' @return A data frame containing the index data for the specified domain.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_index("example_domain")
#' }
load_index <- function(domain, local_embeddings = FALSE) {
  if (local_embeddings) {
    data_dir <-
      glue::glue('{tools::R_user_dir("gpttools", which = "data")}/index/local')
  } else {
    data_dir <-
      glue::glue('{tools::R_user_dir("gpttools", which = "data")}/index')
  }

  if (!dir.exists(data_dir)) {
    cli_inform("No index found. Using sample index for gpttools.")

    if (local_embeddings) {
      sample_index <-
        system.file("sample-index/local/jameshwade-github-io-gpttools.parquet",
          package = "gpttools"
        )
    } else {
      sample_index <-
        system.file("sample-index/jameshwade-github-io-gpttools.parquet",
          package = "gpttools"
        )
    }
    index <- arrow::read_parquet(sample_index)
    invisible(index)
  }

  arrow::read_parquet(glue("{data_dir}/{domain}.parquet"), as_data_frame = FALSE)
}

load_scraped_data <- function(dir_name, file_name) {
  file_path <-
    file.path(
      tools::R_user_dir("gpttools", which = "data"),
      dir_name,
      file_name
    )
  arrow::read_parquet(file_path)
}

#' List Index Files
#'
#' This function lists the index files in the specified directory.
#'
#' @param dir Name of the directory, defaults to "index"
#' @param full_path If TRUE, returns the full path to the index files.
#'
#' @return A character vector containing the names of the index files found in
#' the specified directory.
#'
#' @examples
#' \dontrun{
#' list_index()
#' }
#' @export
list_index <- function(dir = "index", full_path = FALSE) {
  loc <- file.path(tools::R_user_dir("gpttools", "data"), dir)
  cli_inform("Access your index files here: {.file {loc}}")
  if (full_path) {
    list.files(loc, full.names = TRUE)
  } else {
    list.files(loc)
  }
}


#' Delete an Index File
#'
#' Interactively deletes a specified index file from a user-defined directory.
#' Presents the user with a list of available index files and prompts for
#' confirmation before deletion.
#' @export
delete_index <- function() {
  files <- list_index()
  if (length(files) == 0) {
    cli_alert_warning("No index files found.")
    return(invisible())
  }
  cli_alert("Select the index file you want to delete.")
  to_delete <- utils::menu(files)
  confirm_delete <-
    ui_yeah("Are you sure you want to delete {files[to_delete]}?")
  if (confirm_delete) {
    file.remove(file.path(
      tools::R_user_dir("gpttools", "data"),
      "index",
      files[to_delete]
    ))
    cli_alert_success("Index deleted.")
  } else {
    cli_alert_warning("Index not deleted.")
  }
}

combine_index <- function(dir_name) {
  dir <- glue('{tools::R_user_dir("gpttools", which = "data")}/{dir_name}')
  indices <- list_index(dir_name, full_path = FALSE)
  if ("All" %in% indices) {
    cli_abort(
      c(
        "!" = "All.parquet already exists in the directory.",
        "-" = "Please remove it with `delete_index` before combining indexes."
      )
    )
  }
  list_index(dir_name, full_path = TRUE) |>
    arrow::open_dataset() |>
    tibble::as_tibble() |>
    arrow::write_parquet(sink = glue::glue("{dir}/All.parquet"))
}
