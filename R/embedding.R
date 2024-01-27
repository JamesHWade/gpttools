prepare_scraped_files <- function(domain) {
  scraped_dir <- tools::R_user_dir("gpttools", which = "data")
  scraped <-
    arrow::read_parquet(glue("{scraped_dir}/text/{domain}.parquet"))

  if (max(scraped$n_words) > 2e5) {
    max_index <- scraped[which.max(scraped$n_words), ]
    print(max_index |> dplyr::select(-text))
    cli_alert_warning(
      c(
        "!" = "Entry {max_index$link} of {domain} has at least 200,000 words.",
        "i" = "You probably do not want that. Please inspect scraped data."
      )
    )
    if (rlang::is_interactive()) {
      dont_embed <- usethis::ui_nope(
        c(
          "Entry {max_index$link} of {domain} has at least 200,000 words.",
          "You probably do not want that. Please inspect scraped data.",
          "Do you want to continue?"
        )
      )
      if (dont_embed) {
        cli_abort("Embedding aborted at your request.")
      }
    }
  } else {
    cli_alert_warning(
      c(
        "!" = "Entries with more than 200,000 words detected.",
        "i" = "Not an interactive session so not stopping here."
      )
    )
  }

  scraped |>
    dplyr::mutate(
      chunks = purrr::map(text, \(x) {
        chunk_with_overlap(x,
          chunk_size = 500,
          overlap_size = 50,
          doc_id = domain,
          lowercase = FALSE,
          strip_punct = FALSE,
          strip_numeric = FALSE,
          stopwords = NULL
        )
      })
    ) |>
    tidyr::unnest(chunks) |>
    tidyr::unnest(chunks) |>
    dplyr::select(-text) |>
    dplyr::mutate(
      n_tokens = tokenizers::count_characters(chunks) %/% 4,
      hash = cli::hash_md5(chunks)
    ) |>
    check_for_duplicate_text() |>
    dplyr::distinct(hash, .keep_all = TRUE)
}

check_for_duplicate_text <- function(x) {
  if (sum(duplicated(x$hash)) > 0) {
    cli::cli_inform(
      c(
        "!" = "Duplicate text entries detected.",
        "i" = "These are removed by default."
      )
    )
  }
  invisible(x)
}

create_openai_embedding <-
  function(input_text,
           model = getOption(
             "gpttools.openai_embed_model",
             "text-embedding-3-small"
           ),
           openai_api_key = Sys.getenv("OPENAI_API_KEY")) {
    body <- list(
      model = model,
      input = input_text
    )
    embedding <- gptstudio:::query_openai_api(
      task = "embeddings",
      request_body = body,
      openai_api_key = openai_api_key
    )
    tibble::tibble(
      usage = embedding$usage$total_tokens,
      embedding = embedding$data[[1]]$embedding |> list()
    )
  }

add_embeddings <- function(index,
                           local_embeddings = FALSE) {
  if (local_embeddings) {
    model <- get_transformer_model()
    index |>
      dplyr::mutate(
        embeddings = purrr::map(
          .x = chunks,
          .f = \(x) create_text_embeddings(x, model),
          .progress = "Creating Embeddings Locally"
        ),
        embedding_method = glue::glue("local: {getOption(\"gpttools.local_embed_model\")}")
      )
  } else {
    index |>
      dplyr::mutate(
        embeddings = purrr::map(
          .x = chunks,
          .f = create_openai_embedding,
          .progress = "Create Embeddings"
        ),
        embedding_method = "OpenAI"
      )
  }
}

join_embeddings_from_index <- function(x) {
  index <- try(load_index("All"))
  if (is.data.frame(index) && all(c("hash", "embedding") %in% names(index))) {
    all_embeddings <- index |> dplyr::select(hash, embedding)
    x |>
      dplyr::distinct(hash, .keep_all = TRUE) |>
      dplyr::left_join(all_embeddings, by = c("hash"))
  } else {
    invisible(x)
  }
}

create_index <- function(domain,
                         overwrite = FALSE,
                         dont_ask = FALSE,
                         pkg_version = "Not a package",
                         pkg_name = NULL,
                         local_embeddings = FALSE) {
  index_dir <-
    file.path(tools::R_user_dir("gpttools", which = "data"), "index")

  if (local_embeddings) {
    index_dir <- file.path(index_dir, "local")
  }

  if (!dir.exists(index_dir)) {
    dir.create(index_dir, recursive = TRUE)
  }

  index_file <- glue::glue("{index_dir}/{domain}.parquet")

  cli::cli_alert_info("index_file: {index_file}")

  if (file.exists(index_file) && rlang::is_false(overwrite)) {
    cli::cli_alert_warning(
      c(
        "!" = "Index already exists for {domain}.",
        "i" = "Use {.code overwrite = TRUE} to overwrite index."
      )
    )
    return(invisible(FALSE))
  } else if (file.exists(index_file) && rlang::is_true(overwrite)) {
    cli::cli_alert_info(
      c(
        "!" = "Index already exists for this domain.",
        "i" = "Overwriting index."
      )
    )
    old <-
      arrow::read_parquet(index_file) |>
      dplyr::distinct(name, version) |>
      tibble::as_tibble()
    if (nrow(old) > 1) {
      cli::cli_abort(
        c(
          "!" = "Multiple packages found for this domain.",
          "i" = "Please specify the package name and version."
        )
      )
    }
    pkg_name <- old$name
    pkg_version <- old$version
  }

  index <- prepare_scraped_files(domain = domain)
  n_tokens <- sum(index$n_tokens) |> scales_scientific()

  if (dont_ask) {
    cli::cli_inform(c(
      "!" = "You are about to create embeddings for {domain}.",
      "i" = "This will use approx. {n_tokens} tokens."
    ))
    ask_user <- TRUE
  } else {
    cli::cli_inform(c(
      "!" = "You are about to create embeddings for {domain}.",
      "i" = "This will use approx. {n_tokens} tokens.",
      "i" = "Only proceed if you understand the cost.",
      "i" = "Read more about embeddings at {.url
      https://platform.openai.com/docs/guides/embeddings}."
    ))
    ask_user <- usethis::ui_yeah(
      "Would you like to continue with creating embeddings?"
    )
  }
  if (rlang::is_true(ask_user)) {
    index <-
      index |>
      # join_embeddings_from_index() |>
      add_embeddings(local_embeddings = local_embeddings) |>
      tidyr::unnest(embeddings) |>
      dplyr::mutate(
        version = pkg_version,
        name = pkg_name
      )
    arrow::write_parquet(
      x    = index,
      sink = index_file
    )
  } else {
    cli_inform("No index was creates for {domain}")
  }
}


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

  purrr::walk(text_files, function(file_path) {
    domain <- tools::file_path_sans_ext(basename(file_path))
    cli::cli_alert_info(glue("Creating/updating index for domain {domain}..."))
    create_index(
      domain = domain,
      overwrite = overwrite,
      dont_ask = dont_ask,
      local_embeddings = local_embeddings
    )
  })
}


get_top_matches <- function(index, query_embedding, k = 5) {
  k <- min(k, nrow(index))
  index |>
    dplyr::mutate(
      similarity = purrr::map_dbl(embedding, \(x) {
        lsa::cosine(query_embedding, unlist(x))
      })
    ) |>
    dplyr::arrange(dplyr::desc(similarity)) |>
    head(k)
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
    cli::cli_inform("No index found. Using sample index for gpttools.")

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

  if (domain == "All") {
    arrow::open_dataset(data_dir) |> tibble::as_tibble()
  } else {
    arrow::read_parquet(glue("{data_dir}/{domain}.parquet"))
  }
}

load_index_dir <- function(dir_name) {
  dir_name <- glue('{tools::R_user_dir("gpttools", which = "data")}/{dir_name}')
  arrow::open_dataset(dir_name)
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


chunk_with_overlap <- function(x, chunk_size, overlap_size, doc_id, ...) {
  stopifnot(is.character(x), length(x) == 1)
  words <- tokenizers::tokenize_words(x, simplify = TRUE, ...)
  chunks <- list()
  start <- 1
  end <- chunk_size
  while (start <= length(words)) {
    if (end > length(words)) {
      end <- length(words)
    }
    chunk <- words[start:end]
    if (length(chunk) > 0) {
      chunks[[start]] <- chunk
    }
    start <- start + (chunk_size - overlap_size)
    end <- end + (chunk_size - overlap_size)
  }
  if (!is.null(doc_id)) {
    num_chars <- stringr::str_length(length(chunks))
    chunk_ids <- stringr::str_pad(seq_along(chunks),
      side = "left",
      width = num_chars, pad = "0"
    )
    names(chunks) <- stringr::str_c(doc_id, chunk_ids, sep = "-")
  } else {
    names(chunks) <- NULL
  }
  chunks <- purrr::compact(chunks)
  purrr::map(chunks, \(x) stringr::str_c(x, collapse = " "))
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
  cli::cli_inform("Access your index files here: {.file {loc}}")
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
  cli::cli_alert("Select the index file you want to delete.")
  to_delete <- utils::menu(files)
  confirm_delete <-
    usethis::ui_yeah("Are you sure you want to delete {files[to_delete]}?")
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
