prepare_scraped_files <- function(domain) {
  scraped_dir <- tools::R_user_dir("gpttools", which = "data")
  scraped <-
    arrow::read_parquet(glue("{scraped_dir}/text/{domain}.parquet"))

  if (max(scraped$n_words) > 2e5) {
    max_index <- which.max(scraped$n_words)
    cli_warn(
      c(
        "!" = "Entry {max_index} of {domain} has at least 200,000 words.",
        "i" = "You probably do not want that. Please inspect scraped data."
      )
    )
    dont_embed <- usethis::ui_nope("Do you want to continue?")
    if (dont_embed) {
      cli_abort("Embedding aborted at your request.")
    }
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
           model = "text-embedding-ada-002",
           openai_api_key = Sys.getenv("OPENAI_API_KEY")) {
    body <- list(
      model = model,
      input = input_text
    )
    embedding <- query_openai_api(body, openai_api_key, task = "embeddings")
    tibble::tibble(
      usage = embedding$usage$total_tokens,
      embedding = embedding$data$embedding
    )
  }

add_embeddings <- function(index) {
  index |>
    dplyr::mutate(
      embeddings = purrr::map(
        .x = chunks,
        .f = create_openai_embedding,
        .progress = "Create Embeddings"
      )
    ) |>
    tidyr::unnest(embeddings)
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
                         pkg_version = NULL) {
  index_dir <-
    file.path(tools::R_user_dir("gpttools", which = "data"), "index")
  index_file <-
    glue::glue("{index_dir}/{domain}.parquet")

  if (file.exists(index_file) && rlang::is_false(overwrite)) {
    cli::cli_abort(
      c(
        "!" = "Index already exists for this domain.",
        "i" = "Use {.code overwrite = TRUE} to overwrite index."
      )
    )
  }

  index <- prepare_scraped_files(domain = domain)
  n_tokens <- sum(index$n_tokens) |> scales::scientific()

  cli::cli_inform(c(
    "!" = "You are about to create embeddings for {domain}.",
    "i" = "This will use approx. {n_tokens} tokens.",
    "i" = "Only proceed if you understand the cost.",
    "i" = "Read more about embeddings at {.url
      https://platform.openai.com/docs/guides/embeddings}."
  ))

  if (dont_ask) {
    ask_user <- TRUE
  } else {
    ask_user <- usethis::ui_yeah(
      "Would you like to continue with creating embeddings?"
    )
  }
  if (rlang::is_true(ask_user)) {
    index <-
      index |>
      # join_embeddings_from_index() |>
      add_embeddings() |>
      dplyr::mutate(version = pkg_version)
    if (rlang::is_false(dir.exists(index_dir))) {
      dir.create(index_dir, recursive = TRUE)
    }
    arrow::write_parquet(
      x    = index,
      sink = index_file
    )
  } else {
    cli_inform("No index was creates for {domain}")
  }
}

get_top_matches <- function(index, query_embedding, k = 5) {
  k <- min(k, nrow(index))
  index |>
    dplyr::mutate(similarity = purrr::map_dbl(embedding, \(x) {
      lsa::cosine(query_embedding, unlist(x))
    })) |>
    dplyr::arrange(dplyr::desc(similarity)) |>
    head(k)
}

#' Load Index Data for a Domain
#'
#' This function loads the index data for a given domain from a parquet file.
#'
#' @param domain A character string indicating the name of the domain.
#'
#' @return A data frame containing the index data for the specified domain.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_index("example_domain")
#' }
load_index <- function(domain) {
  data_dir <- glue('{tools::R_user_dir("gpttools", which = "data")}/index')
  if (!dir.exists(data_dir)) {
    return(NULL)
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

#' Query an Index
#'
#' This function queries an index with a given question or prompt and returns a
#' set of suggested answers.
#'
#' @param index A pre-built index of text data.
#' @param query A character string representing the question or prompt to query
#'   the index with.
#' @param history A list of the previous chat responses
#' @param task A character string indicating the task to perform, such as
#'   "conservative q&a".
#' @param k An integer specifying the number of top matches to retrieve.
#'
#' @return A list containing the instructions for answering the question, the
#'   context in which the question was asked, and the suggested answer.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' index <- build_index(data)
#' query_index(index, "What is the capital of France?")
#' }
query_index <- function(index, query, history, task = "Context Only", k = 4) {
  arg_match(task, c("Context Only", "Permissive Chat"))

  query_embedding <- create_openai_embedding(input_text = query) |>
    dplyr::pull(embedding) |>
    unlist()

  full_context <- get_top_matches(index, query_embedding, k = k)

  context <-
    full_context |>
    dplyr::pull(chunks) |>
    paste(collapse = "\n\n")

  instructions <-
    switch(task,
      "Context Only" =
        list(
          list(
            role = "system",
            content =
              glue(
                "You are a helpful chat bot that answere questions based on the
                context provided by the user. If the user does not provide
                context, say \"I am not able to answer that question. Maybe
                try rephrasing your question in a different way.\"\n\n
                Context: {context}"
              )
          ),
          list(
            role = "user",
            content = glue("{query}")
          )
        ),
      "Permissive Chat" =
        list(
          list(
            role = "system",
            content =
              glue(
                "You are a helpful chat bot that answere questions based on the
                context provided by the user. If the user does not provide
                context, say \"I am not able to answer that question with the
                context you gave me, but here is my best answer. Maybe
                try rephrasing your question in a different way.\"\n\n
                Context: {context}"
              )
          ),
          list(
            role = "user",
            content = glue("{query}")
          )
        )
    )
  history <-
    purrr::map(history, \(x) if (x$role == "system") NULL else x) |>
    purrr::compact()

  prompt <- c(history, instructions)

  answer <- gptstudio::openai_create_chat_completion(prompt)

  list(prompt, full_context, answer)
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

query_openai_api <- function(body, openai_api_key, task) {
  arg_match(task, c("completions", "chat/completions", "edits", "embeddings"))

  base_url <- glue("https://api.openai.com/v1/{task}")

  headers <- c(
    "Authorization" = glue("Bearer {openai_api_key}"),
    "Content-Type" = "application/json"
  )

  response <-
    httr::RETRY("POST",
      url = base_url,
      httr::add_headers(headers), body = body,
      encode = "json",
      quiet = TRUE
    )

  parsed <- response |>
    httr::content(as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON(flatten = TRUE)

  if (httr::http_error(response)) {
    cli_alert_warning(c(
      "x" = glue("OpenAI API request failed [{httr::status_code(response)}]."),
      "i" = glue("Error message: {parsed$error$message}")
    ))
  }
  parsed
}

#' List Index Files
#'
#' @description
#' This function lists the index files in the specified directory.
#'
#' @return A character vector containing the names of the index files found in
#' the specified directory.
#'
#' @examples
#' \dontrun{
#' list_index()
#' }
#' @export
list_index <- function() {
  list.files(
    file.path(tools::R_user_dir("gpttools", "data"), "index")
  )
}
