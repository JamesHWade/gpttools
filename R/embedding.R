prepare_scraped_files <- function(domain) {
  scraped_dir <- tools::R_user_dir("gpttools", which = "data")
  scraped <-
    arrow::read_parquet(glue("{scraped_dir}/text/{domain}.parquet"))

  if (max(scraped$n_words) > 1e6) {
    max_index <- scraped[which.max(scraped$n_words), ]
    cli_alert_warning(
      c(
        "!" = "Entry {max_index$link} of {domain} has at least 200,000 words.",
        "i" = "You probably do not want that. Please inspect scraped data."
      )
    )
    if (rlang::is_interactive()) {
      embed_long_text <- ui_yeah(
        c(
          "Entry {max_index$link} of {domain} has at least 200,000 words.",
          "You probably do not want that. Please inspect scraped data.",
          "Do you want to continue?"
        )
      )
      if (!embed_long_text) {
        cli_abort("Embedding aborted at your request.")
      }
    } else {
      cli_alert_warning(
        c(
          "!" = "Entries with more than 200,000 words detected.",
          "i" = "Not an interactive session so not stopping here."
        )
      )
    }
  }

  scraped |>
    dplyr::mutate(
      chunks = map(text, \(x) {
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
      hash = hash_md5(chunks)
    ) |>
    check_for_duplicate_text() |>
    dplyr::distinct(hash, .keep_all = TRUE)
}

check_for_duplicate_text <- function(x) {
  if (sum(duplicated(x$hash)) > 0) {
    cli_inform(
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
        embeddings = map(
          .x = chunks,
          .f = \(x) create_text_embeddings(x, model),
          .progress = "Creating Embeddings Locally"
        ),
        embedding_method =
          glue::glue("local: {getOption(\"gpttools.local_embed_model\")}")
      )
  } else {
    index |>
      dplyr::mutate(
        embeddings = map(
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

  cli_alert_info("index_file: {index_file}")

  if (file.exists(index_file) && rlang::is_false(overwrite)) {
    cli_alert_warning(
      c(
        "!" = "Index already exists for {domain}.",
        "i" = "Use {.code overwrite = TRUE} to overwrite index."
      )
    )
    return(invisible(FALSE))
  } else if (file.exists(index_file) && rlang::is_true(overwrite)) {
    cli_alert_info(
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
      cli_abort(
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
    cli_inform(c(
      "!" = "You are about to create embeddings for {domain}.",
      "i" = "This will use approx. {n_tokens} tokens."
    ))
    ask_user <- TRUE
  } else {
    cli_inform(c(
      "!" = "You are about to create embeddings for {domain}.",
      "i" = "This will use approx. {n_tokens} tokens.",
      "i" = "Only proceed if you understand the cost.",
      "i" = "Read more about embeddings at {.url
      https://platform.openai.com/docs/guides/embeddings}."
    ))
    ask_user <- ui_yeah(
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

get_top_matches <- function(index, query_embedding, k = 5) {
  k <- min(k, nrow(index))
  index |>
    tibble::as_tibble() |>
    dplyr::mutate(
      similarity = map_dbl(embedding, \(x) {
        lsa::cosine(query_embedding, unlist(x))
      })
    ) |>
    dplyr::arrange(dplyr::desc(similarity)) |>
    head(k)
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
  chunks <- compact(chunks)
  map(chunks, \(x) stringr::str_c(x, collapse = " "))
}
