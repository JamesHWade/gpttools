prepare_scraped_files <- function(domain) {
  arrow::read_parquet(glue("text/{domain}.parquet")) |>
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
    dplyr::rename(original_text = text) |>
    dplyr::mutate(n_tokens = tokenizers::count_characters(chunks) %/% 4)
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
    embedding$usage$total_tokens
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

create_index <- function(domain) {
  index <-
    prepare_scraped_files(domain = domain) |>
    add_embeddings()
  arrow::write_feather(index, sink = glue::glue("indices/{domain}.feather"))
  index
}

get_top_matches <- function(index, query_embedding, k = 5) {
  index |>
    dplyr::mutate(similarity = purrr::map_dbl(embedding, \(x) {
      lsa::cosine(query_embedding, unlist(x))
    })) |>
    dplyr::arrange(dplyr::desc(similarity)) |>
    head(k)
}

#' Load Index Data for a Domain
#'
#' This function loads the index data for a given domain from a Feather file.
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
  arrow::read_feather(glue("indices/{domain}.feather"))
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

  cli_inform("Embedding...")

  history <-
    purrr::map(history, \(x) if (x$role == "system") NULL else x) |>
    purrr::compact()

  prompt <- c(history, instructions)

  answer <- openai_create_chat_completion(prompt)

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
    num_chars <- stringi::stri_length(length(chunks))
    chunk_ids <- stringi::stri_pad_left(seq_along(chunks),
      width = num_chars, pad = "0"
    )
    names(chunks) <- stringi::stri_c(doc_id, chunk_ids, sep = "-")
  } else {
    names(chunks) <- NULL
  }
  chunks <- purrr::compact(chunks)
  out <- lapply(chunks, stringi::stri_c, collapse = " ")
  out
}
