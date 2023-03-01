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
    mutate(similarity = map_dbl(embedding, \(x) {
      lsa::cosine(query_embedding, unlist(x))
    })) |>
    arrange(desc(similarity)) |>
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
  arrow::read_feather(glue("indices/{domain}.feather"), as_data_frame = FALSE)
}


#' Query an Index
#'
#' This function queries an index with a given question or prompt and returns a
#' set of suggested answers.
#'
#' @param index A pre-built index of text data.
#' @param query A character string representing the question or prompt to query
#'   the index with.
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
query_index <- function(index, query, task = "conservative q&a", k = 4) {
  arg_match(
    task,
    c(
      "conservative q&a", "permissive q&a",
      "paragraph about a question", "bullet points",
      "summarize problems given a topic",
      "extract key libraries and tools",
      "simple instructions", "summarize"
    )
  )
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
      "conservative q&a" =
        glue::glue(
          "Answer the question based on the context below, and if the question
          can't be answered based on the context, say \"I don't know\"\n\n
          Context:\n{context}\n\n---\n\nQuestion: {query}\nAnswer:"
        ),
      "permissive q&a" =
        glue::glue(
          "Answer the question based on the context below, and if the question
          can't be answered based on the context, say \"This is a tough
          question but I can answer anyway.\"\n\n
          Context:\n{context}\n\n---\n\nQuestion: {query}\nAnswer:"
        ),
      "paragraph about a question" =
        glue::glue(
          "Write a paragraph, addressing the question, and use the text below
          to obtain relevant information\"\n\nContext:\n
          {context}\n\n---\n\nQuestion: {query}\nParagraph long Answer:"
        ),
      "bullet points" =
        glue::glue(
          "Write a bullet point list of possible answers, addressing the
          question, and use the text below to obtain relevant information\"\n\nC
          ontext:\n{context}\n\n---\n\nQuestion: {query}\nBullet point
          Answer:"
        ),
      "summarize problems given a topic" =
        glue::glue(
          "Write a summary of the problems addressed by the questions below\"\n
          \n{context}\n\n---\n\n"
        ),
      "extract key libraries and tools" =
        glue::glue("Write a list of libraries and tools present in the context
                   below\"\n\nContext:\n{context}\n\n---\n\n"),
      "simple instructions" =
        glue::glue("{query} given the context below \n\n
                   {context}\n\n---\n\n"),
      "summarize" =
        glue::glue("Write an elaborate, paragraph long summary about
                   \"{query}\" given the questions and answers from a public
                   forum or documentation page on this topic\n\n{context}\n\n
                   ---\n\nSummary:"),
    )

  n_tokens <- tokenizers::count_characters(instructions) %/% 4
  if (n_tokens > 3500) {
    answer <-
      list(
        choice = list(
          text = "Too many tokens. Please lower the number of documents (k)."
        )
      )
  } else {
    answer <- openai_create_completion(
      model = "text-davinci-003",
      prompt = instructions,
      max_tokens = as.integer(3800L - n_tokens)
    )
  }
  list(instructions, full_context, answer)
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
