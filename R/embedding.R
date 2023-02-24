split_text_files <- function(domain) {
  file_names <- fs::dir_ls(glue::glue("text/{domain}"), recurse = TRUE)

  purrr::map(file_names, \(file){
    tibble::tibble(
      fname = basename(file),
      text = readr::read_lines(file) |> unique() |> paste(collapse = " ")
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(chunks = purrr::map(text, \(x){
      tokenizers::chunk_text(x, chunk_size = 500)
    })) |>
    tidyr::unnest(chunks) |>
    tidyr::unnest(chunks) |>
    dplyr::rename(original_text = text) |>
    dplyr::mutate(n_words = tokenizers::count_words(chunks))

}

create_openai_embedding <- function(input_text,
                                    model = "text-embedding-ada-002",
                                    openai_api_key = Sys.getenv("OPENAI_API_KEY")) {

  body <- list(
    model = model,
    input = input_text
  )
  embedding <- query_openai_api(body, openai_api_key, task = "embeddings")
  embedding$usage$total_tokens
  tibble::tibble(usage     = embedding$usage$total_tokens,
                 embedding = embedding$data$embedding)
}

add_embeddings <- function(index) {
  index |>
    dplyr::rowwise() |>
    dplyr::mutate(embeddings = create_openai_embedding(chunks),
                  usage = embeddings |> dplyr::pull(usage),
                  embedding = embeddings |> dplyr::pull(embedding)) |>
    dplyr::ungroup()
}

create_index <- function(domain) {
  index <- split_text_files(domain = domain) |>
    add_embeddings()
  arrow::write_feather(index, sink = "indices/{domain}.feather")
  index
}

get_top_matches <- function(index, query_embedding, k = 5) {
  index |>
    dplyr::rowwise() |>
    dplyr::mutate(similarity = lsa::cosine(query_embedding,
                                           embedding |> unlist())) |>
    dplyr::arrange(desc(similarity)) |>
    head(k)
}

query_index <- function(index, query, task = "conservative q&a", k = 4) {
  arg_match(task,
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

  context <- get_top_matches(index, query_embedding, k = k) |>
    dplyr::pull(chunks) |>
    paste(collapse = "\n\n")

  instructions <-
    switch(
      task,
      "conservative q&a" =
        glue::glue(
          "Answer the question based on the context below, and if the question
          can't be answered based on the context, say \"I don't know\"\n\n
          Context:\n{context}\n\n---\n\nQuestion: {query}\nAnswer:"),
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
          {context}\n\n---\n\nQuestion: {query}\nParagraph long Answer:"),
      "bullet points" =
        glue::glue(
          "Write a bullet point list of possible answers, addressing the
          question, and use the text below to obtain relevant information\"\n\nC
          ontext:\n{context}\n\n---\n\nQuestion: {query}\nBullet point
          Answer:"),
      "summarize problems given a topic" =
        glue::glue(
          "Write a summary of the problems addressed by the questions below\"\n
          \n{context}\n\n---\n\n"),
      "extract key libraries and tools" =
        glue::glue("Write a list of libraries and tools present in the context
                   below\"\n\nContext:\n{context}\n\n---\n\n"),
      "simple instructions" =
        glue::glue("{query} given the common questions and answers below \n\n
                   {context}\n\n---\n\n"),
      "summarize" =
        glue::glue("Write an elaborate, paragraph long summary about
                   \"{query}\" given the questions and answers from a public
                   forum or documentation page on this topic\n\n{context}\n\n
                   ---\n\nSummary:"),
    )

  n_tokens <- tokenizers::count_characters(instructions) / 4
  n_tokens <- min(3500L, as.integer(n_tokens))

  print(n_tokens)

  answer <- openai_create_completion(model = "text-davinci-003",
                                     prompt = instructions,
                                     max_tokens = as.integer(4097L - n_tokens))

  list(instructions, answer)
}
