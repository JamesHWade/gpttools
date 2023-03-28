get_history_path <- function(file_name = "chat_history") {
  file.path(
    tools::R_user_dir("gpttools", which = "data"),
    glue::glue("{file_name}.parquet")
  )
}

create_history <- function(file_name = "chat_history",
                           overwrite = FALSE) {
  file_path <- get_history_path(file_name)

  if (!file.exists(file_path) || overwrite) {
    query_history <- tibble::tibble(
      id = integer(),
      timestamp = lubridate::now(),
      role = character(),
      content = character(),
    )
    arrow::write_parquet(query_history, file_path)
  }
}


read_history <- function(file_name = "chat_history") {
  file_path <- get_history_path(file_name)
  arrow::read_parquet(file_path)
}


save_history <- function(file_name = "chat_history",
                         history = read_history(),
                         role,
                         content) {
  file_path <- get_history_path(file_name)

  new_entry <- tibble::tibble(
    id = as.integer(0), # dummy value, will be replaced
    timestamp = lubridate::now(),
    role = role,
    content = content
  )

  new_entry$id <- max(0, history$id, na.rm = TRUE) + 1
  query_history <- history |> dplyr::bind_rows(new_entry)

  arrow::write_parquet(query_history, file_path)
}


create_history_embeddings <- function(file_name = "chat_embeddings",
                                      overwrite = FALSE) {
  file_path <- get_history_path(file_name)

  if (!file.exists(file_path) || overwrite) {
    query_history_embeddings <- tibble(
      id = integer(),
      embedding = list()
    )

    arrow::write_parquet(query_history_embeddings, file_path)
  }
}


save_history_and_embeddings <- function(file_name = "chat_history",
                                        role,
                                        content,
                                        history = read_query_history(),
                                        overwrite = FALSE) {
  create_history()
  create_history_embeddings()
  save_history(file_name, history, role, content)
  embedding <- create_openai_embedding(query)
  file_path <- get_history_path("query_history_embeddings")
  query_history_embeddings <- arrow::read_parquet(file_path)

  new_embedding <- tibble(
    id = max(0, query_history_embeddings$id, na.rm = TRUE) + 1,
    embedding = list(embedding$embedding)
  )

  query_history_embeddings <-
    query_history_embeddings |> dplyr::bind_rows(new_embedding)
  arrow::write_parquet(query_history_embeddings, file_path)
}


custom_chat_function <- function(index,
                                 query,
                                 history,
                                 task = "Context Only",
                                 k = 4,
                                 save_history = TRUE,
                                 overwrite = FALSE) {
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
                "You are a helpful chat bot that answers questions based
                     on the context provided by the user. If the user does not
                     provide context, say \"I am not able to answer that
                     question. Maybe try rephrasing your question in a
                     different way.\"\n\n---\n\nContext:\n\n{context}\n\n---"
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
                "You are a helpful chat bot that answers questions based
                     on the context provided by the user. If the user does not
                     provide context, say \"I am not able to answer that
                     question with the context you gave me, but here is my best
                     answer. Maybe try rephrasing your question in a different
                     way.\"\n\n---\n\nContext:\n\n{context}\n\n---"
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

  history <- read_query_history(file_name)

  if (rlang::is_true(save_history)) {
    save_history_and_embeddings(
      file_name = file_name,
      role      = "system",
      content   = instructions[[1]]$content,
      history   = history,
      overwrite = overwrite
    )

    save_history_and_embeddings(
      file_name = file_name,
      role      = "user",
      content   = query,
      history   = history,
      overwrite = overwrite
    )

    save_history_and_embeddings(
      file_name = file_name,
      role      = "assistant",
      content   = answer$response["choices"][[1]]$text,
      history   = history,
      overwrite = overwrite
    )
  }

  list(prompt, full_context, answer)
}
