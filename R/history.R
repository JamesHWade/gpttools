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
      hash = character()
    )
    arrow::write_parquet(query_history, file_path)
  }
}

read_history <- function(file_name = "chat_history") {
  file_path <- get_history_path(file_name)
  arrow::read_parquet(file_path)
}

save_history <- function(file_name = "chat_history",
                         role,
                         content,
                         overwrite = FALSE) {
  create_history(file_name, overwrite = overwrite)
  history <- read_history(file_name)
  embedding <- create_openai_embedding(content)

  new_entry <- tibble::tibble(
    id        = max(0, history$id, na.rm = TRUE) + 1,
    timestamp = lubridate::now(),
    role      = role,
    content   = content,
    embedding = embedding$embedding,
    hash      = hash_md5(content)
  )

  new_entry |>
    rbind(history) |>
    arrow::write_parquet(get_history_path(file_name))
}

get_query_embedding <- function(query) {
  create_openai_embedding(input_text = query) |>
    dplyr::pull(embedding) |>
    unlist()
}

get_query_context <- function(query_embedding, full_context, k, column_name = "chunks") {
  check_context(full_context)
  get_top_matches(full_context, query_embedding, k = k) |>
    dplyr::pull({{ column_name }}) |>
    paste(collapse = "\n\n")
}

check_context <- function(context) {
  if (rlang::is_null(context)) {
    cli_abort(
      "You specified that context should be added but none was provided."
    )
  } else if (!is.data.frame(context)) {
    cli_abort(
      "You passed a {class(context)} to but a data.frame was expected."
    )
  }
}

custom_chat_function <- function(query,
                                 index = NULL,
                                 add_context = TRUE,
                                 chat_history = NULL,
                                 session_history = NULL,
                                 add_history = TRUE,
                                 task = "Context Only",
                                 k = 4,
                                 save_history = TRUE,
                                 overwrite = FALSE) {
  arg_match(task, c("Context Only", "Permissive Chat"))

  if (rlang::is_true(add_context) || rlang::is_true(add_history)) {
    query_embedding <- get_query_embedding(query)
  }

  if (rlang::is_true(add_context)) {
    context <-
      get_query_context(query_embedding, index, k, column_name = "chunks")
  }

  cli::cli_inform("up here")

  if (rlang::is_true(add_history)) {
    cli::cli_inform("some history")
    related_history <-
      get_query_context(query_embedding, history, k, column_name = "content")
    cli::cli_inform("not the problem")
  }

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
                     provide related context, say \"I am not able to answer that
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

  cli_inform("here")
  # session_history <-
  #   purrr::map(session_history, \(x) if (x$role == "system") NULL else x) |>
  #   purrr::compact()

  prompt <- c(session_history, instructions)

  cli_inform("overhere")
  answer <- gptstudio::openai_create_chat_completion(prompt)

  cli_inform("made it")
  if (rlang::is_true(save_history)) {
    save_history_and_embeddings(
      file_name = file_name,
      role      = "system",
      content   = instructions[[1]]$content,
      overwrite = overwrite
    )

    save_history_and_embeddings(
      file_name = file_name,
      role      = "user",
      content   = query,
      overwrite = overwrite
    )

    save_history_and_embeddings(
      file_name = file_name,
      role      = "assistant",
      content   = answer$response["choices"][[1]]$text,
      overwrite = overwrite
    )
  }

  list(prompt, full_context, answer)
}
