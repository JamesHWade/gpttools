get_history_path <- function(file_name = "chat_history") {
  file.path(
    tools::R_user_dir("gpttools", which = "data"),
    glue::glue("{file_name}.parquet")
  )
}

#' Read chat history from a file
#'
#' @param file_name Name of the chat history file to read from. Default is
#'   "chat_history".
#'
#' @return A dataframe containing the chat history or NULL if the file doesn't
#'   exist.
#' @export
#'
#' @examples
#' \dontrun{
#' # Read chat history from the default file
#' history <- read_history()
#' }
read_history <- function(file_name = "chat_history") {
  file_path <- get_history_path(file_name)
  if (file.exists(file_path)) {
    arrow::read_parquet(file_path)
  } else {
    NULL
  }
}

#' Delete chat history files
#'
#' This function interactively deletes chat history files stored in the user's
#' directory. It lists all the .parquet files in the gpttools data directory and
#' prompts the user for confirmation before deleting each file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Interactively delete chat history files
#' delete_history()
#' }
delete_history <- function() {
  if (!interactive()) {
    invisible()
  }
  history_files <- list.files(tools::R_user_dir("gpttools", which = "data"),
    pattern = "*.parquet",
    full.names = TRUE
  )

  purrr::map(history_files, \(x) {
    delete_file <- usethis::ui_yeah("Do you want to delete {basename(x)}?")
    if (delete_file) {
      file.remove(x)
    } else {
      cli_inform("{x} was **not** deleted.")
    }
  })
  invisible()
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

get_query_context <- function(query_embedding, full_context, k) {
  check_context(full_context)
  get_top_matches(full_context, query_embedding, k = k)
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


#' chat_with_context
#'
#' This function allows you to chat with a chatbot that answers questions based
#' on the provided context and chat history. It uses GPT-4 architecture to
#' generate responses.
#'
#' @param query The input query to be processed.
#' @param index Index to look for context.
#' @param add_context Whether to add context to the query or not. Default is
#'   TRUE.
#' @param chat_history Chat history dataframe for reference.
#' @param history_name Name of the file where chat history is stored.
#' @param session_history Session history data for reference.
#' @param add_history Whether to add chat history to the query or not. Default
#'   is TRUE.
#' @param task Task type, either "Context Only" or "Permissive Chat". Default is
#'   "Context Only".
#' @param k_context Number of top context matches to consider. Default is 4.
#' @param k_history Number of top chat history matches to consider. Default is
#'   4.
#' @param save_history Whether to save the chat history or not. Default is TRUE.
#' @param overwrite Whether to overwrite the history file or not. Default is
#'   FALSE.
#'
#' @return A list containing the prompt, context, and answer.
#' @export
#'
#' @examples
#' \dontrun{
#' # Define a query and context
#' query <- "What is the capital of France?"
#' context <- "France is a country in Western Europe. Its capital is a famous
#' city known for its culture, art, and history."
#'
#' # Call the chat_with_context function
#' result <- chat_with_context(query = query, context = context)
#' }
chat_with_context <- function(query,
                              index = NULL,
                              add_context = TRUE,
                              chat_history = NULL,
                              history_name = "chat_history",
                              session_history = NULL,
                              add_history = TRUE,
                              task = "Context Only",
                              k_context = 4,
                              k_history = 4,
                              save_history = TRUE,
                              overwrite = FALSE) {
  arg_match(task, c("Context Only", "Permissive Chat"))

  if (rlang::is_true(add_context) || rlang::is_true(add_history)) {
    query_embedding <- get_query_embedding(query)
  }

  if (rlang::is_true(add_context)) {
    full_context <-
      get_query_context(
        query_embedding,
        index,
        k_context
      )
    context <- full_context |>
      dplyr::pull("chunks") |>
      paste(collapse = "\n\n")
  } else {
    context <- "No additional context provided."
  }

  if (rlang::is_true(add_history) && !rlang::is_null(chat_history)) {
    if (nrow(chat_history) == 0) {
      related_history <- "No related history found."
    }
    related_history <-
      get_query_context(
        query_embedding,
        chat_history,
        k_history
      ) |>
      dplyr::pull("content") |>
      paste(collapse = "\n\n")
  } else {
    related_history <- "No related history found."
  }


  prompt_instructions <-
    switch(task,
      "Context Only" =
        list(
          list(
            role = "system",
            content =
              glue(
                "You are a helpful chat bot that answers questions based on ",
                "the context provided by the user. If the user does not ",
                "provide related context, say \"I am not able to answer that ",
                "question. Maybe try rephrasing your question in a different ",
                "way.\""
              )
          )
        ),
      "Permissive Chat" =
        list(
          list(
            role = "system",
            content =
              glue(
                "You are a helpful chat bot that answers questions based on ",
                "on the context provided by the user. If the user does not ",
                "provide context, answer the quest but first say \"I am not ",
                "able to answer that question with the context you gave me, ",
                "but here is my best answer.",
              )
          )
        )
    )

  prompt_context <- list(
    list(
      role = "user",
      content = glue("---\nContext:\n{context}\n---")
    ),
    list(
      role = "user",
      content = glue("---\nRelated Chat History:\n{related_history}\n---")
    )
  )

  prompt_query <- list(
    list(
      role = "user",
      content = glue("{query}")
    )
  )

  session_history <-
    purrr::map(session_history, \(x) {
      if (x$role == "system") {
        NULL
      } else if (stringr::str_detect(
        x$content,
        pattern = "---\nContext:\n|---\nRelated Chat History:\n"
      )
      ) {
        NULL
      } else {
        x
      }
    }) |>
    purrr::compact()

  prompt <- c(
    session_history,
    prompt_instructions,
    prompt_context,
    prompt_query
  )

  cli::cat_print(prompt)

  answer <- gptstudio::openai_create_chat_completion(prompt)

  if (rlang::is_true(save_history)) {
    purrr::map(prompt, \(x) {
      save_history(
        file_name = history_name,
        role      = "system",
        content   = x$content,
        overwrite = overwrite
      )
    })
    save_history(
      file_name = history_name,
      role      = "assistant",
      content   = answer$choices$message.content,
      overwrite = overwrite
    )
  }

  prompt_without_context <-
    c(session_history, prompt_query)

  list(prompt_without_context, full_context, answer)
}
