get_history_path <- function(file_name = "chat_history", local = FALSE) {
  if (local) {
    file.path(
      tools::R_user_dir("gpttools", which = "data"),
      glue::glue("local/{file_name}.parquet")
    )
  } else {
    file.path(
      tools::R_user_dir("gpttools", which = "data"),
      glue::glue("{file_name}.parquet")
    )
  }
}

#' Read chat history from a file
#'
#' @param file_name Name of the chat history file to read from. Default is
#'   "chat_history".
#' @param local Whether to read from history made with local or OpenAI
#' embeddings.
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
read_history <- function(file_name = "chat_history", local) {
  file_path <- get_history_path(file_name, local = local)
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
#' @param local Whether to delete history made with local or OpenAI embeddings.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Interactively delete chat history files
#' delete_history()
#' }
delete_history <- function(local = FALSE) {
  if (!interactive()) {
    invisible()
  }
  history_files <- get_history_path(local = local)

  map(history_files, \(x) {
    delete_file <- ui_yeah("Do you want to delete {basename(x)}?")
    if (delete_file) {
      file.remove(x)
    } else {
      cli_alert_info("{x} was **not** deleted.")
    }
  })
  invisible()
}

create_history <- function(file_name = "chat_history",
                           local = FALSE,
                           overwrite = FALSE) {
  file_path <- get_history_path(file_name, local = local)

  if (!dir.exists(tools::file_path_sans_ext(file_path))) {
    dir.create(tools::file_path_sans_ext(file_path),
      recursive = TRUE, showWarnings = FALSE
    )
  }

  if (!file.exists(file_path) || overwrite) {
    query_history <- tibble::tibble(
      id = integer(),
      timestamp = Sys.time(),
      role = character(),
      content = character(),
      hash = character()
    )
    arrow::write_parquet(query_history, file_path)
  }
}

save_user_history <- function(file_name = "chat_history",
                              role,
                              content,
                              local = FALSE,
                              model = NULL,
                              overwrite = FALSE) {
  create_history(file_name, overwrite = overwrite, local = local)
  history <- read_history(file_name, local = local)
  embedding <- get_query_embedding(content, local = local, model = model)

  new_entry <- tibble::tibble(
    id        = max(0, history$id, na.rm = TRUE) + 1,
    timestamp = Sys.time(),
    role      = role,
    content   = content,
    embedding = list(embedding),
    hash      = hash_md5(content)
  )

  new_entry |>
    rbind(history) |>
    arrow::write_parquet(get_history_path(file_name, local = local))
}

get_query_embedding <- function(query, local = FALSE, model = NULL) {
  if (local) {
    create_text_embeddings(query,
      model = model
    ) |>
      dplyr::pull(embedding) |>
      unlist()
  } else {
    create_openai_embedding(input_text = query) |>
      dplyr::pull(embedding) |>
      unlist()
  }
}

get_query_context <- function(query_embedding, full_context, k) {
  check_context(full_context)
  get_top_matches(full_context, query_embedding, k = k)
}

check_context <- function(context) {
  if (rlang::is_null(context)) {
    cli_alert_warning(
      "You specified that context should be added but none was provided."
    )
  } else if (!is.data.frame(context)) {
    cli_alert_warning(
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
#' @param service Name of the AI service to use, defaults to openai.
#' @param model Name of the openai model to use, defaults to gpt-3.5-turbo
#' @param index Index to look for context.
#' @param add_context Whether to add context to the query. Options are
#' `"always"`, `"sometimes"`, and `"never"`. The default is `"sometimes"`.
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
#' @param local Whether to use the local model or not. Default is FALSE.
#' @param embedding_model A model object to use for embedding. Only needed if
#' local is TRUE. Default is NULL.
#' @param stream Whether to stream the response or not. Default is FALSE.
#' @param rv A reactive value to store the response. Default is NULL.
#'
#' @return A list containing the prompt, context, and answer.
#' @export
#'
#' @examplesIf rlang::is_interactive()
#' rlang::is_interactive()
#' query <- "What is the capital of France?"
#' result <- chat_with_context(query = query, context = context)
chat_with_context <- function(query,
                              service = "openai",
                              model = "gpt-4-turbo-preview",
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
                              overwrite = FALSE,
                              local = FALSE,
                              embedding_model = NULL,
                              stream = FALSE,
                              rv = NULL) {
  arg_match(task, c("Context Only", "Permissive Chat"))

  if (add_context == "sometimes") {
    check_context <- TRUE
  } else if (add_context == "never") {
    check_context <- FALSE
    add_context <- FALSE
  } else if (add_context == "always") {
    check_context <- FALSE
    add_context <- TRUE
  }

  if (rlang::is_true(check_context)) {
    need_context <- is_context_needed(
      user_prompt = query,
      service = service,
      model = model
    )
  } else {
    need_context <- TRUE
  }

  if (rlang::is_true(add_context) || rlang::is_true(add_history)) {
    cli_alert_info("Creating embedding from query.")
    query_embedding <- get_query_embedding(query,
      local = local,
      model = embedding_model
    )
  }

  if (rlang::is_true(add_context) && rlang::is_true(need_context)) {
    cli_alert_info("Attempting to add context to query.")
    full_context <-
      get_query_context(
        query_embedding,
        index,
        k_context
      )

    context <-
      full_context |>
      dplyr::select(source, link, chunks) |>
      pmap(\(source, link, chunks) {
        glue::glue("Source: {source}
                   Link: {link}
                   Text: {chunks}")
      }) |>
      unlist() |>
      paste(collapse = "\n\n")
  } else {
    full_context <- "No context provided."
    context <- "No additional context provided."
  }

  if (rlang::is_true(add_history) && rlang::is_true(need_context)) {
    cli_alert_info("Attempting to add chat history to query.")
    cli_alert_info("Chat history: {class(chat_history)}")
    if (rlang::is_null(chat_history)) {
      related_history <- "No related history found."
    } else {
      related_history <-
        get_query_context(
          query_embedding,
          chat_history,
          k_history
        ) |>
        dplyr::distinct(content) |>
        dplyr::pull(content) |>
        paste(collapse = "\n\n")
    }
  } else {
    cli_alert_info("Not attempting to add chat history to query.")
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
                "You are a helpful chat bot that answers questions based on
                     the context provided by the user. If the user does not
                     provide related context and you need context to respond
                     accurately, say \"I am not able to answer that question.
                     Maybe try rephrasing your question in a different way.\""
              )
          )
        ),
      "Permissive Chat" =
        list(
          list(
            role = "system",
            content =
              glue(
                "You are a helpful chat bot that answers questions based on
                 the context provided by the user."
              )
          )
        )
    )

  prompt_context <- list(
    list(
      role = "system",
      content = "You provide succinct, concise, and accurate responses."
    ),
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
    map(session_history, \(x) {
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
    compact()

  prompt <- c(
    session_history,
    prompt_instructions,
    prompt_context,
    prompt_query
  )

  simple_prompt <- prompt |>
    map_chr(.f = "content") |>
    paste(collapse = "\n\n")

  cli_alert_info("Service: {service}")
  cli_alert_info("Model: {model}")

  cli_alert_info("Stream: {stream}")

  if (rlang::is_true(stream)) {
    cli_alert_info("Attempting to stream chat.")
    if (rlang::is_null(rv)) {
      answer <-
        stream_chat_openai(
          prompt = simple_prompt,
          element_callback = create_handler(service)
        )
    } else {
      stream_chat(
        prompt = simple_prompt,
        service = service,
        r = rv,
        output_id = "streaming",
        where = "shiny"
      )
      answer <- rv$response
    }
  } else {
    answer <- chat(
      prompt = simple_prompt,
      service = service,
      model = model,
      stream = FALSE
    )
  }

  if (save_history) {
    map(prompt, \(x) {
      save_user_history(
        file_name = history_name,
        role      = "system",
        content   = x$content,
        local     = local,
        model     = embedding_model,
        overwrite = overwrite
      )
    })
    save_user_history(
      file_name = history_name,
      role      = "assistant",
      content   = answer,
      local     = local,
      model     = embedding_model,
      overwrite = overwrite
    )
  }

  prompt_without_context <-
    c(session_history, prompt_query)

  list(prompt_without_context, full_context, answer)
}


is_context_needed <- function(user_prompt,
                              service = getOption("gpttools.service"),
                              model = getOption("gpttools.model")) {
  prompt <-
    glue::glue("Consider if additional context or history would be useful to
               accurately respond to this user prompt. Useful context may include
               information like package documentation, textbook excerpts, or
               other relevant details.

               Respond with TRUE if such context is likely to enhance the
               response. Respond with FALSE only if the
               query seems straightforward and well within your existing
               knowledge base.

               Most queries benefit from additional context.

               Respond ONLY with TRUE or FALSE.
               \n\n{user_prompt}")

  chat(
    prompt = prompt,
    service = service,
    model = model,
    stream = FALSE
  ) |>
    as.logical()
}
