query_openai_azure <- function(deployment_name,
                               task = "chat/completions",
                               body = NULL,
                               api_key = Sys.getenv("AZURE_OPENAI_KEY"),
                               base_url = Sys.getenv("AZURE_OPENAI_ENDPOINT")) {
  arg_match(task, c("chat/completions", "embeddings"))

  api_version <- switch(task,
    "chat/completions" = "chat/completions?api-version=2023-03-15-preview",
    "embeddings"       = "embeddings?api-version=2022-12-01"
  )

  req <- httr2::request(base_url)

  resp <-
    req |>
    httr2::req_url_path_append("openai/deployments") |>
    httr2::req_url_path_append(deployment_name) |>
    httr2::req_url_path_append(api_version) |>
    httr2::req_user_agent("gpttools: https://github.com/jameshwade/gpttools") |>
    httr2::req_headers(
      "api-key" = api_key,
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_retry() |>
    httr2::req_throttle(4) |>
    httr2::req_perform()

  resp |> httr2::resp_body_json(simplifyVector = TRUE)
}


create_openai_embedding_azure <- function(input_text) {
  body <- list(input = input_text)
  embedding <- query_openai_azure(
    deployment_name = Sys.getenv("AZURE_OPENAI_EMBEDDINGS_DEPLOYMENT_NAME"),
    body = body,
    task = "embeddings"
  )
  tibble::tibble(
    usage = embedding$usage$total_tokens,
    embedding = embedding$data$embedding
  )
}

create_openai_chat_completion_azure <- function(prompt) {
  body <- list(messages = prompt)
  query_openai_azure(
    deployment_name = Sys.getenv("AZURE_OPENAI_CHAT_DEPLOYMENT_NAME"),
    body = body,
    task = "chat/completions"
  )
}


add_embeddings_azure <- function(index) {
  index |>
    dplyr::mutate(
      embeddings = purrr::map(
        .x = chunks,
        .f = create_openai_embedding_azure,
        .progress = "Create Embeddings"
      )
    ) |>
    tidyr::unnest(embeddings)
}

create_index_azure <- function(domain,
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
  n_tokens <- sum(index$n_tokens)

  cli::cli_inform(c(
    "!" = "You are about to create embeddings for {domain}.",
    "i" = "This will use approx. {scales::scientific(n_tokens)} tokens.",
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
      add_embeddings_azure() |>
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

#' chat_with_context_azure
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
chat_with_context_azure <- function(query,
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
    query_embedding <- get_query_embedding_azure(query)
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

  answer <- create_openai_chat_completion_azure(prompt)

  if (rlang::is_true(save_history)) {
    purrr::map(prompt, \(x) {
      save_history_azure(
        file_name = history_name,
        role      = "system",
        content   = x$content,
        overwrite = overwrite
      )
    })
    save_history_azure(
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

save_history_azure <- function(file_name = "chat_history",
                               role,
                               content,
                               overwrite = FALSE) {
  create_history(file_name, overwrite = overwrite)
  history <- read_history(file_name)
  embedding <- create_openai_embedding_azure(content)

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


get_query_embedding_azure <- function(query) {
  create_openai_embedding_azure(input_text = query) |>
    dplyr::pull(embedding) |>
    unlist()
}
