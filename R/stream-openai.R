chat_openai <- function(prompt = "Tell me a joke about R.",
                        model = "gpt-3.5-turbo",
                        history = NULL,
                        temperature = NULL,
                        stream = FALSE) {
  response <-
    req_chat(
      prompt = prompt,
      model = model,
      history = history,
      temperature = temperature,
      stream = is_true(stream)
    ) |>
    resp_chat()

  class(response) <- c("chat_tibble", class(response))

  invisible(response)
}

#' @export
print.chat_tibble <- function(x, ...) {
  n <- nrow(x)

  for (i in seq_len(n)) {
    print_role <- rule(stringr::str_to_title(x$role[i]))
    print_role <-
      switch(x$role[i],
        "assistant" = col_green(print_role),
        "system"    = col_silver(print_role),
        "user"      = col_blue(print_role)
      )
    writeLines(print_role)
    writeLines(x$content[i])
  }
  invisible(x)
}


# Make API Request --------------------------------------------------------

req_base_openai <- function(
    url = getOption("gpttools.url", "https://api.openai.com/")) {
  request(url) |>
    req_url_path_append("v1", "chat", "completions")
}

req_auth_openai <- function(request) {
  request |> req_auth_bearer_token(token = Sys.getenv("OPENAI_API_KEY"))
}

req_body_openai <- function(request,
                            prompt = "Tell me a joke about R.",
                            model = "gpt-4-turbo-preview",
                            history = NULL,
                            temperature = 0.7,
                            stream = FALSE) {
  if (!is_null(history)) {
    prompt <- add_history(prompt, history)
  } else {
    prompt <- list(list(role = "user", content = prompt))
  }

  body <-
    list(
      model = model,
      messages = prompt,
      temperature = temperature,
      stream = is_true(stream)
    )

  request |>
    req_body_json(data = body)
}

add_history <- function(prompt, history) {
  history <-
    pmap(history, \(x) {
      list(
        role = history$role,
        content = history$content
      )
    })
  list(
    history,
    list(
      role = "user",
      content = prompt
    )
  )
}

req_chat <- function(prompt, model, history, temperature, stream = FALSE) {
  req <-
    req_base_openai() |>
    req_auth_openai() |>
    req_body_openai(
      prompt = prompt,
      model = model,
      history = history,
      temperature = temperature,
      stream = is_true(stream)
    ) |>
    req_retry(max_tries = 3) |>
    req_error(is_error = function(resp) FALSE)

  if (is_true(stream)) {
    req |>
      req_perform_stream(
        callback = create_handler("openai"),
        buffer_kb = 0.01
      )
  } else {
    req |>
      req_perform()
  }
}


# Process API Response ----------------------------------------------------

resp_chat <- function(response) {
  response |>
    resp_chat_error() |>
    resp_body_json(simplifyVector = TRUE) |>
    pluck("choices", "message") |>
    tibble::as_tibble()
}

resp_chat_error <- function(response) {
  if (resp_is_error(response)) {
    status <- resp_status(response)
    description <- resp_status_desc(response)

    cli_abort(message = c(
      "x" = glue::glue("OpenAI API request failed. Error {status} - {description}"),
      "i" = "Visit the OpenAI API documentation for more details"
    ))
  } else {
    invisible(response)
  }
}
