stream_chat_openai <- function(prompt = NULL,
                               element_callback = create_handler("openai"),
                               model = getOption("gpttools.model", "gpt-4-turbo-preview"),
                               openai_api_key = Sys.getenv("OPENAI_API_KEY"),
                               shiny = FALSE) {
  messages <- list(
    list(
      role = "user",
      content = prompt
    )
  )

  # Set the request body
  body <- list(
    model = model,
    stream = TRUE,
    messages = messages
  )

  response <-
    request("https://api.openai.com/v1/chat/completions") |>
    req_auth_bearer_token(token = openai_api_key) |>
    req_body_json(data = body) |>
    req_retry(max_tries = 3) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform_stream(
      callback = element_callback,
      buffer_kb = 0.01,
      round = "line"
    )

  if (resp_is_error(response)) {
    status <- resp_status(response)
    description <- resp_status_desc(response)

    cli_abort(message = c(
      "x" = glue::glue("OpenAI API request failed. Error {status} - {description}"),
      "i" = "Visit the OpenAI API documentation for more details"
    ))
  }

  invisible(response)
}
