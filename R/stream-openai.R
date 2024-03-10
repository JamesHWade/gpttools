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
    httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_auth_bearer_token(token = openai_api_key) |>
    httr2::req_body_json(data = body) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform_stream(
      callback = element_callback,
      buffer_kb = 0.01,
      round = "line"
    )

  if (httr2::resp_is_error(response)) {
    status <- httr2::resp_status(response)
    description <- httr2::resp_status_desc(response)

    cli::cli_abort(message = c(
      "x" = glue::glue("OpenAI API request failed. Error {status} - {description}"),
      "i" = "Visit the OpenAI API documentation for more details"
    ))
  }

  invisible(response)
}
