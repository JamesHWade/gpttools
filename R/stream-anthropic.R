stream_chat_anthropic <- function(prompt,
                                  element_callback = create_handler("anthropic"),
                                  model = "claude-2",
                                  key = Sys.getenv("ANTHROPIC_API_KEY")) {
  request_body <- list(
    prompt = glue::glue("\n\nHuman: {prompt}\n\nAssistant:"),
    model = model,
    max_tokens_to_sample = 256,
    stream = TRUE
  )

  response <-
    httr2::request("https://api.anthropic.com/v1/complete") |>
    httr2::req_headers(
      `accept` = "application/json",
      `anthropic-version` = "2023-06-01",
      `content-type` = "application/json",
      `x-api-key` = key
    ) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(data = request_body) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform_stream(callback = element_callback, buffer_kb = 0.01)

  # error handling
  if (httr2::resp_is_error(response)) {
    status <- httr2::resp_status(response)
    description <- httr2::resp_status_desc(response)

    cli::cli_abort(message = c(
      "x" = "Anthropic API request failed. Error {status} - {description}",
      "i" = "Visit the Anthropic API documentation for more details"
    ))
  }
}
