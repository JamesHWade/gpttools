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
    request("https://api.anthropic.com/v1/complete") |>
    req_headers(
      `accept` = "application/json",
      `anthropic-version` = "2023-06-01",
      `content-type` = "application/json",
      `x-api-key` = key
    ) |>
    req_method("POST") |>
    req_body_json(data = request_body) |>
    req_retry(max_tries = 3) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform_stream(callback = element_callback, buffer_kb = 0.01)

  # error handling
  if (resp_is_error(response)) {
    status <- resp_status(response)
    description <- resp_status_desc(response)

    cli_abort(message = c(
      "x" = "Anthropic API request failed. Error {status} - {description}",
      "i" = "Visit the Anthropic API documentation for more details"
    ))
  }
}
