stream_chat_cohere <- function(prompt,
                               model = "command",
                               element_callback = create_handler("cohere"),
                               key = Sys.getenv("COHERE_API_KEY")) {
  request_body <- list(
    message = prompt,
    model = model,
    stream = TRUE
  )

  response <-
    request("https://api.cohere.ai/v1/chat") |>
    req_headers(
      `accept` = "application/json",
      `Authorization` = paste("Bearer", key),
      `content-type` = "application/json"
    ) |>
    req_body_json(data = request_body) |>
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

    cli_abort(c(
      "x" = glue("Cohere API request failed. Error {status} - {description}"),
      "i" = "Visit the Cohere API documentation for more details"
    ))
  }
  invisible(response)
}
