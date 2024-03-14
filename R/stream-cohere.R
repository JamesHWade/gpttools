stream_chat_cohere <- function(prompt,
                               model = getOption("gpttools.model", "command"),
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
<<<<<<< HEAD
=======
    req_method("POST") |>
>>>>>>> 997b9a4 (import httr2, cli, and rlang)
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

<<<<<<< HEAD
    cli_abort(c(
      "x" = glue("Cohere API request failed. Error {status} - {description}"),
=======
    cli_abort(message = c(
      "x" = glue::glue("Cohere API request failed. Error {status} - {description}"),
>>>>>>> 997b9a4 (import httr2, cli, and rlang)
      "i" = "Visit the Cohere API documentation for more details"
    ))
  }
  invisible(response)
}
