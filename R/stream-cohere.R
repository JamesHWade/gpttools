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
    httr2::request("https://api.cohere.ai/v1/chat") |>
    httr2::req_headers(
      `accept` = "application/json",
      `Authorization` = paste("Bearer", key),
      `content-type` = "application/json"
    ) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(data = request_body) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform_stream(callback = element_callback,
                              buffer_kb = 0.01,
                              round = "line")

  if (httr2::resp_is_error(response)) {
    status <- httr2::resp_status(response)
    description <- httr2::resp_status_desc(response)

    cli::cli_abort(message = c(
      "x" = glue::glue("Cohere API request failed. Error {status} - {description}"),
      "i" = "Visit the Cohere API documentation for more details"
    ))
  }
  invisible(response)
}
