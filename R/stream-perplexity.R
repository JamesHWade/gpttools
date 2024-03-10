stream_chat_perplexity <- function(prompt,
                                   element_callback = create_handler("perplexity"),
                                   model = getOption("gpttools.model", "sonar-small-chat"),
                                   api_key = Sys.getenv("PERPLEXITY_API_KEY")) {
  request_body <- list(
    model = model,
    messages = list(
      list(role = "user", content = prompt)
    ),
    stream = TRUE
  )

  response <-
    request("https://api.perplexity.ai/chat/completions") |>
    req_headers(
      accept = "application/json",
      "Content-Type" = "application/json",
      Authorization = paste("Bearer", api_key)
    ) |>
    req_body_json(data = request_body) |>
    req_retry(max_tries = 3) |>
    req_perform_stream(callback = element_callback,
                              buffer_kb = 0.01,
                              round = "line")

  if (httr2::resp_is_error(response)) {
    status <- httr2::resp_status(response)
    description <- httr2::resp_status_desc(response)

    cli::cli_abort(message = c(
      "x" = glue::glue("Perplexity API request failed. Error {status} - {description}"),
      "i" = "Visit the Perplexity API documentation for more details"
    ))
  }
}
