stream_chat_perplexity <- function(prompt,
                                   element_callback = create_handler("perplexity"),
                                   model = getOption("gpttools.model", "pplx-7b-chat"),
                                   api_key = Sys.getenv("PERPLEXITY_API_KEY")) {
  request_body <- list(
    model = model,
    messages = list(
      list(role = "user", content = prompt)
    ),
    stream = TRUE
  )

  response <-
    httr2::request("https://api.perplexity.ai/chat/completions") |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      accept = "application/json",
      "Content-Type" = "application/json",
      Authorization = paste("Bearer", api_key)
    ) |>
    httr2::req_body_json(data = request_body) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform_stream(callback = element_callback, buffer_kb = 0.01)

  if (httr2::resp_is_error(response)) {
    status <- httr2::resp_status(response)
    description <- httr2::resp_status_desc(response)
    stop("Perplexity API request failed with error ", status, ": ", description, call. = FALSE)
  }
}
