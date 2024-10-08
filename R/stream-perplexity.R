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
    req_perform_stream(callback = element_callback, buffer_kb = 0.01)

  if (resp_is_error(response)) {
    status <- resp_status(response)
    description <- resp_status_desc(response)
    stop("Perplexity API request failed with error ", status, ": ", description, call. = FALSE)
  }
}
