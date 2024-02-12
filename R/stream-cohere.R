stream_chat_cohere <- function(prompt,
                               model = getOption("gpttools.model", "command"),
                               element_callback = create_stream_handler_cohere(),
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
    httr2::req_perform_stream(callback = element_callback, buffer_kb = 0.01)

  if (httr2::resp_is_error(response)) {
    status <- httr2::resp_status(response)
    description <- httr2::resp_status_desc(response)

    cli::cli_abort(message = c(
      "x" = glue::glue("Cohere API request failed. Error {status} - {description}"),
      "i" = "Visit the Cohere API documentation for more details"
    ))
  }
}

create_stream_handler_cohere <- function() {
  env <- rlang::env()

  function(x) {
    x <- rawToChar(x)
    # cat(x)

    pattern <-
      '\\{"is_finished":false,"event_type":"text-generation","text":".*"\\}'

    if (rlang::is_null(env$resp)) {
      env$resp <- x
    } else {
      env$resp <- paste0(env$resp, x)
    }
    if (stringr::str_detect(env$resp, pattern)) {
      parsed <- stringr::str_extract(env$resp, pattern) |>
        jsonlite::fromJSON() |>
        purrr::pluck("text")

      env$full_resp <- paste0(env$full_resp, parsed)

      rstudioapi::setGhostText(env$full_resp)
      # cat(parsed)

      # # Uncomment and customize if you need to update UI components in a Shiny app:
      # shinyjs::html(output_id, env$full_resp)
      # r$response <- env$full_resp

      env$resp <- stringr::str_split(env$resp, pattern)
      env$resp <- env$resp[[1]][[length(env$resp[[1]])]]
    }
    TRUE
  }
}
