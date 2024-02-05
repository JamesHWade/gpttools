create_stream_handler_anthropic <- function() {
  env <- rlang::env()

  function(x) {
    x <- rawToChar(x)

    pattern <- "\\{\"type\":\"completion\",.*\"log_id\":\"compl_[^\"]*\"\\}"

    if (rlang::is_null(env$resp)) {
      env$resp <- x
    } else {
      env$resp <- paste0(env$resp, x)
    }
    if (stringr::str_detect(env$resp, pattern)) {
      parsed <- stringr::str_extract(env$resp, pattern) |>
        jsonlite::fromJSON() |>
        purrr::pluck("completion")

      env$full_resp <- paste0(env$full_resp, parsed)

      cat(parsed)

      # Use shinyjs to update a div with the response
      # shinyjs::html(output_id, env$full_resp)
      # r$response <- env$full_resp

      env$resp <- stringr::str_split(env$resp, pattern)
      env$resp <- env$resp[[1]][[length(env$resp[[1]])]]
    }
    TRUE
  }
}

create_stream_handler_anthropic_for_shiny <- function(r, output_id) {
  env <- rlang::env()

  function(x) {
    x <- rawToChar(x)

    # cat(x)

    pattern <- "\\{\"type\":\"completion\",.*\"log_id\":\"compl_[^\"]*\"\\}"

    if (rlang::is_null(env$resp)) {
      env$resp <- x
    } else {
      env$resp <- paste0(env$resp, x)
    }
    if (stringr::str_detect(env$resp, pattern)) {
      parsed <- stringr::str_extract(env$resp, pattern) |>
        jsonlite::fromJSON() |>
        purrr::pluck("completion")

      env$full_resp <- paste0(env$full_resp, parsed)

      # Use shinyjs to update a div with the response
      shinyjs::html(output_id, env$full_resp)
      r$response <- env$full_resp

      env$resp <- stringr::str_split(env$resp, pattern)
      env$resp <- env$resp[[1]][[length(env$resp[[1]])]]
    }
    TRUE
  }
}

stream_chat_anthropic <- function(prompt,
                                  element_callback = create_stream_handler_anthropic(),
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
