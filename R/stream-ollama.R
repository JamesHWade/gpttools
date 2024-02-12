stream_chat_ollama <- function(prompt,
                               model = getOption("gpttools.model"),
                               element_callback = create_stream_handler_ollama()) {
  body <- list(
    model = model,
    prompt = prompt,
    stream = TRUE
  )

  # ollama_is_available()
  url <- Sys.getenv("OLLAMA_HOST", "http://localhost:11434")
  response <-
    httr2::request(url) |>
    httr2::req_url_path_append("api") |>
    httr2::req_url_path_append("generate") |>
    httr2::req_body_json(data = body) |>
    httr2::req_perform_stream(callback = element_callback, buffer_kb = 0.01)

  if (httr2::resp_is_error(response)) {
    status <- httr2::resp_status(response)
    description <- httr2::resp_status_desc(response)

    cli::cli_abort(message = c(
      "x" = glue::glue("Ollama API request failed. Error {status} - {description}"),
      "i" = "Visit the Ollama API documentation for more details"
    ))
  }
}

ollama_is_available <- function(verbose = FALSE) {
  request <- Sys.getenv("OLLAMA_HOST", "http://localhost:11434") |>
    httr2::request()

  check_value <- logical(1)

  rlang::try_fetch(
    {
      response <- httr2::req_perform(request) |>
        httr2::resp_body_string()

      if (verbose) cli::cli_alert_success(response)
      check_value <- TRUE
    },
    error = function(cnd) {
      if (inherits(cnd, "httr2_failure")) {
        if (verbose) cli::cli_alert_danger("Couldn't connect to Ollama in {.url {ollama_api_url()}}. Is it running there?")
      } else {
        if (verbose) cli::cli_alert_danger(cnd)
      }
      check_value <- FALSE
    }
  )

  invisible(check_value)
}

create_stream_handler_ollama <- function() {
  env <- rlang::env()

  function(x) {
    x <- rawToChar(x)

    pattern <- '\\{"model":.*"done":false\\}'

    if (rlang::is_null(env$resp)) {
      env$resp <- x
    } else {
      env$resp <- paste0(env$resp, x)
    }
    if (stringr::str_detect(env$resp, pattern)) {
      parsed <- stringr::str_extract(env$resp, pattern) |>
        jsonlite::fromJSON() |>
        purrr::pluck("response")

      env$full_resp <- paste0(env$full_resp, parsed)

      cat(parsed)

      # # Uncomment and customize if you need to update UI components in a Shiny app:
      # shinyjs::html(output_id, env$full_resp)
      # r$response <- env$full_resp

      env$resp <- stringr::str_split(env$resp, pattern)
      env$resp <- env$resp[[1]][[length(env$resp[[1]])]]
    }
    TRUE
  }
}
