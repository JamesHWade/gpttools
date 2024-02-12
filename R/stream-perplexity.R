request_base_perplexity <- function(api_key = Sys.getenv("PERPLEXITY_API_KEY")) {
  url <- "https://api.perplexity.ai/chat/completions"
  httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      accept = "application/json",
      "Content-Type" = "application/json",
      Authorization = paste("Bearer", api_key)
    )
}

query_api_perplexity <- function(request_body, api_key = Sys.getenv("PERPLEXITY_API_KEY")) {
  response <- request_base_perplexity(api_key) |>
    httr2::req_body_json(data = request_body) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()

  if (httr2::resp_is_error(response)) {
    status <- httr2::resp_status(response)
    description <- httr2::resp_status_desc(response)
    stop("Perplexity API request failed with error ", status, ": ", description, call. = FALSE)
  }

  httr2::resp_body_json(response)
}

create_stream_handler_perplexity <- function(output_id = NULL, r = NULL) {
  env <- rlang::env()
  function(x) {
    x <- rawToChar(x)
    # cat(x)
    pattern <- '\\{"id".*?\\}\\}\\]\\}'

    if (rlang::is_null(env$resp)) {
      env$resp <- x
    } else {
      env$resp <- paste0(env$resp, x)
    }

    if (stringr::str_detect(env$resp, pattern)) {
      parsed_no_pluck <<- stringr::str_extract(env$resp, pattern) |>
        jsonlite::fromJSON()

      parsed <-
        parsed_no_pluck |>
        purrr::pluck("choices", "delta", "content")

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

stream_chat_perplexity <- function(prompt,
                                   element_callback = create_stream_handler_perplexity(),
                                   model = getOption("gpttools.model", "pplx-7b-chat"),
                                   api_key = Sys.getenv("PERPLEXITY_API_KEY")) {
  request_body <- list(
    model = model,
    messages = list(
      list(role = "user", content = prompt)
    ),
    stream = TRUE
  )

  response <- request_base_perplexity(api_key) |>
    httr2::req_body_json(data = request_body) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform_stream(callback = element_callback, buffer_kb = 0.01)

  if (httr2::resp_is_error(response)) {
    status <- httr2::resp_status(response)
    description <- httr2::resp_status_desc(response)
    stop("Perplexity API request failed with error ", status, ": ", description, call. = FALSE)
  }
}
