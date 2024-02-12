stream_chat <- function(prompt,
                        service = getOption("gpttools.service"),
                        r = NULL,
                        output_id = "streaming",
                        where = "console") {
  switch(service,
    "openai" = {
      response <- stream_chat_openai(
        prompt = prompt,
        element_callback = create_handler("openai", r, output_id, where)
      )
    },
    "anthropic" = {
      response <- stream_chat_anthropic(
        prompt = prompt,
        element_callback = create_handler("anthropic", r, output_id, where)
      )
    },
    "perplexity" = {
      response <- stream_chat_perplexity(
        prompt = prompt,
        element_callback = create_handler("perplexity", r, output_id, where)
      )
    },
    "cohere" = {
      response <- stream_chat_cohere(
        prompt = prompt,
        element_callback = create_handler("cohere", r, output_id, where)
      )
    },
    "ollama" = {
      response <- stream_chat_ollama(
        prompt = prompt,
        element_callback = create_handler("ollama", r, output_id, where)
      )
    }
  )
}

create_handler <- function(service = "openai",
                           r = NULL,
                           output_id = "streaming",
                           where = "console") {
  env <- rlang::env()
  env$resp <- NULL
  env$full_resp <- NULL

  stream_details <- get_stream_pattern(service)
  new_pattern <- stream_details$pattern
  new_pluck <- stream_details$pluck

  # Use quosures to capture the expressions
  pattern_quo <- rlang::quo(new_pattern)

  mask <- new_data_mask(env)

  # Define the handler function with injected values
  rlang::eval_tidy(rlang::expr(function(x) {
    x <- rawToChar(x)
    pattern <- !!pattern_quo
    if (rlang::is_null(env$resp)) {
      env$resp <- x
    } else {
      env$resp <- paste0(env$resp, x)
    }

    if (stringr::str_detect(env$resp, pattern)) {
      parsed <- stringr::str_extract(env$resp, pattern) |>
        jsonlite::fromJSON() |>
        purrr::pluck(!!!new_pluck)
      env$full_resp <- paste0(env$full_resp, parsed)

      if (where == "shiny") {
        shinyjs::html(
          output_id,
          shiny::markdown(paste("**Assistant**", env$full_resp, sep = "\n\n"))
        )

        r$response <- env$full_resp
      } else if (where == "console") {
        cat(parsed)
      } else if (where == "source") {
        rlang::check_installed("pak")
        rlang::check_installed("rstudioapi",
          version = "0.15.0.9",
          action = \(pkg, ...) pak::pak("rstudio/rstudioapi")
        )
        rstudioapi::setGhostText(env$full_resp)
      }

      env$resp <- stringr::str_split(env$resp, pattern)
      env$resp <- env$resp[[1]][[length(env$resp[[1]])]]
    }
    TRUE
  }), mask)
}

get_stream_pattern <- function(service) {
  switch(service,
    "openai" = {
      pattern <- '\\{"id":.*?\\}\\]\\}'
      pluck <- c("choices", "delta", "content")
    },
    "anthropic" = {
      pattern <- "\\{\"type\":\"completion\",.*\"log_id\":\"compl_[^\"]*\"\\}"
      pluck <- "completion"
    },
    "perplexity" = {
      pattern <- '\\{"id".*?\\}\\}\\]\\}'
      pluck <- c("choices", "delta", "content")
    },
    "cohere" = {
      pattern <-
        '\\{"is_finished":false,"event_type":"text-generation","text":".*"\\}'
      pluck <- "text"
    },
    "ollama" = {
      pattern <- '\\{"model":.*"done":false\\}'
      pluck <- "response"
    }
  )
  list(pattern = pattern, pluck = pluck)
}
