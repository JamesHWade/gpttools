chat_openai <- function(prompt = "Tell me a joke about the R language.",
                        model = "gpt-3.5-turbo",
                        history = NULL,
                        temperature = NULL,
                        stream = FALSE) {

  prompt <- prompt |> add_history(history)

  response <-
    req_chat_openai(
      prompt = prompt,
      model = model,
      temperature = temperature,
      stream = is_true(stream)
    ) |>
    resp_chat_openai(stream = is_true(stream))

  response <- c(prompt,
                list(list(role = response$role, content = response$content)))

  class(response) <- c("chat_list", class(response))

  response

}

#' @export
print.chat_list <- function(x, ...) {
  n <- length(x)

  writeLines("\n")

  for (i in seq_len(n)) {
    print_role <- rule(stringr::str_to_title(x[[i]]$role))
    print_role <-
      switch(x[[i]]$role,
             "assistant" = col_green(print_role),
             "system"    = col_silver(print_role),
             "user"      = col_blue(print_role)
      )
    writeLines(print_role)
    writeLines(x[[i]]$content)
  }

  writeLines("\n")
  invisible(x)
}


# Make API Request --------------------------------------------------------

req_base_openai <- function(
    url = getOption("gpttools.url", "https://api.openai.com/")) {
  request(url) |>
    req_url_path_append("v1", "chat", "completions")
}

req_auth_openai <- function(request) {
  request |> req_auth_bearer_token(token = Sys.getenv("OPENAI_API_KEY"))
}

req_body_openai <- function(request,
                            prompt = "Tell me a joke about the R language.",
                            model = "gpt-4-turbo-preview",
                            history = NULL,
                            temperature = 0.7,
                            stream = FALSE) {
  body <-
    list(
      model = model,
      messages = prompt,
      temperature = temperature,
      stream = is_true(stream)
    )

  request |>
    req_body_json(data = body)
}


req_chat_openai <- function(prompt, model, temperature, stream = FALSE) {
  req <-
    req_base_openai() |>
    req_auth_openai() |>
    req_body_openai(
      prompt = prompt,
      model = model,
      temperature = temperature,
      stream = is_true(stream)
    ) |>
    req_retry(max_tries = 3) |>
    req_error(is_error = function(resp) FALSE)

  if (is_true(stream)) {
    env <- caller_env()
    req |>
      req_perform_stream(
        callback = \(x) stream_callback_openai(x, env),
        buffer_kb = 0.01,
        round = "line"
      )
    tibble::tibble(role = "assistant", content = env$response)
  } else {
    req |>
      req_perform()
  }
}

#' @importFrom jsonlite fromJSON
stream_callback_openai <- function(x, env) {
  txt <- rawToChar(x)

  lines <- str_split(txt, "\n")[[1]]
  lines <- lines[lines != ""]
  lines <- str_replace_all(lines, "^data: ", "")
  lines <- lines[!str_detect(lines, "\"finish_reason\":\"stop\"")]
  lines <- lines[lines != "[DONE]"]

  tokens <- map_chr(lines, \(line) {
    chunk <- jsonlite::fromJSON(line)
    chunk$choices$delta$content
  })

  env$response <- paste0(env$response, tokens)

  cat(tokens)

  TRUE
}

# Process API Response ----------------------------------------------------

resp_chat_openai <- function(response, stream) {
  resp <- response

  if (is_true(stream)) {
    resp
  } else {
    resp |>
      resp_chat_error_openai() |>
      resp_body_json(simplifyVector = TRUE) |>
      pluck("choices", "message")
  }
}

resp_chat_error_openai <- function(response) {
  if (resp_is_error(response)) {
    status <- resp_status(response)
    description <- resp_status_desc(response)

    cli_abort(c(
      "x" = glue("OpenAI API request failed. Error {status} - {description}"),
      "i" = "Visit the OpenAI API documentation for more details"
    ))
  } else {
    invisible(response)
  }
}

add_history <- function(prompt, history = NULL) {
  c(
    history,
    list(
      list(
        role = "user",
        content = prompt
      )
    )
  ) |>
    purrr::compact()
}
