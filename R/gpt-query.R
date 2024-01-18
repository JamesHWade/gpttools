#' Wrapper around selectionGet to help with testthat
#'
#' @return Text selection via `rstudioapi::selectionGet`
#'
#' @export
get_selection <- function() {
  rstudioapi::verifyAvailable()
  rstudioapi::selectionGet()
}

#' Wrapper around selectionGet to help with testthat
#'
#' @param improved_text Text from model queries to inert into script or document
#'
#' @export
insert_text <- function(improved_text) {
  rstudioapi::verifyAvailable()
  rstudioapi::insertText(NULL, improved_text)
}


# write a function to take the output of this function; return only the R code
gpt_chat <- function(instructions,
                     service = getOption("gpttools.service", "openai"),
                     model = getOption("gpttools.model", "gpt-4")) {
  query <- get_selection()
  cli::cli_inform("Selection: {query}")
  prompt <-
    list(
      list(
        role = "system",
        content = glue("{instructions}")
      ),
      list(
        role = "user",
        content = glue("{query}")
      )
    )
  cli::cli_process_start(msg = "Sending query to {service}")
  cli::cli_progress_update()
  simple_prompt <- prompt |>
    purrr::map_chr(.f = "content") |>
    paste(collapse = "\n\n")

  cat(simple_prompt, "\n\n")

  cli::cli_inform("Service: {service}")
  cli::cli_inform("Model: {model}")

  answer <-
    gptstudio:::gptstudio_create_skeleton(
      service = service,
      model = model,
      prompt = simple_prompt,
      stream = FALSE
    ) |>
    gptstudio:::gptstudio_request_perform()

  cli::cli_process_done(msg_done = "Received response from {service}")
  text_to_insert <- c(
    as.character(query),
    as.character(answer$choices$message.content)
  )
  cli::cli_inform("Text to insert: {text_to_insert}")
  insert_text(text_to_insert)
}

# The new function to extract only R code would be:
extract_code <- function(response_with_code) {
  prompt <-
    list(
      list(
        role = "system",
        content = paste(
          "Extract only the R code from the user provided input.",
          "Do not provide anything besides R code in response.",
          "The code will be evaluated by the R console.",
          "No free text at all. No code blocks. Only R code.",
          collapse = " "
        )
      ),
      list(
        role = "user",
        content = response_with_code
      )
    )
  answer <- gptstudio::openai_create_chat_completion(prompt)
  code <- stringr::str_remove_all(answer$choices$message.content,
    pattern = "(?i)```\\{?[Rr]?\\}?"
  )
}

run_bg_code <- function(code) {
  tmpfile <- tempfile(fileext = ".R")
  readr::write_lines(code, tmpfile)
  callr::r_bg(run_code, args = list(code))
}

query_openai <- function(task = "chat/completions",
                         body = NULL,
                         api_key = Sys.getenv("OPENAI_API_KEY"),
                         base_url = "https://api.openai.com/v1",
                         model = "gpt-3.5-turbo") {
  arg_match(task, c("chat/completions", "embeddings"))

  req <- httr2::request(base_url)

  if (task == "chat/completions") {
    body <- list(
      model = model,
      messages = body
    )
  }

  resp <-
    req |>
    httr2::req_url_path_append(task) |>
    httr2::req_user_agent("gpttools: https://github.com/jameshwade/gpttools") |>
    httr2::req_headers(
      "Authorization" = glue("Bearer {api_key}"),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_retry() |>
    httr2::req_throttle(4) |>
    httr2::req_perform()

  resp |> httr2::resp_body_json(simplifyVector = TRUE)
}

check_to_add_context <- function(query, model = "gpt-3.5-turbo") {
  # nolint start
  body <- list(
    list(
      role = "system",
      content = "Determine if the user provided prompt needs additional context to provide a useful response. Provide your response as json where \"add_context\" is either TRUE or FALSE. Do not provide any additional details or response."
    ),
    list(
      role = "user",
      content = query
    )
  )
  # nolint end

  response <- query_openai(
    task = "chat/completions",
    body = body,
    model = model
  )
  jsonlite::fromJSON(response$choices$message$content)
}
