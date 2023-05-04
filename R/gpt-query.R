#' Use GPT to improve text
#'
#' This function uses the GPT model from OpenAI to improve the spelling and
#' grammar of the selected text in the current RStudio session.
#'
#' @param model The name of the GPT model to use.
#' @param prompt Instructions for the insertion
#' @param temperature A parameter for controlling the randomness of the GPT
#' model's output.
#' @param max_tokens Maximum number of tokens to return (related to length of
#' response), defaults to 500
#' @param openai_api_key An API key for the OpenAI API.
#' @param append_text Add text to selection rather than replace, defaults to
#' FALSE
#'
#' @return Nothing is returned. The improved text is inserted into the current
#' RStudio session.
#' @export
gpt_insert <- function(model,
                       prompt,
                       temperature = 0.1,
                       max_tokens = getOption("gpttools.max_tokens"),
                       openai_api_key = Sys.getenv("OPENAI_API_KEY"),
                       append_text = FALSE) {
  gptstudio::check_api()
  selection <- get_selection()
  cli_inform("Asking GPT for help...")

  prompt <- paste(prompt, selection$value)

  edit <- gptstudio::openai_create_completion(
    model = model,
    prompt = prompt,
    temperature = temperature,
    max_tokens = max_tokens,
    openai_api_key = openai_api_key,
  )

  cli_inform("Inserting text from GPT...")

  if (append_text) {
    improved_text <- c(selection$value, edit$choices$text)
  } else {
    improved_text <- c(edit$choices$text, selection$value)
  }

  insert_text(improved_text)
}


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
  rstudioapi::insertText(improved_text)
}


# write a function to take the output of this function and return only the R code
gpt_chat <- function(instructions) {
  gptstudio::check_api()
  query <- get_selection()
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
  answer <- gptstudio::openai_create_chat_completion(prompt)
  text_to_insert <- c(
    as.character(query),
    as.character(answer$choices$message.content)
  )
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

  resp <-
    req |>
    httr2::req_url_path_append(task) |>
    httr2::req_user_agent("gpttools: https://github.com/jameshwade/gpttools") |>
    httr2::req_headers(
      "Authorization" = glue("Bearer {api_key}"),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(list(model = model, messages = body)) |>
    httr2::req_retry() |>
    httr2::req_throttle(4) |>
    httr2::req_perform()

  resp |> httr2::resp_body_json(simplifyVector = TRUE)
}
