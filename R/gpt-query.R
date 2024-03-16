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
  cli_inform("Selection: {query}")
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
  cli_inform("Service: {service}")
  cli_inform("Model: {model}")
  cli_inform("Sending query... this can take up to 3 minutes.")
  simple_prompt <- prompt |>
    map_chr(.f = "content") |>
    paste(collapse = "\n\n")

  answer <- chat(
    prompt = simple_prompt,
    service = service,
    model = model,
    stream = FALSE
  )

  cli_process_done(msg_done = "Received response from {service}")
  text_to_insert <- as.character(answer)
  insert_text(text_to_insert)
}

query_openai <- function(task = "chat/completions",
                         body = NULL,
                         api_key = Sys.getenv("OPENAI_API_KEY"),
                         base_url = "https://api.openai.com/v1",
                         model = "gpt-3.5-turbo") {
  arg_match(task, c("chat/completions", "embeddings"))

  req <- request(base_url)

  if (task == "chat/completions") {
    body <- list(
      model = model,
      messages = body
    )
  }

  resp <-
    req |>
    req_url_path_append(task) |>
    req_user_agent("gpttools: https://github.com/jameshwade/gpttools") |>
    req_headers(
      "Authorization" = glue("Bearer {api_key}"),
      "Content-Type" = "application/json"
    ) |>
    req_body_json(body) |>
    req_retry() |>
    req_throttle(4) |>
    req_perform()

  resp |> resp_body_json(simplifyVector = TRUE)
}
