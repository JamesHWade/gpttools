query_azure_openai <- function(deployment_name = Sys.getenv("AZURE_OPENAI_DEPLOYMENT_NAME"),
                               task = "chat/completions",
                               body = NULL,
                               api_key = Sys.getenv("AZURE_OPENAI_KEY")) {
  arg_match(task, c("chat/completions", "embeddings"))

  api_version <-
    switch(task,
      "chat/completions" = "chat/completions?api-version=2023-03-15-preview",
      "embeddings"       = "embeddings?api-version=2022-12-01"
    )

  req <- httr2::request("https://ai0028.openai.azure.com")

  resp <-
    req |>
    httr2::req_url_path_append("openai/deployments") |>
    httr2::req_url_path_append(deployment_name) |>
    httr2::req_url_path_append(api_version) |>
    httr2::req_user_agent("gpttools: https://github.com/jameshwade/gpttools") |>
    httr2::req_headers(
      "api-key" = api_key,
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_retry() |>
    httr2::req_throttle(0.5) |>
    httr2::req_perform()

  resp |> httr2::resp_body_json(simplifyVector = TRUE)
}


create_azure_openai_embedding <- function(input_text) {
  body <- list(input = input_text)
  embedding <- query_azure_openai(
    deployment_name = "embeddings",
    body = body,
    task = "embeddings"
  )
  tibble::tibble(
    usage = embedding$usage$total_tokens,
    embedding = embedding$data$embedding
  )
}

create_azure_openai_chat_completion <- function(prompt) {
  body <- list(messages = prompt)
  query_azure_openai(body = body, task = "chat/completions")
}


add_azure_embeddings <- function(index) {
  index |>
    dplyr::mutate(
      embeddings = purrr::map(
        .x = chunks,
        .f = create_azure_openai_embedding,
        .progress = "Create Embeddings"
      )
    ) |>
    tidyr::unnest(embeddings)
}

create_azure_index <- function(domain,
                               overwrite = FALSE,
                               dont_ask = FALSE,
                               pkg_version = NULL) {
  index_dir <-
    file.path(tools::R_user_dir("gpttools", which = "data"), "index")
  index_file <-
    glue::glue("{index_dir}/{domain}.parquet")

  if (file.exists(index_file) && rlang::is_false(overwrite)) {
    cli::cli_abort(
      c(
        "!" = "Index already exists for this domain.",
        "i" = "Use {.code overwrite = TRUE} to overwrite index."
      )
    )
  }
  index <- prepare_scraped_files(domain = domain)
  n_tokens <- sum(index$n_tokens)

  cli::cli_inform(c(
    "!" = "You are about to create embeddings for {domain}.",
    "i" = "This will use approx. {scales::scientific(n_tokens)} tokens.",
    "i" = "Only proceed if you understand the cost.",
    "i" = "Read more about embeddings at {.url
      https://platform.openai.com/docs/guides/embeddings}."
  ))

  if (dont_ask) {
    ask_user <- TRUE
  } else {
    ask_user <- usethis::ui_yeah(
      "Would you like to continue with creating embeddings?"
    )
  }
  if (rlang::is_true(ask_user)) {
    index <-
      index |>
      add_azure_embeddings() |>
      dplyr::mutate(version = pkg_version)
    if (rlang::is_false(dir.exists(index_dir))) {
      dir.create(index_dir, recursive = TRUE)
    }
    arrow::write_parquet(
      x    = index,
      sink = index_file
    )
  } else {
    cli_inform("No index was creates for {domain}")
  }
}
