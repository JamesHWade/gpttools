stream_chat_azure_openai <- function(prompt = NULL,
                                     element_callback = create_handler("openai"),
                                     shiny = FALSE,
                                     use_token = Sys.getenv("AZURE_OPENAI_USE_TOKEN")) {
  messages <- list(
    list(
      role = "user",
      content = prompt
    )
  )

  body <- list(
    stream = TRUE,
    messages = messages
  )


  response <-
    httr2::request(Sys.getenv("AZURE_OPENAI_ENDPOINT")) |>
    httr2::req_url_path_append("openai/deployments") |>
    httr2::req_url_path_append(Sys.getenv("AZURE_OPENAI_DEPLOYMENT_NAME")) |>
    httr2::req_url_path_append(Sys.getenv("AZURE_OPENAI_TASK")) |>
    httr2::req_url_query("api-version" = Sys.getenv("AZURE_OPENAI_API_VERSION")) |>
    httr2::req_headers(
      "api-key" = Sys.getenv("AZURE_OPENAI_KEY"),
      "Content-Type" = "application/json"
    )

  if (use_token) {
    token <- retrieve_azure_token()
    response <- response |> httr2::req_auth_bearer_token(token = token)
  }

  response <-
    response |>
    httr2::req_body_json(data = body) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform_stream(
      callback = element_callback,
      buffer_kb = 0.01
    )

  invisible(response)
}


retrieve_azure_token <- function() {
  rlang::check_installed("AzureRMR")
  token <- AzureRMR::create_azure_login(
    tenant = Sys.getenv("AZURE_OPENAI_TENANT_ID"),
    app = Sys.getenv("AZURE_OPENAI_CLIENT_ID"),
    password = Sys.getenv("AZURE_OPENAI_CLIENT_SECRET"),
    host = "https://cognitiveservices.azure.com/",
    scopes = ".default"
  )
  invisible(token$token$credentials$access_token)
}
