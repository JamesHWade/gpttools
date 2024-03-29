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
    request(Sys.getenv("AZURE_OPENAI_ENDPOINT")) |>
    req_url_path_append("openai/deployments") |>
    req_url_path_append(Sys.getenv("AZURE_OPENAI_DEPLOYMENT_NAME")) |>
    req_url_path_append(Sys.getenv("AZURE_OPENAI_TASK")) |>
    req_url_query("api-version" = Sys.getenv("AZURE_OPENAI_API_VERSION")) |>
    req_headers(
      "api-key" = Sys.getenv("AZURE_OPENAI_KEY"),
      "Content-Type" = "application/json"
    )

  if (use_token) {
    token <- retrieve_azure_token()
    response <- response |> req_auth_bearer_token(token = token)
  }

  response <-
    response |>
    req_body_json(data = body) |>
    req_retry(max_tries = 3) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform_stream(
      callback = element_callback,
      buffer_kb = 0.01
    )

  invisible(response)
}


retrieve_azure_token <- function() {
  rlang::check_installed("AzureRMR")

  token <-
    try(
      AzureRMR::get_azure_login(
        tenant = Sys.getenv("AZURE_OPENAI_TENANT_ID"),
        app = Sys.getenv("AZURE_OPENAI_CLIENT_ID"),
        scopes = ".default",
        auth_type = "client_credentials"
      )
    )

  if (inherits(token, "try-error")) {
    token <- AzureRMR::create_azure_login(
      tenant = Sys.getenv("AZURE_OPENAI_TENANT_ID"),
      app = Sys.getenv("AZURE_OPENAI_CLIENT_ID"),
      password = Sys.getenv("AZURE_OPENAI_CLIENT_SECRET"),
      host = "https://cognitiveservices.azure.com/",
      scopes = ".default"
    )
  }
  invisible(token$token$credentials$access_token)
}
