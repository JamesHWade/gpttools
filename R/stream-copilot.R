get_copilot_oauth <- function(dir = "~/.config/github-copilot") {
  hosts <- jsonlite::read_json(file.path(dir, "hosts.json"))
  hosts[[1]]$oauth_token
}

chat_copilot <- function() {
  token <- request("https://api.github.com/copilot_internal/v2/token") |>
    req_auth_bearer_token(oauth_token) |>
    req_perform() |>
    resp_body_json() |>
    pluck("token")

  request("https://api.githubcopilot.com/chat/completions") |>
    req_auth_bearer_token(token = token) |>
    req_headers("Editor-Version" = "vscode/9.9.9") |>
    req_body_json(
      data = list(
        messages = list(
          list(
            role = "user",
            content = "Tell me a joke about the R language."
          )
        ),
        model = "copilot"
      )
    ) |>
    req_perform() |>
    resp_chat_openai(stream = FALSE)
}
