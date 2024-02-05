stream_chat_openai <- function(prompt = NULL,
                               element_callback = create_stream_handler_openai(),
                               model = "gpt-4-turbo-preview",
                               openai_api_key = Sys.getenv("OPENAI_API_KEY"),
                               shiny = FALSE) {
  messages <- list(
    list(
      role = "user",
      content = prompt
    )
  )

  # Set the request body
  body <- list(
    model = model,
    stream = TRUE,
    messages = messages
  )

  response <-
    httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_auth_bearer_token(token = openai_api_key) |>
    httr2::req_body_json(data = body) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform_stream(
      callback = element_callback,
      buffer_kb = 0.01
    )

  invisible(response)
}


create_stream_handler_openai <- function() {
  # Create an environment to hold the state
  env <- rlang::env()
  env$resp <- NULL

  function(x) {
    pattern <- '\\{"id":.*?\\}\\]\\}'
    x <- rawToChar(x)
    if (rlang::is_null(env$resp)) {
      env$resp <- x
    } else {
      env$resp <- paste0(env$resp, x)
    }

    if (stringr::str_detect(env$resp, pattern)) {
      parsed <- stringr::str_extract(env$resp, pattern) |>
        jsonlite::fromJSON() |>
        purrr::pluck("choices") |>
        purrr::pluck("delta") |>
        purrr::pluck("content")

      cat(parsed)

      # Reset resp after processing
      env$resp <- stringr::str_split(env$resp, pattern)

      env$resp <- env$resp[[1]][[length(env$resp[[1]])]]
    }
    TRUE
  }
}

# create_handler_for_shiny <- function(r, output_id = "streaming") {
#   env <- rlang::env()
#   env$resp <- NULL
#   env$full_resp <- NULL
#
#   function(x) {
#     pattern <- '\\{"id":.*?\\}\\]\\}'
#     x <- rawToChar(x)
#
#     if (rlang::is_null(env$resp)) {
#       env$resp <- x
#     } else {
#       env$resp <- paste0(env$resp, x)
#     }
#
#     if (stringr::str_detect(env$resp, pattern)) {
#       parsed <- stringr::str_extract(env$resp, pattern) |>
#         jsonlite::fromJSON() |>
#         purrr::pluck("choices", "delta", "content")
#
#       env$full_resp <- paste0(env$full_resp, parsed)
#
#       # Use shinyjs to update a div with the response
#       shinyjs::html(output_id,
#                     shiny::markdown(paste("**Assistant**", env$full_resp, sep = "\n\n")))
#       r$response <- env$full_resp
#
#       env$resp <- stringr::str_split(env$resp, pattern)
#       env$resp <- env$resp[[1]][[length(env$resp[[1]])]]
#     }
#     TRUE
#   }
# }


# response <-
#   stream_chat_completion(
#     message = "Tell me a story about a magical backpack.",
#     element_callback = function(content) cat(content, "\n")
#   )
#
# result <-
#   callr::r(\() {
#     stream_chat_completion(
#       message = "Tell me a story about a magical backpack.",
#       element_callback = function(content) cat(content, "\n")
#     )
#   })
#
# Define a wrapper function to call in the subprocess
# subprocess_function <- function(stream_chat_completion, create_stream_handler, message, element_callback) {
#   environment(stream_chat_completion) <- environment()
#   environment(create_stream_handler) <- environment()
#   stream_chat_completion(
#     message = message,
#     element_callback = element_callback
#   )
# }
#
# # Call the subprocess with the wrapper function, passing the necessary objects and arguments
# result <- callr::r_bg(
#   func = subprocess_function,
#   args = list(
#     stream_chat_completion = stream_chat_completion,
#     create_stream_handler = create_stream_handler,
#     message = "Tell me a story about a magical backpack.",
#     element_callback = function(content) cat(content, "\n")
#   )
# )
#
# output <- NULL
# while (rlang::is_true(result$is_alive())) {
#   cat(result$read_output())
#   output <- paste0(output, result$read_output())
#   Sys.sleep(0.1)
# }
