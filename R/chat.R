#' Chat for gpttools
#'
#' This function provides a high-level interface for communicating with various
#' services and models supported by gpttools. It orchestrates the creation,
#' configuration, and execution of a request based on user inputs and options
#' set for gpttools The function supports a range of tasks from text
#' generation to code synthesis and can be customized according to skill level
#' and coding style preferences.
#'
#' @param prompt A string containing the initial prompt or question to be sent
#'   to the model. This is a required parameter.
#' @param service The AI service to be used for the request. If not explicitly
#'   provided, this defaults to the value set in
#'   `getOption("gptstudio.service")`. If the option is not set, make sure to
#'   provide this parameter to avoid errors.
#' @param history An optional parameter that can be used to include previous
#'   interactions or context for the current session. Defaults to a system
#'   message indicating "You are an R chat assistant".
#' @param stream A logical value indicating whether the interaction should be
#'   treated as a stream for continuous interactions. If not explicitly
#'   provided, this defaults to the value set in
#'   `getOption("gptstudio.stream")`.
#' @param model The specific model to use for the request. If not explicitly
#'   provided, this defaults to the value set in `getOption("gptstudio.model")`.
#' @param skill A character string indicating the skill or capability level of
#'   the user. This parameter allows for customizing the behavior of the model
#'   to the user. If not explicitly provided, this defaults to the value set in
#'   `getOption("gptstudio.skill")`.
#' @param style The coding style preferred by the user for code generation
#'   tasks. This parameter is particularly useful when the task involves
#'   generating code snippets or scripts. If not explicitly provided, this
#'   defaults to the value set in `getOption("gptstudio.code_style")`.
#' @param task The specific type of task to be performed, ranging from text
#'   generation to code synthesis, depending on the capabilities of the model.
#'   If not explicitly provided, this defaults to the value set in
#'   `getOption("gptstudio.task")`.
#' @param custom_prompt An optional parameter that provides a way to extend or
#'   customize the initial prompt with additional instructions or context.
#' @param process_response A logical indicating whether to process the model's
#'   response. If `TRUE`, the response will be passed to
#'   `gptstudio_response_process()` for further processing. Defaults to `FALSE`.
#'   Refer to `gptstudio_response_process()` for more details.,
#' @param where A character string indicating the location or environment where
#'   the chat is taking place. Options are `c("console", "source", and "shiny")`. The
#'   default is `""`, which means the chat is taking place in the R console.
#' @param ... Reserved for future use.
#'
#' @return Depending on the task and processing, the function returns the
#'   response from the model, which could be text, code, or any other structured
#'   output defined by the task and model capabilities. The precise format and
#'   content of the output depend on the specified options and the capabilities
#'   of the selected model.
#'
#' @examples
#' \dontrun{
#' # Basic usage with a text prompt:
#' result <- chat("What is the weather like today?")
#'
#' # Advanced usage with custom settings, assuming appropriate global options are set:
#' result <- chat(
#'   prompt = "Write a simple function in R",
#'   skill = "advanced",
#'   style = "tidyverse",
#'   task = "coding"
#' )
#'
#' # Usage with explicit service and model specification:
#' result <- chat(
#'   prompt = "Explain the concept of tidy data in R",
#'   service = "openai",
#'   model = "gpt-4-turbo-preview",
#'   skill = "intermediate",
#'   task = "general"
#' )
#' }
#'
#' @export
chat <- function(prompt,
                 service = getOption("gpttools.service"),
                 history = list(list(role = "system", content = "You are an R chat assistant")),
                 stream = getOption("gpttools.stream", TRUE),
                 model = getOption("gpttools.model"),
                 skill = getOption("gpttools.skill", NULL),
                 style = getOption("gpttools.code_style", "no preference"),
                 task = NULL,
                 custom_prompt = NULL,
                 process_response = FALSE,
                 where = "console",
                 ...) {
  if (rlang::is_false(stream) || service %in% c("google", "huggingface")) {
    response <-
      gptstudio::gptstudio_create_skeleton(
        service = service,
        prompt = prompt,
        history = history,
        stream = stream,
        model = model,
        ...
      ) |>
      gptstudio::gptstudio_skeleton_build(
        skill = skill,
        style = style,
        task = task,
        custom_prompt = custom_prompt
      ) |>
      gptstudio::gptstudio_request_perform()
    if (process_response) {
      response |> gptstudio::gptstudio_response_process()
    } else {
      response$response
    }
  } else {
    stream_chat(
      prompt = prompt,
      service = service,
      r = NULL,
      output_id = NULL,
      where = where
    )
  }
}

#' Ghost Chat
#'
#' @inheritParams chat
#' @export
ghost_chat <- function(service = getOption("gpttools.service", "openai"),
                       stream = TRUE,
                       where = "source") {
  context <- get_cursor_context()

  instructions <- glue::glue(
    "You are an expert coding assistant that provides brief code suggestions
    directly into the files as code. Your response will go directly into an
    .{context$file_ext} file. You response should only contain code or code
    comments. Do not add freetext.
    You are given context above and below the current cursor position.

    Here is an example:

    library(tidyverse)

    p1 <-
      ggplot(mtcars, aes(x = mpg, y = wt)) +
      geom_point() +
      geom_smooth(method = 'lm') +
      labs(title = 'MPG vs. Weight', x = 'Miles per Gallon', y = 'Weight') +
      [[start here]]

    ggsave(\"myplot.png\", p1)

    Your reponse begins at the placeholder [[start_here]].

    Here is the context:

    {context$above}
    {context$below}"
  )
  stream_chat(
    prompt    = instructions,
    service   = service,
    r         = NULL,
    output_id = NULL,
    where     = where
  )
}

get_cursor_context <- function(context_lines = 20,
                               placeholder = "[[start_here]]") {
  doc <- rstudioapi::getSourceEditorContext()
  cursor_line <- doc$selection[[1]]$range$start[1]
  cursor_pos <- doc$selection[[1]]$range$end
  start_line <- max(1, cursor_line - context_lines)
  end_line <- min(length(doc$content), cursor_line + context_lines)

  original_str <- doc$contents[cursor_line]
  doc$contents[cursor_line] <-
    stringr::str_c(
      stringr::str_sub(original_str, end = cursor_pos[2] - 1),
      placeholder,
      stringr::str_sub(original_str, start = cursor_pos[2])
    )

  context_above <- if (start_line < cursor_line) {
    doc$content[(start_line):(cursor_line)] |>
      paste0(collapse = "\n")
  } else {
    character(0)
  }

  context_below <- if (end_line > cursor_line) {
    doc$content[(cursor_line + 1):end_line] |>
      paste0(collapse = "\n")
  } else {
    ""
  }

  if (doc$path == "") {
    file_ext <- "R"
  } else {
    file_ext <- doc$path |> tools::file_ext()
  }
  list(
    above    = context_above,
    below    = context_below,
    cursor   = cursor_pos,
    file_ext = file_ext
  )
}
