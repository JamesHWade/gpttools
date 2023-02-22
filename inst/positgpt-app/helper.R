js <- '
        $(document).keyup(function(event) {
  if ($("#chat_input").is(":focus") && (event.keyCode == 13)) {
      $("#chat").click();
  }
});
'
make_chat_history <- function(history, new_prompt, new_response) {
  new_response <-
    list(
      strong("Question"),
      markdown(new_prompt),
      strong("Response"),
      markdown(new_response)
    )
  if (is_null(history)) {
    new_response
  } else {
    c(history, new_response)
  }
}

load_llama_index <- function(domain) {
  check_python_configuration()
  index <- switch(domain,
    "Posit Docs" = "docs.posit.co",
    "Posit Solutiosn" = "solutions.posit.co",
    "Quarto" = "quarto.org"
  )
  llama <- reticulate::import("llama_index")
  gpt_simple_vector_index <- llama$GPTSimpleVectorIndex
  gpt_simple_vector_index$load_from_disk(glue::glue("{index}.json"))
}

query_llama_index <- function(index, query) {
  check_python_configuration()
  index$query(query)
}

check_python_configuration <- function() {
  rlang::check_installed("reticulate")
  modules <- c("llama_index")
  purrr::walk(modules, \(mod) {
    if (!reticulate::py_module_available(mod)) {
      cli::cli_warn(
        c(
          "!" = "Python module {mod} is required but not found.",
          "i" = "See reticulate documentation for installation help:
          {.url https://rstudio.github.io/reticulate/}."
        )
      )
    }
  })
}

chat_card <- card(
  # height = "220px",
  card_header("Write Prompt", class = "bg-primary"),
  card_body(
    textAreaInput(
      inputId = "chat_input", label = NULL,
      value = "", resize = "vertical",
      rows = 3, width = "100%"
    ),
    actionButton(
      width = "100%",
      inputId = "chat", label = "Chat",
      icon = icon("robot"), class = "btn-primary"
    ),
    hr(),
    selectInput(
      "source", "Data Source",
      choices = c("Posit Docs", "Posit Solutions", "Quarto"),
      width = "90%"
    )
  )
)
