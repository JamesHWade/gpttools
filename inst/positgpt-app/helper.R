js <- '
        $(document).keyup(function(event) {
  if ($("#chat_input").is(":focus") && (event.keyCode == 13)) {
      $("#chat").click();
  }
});
'
make_chat_history <- function(history) {
  cli_inform("Making history...")
  history <-
    purrr::map(history, \(x) if (x$role == "system") NULL else x) |>
    purrr::compact()

  purrr::map(history, \(x) {
    list(
      strong(toupper(x$role)),
      markdown(x$content)
    )
  })
}

chat_card <- card(
  height = "600px",
  card_header("Write Prompt", class = "bg-primary"),
  card_body(
    fill = TRUE,
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
    fluidRow(
      selectInput(
        "source", "Data Source",
        choices = c(
          "Posit Docs" = "docs.posit.co",
          "Posit Solutions" = "solutions.posit.co",
          "Quarto" = "quarto.org",
          "R4DS" = "r4ds.hadley.nz"
        ),
        width = "50%"
      ),
      selectInput(
        "task", "Task",
        choices = c(
          "conservative q&a", "permissive q&a",
          "paragraph about a question", "bullet points",
          "summarize problems given a topic",
          "extract key libraries and tools",
          "simple instructions", "summarize"
        ), width = "50%"
      ),
      sliderInput(
        "n_docs", "Number of Documents to Include",
        min = 0, max = 20, value = 3, width = "90%"
      )
    )
  )
)
