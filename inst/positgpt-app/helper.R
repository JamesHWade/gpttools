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
  if (rlang::is_null(history)) {
    new_response
  } else {
    c(history, new_response)
  }
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
        choices = c("Posit Docs"      = "docs.posit.co",
                    "Posit Solutions" = "solutions.posit.co",
                    "Quarto"          = "quarto.org",
                    "R4DS"            = "r4ds.hadley.nz"),
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
        min = 0, max = 40, value = 4, width = "90%"
      )
    )
  )
)
