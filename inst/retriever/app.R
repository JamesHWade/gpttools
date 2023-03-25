rlang::check_installed(
  c("shiny", "cli", "glue", "gptstudio")
)

rlang::check_installed("bslib", version = "0.4.2")
gptstudio::check_api()
indices <- gpttools:::list_index() |> tools::file_path_sans_ext()

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "morph", version = 5),
  title = "Retreiver from gpttools",
  shiny::br(),
  bslib::layout_column_wrap(
    width = 1 / 2,
    fill = FALSE,
    height = "1200px",
    heights_equal = "row",
    shiny::div(
      id = "chat_box",
      bslib::card(
        height = "600px",
        bslib::card_header("Write Prompt", class = "bg-primary"),
        bslib::card_body(
          fill = TRUE,
          shiny::textAreaInput(
            inputId = "chat_input", label = NULL,
            value = "", resize = "vertical",
            rows = 3, width = "100%"
          ),
          shiny::actionButton(
            width = "100%",
            inputId = "chat", label = "Chat",
            icon = shiny::icon("robot"), class = "btn-primary"
          ),
          shiny::hr(),
          shiny::fluidRow(
            shiny::selectInput(
              "source", "Data Source",
              choices = indices,
              width = "100%"
            ),
            shiny::selectInput(
              "task", "Task",
              choices = c(
                "Context Only", "Permissive Chat"
              ),
              width = "100%"
            ),
            shiny::sliderInput(
              "n_docs", "Number of Documents to Include",
              min = 0, max = 20, value = 3, width = "90%"
            )
          )
        )
      )
    ),
    shiny::uiOutput("all_chats_box")
  )
)

server <- function(input, output, session) {
  r <- shiny::reactiveValues()
  r$all_chats_formatted <- NULL
  r$all_chats <- NULL

  index <- shiny::reactive(load_index(input$source))
  shiny::observe({
    interim <- query_index(index(),
      query = input$chat_input,
      history = r$all_chats,
      task = input$task,
      k = input$n_docs
    )
    new_response <- interim[[3]]$choices
    r$context_links <- c(r$context_links, interim[[2]]$link)
    r$all_chats <-
      c(
        interim[[1]],
        list(
          list(
            role    = new_response$message.role,
            content = new_response$message.content
          )
        )
      )
    r$all_chats_formatted <- gptstudio::make_chat_history(r$all_chats)
    shiny::updateTextAreaInput(session, "chat_input", value = "")
  }) |>
    shiny::bindEvent(input$chat)

  output$all_chats_box <- renderUI({
    shiny::req(length(r$context_links) > 0)
    bslib::card(
      bslib::card_header("Chat History", class = "bg-primary"),
      bslib::card_body(
        fill = TRUE,
        r$all_chats_formatted,
        shiny::markdown("**Sources**"),
        shiny::markdown(paste0("* ", unique(r$context_links), collapse = "\n"))
      )
    )
  })
  shiny::observeEvent(input$cancel, shiny::stopApp())
}

shinyApp(ui, server)
