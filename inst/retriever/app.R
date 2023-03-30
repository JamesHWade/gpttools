rlang::check_installed(
  c("shiny", "cli", "glue", "gptstudio", "gpttools", "waiter")
)
library(waiter)
library(gpttools)

rlang::check_installed("bslib", version = "0.4.2")
gptstudio::check_api()
indices <- gpttools::list_index() |> tools::file_path_sans_ext()

ui <- fluidPage(
  useWaiter(),
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
              choices = c("All", indices),
              width = "33%"
            ),
            shiny::selectInput(
              "task", "Task",
              choices = c(
                "Context Only", "Permissive Chat"
              ),
              selected = "Permissive Chat",
              width = "33%"
            ),
            shiny::radioButtons(
              "save_history", "Save & Use History",
              choiceNames = c("Yes", "No"),
              choiceValues = c(TRUE, FALSE),
              selected = TRUE,
              width = "33%"
            ),
            shiny::sliderInput(
              "n_docs", "Documents to Include (#)",
              min = 0, max = 20, value = 3, width = "50%"
            ),
            shiny::sliderInput(
              "n_history", "Chat History to Include (#)",
              min = 0, max = 20, value = 3, width = "50%"
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

  index <- shiny::reactive(gpttools::load_index(input$source))
  shiny::observe({
    waiter::waiter_show(
      html = shiny::tagList(spin_flower(), shiny::h3("Asking ChatGPT...")),
      color = waiter::transparent(0.5)
    )
    cli::cli_inform("here")
    interim <- chat_with_context(
      query = input$chat_input,
      index = index(),
      add_context = TRUE,
      chat_history = read_history(),
      session_history = r$all_chats,
      add_history = input$save_history,
      task = input$task,
      k_context = input$n_docs,
      k_history = input$n_history,
      save_history = input$save_history,
      overwrite = FALSE
    )
    cli::cli_inform("there")
    new_response <- interim[[3]]$choices
    cli::cli_inform("still going")
    r$context_links <- c(r$context_links, interim[[2]]$link)
    cli::cli_inform("down here")
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
    cli::cli_inform("did formatting mess up?")
    r$all_chats_formatted <- gptstudio::make_chat_history(r$all_chats)
    waiter::waiter_hide()
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
