library(shiny)
library(shinyjs)
library(bslib)
library(cli)
library(glue)
library(gpttools)
source("helper.R")

ui <- fluidPage(
  theme = bs_theme(bootswatch = "morph", version = 5),
  title = "PositGPT",
  tags$script(HTML(js)),
  br(),
  useShinyjs(),
  div(
    id = "hide_if_api_valid",
    card(
      card_header("OpenAI API Key", class = "bg-primary"),
      card_body(
        fluidRow(
          textInput("api_key", "Enter your OpenAI API key:",
            placeholder = "API Key", width = "100%"
          ),
          actionButton("submit", "Submit",
            icon = icon("check"),
            width = "100%", class = "btn-primary"
          )
        ),
      )
    )
  ),
  layout_column_wrap(
    width = 1 / 2,
    fill = TRUE,
    div(
      id = "chat_box",
      chat_card
    ),
    uiOutput("all_chats_box")
  )
)

server <- function(input, output, session) {
  r <- reactiveValues()
  r$all_chats <- ""
  r$all_chats_formatted <- NULL

  observe({
    if (check_api_connection(Sys.getenv("OPENAI_API_KEY"),
      update_api = FALSE
    )) {
      shinyjs::hide("hide_if_api_valid")
      shinyjs::enable("chat_box")
    } else {
      shinyjs::disable("chat_box")
      shinyWidgets::sendSweetAlert(
        title = "Invalid Key",
        text  = "API Key was not validated. Please enter a valid key."
      )
    }
  })

  observeEvent(input$submit, {
    Sys.setenv(OPENAI_API_KEY = input$api_key)
    if (check_api_connection(Sys.getenv("OPENAI_API_KEY"),
      update_api = FALSE
    )) {
      shinyjs::hide("hide_if_api_valid")
      shinyWidgets::sendSweetAlert(
        title = "Valid Key",
        text  = "API Key is valid. Have fun!",
      )
      shinyjs::enable("chat_box")
    } else {
      shinyWidgets::sendSweetAlert(
        title = "Invalid Key",
        text  = "API Key was not validated. Please enter a valid key."
      )
    }
  })

  index <- reactive(load_index(input$source))
  observe({
    cli_inform(c("i" = "Querying OpenAI's API..."))
    new_prompt <- input$chat_input
    prompt <- glue(r$all_chats, new_prompt, .sep = " ")
    cli_rule("Prompt")
    cat_print(prompt)
    interim <- query_index(index(), query = prompt, task = input$task)
    cli_inform(c("i" = "Response received."))
    new_response <- interim[[3]]$choices$text
    cli_rule("Response")
    cat_print(new_response)
    r$all_chats <- glue(r$all_chats, new_prompt, new_response)
    cat_print(r$all_chats)
    r$all_chats_formatted <-
      make_chat_history(r$all_chats_formatted, input$chat_input, new_response)
    output$all_chats_box <- renderUI(
      card(
        card_header("Chat History", class = "bg-primary"),
        card_body(
          fill = TRUE,
          r$all_chats_formatted
        )
      )
    )
    updateTextAreaInput(session, "chat_input", value = "")
  }) |>
    bindEvent(input$chat)

  observeEvent(input$cancel, stopApp())
}

shinyApp(ui, server)
