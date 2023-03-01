library(shiny)
library(shinyjs)
library(bslib)
library(cli)
library(glue)
library(gpttools)
source("helper.R")

api_start <- check_api()

ui <- fluidPage(
  theme = bs_theme(bootswatch = "morph", version = 5),
  title = "PositGPT",
  tags$script(HTML(js)),
  br(),
  useShinyjs(),
  shinyjs::hidden(
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
          )
        )
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
    if (rlang::is_true(api_start)) {
      shinyjs::hide("hide_if_api_valid")
      shinyjs::enable("chat_box")
    } else {
      shinyjs::disable("chat_box")
      shinyjs::show("hide_if_api_valid")
    }
  })

  observeEvent(input$submit, {
    Sys.setenv(OPENAI_API_KEY = input$api_key)
    api_check <- check_api_connection(Sys.getenv("OPENAI_API_KEY"),
      update_api = FALSE
    )
    if (rlang::is_true(api_check)) {
      cli::cli_inform("API checked and validated.")
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
    interim <- query_index(index(),
      query = prompt,
      task = input$task,
      k = input$n_docs
    )
    cli_inform(c("i" = "Response received."))
    new_response <- interim[[3]]$choices$text
    r$context_links <- c(r$context_links, interim[[2]]$link)
    cli_rule("Response")
    r$all_chats <- glue(r$all_chats, new_prompt, new_response)
    print(r$all_chats)
    cat_print(r$all_chats)
    r$all_chats_formatted <-
      make_chat_history(r$all_chats_formatted, input$chat_input, new_response)
    updateTextAreaInput(session, "chat_input", value = "")
  }) |>
    bindEvent(input$chat)

  output$all_chats_box <- renderUI({
    req(length(r$context_links) > 0)
    card(
      card_header("Chat History", class = "bg-primary"),
      card_body(
        fill = TRUE,
        r$all_chats_formatted,
        markdown("**Sources**"),
        markdown(paste0("* ", unique(r$context_links), collapse = "\n"))
      )
    )
  })
  observeEvent(input$cancel, stopApp())
}

shinyApp(ui, server)
