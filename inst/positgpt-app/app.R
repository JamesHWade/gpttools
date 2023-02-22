library(reticulate)
library(shiny)
library(bslib)
library(cli)
library(glue)
library(rlang)
library(gpttools)
source("helper.R")

ui <- fluidPage(
  theme = bs_theme(bootswatch = "litera", version = 5),
  title = "PositGPT",
  tags$script(HTML(js)),
  br(),
  layout_column_wrap(
    width = 1 / 2,
    fill = TRUE,
    chat_card,
    uiOutput("all_chats_box")
  )
)

server <- function(input, output, session) {
  r <- reactiveValues()
  r$all_chats <- ""
  r$all_chats_formatted <- NULL

  index <- reactive(load_llama_index(input$source))
  observe({
    cli_inform(c("i" = "Querying OpenAI's API..."))
    new_prompt <- input$chat_input
    prompt <- glue(r$all_chats, new_prompt, .sep = " ")
    cli_rule("Prompt")
    cat_print(prompt)
    interim <- query_llama_index(index(), prompt)
    cli_inform(c("i" = "Response received."))
    new_response <- interim$response
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
