library(httr2)
library(shiny)
library(bslib)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  shinyjs::useShinyjs(),
  textInput("message", label = "Ask away", placeholder = "Simple ggplot2, be brief."),
  actionButton("send", "Send"),
  # div(id = "response"),
  bslib::card(
    bslib::card_header("Response"),
    textOutput(outputId = "response"),
    h3("some text")
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(response = NULL)
  rv$response <- NULL
  observeEvent(input$send, {
    req(input$message)
    shinyjs::show("response")
    stream_chat_openai(
      prompt = input$message,
      element_callback = create_handler("openai",
                                        r = rv,
                                        output_id = "response",
                                        where = "shiny")
    )
    shinyjs::hide("response")
  })

  output$response2 <- renderText({
    rv$response
  })
}

shinyApp(ui, server)
