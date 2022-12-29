library(gpttools)
ui <- miniUI::miniPage(

  miniUI::gadgetTitleBar("Document your data with gpttools"),
  miniUI::miniContentPanel(
    shiny::fillCol(
      flex = c(3,1),
      shiny::fillRow(
        flex = c(1,1),
        shiny::column(
          12,
          shiny::selectInput(
            inputId = "dataframes",
            label   = "What data do you want to document?",
            choices = NULL,
            width   = "90%"),
          helpText("Only dataframes in the global environment are shown."),
          shiny::selectInput(
            inputId = "sum_method",
            label = "What method should be used to summarize data?",
            choices = c("skimr", "glimpse", "summary"),
            width = "90%"
          ),
          helpText("Different summary methods may produce different skeletons."),
          shiny::sliderInput(
            inputId = "temperature",
            label = "Model temperature",
            min = 0,
            max = 1,
            value = .7,
            width="90%"
          ),
          helpText("A parameter for controlling the randomness of the GPT model's output."),
          shiny::sliderInput(
            inputId = "max_tokens",
            label = "Maximum number of tokens to spend.",
            min = 12,
            max = 1000,
            value = 100,
            width="90%"
          ),
          helpText("If your reponse is cutoff, you can increase the number of tokens (at increase cost!).")
        ),
        column(
          width = 12,
          shiny::textAreaInput(inputId="prompt",
                               label="Prompt for the model to use to document your data",
                               value="",
                               rows = 5,
                               width="90%",
                               height = "400"),
          shiny::actionButton(inputId = "update_prompt",
                              label = "Update Prompt",
                              icon = icon("rotate-right")),
          shiny::actionButton(inputId = "query_gpt",
                              label = "Document Data",
                              icon = icon("wand-magic-sparkles"))
        )),
      shiny::verbatimTextOutput(outputId = "response",
                                placeholder = T)
    )

  ))

server <- function(input, output, session) {
  dataframes <- reactive(collect_dataframes())
  prepped_prompt <- reactive({
    req(stringr::str_length(input$dataframes) > 0)
    prep_data_prompt(
      get(rlang::sym(input$dataframes)),
      method = input$sum_method,
      prompt = "Create a roxygen skeleton to document this data. Include at least `title`, `format`, and `describe` fields. Provide range, number of missing values, and type for each column in the data frame. Follow roxygen2 conventions:\n\n")
  })
  observe(
    updateSelectInput(session = session,
                      inputId = "dataframes",
                      choices = dataframes()))
  shiny::observeEvent(input$update_prompt, {
    cli::cli_alert_info("Updating prompt")
    updateTextAreaInput(
      session = session,
      inputId = "prompt",
      value = prepped_prompt())
  })
  shiny::observeEvent(input$query_gpt,{
    cli::cli_alert_info("Querying GPT")
    interim <- gpttools:::openai_create_completion(
      model = "text-davinci-003",
      prompt = input$prompt,
      temperature = input$temperature,
      max_tokens = input$max_tokens,
      openai_api_key = Sys.getenv("OPENAI_API_KEY"),
      openai_organization = NULL
    )
    cli::cli_alert_info("Query complete. Providing output text.")

    interim$choices

    output$response <- shiny::renderText(interim$choices[1,1])

  })

  shiny::observeEvent(input$cancel, stopApp())
}

shiny::shinyApp(ui, server)
