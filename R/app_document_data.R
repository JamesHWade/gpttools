#' Document Data
#'
#' This function runs a Shiny application to view and document data.
#'
#' @export
document_data <- function() {
  check_api()
  withr::local_options(shiny.launch.browser = .rs.invokeShinyPaneViewer)
  shiny::runApp(system.file(package = "gpttools", "document_data"))
}

#' Shiny app that supports Document Data addin
#'
#' @return Nothing is returned, a shiny app is run
#' @export
run_document_data <- function() {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Document your data with gpttools"),
    miniUI::miniContentPanel(
      shiny::fillCol(
        flex = c(1, 1),
        shiny::fillRow(
          flex = c(1, 1),
          shiny::fillCol(
            flex = c(1, 1, 2, 2, 2),
            shiny::fillRow(
              shiny::selectInput(
                inputId = "dataframes",
                label   = "What data do you want to document?",
                choices = NULL,
                width   = "90%"
              ),
              shiny::selectInput(
                inputId = "sum_method",
                label = "What method should be used to summarize data?",
                choices = c("skimr", "skimr_lite", "column_types", "summary"),
                width = "90%"
              )
            ),
            shiny::helpText(
              "Only dataframes in the global environment are shown.\n
                   Summary methods may produce different skeletons."
            ),
            shiny::fillRow(
              shiny::sliderInput(
                inputId = "temperature",
                label = "Model temperature",
                min = 0,
                max = 1,
                value = .7,
                width = "90%"
              ),
              shiny::sliderInput(
                inputId = "max_tokens",
                label = "Maximum number of tokens to spend.",
                min = 12,
                max = 1000,
                value = 100,
                width = "90%"
              )
            ),
            shiny::helpText(
              "Temperature is a parameter for controlling the randomness of
                   the GPT model's output. Tokens refers to the cost of a model
                   query. One token refers to about 4 letters. If your reponse
                   is cutoff, you can increase the number of tokens (at increase
                   cost!)."
            ),
            shiny::fillRow(
              shiny::actionButton(
                inputId = "update_prompt",
                label = "Update Prompt",
                icon = shiny::icon("rotate-right"),
                width = "90%"
              ),
              shiny::actionButton(
                inputId = "query_gpt",
                label = "Document Data",
                icon = shiny::icon("wand-magic-sparkles"),
                width = "90%"
              )
            )
          ),
          shiny::column(
            width = 12,
            shiny::textAreaInput(
              inputId = "prompt",
              label = "Prompt for the model to use to document your data",
              value = "",
              rows = 10,
              width = "100%",
              height = "400"
            ),
            shiny::h3("Model Response"),
            shiny::verbatimTextOutput(
              outputId = "response",
              placeholder = TRUE
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    dataframes <- shiny::reactive(collect_dataframes())
    prepped_prompt <- shiny::reactive({
      shiny::req(nchar(input$dataframes) > 0)
      prep_data_prompt(
        get(rlang::sym(input$dataframes)),
        method = input$sum_method,
        prompt = "Create a roxygen skeleton to document this data. Include at
      least `title`, `format`, and `describe` fields. Provide range, number of
      missing values, and type for each column in the data frame. Follow
      roxygen2 conventions:\n\n"
      )
    })
    shiny::observe(
      shiny::updateSelectInput(
        session = session,
        inputId = "dataframes",
        choices = dataframes()
      )
    )
    shiny::observeEvent(input$update_prompt, {
      cli::cli_alert_info("Updating prompt")
      shiny::updateTextAreaInput(
        session = session,
        inputId = "prompt",
        value = prepped_prompt()
      )
    })
    shiny::observeEvent(input$query_gpt, {
      cli::cli_alert_info("Querying GPT")
      interim <- openai_create_completion(
        model = "text-davinci-003",
        prompt = input$prompt,
        temperature = input$temperature,
        max_tokens = input$max_tokens,
        openai_api_key = Sys.getenv("OPENAI_API_KEY"),
        openai_organization = NULL
      )
      cli::cli_alert_info("Query complete. Providing output text.")
      output$response <- shiny::renderText(interim$choices[1, 1])
    })

    shiny::observeEvent(input$cancel, shiny::stopApp())
  }

  shiny::shinyApp(ui, server)
}
