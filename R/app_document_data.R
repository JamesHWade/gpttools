#' Document Data
#'
#' This function runs a Shiny application to view and document data.
#'
#' @export
document_data <- function() {
  gptstudio::check_api()
  withr::local_options(shiny.launch.browser = ".rs.invokeShinyPaneViewer")
  run_document_data()
}

#' Shiny app that supports Document Data addin
#'
#' @return Nothing is returned, a shiny app is run
#' @export
run_document_data <- function() {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(
      title = "Document your data with gpttools",
      left  = NULL,
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      shiny::fillCol(
        shiny::fillRow(
          shiny::fillCol(
            flex = c(1, 1, 1.5, 2, 1),
            shiny::fillRow(
              shiny::selectInput(
                inputId = "dataframes",
                label   = "Select data",
                choices = NULL,
                width   = "90%"
              ),
              shiny::selectInput(
                inputId = "sum_method",
                label = "Select summary method",
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
                label = "Maximum tokens",
                min = 12,
                max = 1000,
                value = 100,
                width = "90%"
              )
            ),
            shiny::helpText(
              "Temperature is a parameter for controlling the randomness of
              the GPT model's output. Tokens refers to the cost of a model
              query. One token refers to about 4 letters. If your reponse is
              cutoff, you can increase the number of tokens (at increase
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
              width = "100%"
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
        get(sym(input$dataframes)),
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
    shiny::observe({
      cli_inform("Updating prompt")
      shiny::updateTextAreaInput(
        session = session,
        inputId = "prompt",
        value = prepped_prompt()
      )
    }) |>
      shiny::bindEvent(input$update_prompt)

    shiny::observe({
      cli_inform(c("i" = "Querying OpenAI's API..."))

      interim <- openai_create_completion(
        model = "text-davinci-003",
        prompt = input$prompt,
        temperature = input$temperature,
        max_tokens = input$max_tokens,
        openai_api_key = Sys.getenv("OPENAI_API_KEY")
      )

      cli_inform(c("i" = "Response received. Providng output text."))
      output$response <- shiny::renderText(interim$choices[1, 1])
    }) |>
      shiny::bindEvent(input$query_gpt)

    shiny::observe(shiny::stopApp()) |> shiny::bindEvent(input$done)
  }

  shiny::shinyApp(ui, server)
}

run_document_data()
