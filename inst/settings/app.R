library(shiny)
library(bslib)
library(bsicons)
library(gpttools)
library(gptstudio)

api_services <-
  utils::methods("gptstudio_request_perform") |>
  stringr::str_remove(
    pattern = "gptstudio_request_perform.gptstudio_request_"
  ) |>
  discard(~ .x == "gptstudio_request_perform.default")

# Define UI
ui <- page_fillable(
  theme = bs_theme(bootswatch = "litera"),
  card(
    card_header(
      bs_icon("gear", class = "ms-auto"),
      "`gpttools` Settings",
      bs_icon("gear", class = "ms-auto")
    ),
    layout_column_wrap(
      card(
        card_header("Data & Task", bs_icon("robot", class = "ms-auto")),
        selectInput(
          "source", "Data Source",
          choices = NULL,
          multiple = TRUE
        ),
        selectInput(
          "task", "Task",
          choices = c("Context Only", "Permissive Chat"),
          selected = getOption("gpttools.task", "Permissive Chat")
        ),
        radioButtons(
          "test_code", "Test Code",
          choiceNames = c("Yes", "No"),
          choiceValues = c(TRUE, FALSE),
          inline = TRUE,
          selected = getOption("gpttools.test_code", FALSE)
        )
      ),
      card(
        card_header("Service & Model", bs_icon("sliders", class = "ms-auto")),
        selectInput(
          "service", "AI Service",
          choices = api_services,
          selected = getOption("gpttools.service", "openai")
        ),
        selectInput("model", "Model",
          choices = NULL
        ),
        radioButtons(
          "stream", "Stream",
          choiceNames = c("Yes", "No"),
          choiceValues = c(TRUE, FALSE),
          selected = TRUE,
          inline = TRUE
        ),
        radioButtons(
          "local", "Local Embeddings",
          choiceNames = c("Yes", "No"),
          choiceValues = c(TRUE, FALSE),
          selected = getOption("gpttools.local_embed", FALSE),
          inline = TRUE
        ),
        selectInput(
          "embed_model", "OpenAI Embedding Model",
          choices = c(
            "text-embedding-3-small",
            "text-embedding-3-large",
            "text-embedding-ada-002"
          ),
          selected = getOption(
            "gpttools.openai_embed_model",
            "text-embedding-3-small"
          )
        ),
        selectInput(
          "local_embed_model", "Local Embedding Model",
          choices = c(
            "BAAI/bge-large-en-v1.5",
            "jinaai/jina-embeddings-v2-base-en",
            "BAAI/bge-small-en-v1.5"
          ),
          selected = getOption(
            "gpttools.local_embed_model",
            "BAAI/bge-small-en-v1.5"
          )
        )
      ),
      card(
        card_header("History & Context", bs_icon("book", class = "ms-auto")),
        radioButtons(
          "save_history", "Save & Use History",
          choiceNames = c("Yes", "No"),
          choiceValues = c(TRUE, FALSE),
          selected = getOption("gpttools.save_history", FALSE),
          inline = TRUE,
        ),
        radioButtons(
          "add_context", "Always Add Context",
          choices = c("sometimes", "always", "never"),
          selected = getOption("gpttools.add_context", "sometimes"),
          inline = TRUE
        ),
        sliderInput(
          "n_docs", "Docs to Include (#)",
          min = 0,
          max = 20,
          value = getOption("gpttools.k_context", 4)
        ),
        sliderInput(
          "n_history", "Chat History to Include (#)",
          min = 0, max = 20,
          value = getOption("gpttools.k_history", 4)
        )
      )
    ),
    actionButton("save_settings", "Save Settings", icon = icon("save"), class = "btn-primary")
  )
)

# Define server logic
server <- function(input, output, session) {
  observe(
    if (input$service %in% c("google", "huggingface", "azure_openai")) {
      updateRadioButtons(
        session,
        "stream",
        selected = FALSE
      )
      shinyjs::disable("stream")
    } else {
      shinyjs::enable("stream")
    }
  )

  indices <- reactive({
    req(input$local)
    if (input$local == TRUE) {
      list_index(dir = "index/local") |> tools::file_path_sans_ext()
    } else {
      list_index() |> tools::file_path_sans_ext()
    }
  })

  observe(
    updateSelectInput(session, "source",
      choices = c("All", indices()),
      selected = getOption("gpttools.sources", "All")
    )
  )

  observe(
    updateSelectInput(
      session,
      "model",
      choices = gptstudio::get_available_models(service = input$service),
      selected = getOption("gpttools.model")
    )
  )

  observe({
    modalDialog(
      "Would you like to save your settings and close the app?",
      title = "Save Settings",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK")
      ),
      size = "m",
      easyClose = FALSE,
      fade = TRUE
    ) |> showModal()
  }) |> bindEvent(input$save_settings)

  observe({
    save_user_config(
      service = input$service,
      model = input$model,
      task = input$task,
      local_embed = input$local,
      openai_embed_model = input$openai_embed_model,
      local_embed_model = input$local_embed_model,
      k_context = input$n_docs,
      k_history = input$n_history,
      save_history = input$save_history,
      sources = input$source,
      run_code = input$test_code,
      add_context = input$add_context,
      persist = TRUE
    )
    removeModal()
    shiny::stopApp()
  }) |> bindEvent(input$ok)
}

# Run the app
shinyApp(ui, server)
