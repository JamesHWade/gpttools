rlang::check_installed(
  c(
    "shiny", "bslib", "bsicons", "cli", "glue",
    "gptstudio", "gpttools", "waiter", "reprex", "clipr"
  )
)

library(gpttools)
library(gptstudio)
library(shiny)
library(bslib)
library(bsicons)
library(waiter)
library(reprex)

window_height_ui <- function(id) {
  ns <- NS(id)
  namespaced_id <- ns("window_height")

  tags$head(tags$script(HTML(
    sprintf("
      function send_window_height() {
        var height = $(window).height();
        Shiny.setInputValue('%s', height);
      }

      $(document).on('shiny:connected', function() {
        send_window_height();
      });

      $(window).on('resize', function() {
        send_window_height();
      });
    ", namespaced_id)
  )))
}

window_height_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      input$window_height
    })
  })
}

make_chat_history <- function(chats) {
  history <-
    purrr::discard(chats, \(x) x$role == "system") |>
    purrr::map(\(x) {
      list(
        strong(stringr::str_to_title(x$role)),
        markdown(x$content)
      )
    }) |>
    purrr::list_flatten()
  history
}

api_services <-
  utils::methods("gptstudio_request_perform") |>
  stringr::str_remove(
    pattern = "gptstudio_request_perform.gptstudio_request_"
  ) |>
  purrr::discard(~ .x == "gptstudio_request_perform.default")

ui <- page_fillable(
  useWaiter(),
  shinyjs::useShinyjs(),
  window_height_ui("height"),
  theme = bs_theme(bootswatch = "litera") |>
    bs_add_rules(".scrollable-popover .popover-body
                 { max-height: 400px; overflow-y: auto; }"),
  tags$head(tags$script(HTML("
  $(document).on('keydown', '#chat_input', function(e) {
    if ((e.keyCode == 10 || e.keyCode == 13) && (!e.shiftKey)) {
      e.preventDefault();
      setTimeout(function() {
        $('#chat').click();
      }, 500);
     }
  });"))),
  title = "Retreiver from gpttools",
  card(
    card_header(
      "Chat with Retrieval",
      class = "bg-primary d-flex align-items-center",
      div(
        class = "d-flex align-items-center ms-auto",
        actionButton(
          "clear_history",
          "",
          class = "btn btn-primary",
          icon = icon("eraser")
        ),
        popover(
          id = "settings",
          options = list(customClass = "scrollable-popover"),
          bs_icon("gear", class = "ms-auto"),
          accordion_panel(
            "Data & Task",
            icon = bs_icon("robot", class = "ms-auto"),
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
            ),
          ),
          br(),
          accordion_panel(
            "Service & Model",
            icon = bs_icon("sliders", class = "ms-auto"),
            selectInput(
              "service", "AI Service",
              choices = api_services,
              selected = getOption("gpttools.service", "openai")
            ),
            selectInput("model", "Model",
              choices = NULL
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
          br(),
          accordion_panel(
            "History & Context",
            icon = bs_icon("book", class = "ms-auto"),
            radioButtons(
              "add_context", "Always Add Context",
              choices = c("sometimes", "always", "never"),
              selected = getOption("gpttools.add_context", "sometimes"),
              inline = FALSE
            ),
            radioButtons(
              "save_history", "Save & Use History",
              choiceNames = c("Yes", "No"),
              choiceValues = c(TRUE, FALSE),
              selected = getOption("gpttools.save_history", FALSE),
              inline = TRUE,
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
          ),
          br(),
          actionButton(
            "save_settings", "Save Settings",
            icon = icon("save", class = "ms-auto"),
            class = "btn-primary"
          ),
          title = "App Settings"
        )
      )
    ),
    uiOutput("all_chats_box"),
    div(
      class = "mt-auto",
      style = htmltools::css(
        "margin-left" = "20px",
        "margin-right" = "20px"
      ),
      fluidRow(
        column(
          10,
          textAreaInput(
            inputId = "chat_input", label = NULL, rows = 1,
            value = "", width = "100%", resize = "vertical"
          )
        ),
        column(
          2,
          actionButton(
            inputId = "chat",
            label = icon("fas fa-paper-plane"),
            class = "btn-primary m-1",
            width = "100%"
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  set_user_config()
  r <- reactiveValues()
  r$all_chats_formatted <- NULL
  r$all_chats <- NULL
  r$context_links <- NULL
  height <- window_height_server("height")
  transformer_model <- reactive({
    if (input$local) {
      get_transformer_model()
    } else {
      NULL
    }
  })
  index <- reactive({
    if (input$local == TRUE) {
      if ("All" %in% input$source) {
        load_index(domain = "All", local_embeddings = TRUE)
      } else {
        purrr::map(input$source, \(x) {
          load_index(x, local_embeddings = TRUE) |>
            tibble::as_tibble()
        }) |>
          dplyr::bind_rows()
      }
    } else if ("All" %in% input$source) {
      load_index(domain = "All", local_embeddings = FALSE)
    } else {
      purrr::map(input$source, \(x) {
        load_index(x, local_embeddings = FALSE) |>
          tibble::as_tibble()
      }) |>
        dplyr::bind_rows()
    }
  })

  observe(
    if (input$service %in% c("google", "huggingface")) {
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

  observe({
    r$all_chats <- NULL
    r$all_chats_formatted <- NULL
    r$context_links <- NULL
  }) |> bindEvent(input$clear_history)

  indices <- reactive({
    req(input$local)
    if (input$local == TRUE) {
      list_index(dir = "index/local") |> tools::file_path_sans_ext()
    } else {
      list_index() |> tools::file_path_sans_ext()
    }
  })
  observe(
    updateSelectInput(
      session,
      "model",
      choices = gptstudio::get_available_models(service = input$service),
      selected = getOption("gpttools.model")
    )
  )
  observe(updateSelectInput(session, "source",
    choices = c("All", indices()),
    selected = getOption("gpttools.sources", "All")
  ))
  observe({
    toggle_popover("settings", show = FALSE)
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
  }) |> bindEvent(input$ok)

  observe({
    if (rlang::is_false(input$stream) ||
      input$service %in% c("google", "azure_openai", "huggingface")) {
      waiter_show(
        html = spin_3circles(),
        color = transparent(0.5)
      )
    }
    interim <- chat_with_context(
      query = input$chat_input,
      service = input$service,
      model = input$model,
      index = index(),
      add_context = input$add_context,
      chat_history = read_history(local = input$local),
      session_history = r$all_chats,
      add_history = input$save_history,
      task = input$task,
      k_context = input$n_docs,
      k_history = input$n_history,
      save_history = input$save_history,
      overwrite = FALSE,
      local = input$local,
      embedding_model = transformer_model(),
      stream = input$stream |> as.logical(),
      rv = r
    )
    new_response <- interim[[3]]

    waiter_hide()

    if (is.character(interim[[2]])) {
      if (length(r$context_links) == 0) {
        r$context_links <- "No context used so far."
      }
    } else {
      new_links <- interim[[2]]$link
      r$context_links <- c(r$context_links, new_links)
    }
    if (length(r$context_links) > 1) {
      r$context_links <-
        r$context_links[r$context_links != "No context used so far."]
    }

    r$all_chats <-
      c(
        interim[[1]],
        list(
          list(
            role    = "assistant",
            content = new_response
          )
        )
      )

    if (input$test_code) {
      code_from_response <- extract_code_chunks(new_response)
    } else {
      code_from_response <- NULL
    }

    if (!is.null(code_from_response)) {
      new_reprex <- run_extracted_code(code_from_response)
      r$all_chats <-
        c(
          r$all_chats,
          list(
            list(
              role    = "reprex",
              content = new_reprex
            )
          )
        )
    }

    r$all_chats_formatted <- make_chat_history(r$all_chats)
    updateTextAreaInput(session, "chat_input", value = "")
  }) |>
    bindEvent(input$chat)

  output$sources <- renderUI({
    if (is.null(r$context_links)) {
      return(NULL)
    } else {
      list(
        markdown("**Sources**"),
        markdown(paste0("* ", unique(r$context_links), collapse = "\n"))
      )
    }
  })

  output$all_chats_box <- renderUI({
    req(height())
    card(
      height = height() - 200,
      r$all_chats_formatted,
      textOutput(outputId = "streaming"),
      uiOutput(outputId = "sources")
    )
  })
}

shinyApp(ui, server)
