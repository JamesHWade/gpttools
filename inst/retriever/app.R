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
  print(history)
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
  waiterOnBusy(
    html = spin_3circles(),
    color = transparent(0.5)
  ),
  window_height_ui("height"),
  theme = bs_theme(bootswatch = "litera", version = 5) |>
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
          )
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
            "test_code", "Test Code",
            choiceNames = c("Yes", "No"),
            choiceValues = c(TRUE, FALSE),
            inline = TRUE,
            selected = getOption("gpttools.test_code", FALSE)
          ),
          radioButtons(
            "local", "Local Embeddings",
            choiceNames = c("Yes", "No"),
            choiceValues = c(TRUE, FALSE),
            selected = getOption("gpttools.local_embed"),
            inline = TRUE,
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
      if (input$source == "All") {
        load_index(domain = "All", local_embeddings = TRUE)
      } else {
        purrr::map(input$source, \(x) {
          load_index(x, local_embeddings = TRUE) |>
            tibble::as_tibble()
        }) |>
          dplyr::bind_rows()
      }
    } else if (input$source == "All") {
      load_index(domain = "All", local_embeddings = TRUE)
    } else {
      purrr::map(input$source, \(x) {
        load_index(x, local_embeddings = FALSE) |>
          tibble::as_tibble()
      }) |>
        dplyr::bind_rows()
    }
  })

  indices <- reactive({
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
                            selected = getOption("gpttools.sources")
  ))
  observe({
    toggle_popover("settings", show = FALSE)
    save_user_config(
      service = input$service,
      model = input$model,
      task = input$task,
      embeddings = input$local,
      openai_embed_model = input$openai_embed_model,
      local_embed_model = input$local_embed_model,
      k_context = input$n_docs,
      k_history = input$n_history,
      save_history = input$save_history,
      sources = input$source,
      persist = TRUE
    )
  }) |> bindEvent(input$save_settings)
  observe({
    interim <- chat_with_context(
      query = input$chat_input,
      service = input$service,
      model = input$model,
      index = index(),
      add_context = TRUE,
      chat_history = read_history(local = input$local),
      session_history = r$all_chats,
      add_history = input$save_history,
      task = input$task,
      k_context = input$n_docs,
      k_history = input$n_history,
      save_history = input$save_history,
      overwrite = FALSE,
      local = input$local,
      embedding_model = transformer_model()
    )
    new_response <- interim[[3]]

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
      cat(new_reprex)
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

  output$all_chats_box <- renderUI({
    req(length(r$context_links) > 0)
    card(
      height = height() - 200,
      card_body(
        r$all_chats_formatted,
        markdown("**Sources**"),
        markdown(paste0("* ", unique(r$context_links), collapse = "\n"))
      )
    )
  })
}

shinyApp(ui, server)
