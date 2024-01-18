rlang::check_installed(
  c(
    "shiny", "bslib", "bsicons", "cli", "glue",
    "gptstudio", "gpttools", "waiter"
  )
)

library(gpttools)
library(gptstudio)
library(shiny)
library(bslib)
library(bsicons)
library(waiter)

window_height_ui <- function(id) {
  ns <- shiny::NS(id)
  namespaced_id <- ns("window_height")

  shiny::tags$head(shiny::tags$script(HTML(
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
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      input$window_height
    })
  })
}

make_chat_history <- function(chats) {
  purrr::discard(chats, \(x) x$role == "system") |>
    purrr::map(\(x) {
      list(
        shiny::strong(stringr::str_to_title(x$role)),
        shiny::markdown(x$content)
      )
    }) |>
    purrr::list_flatten()
}

api_services <-
  utils::methods("gptstudio_request_perform") |>
  stringr::str_remove(
    pattern = "gptstudio_request_perform.gptstudio_request_"
  ) |>
  purrr::discard(~ .x == "gptstudio_request_perform.default")

ui <- page_fluid(
  waiter::use_waiter(),
  window_height_ui("height"),
  theme = bs_theme(bootswatch = "morph", version = 5),
  tags$style("
    .card, .accordion {
      box-shadow: none !important;
    }
  "),
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
  br(),
  layout_column_wrap(
    width = 1,
    height = "100%",
    heights_equal = "row",
    card(
      card_header("Write Prompt",
        class = "bg-primary d-flex align-items-center",
        popover(
          bs_icon("gear", class = "ms-auto"),
          accordion_panel(
            "Data & Task",
            icon = bs_icon("robot", class = "ms-auto"),
            selectInput(
              "source", "Data Source",
              choices = NULL
            ),
            selectInput(
              "task", "Task",
              choices = c("Context Only", "Permissive Chat"),
              selected = "Permissive Chat",
            )
          ),
          br(),
          accordion_panel(
            "Preferences",
            icon = bs_icon("sliders", class = "ms-auto"),
            selectInput(
              "service", "AI Service",
              choices = api_services,
              selected = "openai"
            ),
            selectInput("model", "Model",
              choices = NULL
            ),
            radioButtons(
              "save_history", "Save & Use History",
              choiceNames = c("Yes", "No"),
              choiceValues = c(TRUE, FALSE),
              selected = TRUE, inline = TRUE,
            ),
            radioButtons(
              "local", "Local Embeddings",
              choiceNames = c("Yes", "No"),
              choiceValues = c(TRUE, FALSE),
              selected = FALSE, inline = TRUE,
            ),
            sliderInput(
              "n_docs", "Docs to Include (#)",
              min = 0, max = 20, value = 3
            ),
            sliderInput(
              "n_history", "Chat History to Include (#)",
              min = 0, max = 20, value = 3
            )
          ),
          title = "Plot settings"
        )
      ),
      uiOutput("all_chats_box"),
      layout_column_wrap(
        width = NULL, fill = FALSE,
        style = htmltools::css(grid_template_columns = "3fr 1fr"),
        card(
          textAreaInput(
            inputId = "chat_input", label = NULL,
            value = "", resize = "vertical", rows = 1,
            width = "100%"
          )
        ),
        card(
          class = "btn-primary",
          actionButton(
            inputId = "chat", label = "Chat",
            icon = icon("robot"),
            width = "100%", class = "btn-sucess"
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  r <- reactiveValues()
  r$all_chats_formatted <- NULL
  r$all_chats <- NULL
  height <- window_height_server("height")
  index <- reactive({
    if (input$local == TRUE) {
      load_index(glue::glue("local/{input$source}"))
    } else {
      load_index(input$source)
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
      choices = gptstudio::get_available_models(service = input$service)
    )
  )
  observe(updateSelectInput(session, "source", choices = indices()))
  observe({
    waiter_show(
      html = tagList(
        waiter::spin_facebook(),
        h3("Asking ChatGPT...")
      ),
      color = waiter::transparent(0.5)
    )
    interim <- chat_with_context(
      query = input$chat_input,
      service = input$service,
      model = input$model,
      index = index(),
      add_context = TRUE,
      chat_history = read_history(),
      session_history = r$all_chats,
      add_history = input$save_history,
      task = input$task,
      k_context = input$n_docs,
      k_history = input$n_history,
      save_history = input$save_history,
      overwrite = FALSE,
      local = input$local
    )
    new_response <- interim[[3]]
    r$context_links <- c(r$context_links, interim[[2]]$link)
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
    r$all_chats_formatted <- make_chat_history(r$all_chats)
    waiter::waiter_hide()
    updateTextAreaInput(session, "chat_input", value = "")
  }) |>
    bindEvent(input$chat)

  output$all_chats_box <- renderUI({
    req(length(r$context_links) > 0)
    card(
      height = height() - 300,
      card_body(
        r$all_chats_formatted,
        markdown("**Sources**"),
        markdown(paste0("* ", unique(r$context_links), collapse = "\n"))
      )
    )
  })
}

shinyApp(ui, server)
