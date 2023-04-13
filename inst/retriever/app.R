rlang::check_installed(c(
  "shiny", "bsicons", "cli", "glue", "gptstudio",
  "gpttools", "waiter", "htmltools", "withr"
))
library(gpttools)

rlang::check_installed("bslib", version = "0.4.2.9000")
rlang::check_installed("bsicons")

window_height_ui <- function(id) {
  ns <- shiny::NS(id)
  namespaced_id <- ns("window_height")

  shiny::tags$head(shiny::tags$script(shiny::HTML(
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

indices <- gpttools::list_index() |> tools::file_path_sans_ext()

ui <- bslib::page_fluid(
  waiter::use_waiter(),
  window_height_ui("height"),
  theme = bslib::bs_theme(bootswatch = "morph", version = 5),
  shiny::tags$style("
    .card, .accordion {
      box-shadow: none !important;
    }
  "),
  title = "Retreiver from gpttools",
  bslib::layout_sidebar(
    border = TRUE,
    border_radius = FALSE,
    sidebar = bslib::sidebar(
      position = "left",
      open = FALSE,
      bslib::accordion_panel(
        "Data & Task",
        icon = bsicons::bs_icon("robot"),
        shiny::selectInput(
          "source", "Data Source",
          choices = c("All", indices)
        ),
        shiny::selectInput(
          "task", "Task",
          choices = c("Context Only", "Permissive Chat"),
          selected = "Permissive Chat",
        )
      ),
      shiny::br(),
      bslib::accordion_panel(
        "Preferences",
        icon = bsicons::bs_icon("gear-wide-connected"),
        shiny::radioButtons(
          "save_history", "Save & Use History",
          choiceNames = c("Yes", "No"),
          choiceValues = c(TRUE, FALSE),
          selected = TRUE, inline = TRUE,
        ),
        shiny::sliderInput(
          "n_docs", "Docs to Include (#)",
          min = 0, max = 20, value = 3
        ),
        shiny::sliderInput(
          "n_history", "Chat History to Include (#)",
          min = 0, max = 20, value = 3
        )
      )
    ),
    bslib::layout_column_wrap(
      width = 1,
      height = "100%",
      heights_equal = "row",
      shiny::uiOutput("all_chats_box"),
      bslib::card(
        bslib::card_header("Write Prompt", class = "bg-primary"),
        bslib::layout_column_wrap(
          width = NULL, fill = FALSE,
          style = htmltools::css(grid_template_columns = "3fr 1fr"),
          bslib::card(
            shiny::textAreaInput(
              inputId = "chat_input", label = NULL,
              value = "", resize = "vertical", rows = 1,
              width = "100%"
            )
          ),
          bslib::card(
            class = "btn-primary",
            shiny::actionButton(
              inputId = "chat", label = "Chat",
              icon = shiny::icon("robot"),
              width = "100%", class = "btn-sucess"
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  r <- shiny::reactiveValues()
  r$all_chats_formatted <- NULL
  r$all_chats <- NULL
  height <- window_height_server("height")
  index <- shiny::reactive(gpttools::load_index(input$source))
  shiny::observe({
    waiter::waiter_show(
      html = shiny::tagList(
        waiter::spin_flower(),
        shiny::h3("Asking ChatGPT...")
      ),
      color = waiter::transparent(0.5)
    )
    interim <- chat_with_context(
      query = input$chat_input,
      index = index(),
      add_context = TRUE,
      chat_history = read_history(),
      session_history = r$all_chats,
      add_history = input$save_history,
      task = input$task,
      k_context = input$n_docs,
      k_history = input$n_history,
      save_history = input$save_history,
      overwrite = FALSE
    )
    new_response <- interim[[3]]$choices
    r$context_links <- c(r$context_links, interim[[2]]$link)
    r$all_chats <-
      c(
        interim[[1]],
        list(
          list(
            role    = new_response$message.role,
            content = new_response$message.content
          )
        )
      )
    r$all_chats_formatted <- gptstudio::make_chat_history(r$all_chats)
    waiter::waiter_hide()
    shiny::updateTextAreaInput(session, "chat_input", value = "")
  }) |>
    shiny::bindEvent(input$chat)

  output$all_chats_box <- renderUI({
    shiny::req(length(r$context_links) > 0)
    bslib::card(
      height = height() - 300,
      bslib::card_header("Chat History", class = "bg-primary"),
      bslib::card_body(
        fill = TRUE,
        r$all_chats_formatted,
        shiny::markdown("**Sources**"),
        shiny::markdown(paste0("* ", unique(r$context_links), collapse = "\n"))
      )
    )
  })
  shiny::observeEvent(input$cancel, shiny::stopApp())
}

shiny::shinyApp(ui, server)
