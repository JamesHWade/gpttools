#' Run Package Selector App
#'
#' Run the package selector shiny app
#'
#' @export
#'
#' @return This function has no return value.
#'
addin_run_select_pkgs <- function() {
  run_select_pkgs_app()
}

#' Addin to scrape installed packages
#'
#' Invokes RStudio addin functionality to scrape select installed packages and
#' create indices for use in the "Chat with Retrieval" application.
#'
#' @export
#' @return No return value, called for side effects only.
#'
#' @examplesIf rlang::is_interactive()
#' # This function is typically run within RStudio as an Addin.
#' # It would not be called directly in the code.
#' addin_scrape_pkgs()
#'
#' @note This addin requires RStudio to be available and will stop with an
#' error message if RStudio API is not accessible.
#'
addin_run_scrape_pkgs <- function() {
  # Check if RStudio API is available
  if (!rstudioapi::isAvailable()) {
    cli_abort("The rstudioapi is not available.")
  }
  # Get user feedback with rstudioapi
  proceed <-
    rstudioapi::showQuestion(
      title = "Scrape Packages",
      # nolint start
      message = "This will scrape installed packages and create indices to use with the \"Chat with Retrieval\" app. Would you like to proceed?"
      # nolint end
    )

  # Proceed with scraping if the user agrees
  if (proceed) {
    cli_alert_info("Scraping packages as a background job.")
    # Run the scrape packages script as a background job
    rstudioapi::jobRunScript(
      path = system.file("scripts/scrape_pkgs.R",
        package = "gpttools"
      ),
      name = "Scraping Pacakges"
    )
  } else {
    cli_alert_info("Scraping cancelled.")
  }
}

#' Run a Shiny App to Select and Save Installed Packages
#'
#' This function launches a Shiny application that allows users to select from a
#' list of installed packages and save their selections.
#'
#' @return None The function is used for its side effect of launching a Shiny
#'   app and doesn't return anything.
#'
#' @details The application provides a sidebar for package selection and an
#' action button to save the selected packages. It displays the selected
#' packages in a data table.
#'
#' @export
#'
#' @examplesIf rlang::is_interactive()
#' run_select_pkgs_app()
run_select_pkgs_app <- function() {
  installed_packages <-
    installed.packages() |>
    tibble::as_tibble() |>
    dplyr::select("Package", "Version", "License", "Built")

  ui <- bslib::page_sidebar(
    title = "Package Selector",
    theme = bslib::bs_theme(version = 5, bootswatch = "litera"),
    sidebar = bslib::sidebar(
      width = 400,
      shiny::selectInput(
        "selected_pkg",
        "Select packages:",
        choices = installed_packages$Package,
        multiple = TRUE,
        selected = use_default_pkgs()
      )
    ),
    shiny::actionButton("save_pkgs", "Save Selected Packages",
      icon = shiny::icon("save", class = "ms-auto"),
      class = "btn-primary"
    ),
    shiny::dataTableOutput("table_packages")
  )
  server <- function(input, output, session) {
    shiny::observe({
      selected_pkgs <-
        installed_packages |>
        dplyr::filter(Package %in% input$selected_pkg) |>
        dplyr::pull(Package)
      try_to_save <- save_pkgs_to_scrape(selected_pkgs)
      if (try_to_save) {
        shiny::showNotification("Saved packages to scrape.")
      }
    }) |>
      shiny::bindEvent(input$save_pkgs)

    output$table_packages <- shiny::renderDataTable(
      {
        installed_packages |> dplyr::filter(Package %in% input$selected_pkg)
      },
      options = list(pageLength = 5)
    )
  }

  shiny::shinyApp(ui, server)
}
