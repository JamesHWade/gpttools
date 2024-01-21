run_select_pkgs_app <- function() {
  installed_packages <-
    installed.packages() |>
    tibble::as_tibble() |>
    dplyr::select("Package", "Version", "License", "Built")

  ui <- bslib::page_sidebar(
    title = "Package Selector",
    theme = bslib::bs_theme(version = 5, bootswatch = "morph"),
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
        dplyr::filter(Package %in% input$selected_pkg)
      try_to_save <- save_pkgs_to_scrape(selected_pkgs)
      if (try_to_save) {
        shiny::showNotification("Saved packages to scrape.")
      }
    }) |> shiny::bindEvent(input$save_pkgs)

    output$table_packages <- shiny::renderDataTable(
      {
        installed_packages |> dplyr::filter(Package %in% input$selected_pkg)
      },
      options = list(pageLength = 10)
    )
  }

  shiny::shinyApp(ui, server)
}
