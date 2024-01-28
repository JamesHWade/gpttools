library(gpttools)

resources <-
  tibble::tribble(
    ~url, ~name,
    "https://docs.posit.co/", "posit docs",
    "https://www.tmwr.org/", "tmwr",
    "https://r4ds.hadley.nz/", "r4ds",
    "https://adv-r.hadley.nz/", "adv-r",
    "https://r-pkgs.org/", "r-pkgs",
    "https://mastering-shiny.org/", "mastering-shiny",
    "https://quarto.org/", "quarto",
    "https://ggplot2-book.org/", "ggplot2-book",
    "https://smltar.com/", "smltar",
    "https://unleash-shiny.rinterface.com/", "unleash-shiny",
    "https://openintro-ims.netlify.app/", "openintro-ims",
    "https://www.bayesrulesbook.com/", "bayesrulesbook",
    "https://engineering-shiny.org/", "engineering-shiny",
    "https://design.tidyverse.org/", "design-tidyverse",
    "https://www.tidyverse.org/", "tidyverse-site",
    "https://r-graphics.org/", "r-graphics",
    "https://socviz.co/", "socviz",
  )

scrape_resources <- function(resources) {
  indices <-
    gpttools:::read_indexed_pkgs(local = TRUE) |>
    dplyr::select(name) |>
    dplyr::distinct() |>
    dplyr::as_tibble()

  sites <- resources |>
    dplyr::anti_join(indices, by = "name")

  if (nrow(sites) == 0) {
    cli_alert_info("No packages to scrape.")
    return(invisible())
  }

  if (rlang::is_interactive()) {
    cli::cli_text("You are about to scrape {nrow(sites)} package site page{?s}")
    continue <- ui_yeah("Do you want to continue?")
  } else {
    continue <- TRUE
  }

  if (!continue) {
    cli_alert_info("Scraping aborted.")
    return(invisible())
  }

  sites |>
    dplyr::select(url, name) |>
    purrr::pmap(.f = \(url, name) {
      crawl(
        url = url,
        index_create = TRUE,
        overwrite = TRUE,
        pkg_name = name,
        service = "local"
      )
    })
}

scrape_resources(resources)
