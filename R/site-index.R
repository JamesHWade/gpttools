get_pkg_doc_page <- function(package_name) {
  if (!rlang::is_installed(package_name)) {
    return(NA)
  }
  links <- packageDescription(package_name)$URL |>
    stringr::str_split("\n", simplify = TRUE) |>
    stringr::str_split(", ", simplify = TRUE) |>
    stringr::str_remove_all(",")
  to_remove <- "github.com|arxiv.org|https://discourse"
  doc_link <- links[!stringr::str_detect(links, to_remove)][1]
  if (length(doc_link) == 0 | is.na(doc_link)) {
    return(NA)
  }
  doc_link
}

get_outdated_pkgs <- function() {
  old.packages() |>
    tibble::as_tibble() |>
    dplyr::select(Package, ReposVer) |>
    dplyr::rename(
      name = Package,
      new_version = ReposVer
    ) |>
    dplyr::mutate(outdated = TRUE)
}

read_indexed_pkgs <- function(local = FALSE) {
  if (local) {
    data_dir <-
      glue::glue('{tools::R_user_dir("gpttools", which = "data")}/index/local')
  } else {
    data_dir <-
      glue::glue('{tools::R_user_dir("gpttools", which = "data")}/index')
  }
  if (dir.exists(data_dir)) {
    arrow::open_dataset(data_dir)
  } else {
    NULL
  }
}

get_pkgs_to_scrape <- function(local = TRUE,
                               pkgs = getOption("gpttools.pkgs")) {
  indices <-
    read_indexed_pkgs(local = local) |>
    dplyr::select(name, version) |>
    dplyr::distinct() |>
    tibble::as_tibble() |>
    dplyr::rename(indexed_version = version)

  installed.packages() |>
    tibble::as_tibble() |>
    dplyr::transmute(
      name = Package,
      installed_version = Version
    ) |>
    dplyr::filter(name %in% pkgs) |>
    dplyr::mutate(url = purrr::map_chr(name, get_pkg_doc_page)) |>
    tidyr::drop_na(url) |>
    dplyr::mutate(source = urltools::domain(url)) |>
    dplyr::left_join(get_outdated_pkgs(), by = "name") |>
    dplyr::left_join(indices, by = c("name" = "name")) |>
    dplyr::distinct(name, .keep_all = TRUE) |>
    dplyr::filter(is.na(indexed_version) |
      indexed_version != installed_version) |>
    dplyr::rename(version = installed_version)
}

#' Scrape packaging sites
#'
#' @details This function scrapes the websites for the packages specified in the
#'   `sites` dataframe. If `sites` is empty, it alerts the user with no packages
#'   to scrape and returns `NULL` invisibly. If the user confirms to proceed, it
#'   scrapes each package site using the supplied details.
#'
#'
#' @param sites A data frame containing the package sites to be scraped. If not
#'   provided, it defaults to `get_pkgs_to_scrape(local = TRUE)`.
#' @param service The service to be used for scraping, defaults to "local".
#' @param index_create Logical indicating whether to create an index, defaults
#'   to `TRUE`.
#' @param overwrite Logical indicating whether to overwrite existing content,
#'   defaults to `TRUE`.
#' @return Invisible `NULL`. The function is called for its side effects.
#' @examplesIf rlang::is_interactive()
#' scrape_pkg_sites()
#' @export
scrape_pkg_sites <- function(sites = get_pkgs_to_scrape(local = TRUE),
                             service = "local",
                             index_create = TRUE,
                             overwrite = TRUE,
                             parallel = FALSE) {
  if (nrow(sites) == 0) {
    cli_alert_info("No packages to scrape.")
    return(invisible())
  }

  if (rlang::is_interactive()) {
    cli::cli_text("You are about to scrape {nrow(sites)} package site page{?s}")
    continue <- usethis::ui_yeah("Do you want to continue?")
  } else {
    continue <- TRUE
  }

  if (!continue) {
    cli_alert_info("Scraping aborted.")
    return(invisible())
  }

  if (rlang::is_true(parallel)) {
    if (rlang::is_installed("furrr")) {
      multisession <- TRUE
    } else {
      multisession <- FALSE
      cli::cli_alert_info("Package `furrr` not installed, using sequential scraping.")
    }
  }

  if (rlang::is_true(multisession)) {
    future::plan(multisession, workers = future::availableCores())
    sites |>
      dplyr::select(url, version, name) |>
      furrr::future_pmap(.f = \(url, version, name) {
        crawl(
          url = url,
          index_create = index_create,
          overwrite = overwrite,
          pkg_version = version,
          pkg_name = name,
          service = service
        )
      })
  } else {
    sites |>
      dplyr::select(url, version, name) |>
      purrr::pmap(.f = \(url, version, name) {
        crawl(
          url = url,
          index_create = index_create,
          overwrite = overwrite,
          pkg_version = version,
          pkg_name = name,
          service = service
        )
      })
  }
}

use_default_pkgs <- function() {
  c(
    "broom", "conflicted", "cli", "dbplyr", "dplyr", "dtplyr", "forcats",
    "ggplot2", "googledrive", "googlesheets4", "haven", "hms", "httr",
    "jsonlite", "lubridate", "magrittr", "modelr", "pillar", "purrr",
    "ragg", "readr", "readxl", "reprex", "rlang", "rstudioapi", "rvest",
    "stringr", "tibble", "tidyr", "xml2", "tidyverse", "broom", "cli",
    "conflicted", "dials", "dplyr", "ggplot2", "hardhat", "infer", "modeldata",
    "parsnip", "purrr", "recipes", "rlang", "rsample", "rstudioapi", "tibble",
    "tidyr", "tune", "workflows", "workflowsets", "yardstick", "tidymodels",
    "shiny", "bslib", "shinyjs", "golem", "httr2", "vetiver", "plumber",
    "embed", "textrecipes", "tidytext", "devtools", "usethis", "roxygen2",
    "pkgdown", "testthat", "knitr", "rmarkdown", "quarto", "gptstudio",
    "rstanarm", "brms", "bayesplot"
  )
}
