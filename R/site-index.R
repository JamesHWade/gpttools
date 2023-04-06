get_pkg_doc_page <- function(package_name) {
  package_path <- system.file(package = package_name)

  if (package_path == "") {
    cli_abort("The package is not installed.")
  }

  desc_path <- file.path(package_path, "DESCRIPTION")
  desc <- read.dcf(desc_path)

  # Extract the URL and BugReports fields from the DESCRIPTION file
  if (!is.null(desc)) {
    desc[1, "URL"] |>
      stringr::str_split(",\\s*") |>
      unlist()
  } else {
    NULL
  }
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

read_indexed_pkgs <- function() {
  data_dir <- glue('{tools::R_user_dir("gpttools", which = "data")}/index')
  if (dir.exists(data_dir)) {
    indices <- arrow::open_dataset(data_dir)
  } else {
    indices <- NULL
  }
}

get_pkgs_to_scrape <- function() {
  if (!is_installed(c("tidyverse", "tidymodels"))) {
    cli_abort(
      "This function assumes tidymodels and tidyverse are installed."
    )
  }

  installed_packages <- tibble::as_tibble(installed.packages())

  pkgs <- c(
    tidyverse::tidyverse_packages(),
    tidymodels::tidymodels_packages(),
    "shiny", "bslib", "shinyjs", "waiter", "golem" # ,
    # "brms", "rstanarm", "lme4"
  )

  package_info <- installed_packages |>
    dplyr::select(Package, Version) |>
    dplyr::rename(
      name = Package,
      version = Version
    ) |>
    dplyr::filter(name %in% pkgs) |>
    dplyr::mutate(url = purrr::map(name, purrr::possibly(get_pkg_doc_page))) |>
    tidyr::unnest_longer(url, indices_include = FALSE) |>
    dplyr::mutate(
      indexed = FALSE,
      source = urltools::domain(url)
    ) |>
    dplyr::left_join(get_outdated_pkgs(), by = "name")

  indices <- read_indexed_pkgs()
  if (!rlang::is_null(indices)) {
    # to do
  }

  package_info |>
    dplyr::filter(!stringr::str_detect(url, "github.com|arxiv.org"))
}

scrape_pkg_sites <- function(sites = get_pkgs_to_scrape()) {
  sites |>
    dplyr::rowwise() |>
    dplyr::mutate(indexed = crawl(url,
      index_create = FALSE,
      overwrite = FALSE,
      pkg_version = version
    ))
}


# packages
# tidyverse, tidymodels, shiny, bslib,
