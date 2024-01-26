get_hyperlinks <- function(url) {
  status <- check_url(url)
  if (identical(status, 200L)) {
    links <- rvest::read_html(url) |>
      rvest::html_nodes("a[href]") |>
      rvest::html_attr("href") |>
      unique()

    tibble::tibble(parent = url, link = links)
  } else {
    cli::cli_warn(c(
      "!" = "URL not valid.",
      "i" = "Tried to scrape {url}",
      "i" = "Status code: {status}"
    ))
  }
}

check_url <- function(url) {
  status <-
    httr2::request(url) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform() |>
    httr2::resp_status()
  status
}

validate_link <- function(link, try_fix = TRUE) {
  if (is_null(link)) {
    return(invisible())
  }
  status <- check_url(link)
  if (!is.null(link) && identical(status, 200L)) {
    if (stringr::str_ends(link, "/")) {
      link <- stringr::str_sub(link, end = -2)
    }
    link
  } else {
    if (try_fix) {
      link <- stringr::str_remove(link, "\\/[\\w-\\.]+\\.html")
      if (identical(check_url(link), 200L)) {
        return(link)
      }
    }
    cli::cli_warn(c(
      "!" = "URL not valid.",
      "i" = "Tried to scrape {link}",
      "i" = "Status code: {status}"
    ))
    NULL
  }
}

recursive_hyperlinks <- function(local_domain,
                                 url,
                                 expanded_urls = NULL,
                                 checked_urls = NULL,
                                 aggressive = FALSE) {
  links <- url[!(url %in% expanded_urls)]
  if (length(links) < 1) {
    return(expanded_urls)
  }
  if (aggressive) {
    domain_pattern <- glue("^https?://(?:.*\\.)?{local_domain}/?")
  } else {
    domain_pattern <- glue("^https?://{local_domain}/?")
  }

  expanded_urls <- c(expanded_urls, links)
  cli::cli_inform(c("i" = "Total urls: {length(expanded_urls)}"))
  links_df <- purrr::map(links, get_hyperlinks,
    .progress = "Getting more links"
  ) |>
    dplyr::bind_rows() |>
    dplyr::filter(!stringr::str_detect(link, "^\\.$|mailto:|\\#|^\\_$")) |>
    dplyr::mutate(link = purrr::map2_chr(parent, link,
      .f = \(x, y) xml2::url_absolute(y, x)
    ))

  cli::cli_inform("Going to check {length(unique(links_df$link))} links")

  new_links <-
    purrr::map(unique(links_df$link), \(x) {
      if (rlang::is_true(stringr::str_detect(x, domain_pattern))) {
        validate_link(x)
      } else {
        NULL
      }
    },
    .progress = "Validating Links"
    ) |>
    unlist()

  checked_urls <- c(checked_urls, links_df$link) |> unique()

  cli::cli_inform("Checked urls: {length(checked_urls)}")

  exclude_exts <- "\\.(xml|mp4|pdf|zip|rar|gz|tar|csv|docx|pptx|xlsx|avi)$"
  new_links <-
    new_links[!grepl(exclude_exts, new_links, ignore.case = TRUE)] |> unique()
  recursive_hyperlinks(local_domain, unique(new_links), expanded_urls, checked_urls)
}

#' Scrape and process all hyperlinks within a given URL
#'
#' This function scrapes all hyperlinks within a given URL and processes the
#' data into a tibble format. It saves the resulting tibble into a parquet file.
#'
#' @param url A character string with the URL to be scraped.
#' @param index_create A logical value indicating whether to create an index.
#' Default is TRUE.
#' @param aggressive A logical value indicating whether to use aggressive link
#' crawling. Default is FALSE.
#' @param overwrite A logical value indicating whether to overwrite scraped
#' pages and index if they already exist. Default is FALSE.
#'  `parallel::detectCores() - 1`
#' @param pkg_version Package version number
#' @param pkg_name Package name
#' @param service The service to use for scraping. Default is "openai". Options
#' are "openai" and "local".
#'
#' @return NULL. The resulting tibble is saved into a parquet file.
#'
#' @export
crawl <- function(url,
                  index_create = TRUE,
                  aggressive = FALSE,
                  overwrite = FALSE,
                  update = FALSE,
                  pkg_version = "not a package",
                  pkg_name = NULL,
                  service = "openai") {
  rlang::arg_match(service, c("openai", "local"))
  parsed_url <- urltools::url_parse(url)
  local_domain <- parsed_url$domain
  url_path <- parsed_url$path
  if (!rlang::is_na(url_path) && rlang::is_false(aggressive)) {
    local_domain <- glue("{local_domain}/{url_path}")
  }

  # check if url has already been scraped
  scraped_data_dir <-
    file.path(tools::R_user_dir("gpttools", which = "data"), "text")
  local_domain_name <-
    stringr::str_replace_all(local_domain, "/|\\.", "-") |>
    stringr::str_remove("-$")
  scraped_text_file <-
    glue::glue("{scraped_data_dir}/{local_domain_name}.parquet")

  cli::cli_inform("Scraped data file: {file.exists(scraped_text_file)}")
  cli::cli_inform("Update: {rlang::is_false(update)}")
  cli::cli_inform("Combined: {file.exists(scraped_text_file) && rlang::is_false(update)}")

  if (file.exists(scraped_text_file) && rlang::is_false(update)) {
    cli::cli_alert_warning(
      c(
        "!" = "Scraped data already exists for this domain.",
        "i" = "Use {.code crawl(<url>, update = TRUE)} to scrape site again."
      )
    )
  } else {
    cli_rule("Crawling {.url {url}}")
    cli_inform(c(
      "i" = "This may take a while.",
      "i" = "Gathering links to scrape"
    ))

    cli::cli_inform("Local domain: {local_domain}")
    cli::cli_inform("Url: {url}")

    links <-
      recursive_hyperlinks(local_domain, url, aggressive = aggressive) |>
      unique()
    cli_inform(c("i" = "Scraping validated links"))
    scraped_data <-
      purrr::map(links, \(x) {
        if (identical(check_url(x), 200L)) {
          tibble::tibble(
            source  = local_domain,
            link    = x,
            text    = paste(scrape_url(x), collapse = " "),
            n_words = tokenizers::count_words(text),
            scraped = lubridate::now()
          )
        } else {
          cli::cli_inform(c(
            "!" = "Skipped {url}",
            "i" = "Status code: {status}"
          ))
        }
      },
      .progress = "Scraping Pages"
      ) |>
      dplyr::bind_rows() |>
      dplyr::distinct()
    cli_inform(c("i" = "Saving scraped data"))
    if (rlang::is_false(dir.exists(scraped_data_dir))) {
      dir.create(scraped_data_dir, recursive = TRUE)
    }
    arrow::write_parquet(
      x    = scraped_data,
      sink = scraped_text_file
    )
  }

  index_dir <-
    file.path(tools::R_user_dir("gpttools", which = "data"), "index")

  if (service == "local") index_dir <- file.path(index_dir, "local")

  index_file <- glue::glue("{index_dir}/{local_domain_name}.parquet")

  if (file.exists(index_file) && rlang::is_false(overwrite)) {
    cli::cli_warn(
      c(
        "!" = "Index already exists for this domain.",
        "i" = "Use {.code crawl(<url>, overwrite = TRUE)} to overwrite."
      )
    )
    return(NULL)
  }

  if (index_create) {
    if (service == "openai") {
      create_index(local_domain_name,
        overwrite = overwrite,
        pkg_version = pkg_version,
        pkg_name = pkg_name
      )
    } else if (service == "local") {
      create_index(local_domain_name,
        overwrite = overwrite,
        pkg_version = pkg_version,
        pkg_name = pkg_name,
        local_embeddings = TRUE,
        dont_ask = TRUE
      )
    }
  }
}

#' Remove new lines from a character vector.
#'
#' Removes all new line characters from a character vector.
#'
#' @param serie A character vector.
#'
#' @return A character vector with all new line characters removed.
#'
#' @export
remove_lines_and_spaces <- function(serie) {
  serie |>
    stringr::str_replace("\n", " ") |>
    stringr::str_replace("\\n", " ") |>
    stringr::str_replace("\\s+", " ") |>
    stringr::str_remove_all("^\\s*$") |>
    unique()
}

#' Scrape text from a URL
#'
#' This function scrapes the text from a URL and returns a character vector.
#'
#' @param url A character string containing a valid URL.
#'
#' @return A character vector containing the text from the URL.
#'
#' @export
scrape_url <- function(url) {
  text <- R.utils::withTimeout(extract_text(url),
    timeout = 10,
    onTimeout = "silent"
  )
  if (is.null(text)) {
    text <- extract_text(url, use_html_text2 = FALSE)
  }
  if ("You need to enable JavaScript to run this app." %in% text) {
    cli_warn("Unable to parse page {url}. JavaScript is required.")
    NULL
  } else {
    text
  }
}


extract_text <- function(url, use_html_text2 = TRUE) {
  exclude_tags <- c(
    "script", "style", "head", "meta", "button", "form", "img", "svg",
    "input", "select", "option", "textarea", "label", "noscript", "canvas",
    "map", "area", "object", "param", "source", "track", "embed", "iframe",
    "video", "audio", "picture", "figure", "nav", "footer", "container",
    "template-article", "header", "datalist", "details", "dialog",
    "mark", "menuitem", "meter", "progress", "time"
  )

  exclude_attributes <- c(
    "js-", "json", "html-widget", "html-fill-item", "html-widget-content",
    "plotly"
  )

  xpath_tags <- exclude_tags |>
    purrr::map_chr(.f = \(x) glue::glue("self::{x}")) |>
    stringr::str_c(collapse = " or ")

  xpath_attributes <- exclude_attributes |>
    purrr::map_chr(.f = \(x) glue::glue("contains(concat(' ', normalize-space(@class), ' '), ' {x}')")) |>
    stringr::str_c(collapse = " or ")

  # Handling general attribute selectors
  general_attributes <- c("role", "aria-", "data-", "id", "class", "style")
  xpath_general_attributes <- general_attributes |>
    purrr::map_chr(.f = \(x) glue::glue("@{x}")) |>
    stringr::str_c(collapse = " or ")

  xpath_combined <- glue::glue("//body//*[not({xpath_tags} or {xpath_attributes} or {xpath_general_attributes})]")

  nodes <- rvest::read_html(url) |>
    rvest::html_elements(xpath = xpath_combined)

  if (use_html_text2) {
    text <- rvest::html_text2(nodes)
  } else {
    text <- rvest::html_text(nodes)
  }
  text |> remove_lines_and_spaces()
}
