get_hyperlinks <- function(url) {
  rlang::check_installed("rvest")
  status <- httr::GET(url) |> httr::status_code()
  if (identical(status, 200L)) {
    tibble::tibble(
      parent = url,
      link = rvest::read_html(url) |>
        rvest::html_nodes("a[href]") |>
        rvest::html_attr("href") |>
        unique()
    )
  } else {
    cli::cli_warn(c(
      "!" = "URL not valid.",
      "i" = "Tried to scrape {url}",
      "i" = "Status code: {status}"
    ))
  }
}

check_url <- function(url) {
  httr::GET(url) |> httr::status_code()
}

validate_link <- function(link, original_link, try_fix = TRUE) {
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
      "i" = "Original link: {original_link}",
      "i" = "Status code: {status}"
    ))
    NULL
  }
}

recursive_hyperlinks <- function(local_domain,
                                 url,
                                 checked_urls = NULL,
                                 aggressive = FALSE) {
  links <- url[!(url %in% checked_urls)]
  if (length(links) < 1) {
    return(checked_urls)
  }

  if (aggressive) {
    domain_pattern <- glue("^https?://(?:.*\\.)?{local_domain}/?")
  } else {
    domain_pattern <- glue("^https?://{local_domain}/?")
  }

  checked_urls <- c(checked_urls, links)
  links_df <- purrr::map(links, get_hyperlinks) |>
    dplyr::bind_rows() |>
    dplyr::filter(!stringr::str_detect(link, "^\\.$|mailto:|^\\.\\.|\\#|^\\_$"))

  new_links <-
    purrr::pmap(as.list(links_df), \(parent, link) {
      clean_link <- NULL
      if (stringr::str_detect(link, domain_pattern)) {
        clean_link <- link
      } else if (stringr::str_detect(link, "^/[^/]|^/+$|^\\./|^[[:alnum:]]") &&
        !stringr::str_detect(link, "^https?://")) {
        if (stringr::str_detect(link, "^\\./")) {
          clean_link <- stringr::str_replace(link, "^\\./", "/")
        } else if (stringr::str_detect(link, "^[[:alnum:]]")) {
          clean_link <- glue::glue("/", link)
        } else {
          clean_link <- link
        }
        clean_link <- glue::glue("{parent}{clean_link}")
      }
      validate_link(clean_link, link)
    }, .progress = "Collect Links") |>
    unlist()
  recursive_hyperlinks(local_domain, unique(new_links), checked_urls)
}

#' Scrape and process all hyperlinks within a given URL
#'
#' This function scrapes all hyperlinks within a given URL and processes the
#' data into a tibble format. It saves the resulting tibble into a parquet file.
#'
#' @param url A character string with the URL to be scraped.
#'
#' @return NULL. The resulting tibble is saved into a parquet file.
#'
#' @export
crawl <- function(url, index_create = FALSE, aggressive = FALSE) {
  local_domain <- urltools::url_parse(url)$domain
  if (!dir.exists("text")) dir.create("text")
  withr::local_options(list(
    cli.progress_show_after = 0,
    cli.progress_clear = FALSE
  ))
  cli_rule("Crawling {.url {url}}")
  cli_inform(c(
    "i" = "This may take a while.",
    "i" = "Gathering links to scrape"
  ))
  links <-
    recursive_hyperlinks(local_domain, url, aggressive = aggressive) |>
    unique()
  cli_inform(c("i" = "Scraping validated links"))
  scraped_data <-
    purrr::map(links, \(x) {
      if (identical(check_url(x), 200L)) {
        tibble::tibble(
          link    = x,
          text    = paste(scrape_url(x), collapse = " "),
          n_words = tokenizers::count_words(text)
        )
      } else {
        cli::cli_inform(c(
          "!" = "Skipped {url}",
          "i" = "Status code: {status}"
        ))
      }
    }, .progress = "Scrape URLs") |>
    dplyr::bind_rows() |>
    dplyr::distinct()
  cli_inform(c("i" = "Saving scraped data"))
  arrow::write_parquet(
    scraped_data,
    glue("text/{local_domain}.parquet")
  )
  if (index_create) {
    create_index(local_domain)
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
remove_new_lines <- function(serie) {
  serie |>
    stringr::str_replace("\n", " ") |>
    stringr::str_replace("\\n", " ") |>
    stringr::str_replace("  ", " ") |>
    stringr::str_replace("  ", " ") |>
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
  rlang::check_installed("rvest")
  exclude_tags <- c("style", "script", "head", "meta", "link", "button")
  text <- rvest::read_html(url) |>
    rvest::html_nodes(xpath = paste("//body//*[not(self::",
      paste(exclude_tags, collapse = " or self::"),
      ")]",
      sep = ""
    )) |>
    rvest::html_text2() |>
    remove_new_lines()
  if ("You need to enable JavaScript to run this app." %in% text) {
    cli::cli_warn("Unable to parse page {url}. JavaScript is required.")
    NULL
  } else {
    text
  }
}
