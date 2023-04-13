get_hyperlinks <- function(url) {
  status <- check_url(url)
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
  status <-
    httr2::request(url) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform() |>
    httr2::resp_status()
  status
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
  cli::cli_inform(c("i" = "Total urls: {length(checked_urls)}"))
  links_df <- furrr::future_map(links, get_hyperlinks) |>
    dplyr::bind_rows() |>
    dplyr::filter(!stringr::str_detect(link, "^\\.$|mailto:|^\\.\\.|\\#|^\\_$"))

  new_links <-
    furrr::future_pmap(as.list(links_df), \(parent, link) {
      clean_link <- xml2::url_absolute(link, parent)
      if (rlang::is_true(stringr::str_detect(clean_link, domain_pattern))) {
        validate_link(clean_link, link)
      } else {
        NULL
      }
    }) |>
    unlist()

  recursive_hyperlinks(local_domain, unique(new_links), checked_urls)
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
#' @param num_cores Number of cores to use. Defaults to
#'  `parallel::detectCores() - 1`
#' @param pkg_version Package version number
#' @param use_azure_openai Whether to use Azure OpenAI for index creation
#'
#' @return NULL. The resulting tibble is saved into a parquet file.
#'
#' @export
crawl <- function(url,
                  index_create = TRUE,
                  aggressive = FALSE,
                  overwrite = FALSE,
                  num_cores = parallel::detectCores() - 1,
                  pkg_version = NULL,
                  use_azure_openai = FALSE) {
  parsed_url <- urltools::url_parse(url)
  local_domain <- parsed_url$domain
  url_path <- parsed_url$path
  if (!rlang::is_na(url_path)) {
    local_domain <- glue("{local_domain}/{url_path}")
  }
  withr::local_options(list(
    cli.progress_show_after = 0,
    cli.progress_clear = FALSE
  ))
  future::plan(future::multisession, workers = num_cores)
  scraped_data_dir <-
    file.path(tools::R_user_dir("gpttools", which = "data"), "text")
  local_domain_name <- stringr::str_replace_all(local_domain, "/|\\.", "-")
  scraped_text_file <-
    glue::glue("{scraped_data_dir}/{local_domain_name}.parquet")

  if (file.exists(scraped_text_file) && rlang::is_false(overwrite)) {
    cli::cli_warn(
      c(
        "!" = "Scraped data already exists for this domain.",
        "i" = "Use {.code crawl(<url>, overwrite = TRUE)} to overwrite."
      )
    )
    return(NULL)
  }

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
    furrr::future_map(links, \(x) {
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
    }) |>
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
  if (index_create) {
    if (use_azure_openai) {
      create_azure_index(local_domain_name,
        overwrite = overwrite,
        pkg_version = pkg_version
      )
    } else {
      create_index(local_domain_name,
        overwrite = overwrite,
        pkg_version = pkg_version
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
  rlang::check_installed("rvest")
  exclude_tags <- c(
    "style", "script", "head", "meta", "link", "button",
    "form", "img", "svg"
  )
  nodes <- rvest::read_html(url) |>
    rvest::html_nodes(
      xpath = paste("//body//*[not(self::",
        paste(exclude_tags, collapse = " or self::"), ")]",
        sep = ""
      )
    )
  if (use_html_text2) {
    text <- rvest::html_text2(nodes)
  } else {
    text <- rvest::html_text(nodes)
  }
  text |> remove_lines_and_spaces()
}
