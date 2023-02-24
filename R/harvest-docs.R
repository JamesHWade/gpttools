# Function to get the hyperlinks from a URL
get_hyperlinks <- function(url) {
  rlang::check_installed("rvest")
  status <- httr::GET(url) |> httr::status_code()
  if (identical(status, 200L)) {
    rvest::read_html(url) |>
      rvest::html_nodes("a[href]") |>
      rvest::html_attr("href") |>
      unique()
  } else {
    cli::cli_abort(c(
      "!" = "URL not valid.",
      "i" = "Tried to scrape {url}",
      "i" = "Status code: {status}"
    ))
  }
}

check_url <- function(url) {
  httr::GET(url) |> httr::status_code()
}

get_domain_hyperlinks <- function(local_domain, url) {
  links <- get_hyperlinks(url)
  purrr::map(links, \(link) {
    clean_link <- NULL
    if (stringr::str_detect(link, paste0("^https?://", local_domain))) {
      clean_link <- link
    } else if (stringr::str_detect(link, "^/[^/]|^/+$|^\\./|^[[:alnum:]]") &&
               !stringr::str_detect(link, "^https?://|\\.\\.|#|mailto:") &&
               !(link == "_")) {
      if (stringr::str_detect(link, "^\\./")) {
        link <- stringr::str_replace(link, "^\\./", "/")
      } else if (stringr::str_detect(link, "^[[:alnum:]]")) {
        link <- glue::glue("/", link)
      }
      clean_link <- glue::glue("https://{local_domain}{link}")
      if (identical(check_url(clean_link), 404L)) {
        url_path <- urltools::url_parse(url)$path |>
          stringr::str_split("/") |>
          unlist()
        url_path1 <- url_path[1]
        clean_link <- glue::glue("https://{local_domain}/{url_path1}{link}")
        if (identical(check_url(clean_link), 404L)) {
          if (length(url_path) > 1) {
            url_path2 <- paste0(url_path[1:2], collapse = "/")
            clean_link <- glue::glue("https://{local_domain}/{url_path2}{link}")
          } else {
            cli::cli_warn(c(
              "!" = "Still not found: {clean_link}",
              "i" = "URL Path: {url_path}"
            ))
          }
        }
      }
    }
    if (!is.null(clean_link) && identical(check_url(clean_link), 200L)) {
      if (stringr::str_ends(clean_link, "/")) {
        clean_link <- stringr::str_sub(clean_link, end = -2)
      }
      clean_link
    } else {
      clean_link <- NULL
    }
  }) |>
    unlist()
}

crawl <- function(url) {
  local_domain <- urltools::url_parse(url)$domain
  queue <- url
  seen <- character()
  write_scrape_dirs(local_domain)
  while (length(queue) > 0) {
    url <- queue[length(queue)]
    cli::cli_inform(c("i" = "Url: {url}"))
    filename <-
      stringr::str_replace(url, "^https?://", "") |>
      stringr::str_replace_all("/", "_")
    text_file <- glue::glue("text/{local_domain}/{filename}.txt")
    if (any(url %in% seen) | file.exists(text_file)) {
      cli::cli_inform(c(
        "!" = "Skipped {url}",
        "i" = "Already seen."
      ))
      queue <- queue[!(queue %in% url)]
    } else if (identical(check_url(url), 200L)) {
      text <- scrape_url(url)
      if (!rlang::is_null(text)) {
        seen <- c(seen, url) |> unique()
        readr::write_lines(text, text_file)
        links <- get_domain_hyperlinks(local_domain, url)
        links <- links[!(links %in% seen)]
        queue <- c(queue, links) |>
          unlist() |>
          unique()
        cli::cli_inform(c("i" = "Queue: {length(queue)}"))
        cli::cli_inform(c("i" = "Seen: {length(seen)}"))
        queue <- queue[!(queue %in% seen)]
      }
    } else {
      cli::cli_inform(c(
        "!" = "Skipped {url}",
        "i" = "Status code: {status}"
      ))
      queue <- queue[!(url %in% queue)]
    }
  }
}

write_scrape_dirs <- function(local_domain) {
  if (!dir.exists("text")) dir.create("text")
  scrape_dir <- paste0("text/", local_domain)
  if (!dir.exists(scrape_dir)) dir.create(scrape_dir)
  if (!dir.exists("processed")) dir.create("processed")
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
#' This function scrapes the text from a URL and returns it as a character vector.
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
    rvest::html_nodes(xpath = "body") |>
    # rvest::html_nodes(xpath = paste("//body//*[not(self::",
    #   paste(exclude_tags, collapse = " or self::"),
    #   ")]",
    #   sep = ""
    # )) |>
    rvest::html_text2() |>
    remove_new_lines()
  if ("You need to enable JavaScript to run this app." %in% text) {
    cli::cli_warn("Unable to parse page {url}. JavaScript is required.")
    NULL
  } else {
    text
  }
}
