# borrowed from scales::scientific()
# Wickham H, Pedersen T, Seidel D (2023). _scales: Scale Functions for
# Visualization_. R package version 1.3.0,
# <https://CRAN.R-project.org/package=scales>.
scales_scientific <- function(x,
                              digits = 3,
                              scale = 1,
                              prefix = "",
                              suffix = "",
                              decimal.mark = ".",
                              trim = TRUE,
                              ...) {
  if (length(x) == 0) {
    return(character())
  }
  x <- signif(x * scale, digits)
  ret <- paste0(
    prefix,
    format(x,
      decimal.mark = decimal.mark,
      trim = trim,
      scientific = TRUE, ...
    ),
    suffix
  )
  ret[is.na(x)] <- NA
  names(ret) <- names(x)
  ret
}

clean_filename <- function(name) {
  name |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[\\\\/:?\"*<>|\\s]+", "-") |>
    stringr::str_replace_all("^-+|-+$", "") |>
    stringr::str_replace_all("--+", "-")
}

#' Repair Index Names
#'
#' This function scans index files in a specified directory, checks if they lack
#' a "name" column, and attempts to repair them by adding a "name" column based
#' on the file name or user input.
#'
#' @param local logical; if TRUE, the function operates on the local index
#'   directory, otherwise it uses the main index directory.
#'
#' @return The function does not return a value, but it modifies the index files
#'   in place by either adding a "name" column based on automated generation or
#'   user input.
#'
#' @examples
#' \dontrun{
#' repair_index_names(TRUE)
#' repair_index_names(FALSE)
#' }
#'
repair_index_names <- function(local = TRUE) {
  if (local) {
    index_dir <- file.path(tools::R_user_dir("gpttools", which = "data"), "index", "local")
  } else {
    index_dir <- file.path(tools::R_user_dir("gpttools", which = "data"), "index")
  }
  index_files <- list.files(index_dir, pattern = "*.parquet", full.names = TRUE)
  for (index_file in index_files) {
    index <- arrow::read_parquet(index_file)
    has_name <- "name" %in% names(index)
    if (has_name) {
      cli::cli_inform("Index already has name column.")
      next
    } else {
      domain <- basename(index_file) |> tools::file_path_sans_ext()
      # nolint start
      new_name <-
        gptstudio:::gptstudio_create_skeleton(
          prompt = glue::glue("Based on the domain, return the name of the package. If it is not a package, make your best guess at the name of the resource. Do not output any text other than the name. This output will be used to create the index column. For example, you should return \"usethis\" for \"usethis-r-lib-org\" and \"tune\" for \"tune-tidymodels-org\", ''\n\nDomain: {domain}\nName: "),
          stream = FALSE
        ) |>
        gptstudio:::gptstudio_request_perform() |>
        purrr::pluck("response")
      # nolint end
      cli::cli_inform(glue::glue("Name for {domain} is {new_name}."))
      use_name <- ui_yeah("Should {new_name} be used as the name for {domain}?")
      if (use_name) {
        index <- index |> dplyr::mutate(name = new_name)
        arrow::write_parquet(index, index_file)
      } else {
        # ask user for name in interactive session
        user_provided_name <-
          readline(glue::glue("Please provide a name for {domain}: "))
        index <- index |> dplyr::mutate(name = user_provided_name)
        arrow::write_parquet(index, index_file)
      }
    }
  }
}

# ui_yeah
#
# Ask the user a yes/no question, expecting a positive response as default.
#
# This function is modified from the usethis package authored by Hadley Wickham,
# Jennifer Bryan, Malcolm Barrett, and others. For details on the licensing,
# see the MIT License included with this package, as per the original usethis package.
ui_yeah <- function(x,
                    yes = c("Yes", "Definitely", "For sure", "Yup", "Yeah", "I agree", "Absolutely"),
                    no = c("No way", "Not now", "Negative", "No", "Nope", "Absolutely not"),
                    n_yes = 1, n_no = 2, shuffle = TRUE,
                    .envir = parent.frame()) {
  x <- glue::glue_collapse(x, "\n")
  x <- glue::glue(x, .envir = .envir)

  if (!is_interactive()) {
    cli::cli_abort(c(
      "User input required, but session is not interactive.",
      "Query: {x}"
    ))
  }

  n_yes <- min(n_yes, length(yes))
  n_no <- min(n_no, length(no))

  qs <- c(sample(yes, n_yes), sample(no, n_no))

  if (shuffle) {
    qs <- sample(qs)
  }

  cli::cli_inform(x)
  out <- utils::menu(qs)
  out != 0L && qs[[out]] %in% yes
}
