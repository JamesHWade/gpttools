#' Document Data
#'
#' This function runs a Shiny application to view and document data.
#'
#' @export
document_data <- function() {
  check_api()
  withr::local_options(shiny.launch.browser = .rs.invokeShinyPaneViewer)
  shiny::runApp(system.file(package = "gpttools", "document_data"))
}

#' Collect Dataframes
#'
#' @description Collect all the dataframes in the global environment.
#'
#' @return A character vector of dataframe names.
#'
#' @export
collect_dataframes <- function() {
  objects <- names(rlang::global_env())
  purrr::map_chr(
    .x = objects,
    .f = ~ if (is.data.frame(get(.x))) { .x } else { NA }) |>
    stats::na.omit()
}

#' Summarize data
#'
#' Summarize a data frame using one of three methods.
#'
#' @param data A data frame
#' @param method A character vector specifying the method to use for summarizing the data.
#'   Must be one of "skimr", "glimpse", or "summary". Default is "skimr".
#'
#' @return Summarized data according to specified method
summarize_data <- function(data, method = c("skimr", "glimpse", "summary")) {
  assertthat::assert_that(is.data.frame(data))

  rlang::arg_match(method)

  switch(method[1],
         "skimr"   = skimr::skim_without_charts(data),
         "glimpse" = dplyr::glimpse(data, width = 80),
         "summary" = summary(data)
  )
}



#' @title Preps OpenAI model prompt for data documentation
#' @description
#'   Prepares data prompt by summarizing data and printing it
#'
#' @param data A data.frame
#' @param method A summarization method, one of "skimr", "glimpse", or "summary"
#' @param prompt A prompt string
#' @return A string
#' @export
#' @examples
#' prep_data_prompt(data = mtcars,
#'                  method = "skimr",
#'                  prompt = "This is a test prompt.")
prep_data_prompt <- function(data, method, prompt) {
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(assertthat::is.string(prompt))

  summarized_data <- summarize_data(data = data, method = method)

  paste(testthat::capture_output(print(summarized_data)), prompt, sep = "\n")

}
