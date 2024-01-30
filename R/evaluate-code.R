#' Extract Code Chunks from Text
#'
#' This function extracts code chunks that are enclosed in triple backticks from
#' a given text input. It supports identifying code blocks that are indicated by
#' triple backticks followed by an optional language identifier. The function
#' returns the extracted code chunks.
#'
#' @param text A character vector containing the text from which code chunks
#'   need to be extracted.
#'
#' @return A list containing the extracted code chunks.
#'
#' @examples
#' sample_text <- "Here is some text.
#' ```
#' This is a code chunk.
#' ```"
#' extract_code_chunks(sample_text)
#'
#' @export
extract_code_chunks <- function(text) {
  # Pattern to match code chunks enclosed in triple backticks
  pattern <- "```[a-z]*\\n([\\s\\S]*?)\\n```"
  # Extract code chunks
  code_chunks <- stringr::str_match_all(text, pattern)[[1]][, 2]
  return(code_chunks)
}

#' Execute and Present Extracted Code Chunks
#'
#' This function takes extracted code chunks and utilizes the
#' `reprex` packages to run them. It then formats the output into a collapsible
#' HTML section for easy sharing and viewing. Images within the output are
#' converted to clickable links to enhance accessibility and compactness of the
#' presentation.
#'
#' @param code A character vector of code to be executed and presented. The code
#'   is run with `reprex::reprex()`, capturing the outcome for presentation.
#'
#' @return An HTML-formatted string that includes the collapsible section for
#'   the code's output. This HTML string can be embedded in web pages, markdown
#'   documents, or viewed directly in browsers. The function is designed to work
#'   seamlessly in interactive environments, hence it might return the output
#'   invisibly.
#'
#' @examplesIf rlang::is_interactive()
#' code <- "plot(1:10)"
#' html_output <- run_extracted_code(code)
#' # The returned value is an HTML string with the plotted output presented within a
#' # collapsible section and any images replaced by links.
#'
#' @export
run_extracted_code <- function(code) {
  rlang::check_installed("reprex")
  reprex::reprex(x = paste0(code, "\n"), venue = "html") |>
    paste0(collapse = "\n") |>
    insert_collapsible_section() |>
    replace_image_with_link() |>
    invisible()
}

insert_collapsible_section <- function(html, summary_text = "Show/Hide Content") {
  # Define the opening part of the collapsible section
  collapsible_start <- paste0("<details><summary>", summary_text, "</summary>")

  # Insert the opening part after </head>
  html_with_start <- stringr::str_replace(html, "</head>", paste0("</head>", collapsible_start))

  # Append </details> at the end of the HTML
  html_with_end <- paste0(html_with_start, "</details>")

  return(html_with_end)
}

replace_image_with_link <- function(html) {
  # Define the pattern to match the <img> tag
  img_pattern <- "<img\\s+[^>]*src=\"([^\"]*)\"[^>]*>"

  # Define the replacement pattern with an anchor tag
  replacement_pattern <- "<a href=\"\\1\">Click to see generated image output</a>"

  # Replace the <img> tag with the anchor tag
  new_html <- stringr::str_replace_all(html, img_pattern, replacement_pattern)

  return(new_html)
}
