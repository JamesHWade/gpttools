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

code <- 'library(ggplot2)

ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(aes(color=class)) +
  geom_smooth(method="lm", se=FALSE, color="black") +
  labs(title="Engine Displacement vs. Highway MPG",
       x="Engine Displacement (liters)",
       y="Highway Miles per Gallon",
       color="Vehicle Class") +
  theme_minimal()'

run_extracted_code <- function(code) {
  clipr::write_clip(code)
  invisible(reprex::reprex(venue = "html") |>
    paste0(collapse = "\n") |>
    insert_collapsible_section() |>
    replace_image_with_link())
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
