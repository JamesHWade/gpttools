#' Ingest PDF and write to index
#'
#' This function reads the text from a PDF file, processes it, and writes the
#' result to an index.
#'
#' @param file_path Character string specifying the path to the PDF file.
#' @param source Character string specifying the source of the PDF.
#' @param link Character string specifying the link to the PDF file (optional).
#' @return The function writes an index in Parquet format to disk.
#' @export
ingest_pdf <- function(file_path, source, link = NULL) {
  text <- pdftools::pdf_text(file_path) |>
    readr::read_lines() |>
    stringr::str_c(collapse = " ") |>
    remove_lines_and_spaces()

  tibble::tibble(
    text = text,
    source = source,
    link = link
  ) |>
    write_index(
      name = glue("{janitor::make_clean_names(source)}.parquet"),
      type = "text"
    )
}

#' Create index from PDF
#'
#' This function creates an index by first ingesting the PDF file and then
#' creating the index.
#'
#' @param file_path Character string specifying the path to the PDF file.
#' @param source Character string specifying the source of the PDF.
#' @param link Character string specifying the link to the PDF file (optional).
#' @param overwrite Logical, whether to overwrite the existing index (default:
#'   FALSE).
#' @return The function writes an index in Parquet format to disk.
#' @export
create_index_from_pdf <- function(file_path,
                                  source,
                                  link = NULL,
                                  overwrite = FALSE) {
  index_name <- janitor::make_clean_names(source)
  ingest_pdf(file_path, source, link)
  create_index(index_name, overwrite = overwrite)
}
