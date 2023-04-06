split_audio <- function(file_path, duration_secs = 500) {
  audio <- tuneR::readMP3(file_path)
  audio_length <- length(audio@left) / audio@samp.rate
  n_chunks <- ceiling(audio_length / duration_secs)
  chunks <- vector("list", n_chunks)

  for (i in 1:n_chunks) {
    start_sample <- (i - 1) * duration_secs * audio@samp.rate + 1
    end_sample <- min(i * duration_secs * audio@samp.rate, length(audio@left))
    chunk <- audio
    chunk@left <- audio@left[start_sample:end_sample]
    chunks[[i]] <- chunk
  }

  chunks
}

transcribe_audio_chunk <-
  function(openai_api_key = Sys.getenv("OPENAI_API_KEY"),
           audio_file,
           model = "whisper-1",
           prompt = NULL,
           language = "en") {
    url <- "https://api.openai.com/v1/audio/transcriptions"

    tmp_file <- tempfile(fileext = ".wav")
    tuneR::writeWave(audio_file, filename = tmp_file, extensible = FALSE)

    body <- list(
      file = httr::upload_file(tmp_file, type = "wav"),
      model = model
    )

    if (!is.null(prompt)) {
      body$prompt <- prompt
    }

    headers <- c(
      "Authorization" = glue::glue("Bearer {openai_api_key}"),
      "Content-Type" = "multipart/form-data"
    )

    response <-
      httr::RETRY("POST",
        url = url,
        httr::add_headers(headers),
        body = body,
        quiet = TRUE
      )

    result <- httr::content(response, "parsed", "application/json")

    file.remove(tmp_file)

    return(result)
  }

write_index <- function(index, name, type = "index") {
  dir <- file.path(tools::R_user_dir("gpttools", which = "data"), type)
  arrow::write_parquet(index, sink = file.path(dir, name))
}

#' Transcribe audio and write to index
#'
#' This function transcribes audio files, processes the text, and writes the
#' result to an index.
#'
#' @param file_path Character string specifying the path to the audio file.
#' @param source Character string specifying the source of the audio.
#' @param link Character string specifying the link to the audio file
#'   (optional).
#' @param prompt Character string specifying the prompt for transcription
#'   (optional).
#' @return The function writes an index in Parquet format to disk.
#' @export
transcribe_audio <- function(file_path, source, link = NULL, prompt = NULL) {
  audio_chunks <- split_audio(file_path = file_path, duration_secs = 180)
  purrr::map(audio_chunks, \(x) {
    tibble::tibble(
      source = source,
      text = transcribe_audio_chunk(audio_file = x, prompt = prompt),
      link = link,
      scraped = lubridate::today()
    )
  }, .progress = "Transcribing Text") |>
    dplyr::bind_rows() |>
    tidyr::unnest(text) |>
    dplyr::mutate(scraped = lubridate::today()) |>
    dplyr::group_by(link, scraped) |>
    dplyr::summarise(text = paste(text, collapse = " "), .groups = "drop") |>
    write_index(glue("{janitor::make_clean_name(source)}.parquet"),
      type = "text"
    )
}

#' Create index from audio
#'
#' This function creates an index by first transcribing the audio file and then
#' creating the index.
#'
#' @param file_path Character string specifying the path to the audio file.
#' @param source Character string specifying the source of the audio.
#' @param link Character string specifying the link to the audio file
#'   (optional).
#' @param overwrite Logical, whether to overwrite the existing index (default:
#'   FALSE).
#' @return The function writes an index in Parquet format to disk.
#' @export
create_index_from_audio <- function(file_path,
                                    source,
                                    link = NULL,
                                    overwrite = FALSE) {
  index_name <- janitor::make_clean_names(source)
  transcribe_audio(file_path, source, link)
  create_index(index_name, overwrite = overwrite)
}
