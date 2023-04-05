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

transcribe_audio_chunk <- function(openai_api_key = Sys.getenv("OPENAI_API_KEY"),
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

transcribe_audio <- function(file_path, link = NULL, prompt = NULL) {
  audio_chunks <- split_audio(file_path = file_path, duration_secs = 180)
  purrr::map(audio_chunks, \(x) {
    tibble::tibble(
      text = transcribe_audio_chunk(audio_file = x, prompt = prompt),
      link = link,
      scraped = lubridate::today()
    )
  }, .progress = "Transcribing Text")
}

write_index <- function(index, name, type = "index") {
  dir <- file.path(tools::R_user_dir("gpttools", which = "data"), type)
  arrow::write_parquet(index, sink = file.path(dir, name))
}

# a <- transcribe_audio(
#   file_path = "~/Downloads/EP.249_-_Andrew_Huberman_FINAL.mp3",
#   link = "[The Drive - How the brain works with Andrew Huberman](https://peterattiamd.com/andrewhuberman/)",
#   prompt = "The Drive Podcast by Peter Attia with Andrew Huberman about How the brain works, Andrew’s fascinating backstory, improving scientific literacy, and more."
# )
#
# b <- a |>
#   dplyr::bind_rows() |>
#   tidyr::unnest(text) |>
#   dplyr::mutate(scraped = lubridate::today()) |>
#   dplyr::group_by(link, scraped) |>
#   dplyr::summarise(text = paste(text, collapse = " "), .groups = "drop")
#
# write_index(b, "huberman_pod.parquet", type = "text")

# a <-
#   load_index_dir("longevity") |>
#   dplyr::collect() |>
#   dplyr::group_by(file_name, transcription_date) |>
#   dplyr::rename(scraped = transcription_date) |>
#   dplyr::summarise(text = paste(text, collapse = " "), .groups = "drop") |>
#   dplyr::mutate(scraped = lubridate::as_datetime(scraped))
#
# # Define the login URL
# url_login <- "https://peterattiamd.com/login"
#
# # Get the login page
# session <- rvest::session(url_login)
#
# # Find the login form and fill in your credentials
# login_form <- session |> rvest::html_form() |> purrr::pluck(2)
# filled_form <- rvest::set_values(login_form,
#                                  log = "james.wade1221@gmail.com",
#                                  pwd = "b$A4SkN#Q93t")
#
# # Submit the login form
# logged_in <- rvest::session_submit(session, filled_form)
#
# # Define the URL of the page with the data
# url_data <- "https://peterattiamd.com/ama32"
#
# # Navigate to the data page
# data_page <- logged_in |> rvest::session_jump_to(url_data)
#
# # Extract the relevant data using CSS selectors
# exclude_tags <- c("style", "script", "head", "meta", "link", "button", "comments")
# data <- data_page |>
#   rvest::read_html() |>
#   rvest::html_nodes(xpath = paste("//body//*[not(self::",
#                                   paste(exclude_tags, collapse = " or self::"),
#                                   ")]",
#                                   sep = "")) |>
#   rvest::html_text2() |>
#   remove_new_lines()
