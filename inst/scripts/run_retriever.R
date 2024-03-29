rlang::is_installed("later")
app_dir <- system.file("retriever/app.R", package = "gpttools")
port <- httpuv::randomPort()
app_url <- glue::glue("http://127.0.0.1:{port}")
later::later(\() rstudioapi::viewer(app_url), delay = 1)
shiny::runApp(app_dir, port = port, launch.browser = FALSE)
