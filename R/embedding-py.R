get_transformer_model <- function(model_name = "all-mpnet-base-v2") {
  py_pkg_is_available()
  transformer <- reticulate::import("sentence_transformers")
  cli::cli_process_start("Downloading model. This may take a few minutes.")
  model <- transformer$SentenceTransformer(model_name)
  cli::cli_process_done()
  model
}

create_text_embeddings <- function(text, model) {
  model$encode(text) |> as.numeric()
}
