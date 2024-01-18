# get_transformer_model <- function(model_name = "jinaai/jina-embeddings-v2-base-en") {
#   py_pkg_is_available()
#   transformer <- reticulate::import("sentence_transformers")
#   cli::cli_process_start("Downloading model. This may take a few minutes.")
#   model <- transformer$SentenceTransformer(model_name)
#   cli::cli_process_done()
#   model
# }

# uses transformers instead of sentence transformers
get_transformer_model <- function(model_name = "jinaai/jina-embeddings-v2-base-en") {
  py_pkg_is_available("transformers")
  transformer <- reticulate::import("transformers")
  cli::cli_process_start("Downloading model. This may take a few minutes.")
  model <- transformer$AutoModel$from_pretrained(model_name,
    trust_remote_code = TRUE
  )
  cli::cli_process_done()
  model
}


create_text_embeddings <- function(text, model) {
  tibble::tibble(
    usage = "local",
    embedding = model$encode(text) |> as.numeric() |> list()
  )
}
