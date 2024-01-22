# nolint
# get_transformer_model <-
# function(model_name = "jinaai/jina-embeddings-v2-base-en") {
#   py_pkg_is_available()
#   transformer <- reticulate::import("sentence_transformers")
#   cli::cli_process_start("Downloading model. This may take a few minutes.")
#   model <- transformer$SentenceTransformer(model_name)
#   cli::cli_process_done()
#   model
# }
# nolint end

#' Get Transformer Model
#'
#' This function is designed to download and load a pre-trained transformer
#' model using the transformers Python library via the reticulate package.
#' It checks for the availability of the required Python package and then
#' downloads the specified transformer model.
#'
#' @param model_name The name of the transformer model to download. This should
#' be in the format "username/modelname" as recognized by the transformers
#' library. Default is "jinaai/jina-embeddings-v2-base-en".
#'
#' @return An object of the downloaded transformer model.
#'
#' @export
#'
#' @note Users of this function need to ensure that the Python environment
#' is set up with the 'transformers' package installed. The function uses
#' the 'reticulate' R package to interface with Python and the user may need
#' to configure it accordingly.
#'
#' @examples
#' \dontrun{
#' # To get the default transformer model:
#' get_transformer_model()
#'
#' # To get a custom transformer model by specifying the model name:
#' get_transformer_model("bert-base-uncased")
#' }
get_transformer_model <-
  function(model_name = "jinaai/jina-embeddings-v2-base-en") {
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
