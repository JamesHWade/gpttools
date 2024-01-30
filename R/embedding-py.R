#' Get Transformer Model
#'
#' This function is designed to download and load a pre-trained transformer
#' model using the transformers Python library via the reticulate package.
#' It checks for the availability of the required Python package and then
#' downloads the specified transformer model.
#'
#' @param model_name The name of the transformer model to download. This should
#' be in the format "username/modelname" as recognized by the transformers
#' library. Default is "BAAI/bge-small-en-v1.5".
#'
#' @return An object of the downloaded transformer model.
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
#'
#' @export
#'
get_transformer_model <-
  function(model_name = getOption(
             "gpttools.local_embed_model",
             "BAAI/bge-small-en-v1.5"
           )) {
    py_pkg_is_available()
    transformer <- reticulate::import("sentence_transformers")
    cli_process_start("Downloading model. This may take a few minutes.")
    model <- transformer$SentenceTransformer(model_name)
    cli_process_done()
    model
  }


create_text_embeddings <- function(text, model) {
  tibble::tibble(
    usage = "local",
    embedding = model$encode(text) |> as.numeric() |> list()
  )
}


colbert_rerank <- function(documents, model_name = "colbert-ir/colbertv2.0") {
  py_pkg_is_available("transformers")
  py_pkg_is_available("torch")
  py_pkg_is_available("time")

  # Import Python modules
  transformers <- reticulate::import("transformers")
  torch <- reticulate::import("torch")
  time <- reticulate::import("time")

  # Load the tokenizer and the model
  tokenizer <- transformers$AutoTokenizer$from_pretrained(model_name)
  model <- transformers$AutoModel$from_pretrained(model_name)

  start <- time$time()

  maxsim <- function(query_embedding, document_embedding) {
    expanded_query <- query_embedding$unsqueeze(2)
    expanded_doc <- document_embedding$unsqueeze(1)

    sim_matrix <- torch$nn$functional$cosine_similarity(expanded_query,
      expanded_doc,
      dim = -1L
    )

    max_sim_scores <- torch$max(sim_matrix, dim = 2L)$values
    avg_max_sim <- torch$mean(max_sim_scores, dim = 1L)
    return(avg_max_sim)
  }

  query_encoding <- tokenizer$encode(query, return_tensors = "pt")
  query_embedding <- model(query_encoding)$last_hidden_state$mean(dim = 1L)

  scores <- list()

  for (document in docs) {
    document_encoding <- tokenizer$encode(document$page_content,
      return_tensors = "pt",
      truncation = TRUE,
      max_length = 512L
    )
    document_embedding <- model(document_encoding)$last_hidden_state

    score <- maxsim(query_embedding$unsqueeze(0), document_embedding)
    scores[[length(scores) + 1]] <- list(
      score = score$item(),
      document = document$page_content
    )
  }

  print(paste0("Took ", time$time() - start, " seconds to re-rank documents with ColBERT."))
  sorted_data <- scores[order(sapply(scores, function(x) x$score), decreasing = TRUE)]
}

# from transformers import AutoTokenizer, AutoModel
#
# # Load the tokenizer and the model
# tokenizer = AutoTokenizer.from_pretrained("colbert-ir/colbertv2.0")
# model = AutoModel.from_pretrained("colbert-ir/colbertv2.0")
# import torch
# import time
#
# start = time.time()
# scores = []
#
# # Function to compute MaxSim
# def maxsim(query_embedding, document_embedding):
#   # Expand dimensions for broadcasting
#   # Query: [batch_size, query_length, embedding_size] -> [batch_size, query_length, 1, embedding_size]
#   # Document: [batch_size, doc_length, embedding_size] -> [batch_size, 1, doc_length, embedding_size]
#   expanded_query = query_embedding.unsqueeze(2)
# expanded_doc = document_embedding.unsqueeze(1)
#
# # Compute cosine similarity across the embedding dimension
# sim_matrix = torch.nn.functional.cosine_similarity(expanded_query, expanded_doc, dim=-1)
#
# # Take the maximum similarity for each query token (across all document tokens)
# # sim_matrix shape: [batch_size, query_length, doc_length]
# max_sim_scores, _ = torch.max(sim_matrix, dim=2)
#
# # Average these maximum scores across all query tokens
# avg_max_sim = torch.mean(max_sim_scores, dim=1)
# return avg_max_sim
#
# # Encode the query
# query_encoding = tokenizer(query, return_tensors='pt')
# query_embedding = model(**query_encoding).last_hidden_state.mean(dim=1)
#
# # Get score for each document
# for document in docs:
#   document_encoding = tokenizer(document.page_content, return_tensors='pt', truncation=True, max_length=512)
# document_embedding = model(**document_encoding).last_hidden_state
#
# # Calculate MaxSim score
# score = maxsim(query_embedding.unsqueeze(0), document_embedding)
# scores.append({
#   "score": score.item(),
#   "document": document.page_content,
# })
#
# print(f"Took {time.time() - start} seconds to re-rank documents with ColBERT.")
# # Sort the scores by highest to lowest and print
# sorted_data = sorted(scores, key=lambda x: x['score'], reverse=True)
# print(json.dumps(sorted_data, indent=2))
