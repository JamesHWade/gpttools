# taken from https://gist.github.com/virattt/b140fb4bf549b6125d53aa153dc53be6

from transformers import AutoTokenizer, AutoModel

# Load the tokenizer and the model
tokenizer = AutoTokenizer.from_pretrained("colbert-ir/colbertv2.0")
model = AutoModel.from_pretrained("colbert-ir/colbertv2.0")
import torch
import time

start = time.time()
scores = []

# Function to compute MaxSim
def maxsim(query_embedding, document_embedding):
    # Expand dimensions for broadcasting
    # Query: [batch_size, query_length, embedding_size] -> [batch_size, query_length, 1, embedding_size]
    # Document: [batch_size, doc_length, embedding_size] -> [batch_size, 1, doc_length, embedding_size]
    expanded_query = query_embedding.unsqueeze(2)
    expanded_doc = document_embedding.unsqueeze(1)

    # Compute cosine similarity across the embedding dimension
    sim_matrix = torch.nn.functional.cosine_similarity(expanded_query, expanded_doc, dim=-1)

    # Take the maximum similarity for each query token (across all document tokens)
    # sim_matrix shape: [batch_size, query_length, doc_length]
    max_sim_scores, _ = torch.max(sim_matrix, dim=2)

    # Average these maximum scores across all query tokens
    avg_max_sim = torch.mean(max_sim_scores, dim=1)
    return avg_max_sim

# Encode the query
query_encoding = tokenizer(query, return_tensors='pt')
query_embedding = model(**query_encoding).last_hidden_state.mean(dim=1)

# Get score for each document
for document in docs:
    document_encoding = tokenizer(document.page_content, return_tensors='pt', truncation=True, max_length=512)
    document_embedding = model(**document_encoding).last_hidden_state

    # Calculate MaxSim score
    score = maxsim(query_embedding.unsqueeze(0), document_embedding)
    scores.append({
        "score": score.item(),
        "document": document.page_content,
    })

print(f"Took {time.time() - start} seconds to re-rank documents with ColBERT.")
# Sort the scores by highest to lowest and print
sorted_data = sorted(scores, key=lambda x: x['score'], reverse=True)
print(json.dumps(sorted_data, indent=2))
