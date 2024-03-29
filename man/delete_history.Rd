% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/history.R
\name{delete_history}
\alias{delete_history}
\title{Delete chat history files}
\usage{
delete_history(local = FALSE)
}
\arguments{
\item{local}{Whether to delete history made with local or OpenAI embeddings.}
}
\description{
This function interactively deletes chat history files stored in the user's
directory. It lists all the .parquet files in the gpttools data directory and
prompts the user for confirmation before deleting each file.
}
\examples{
\dontrun{
# Interactively delete chat history files
delete_history()
}
}
