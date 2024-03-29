% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/index.R
\name{gpttools_index_all_scraped_data}
\alias{gpttools_index_all_scraped_data}
\title{Index All Scraped Data}
\usage{
gpttools_index_all_scraped_data(
  overwrite = FALSE,
  local_embeddings = TRUE,
  dont_ask = TRUE
)
}
\arguments{
\item{overwrite}{A logical value determining whether to overwrite existing
indexes.}

\item{local_embeddings}{A logical indicating whether to use local embeddings
for indexing.}

\item{dont_ask}{A logical value that, if TRUE, disables interactive
confirmation prompts during the indexing process.}
}
\value{
Invisible NULL. The function is called for its side effects.
}
\description{
This function iterates through all the text files in a specified directory,
updating or creating indexes for each domain contained in the file names.
Allows customization of the indexing process through various parameters.
}
\details{
The function first retrieves a list of all text files in the
targeted directory. For each file, it extracts the domain name from the
filename, prints an informative message about the indexing process for that
domain, and then proceeds to create or update the index for the domain
based on the function arguments.
}
\examples{
# Index all scraped data without overwriting existing indexes, using local
# embeddings, and without interactive prompts.

\dontrun{
gpttools_index_all_scraped_data(
  overwrite = FALSE,
  local_embeddings = TRUE,
  dont_ask = TRUE
)
}

}
