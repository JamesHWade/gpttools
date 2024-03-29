% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/history.R
\name{chat_with_context}
\alias{chat_with_context}
\title{chat_with_context}
\usage{
chat_with_context(
  query,
  service = "openai",
  model = "gpt-4-turbo-preview",
  index = NULL,
  add_context = TRUE,
  chat_history = NULL,
  history_name = "chat_history",
  session_history = NULL,
  add_history = TRUE,
  task = "Context Only",
  k_context = 4,
  k_history = 4,
  save_history = TRUE,
  overwrite = FALSE,
  local = FALSE,
  embedding_model = NULL,
  stream = FALSE,
  rv = NULL
)
}
\arguments{
\item{query}{The input query to be processed.}

\item{service}{Name of the AI service to use, defaults to openai.}

\item{model}{Name of the openai model to use, defaults to gpt-3.5-turbo}

\item{index}{Index to look for context.}

\item{add_context}{Whether to add context to the query. Options are
\code{"always"}, \code{"sometimes"}, and \code{"never"}. The default is \code{"sometimes"}.}

\item{chat_history}{Chat history dataframe for reference.}

\item{history_name}{Name of the file where chat history is stored.}

\item{session_history}{Session history data for reference.}

\item{add_history}{Whether to add chat history to the query or not. Default
is TRUE.}

\item{task}{Task type, either "Context Only" or "Permissive Chat". Default is
"Context Only".}

\item{k_context}{Number of top context matches to consider. Default is 4.}

\item{k_history}{Number of top chat history matches to consider. Default is
4.}

\item{save_history}{Whether to save the chat history or not. Default is TRUE.}

\item{overwrite}{Whether to overwrite the history file or not. Default is
FALSE.}

\item{local}{Whether to use the local model or not. Default is FALSE.}

\item{embedding_model}{A model object to use for embedding. Only needed if
local is TRUE. Default is NULL.}

\item{stream}{Whether to stream the response or not. Default is FALSE.}

\item{rv}{A reactive value to store the response. Default is NULL.}
}
\value{
A list containing the prompt, context, and answer.
}
\description{
This function allows you to chat with a chatbot that answers questions based
on the provided context and chat history. It uses GPT-4 architecture to
generate responses.
}
\examples{
\dontshow{if (rlang::is_interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
rlang::is_interactive()
query <- "What is the capital of France?"
result <- chat_with_context(query = query, context = context)
\dontshow{\}) # examplesIf}
}
