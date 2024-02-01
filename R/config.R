#' Save user configuration settings for gpttools
#'
#' This function saves the user's configuration settings for gpttools service
#' to a YAML file.
#' @param service The name of the service to use, default is "openai".
#' @param model The model to use, default is "gpt-4-1106-preview".
#' @param task The task to perform, default is "Permissive Chat".
#' @param local_embed Whether to use local embedding model. Default is "yes".
#' @param openai_embed_model The OpenAI embeddings model to use, default is
#'  "text-embedding-3-small".
#' @param local_embed_model The local embeddings model to use, default is
#' "BAAI/bge-small-en-v1.5".
#' @param k_context The amount of context to keep, default is 4.
#' @param k_history The amount of history to keep, default is 4.
#' @param save_history Logical indicating whether history should be saved,
#' default is TRUE.
#' @param sources The sources to use, default is "All".
#' @param run_code Whether to execute generated code with `reprex::reprex()`
#' @param persist Logical indicating whether to persist the settings, default
#' is TRUE.
#' @return Invisible NULL.
#' @export
save_user_config <- function(service = "openai",
                             model = "gpt-4-turbo-preview",
                             task = "Permissive Chat",
                             local_embed = "Yes",
                             openai_embed_model = "text-embedding-3-small",
                             local_embed_model = "BAAI/bge-small-en-v1.5",
                             k_context = 4,
                             k_history = 4,
                             save_history = "Yes",
                             sources = "All",
                             run_code = "No",
                             persist = TRUE) {
  ops <- tibble::tibble(
    service, model, task, embeddings, openai_embed_model, local_embed_model,
    k_context, k_history, sources, run_code, save_history
  )

  if (persist == TRUE) {
    # save file to R user dir
    dir <- tools::R_user_dir("gpttools", which = "config")
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    path <- file.path(dir, "options.yml")
    yaml::write_yaml(ops, path)
  }

  set_user_config(path)
}

#' Set user configuration settings for gpttools from a file
#'
#' This function sets the user's configuration settings for gpttools by reading
#' a YAML file. If no path is provided, it will look for the file in the default
#'  R user config directory.
#' @param path The path to the config YAML file. If NULL, the default path is
#' used.
#' @return Invisible NULL.
#' @export
set_user_config <- function(path = NULL) {
  if (is.null(path)) {
    path <- file.path(
      tools::R_user_dir("gpttools", which = "config"),
      "options.yml"
    )
  }

  if (file.exists(path)) {
    ops <- yaml::read_yaml(path)
    options(
      gpttools.service = ops$service,
      gpttools.model = ops$model,
      gpttools.task = ops$task,
      gpttools.local_embed = ops$local_embed,
      gpttools.k_context = ops$k_context,
      gpttools.k_history = ops$k_history,
      gpttools.save_history = ops$save_history,
      gpttools.sources = ops$sources,
      gpttools.openai_embed_model = ops$openai_embed_model,
      gpttools.local_embed_model = ops$local_embed_model,
      gpttools.run_code = ops$run_code
    )
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}

save_pkgs_to_scrape <- function(pkgs) {
  dir <- tools::R_user_dir("gpttools", which = "config")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "pkgs_to_scrape.rds")
  saveRDS(pkgs, path)
  TRUE
}

load_pkgs_to_scrape <- function() {
  path <- file.path(
    tools::R_user_dir("gpttools", which = "config"),
    "pkgs_to_scrape.rds"
  )
  if (file.exists(path)) {
    pkgs <- readRDS(path)
    return(pkgs)
  } else {
    invisible(NULL)
  }
}
