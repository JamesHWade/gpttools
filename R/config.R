#' Save user configuration settings for gpttools
#'
#' This function saves the user's configuration settings for gpttools service
#' to a YAML file.
#' @param service The name of the service to use, default is "openai".
#' @param model The model to use, default is "gpt-4-1106-preview".
#' @param task The task to perform, default is "Permissive Chat".
#' @param embeddings The location of embeddings, default is "local".
#' @param k_context The amount of context to keep, default is 4.
#' @param k_history The amount of history to keep, default is 4.
#' @param save_history Logical indicating whether history should be saved,
#' default is TRUE.
#' @param sources The sources to use, default is "All".
#' @param persist Logical indicating whether to persist the settings, default
#' is TRUE.
#' @return Invisible NULL.
#' @export
save_user_config <- function(service = "openai",
                             model = "gpt-4-1106-preview",
                             task = "Permissive Chat",
                             embeddings = "local",
                             k_context = 4,
                             k_history = 4,
                             save_history = TRUE,
                             sources = "All",
                             persist = TRUE) {
  ops <- tibble::tibble(
    service, model, task, embeddings,
    k_context, k_history, sources, save_history
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
      gpttools.local_embed = ops$embeddings,
      gpttools.k_context = ops$k_context,
      gpttools.k_history = ops$k_history,
      gpttools.save_history = ops$save_history,
      gpttools.sources = ops$sources
    )
    return(TRUE)
  } else {
    cli::cli_alert("No config file found. Using default config.")
    return(FALSE)
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
