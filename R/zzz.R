.onLoad <- function(lib, pkg) {
  user_config <- set_user_config()

  if (rlang::is_false(user_config)) {
    op <- options()
    op_gpttools <- list(
      gpttools.service            = "openai",
      gpttools.model              = "gpt-4-turbo-preview",
      gpttools.local_embed        = TRUE,
      gpttools.local_embed_model  = "BAAI/bge-large-en-v1.5",
      gpttools.task               = "Permissive Chat",
      gpttools.k_context          = 4,
      gpttools.k_history          = 4,
      gpttools.save_history       = FALSE,
      gpttools.sources            = "All",
      gpttools.openai_embed_model = "text-embedding-3-small"
    )

    toset <- !(names(op_gpttools) %in% names(op))
    if (any(toset)) options(op_gpttools[toset])
  }

  pkgs <- load_pkgs_to_scrape()
  if (is.null(pkgs)) {
    pkg_op <- !("gpttools.pkgs" %in% names(options()))
    if (pkg_op) options(gpttools.pkgs = use_default_pkgs())
  } else {
    options(gpttools.pkgs = pkgs)
  }
  invisible()
}


global_vars <- c(
  "Package",
  "ReposVer",
  "Version",
  "chunks",
  "content",
  "embedding",
  "embeddings",
  "head",
  "installed.packages",
  "indexed_version",
  "installed_version",
  "link",
  "name",
  "old.packages",
  "package",
  "parent",
  "run_code",
  "scraped",
  "similarity",
  "text",
  ".rs.invokeShinyPaneViewer"
)

utils::globalVariables(global_vars)
