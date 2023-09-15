parse_sentence_transformers_version <- function(version) {
  # returns unquoted string directly passable to pip
  # e.g 'sentence-transformers==2.1.*'

  if (is.null(version) || is.na(version) || version %in% c("", "release")) {
    return("sentence-transformers")
  }

  version <- as.character(version) # if numeric_version()

  if (grepl("^.*\\.whl$", version)) {
    return(normalizePath(version))
  }

  if (!grepl("[><=]", version)) {
    version <- sprintf("==%s.*", version)
  }

  paste0("sentence-transformers", version)
}

#' Install sentence-transformers and its dependencies
#'
#' `install_sentence_transformers()` installs the sentence-transformers python package and its
#' direct dependencies. This code is taken and only slightly modified from the tensorflow and
#' reticulate packages. See the original code here: [tensorflow::install_tensorflow()].
#'
#' @details You may be prompted to download and install
#'   miniconda if reticulate did not find a non-system installation of python.
#'   Miniconda is the recommended installation method for most users, as it
#'   ensures that the R python installation is isolated from other python
#'   installations. All python packages will by default be installed into a
#'   self-contained conda or venv environment named "r-reticulate". Note that
#'   "conda" is the only supported method on M1 Mac.
#
#'   If you initially declined the miniconda installation prompt, you can later
#'   manually install miniconda by running [`reticulate::install_miniconda()`].
#'
#' @section Custom Installation: `install_sentence_transformers()` or isn't
#'   required to use tensorflow with the package.
#'   If you manually configure a python environment with the required
#'   dependencies, you can tell R to use it by pointing reticulate at it,
#'   commonly by setting an environment variable:
#'
#'   ``` R
#'   Sys.setenv("RETICULATE_PYTHON" = "~/path/to/python-env/bin/python")
#'   ```
#'
#' @section Additional Packages:
#'
#'   If you wish to add additional PyPI packages to your Keras / TensorFlow
#'   environment you can either specify the packages in the `extra_packages`
#'   argument of `install_sentence_transformers()`, or alternatively
#'   install them into an existing environment using the
#'   [reticulate::py_install()] function.
#'
#' @inheritParams reticulate::py_install
#'
#' @param version version to install. Valid values include:
#'
#'   + `"release"` installs the latest release version of sentence-transformers
#'    (which may be incompatible with the current version of the R package)
#'
#'   + A version specification like `"2.4"` or `"2.4.0"`. Note that if the patch
#'   version is not supplied, the latest patch release is installed (e.g.,
#'   `"2.4"` today installs version "2.4.2")
#'
#'   + `nightly` for the latest available nightly build.
#'
#'   + The full URL or path to a installer binary or python *.whl file.
#'
#' @param extra_packages Additional Python packages to install along with
#'   TensorFlow.
#'
#' @param restart_session Restart R session after installing (note this will
#'   only occur within RStudio).
#'
#' @param python_version,conda_python_version Pass a string like "3.8" to
#'   request that conda install a specific Python version. This is ignored when
#'   attempting to install in a Python virtual environment.
#'
#' @param pip_ignore_installed Whether pip should ignore installed python
#'   packages and reinstall all already installed python packages. This defaults
#'   to `TRUE`, to ensure that TensorFlow dependencies like NumPy are compatible
#'   with the prebuilt TensorFlow binaries.
#'
#' @param ... other arguments passed to `reticulate::conda_install()` or
#'   `reticulate::virtualenv_install()`, depending on the `method` used.
#'
#' @export
install_sentence_transformers <-
  function(method = c("auto", "virtualenv", "conda"),
           conda = "auto",
           version = NULL,
           envname = NULL,
           extra_packages = NULL,
           restart_session = TRUE,
           conda_python_version = NULL,
           ...,
           pip_ignore_installed = TRUE,
           python_version = conda_python_version) {
    check_installed("reticulate")
    method <- match.arg(method)

    packages <- unique(c(
      parse_sentence_transformers_version(version),
      as.character(extra_packages)
    ))

    # don't double quote if packages were shell quoted already
    packages <- shQuote(gsub("[\"']", "", packages))

    reticulate::py_install(
      packages = packages,
      envname = envname,
      method = method,
      conda = conda,
      python_version = python_version,
      pip = TRUE,
      pip_ignore_installed = pip_ignore_installed,
      ...
    )

    cat("\nInstallation complete.\n\n")

    if (restart_session &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::hasFun("restartSession")) {
      rstudioapi::restartSession()
    }

    invisible(NULL)
  }

py_pkg_is_available <- function(pkg = "sentence-transformers") {
  check_installed("reticulate")
  found_pkg <-
    reticulate::py_list_packages() |> dplyr::filter(package == pkg)

  if (nrow(found_pkg) == 0) {
    py_details <- reticulate::py_config()
    cli::cli_abort(
      c(
        "!" = "Python package `{pkg}` not found.",
        "i" = "Python envrionment: {py_details$prefix}",
        "i" = "Envrionment set by {py_details$forced}",
        "i" = "See {.url https://rstudio.github.io/reticulate/} for help."
      )
    )
  } else {
    cli::cli_alert_success("`{pkg}` package found.")
  }
}
