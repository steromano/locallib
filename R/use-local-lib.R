#' Activate a local library
#'
#' @param path(string) Path to directory containing a local library.
#'
#' @export
use_local_lib <- function(path = NULL) {
  path <- normalize_path(path)

  if (is.activated()) {
    if (meta_data$path == "default") {
      message("default local library is currently activated ", bracket(default_lib()))
      return(invisible())
    }
    if (path == meta_data$path) {
      message("local library already activated ", bracket(local_lib()))
      return(invisible())
    }

    error(
      "a different local library is already activated ", bracket(local_lib()), ". ",
      "Restart R to activate a new one"
    )
  }

  if (!has_local_lib(path)) {
    default_lib_path <- tryCatch(
      default_lib(),
      error = function(e) {
        warn(e$message)
        error("local library not found, with no default available")
      }
    )
    message("activating default local library ", bracket(default_lib_path))
    .libPaths(c(default_lib_path, .libPaths()))

    meta_data$activated <- TRUE
    meta_data$path <- "default"

    return(invisible())
  }

  message("activating local library ", bracket(path))

  lib_path <- file.path(path, "library")
  .libPaths(c(lib_path, .libPaths()))

  meta_data$activated <- TRUE
  meta_data$path <- path

  invisible()
}


is.activated <- function() {
  meta_data$activated
}


# meta data ---------------------------------------------------------------


meta_data <- new.env()
meta_data$activated <- FALSE
meta_data$path <- NULL
meta_data$repo_path <- NULL


# default library ---------------------------------------------------------
default_lib <- function() {
  locallib_home <- Sys.getenv("LOCALLIB_HOME")
  if(locallib_home == "") {
    error("Environment variable LOCALLIB_HOME is unset")
  }
  if(!file.exists(locallib_home) || !file.info(locallib_home)$isdir) {
    error("LOCALLIB_HOME: ", locallib_home, " is not a valid path")
  }
  lib_path <- normalizePath(file.path(locallib_home, "library"), mustWork = FALSE)
  dir.create(lib_path, showWarnings = FALSE)
  lib_path
}

