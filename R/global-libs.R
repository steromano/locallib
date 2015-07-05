#' Activate global library
#' @param name(string) Name of the global library
#'
#' @export
use_global_lib <- function(name) {
  if (is.local_mode_on()) {
    error("a local library is activated. Restart R to activate a global library")
  }

  if (name %notin% global_libs()) {
    error("global library ", name, " doesn't exists. ",
          "Use create_global_lib() to create it.")
  }

  meta_data$global_mode_on <- TRUE

  lib_paths <- c(global_lib(name), .libPaths())
  .libPaths(lib_paths)
  invisible(TRUE)
}

#' Create a global library
#'
#' @param name
#'
#' @details Requires the environment variable `LOCALLIB_HOME` to be set
#' @export
create_global_lib <- function(name)  {
  if (is.local_mode_on()) {
    error("a local library is activated. Restart R to create a global library")
  }

  if (name %in% global_libs()) {
    warn("global library", name, "already exists")
    return(invisible())
  }

  meta_data$global_mode_on <- TRUE
  dir.create(global_lib(name), recursive = TRUE)
  invisible()
}

is.global_mode_on <- function() {
  meta_data$global_mode_on
}

global_lib <- function(name) {
  file.path(locallib_home(), name, "library")
}

#' List global libraries
#'
#' @export
global_libs <- function() {
  list.dirs(locallib_home(), recursive = FALSE, full.names = FALSE)
}

locallib_home <- function() {
  home_path <- Sys.getenv("LOCALLIB_HOME")
  if (home_path == "") {
    warn("Environment variable LOCALLIB_HOME is unset")
    return(NA)
  }
  if (!file.exists(home_path)) {
    error("Invalid LOCALLIB_HOME: ", home_path)
  }
  normalize_path(home_path)
}
