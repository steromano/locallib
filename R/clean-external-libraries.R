#' Clean global libraries
#'
#' @export
clean_external_libs <- function() {
  paths <- unlist(lapply(external_libs(), list.files, full.names = TRUE))
  paths <- Filter(function(path) basename(path) %notin% external_pkgs(), paths)

  if (length(paths) == 0) {
    message("External libraries already clean")
    return(invisible())
  }

  message(
    paste0(
      "The following packages will be removed:\n",
      paste(" -", basename(paths), bracket(dirname(paths)), sep = " ", collapse = "\n")
    )
  )

  ans <- readline("Are you sure you want to continue? [y/n]")

  if (ans != "y") {
    message("no packages will be deleted")
    return(invisible())
  }

  message("cleaning external library(s)")

  for (path in paths) {
    pkg <- basename(path)
    lib <- dirname(path)

    message("- removing package ", pkg, " ", bracket(lib))

    tryCatch(
      remove.packages(pkg, lib),
      error = function(e) {
        message("  * unable to remove package")
      }
    )
  }
}

external_pkgs <- function() {
  c(BASE_AND_RECOMMENDED, "drat", "git2r", "locallib", "yaml")
}


external_libs <- function() {
  out <- .libPaths()

  ll_home <- locallib_home()
  if (!is.na(ll_home)) {
    out <- Filter(function(x) !grepl(ll_home, x), out)
  }

  if (is.local_mode_on()) {
    out <- Filter(function(x) x != local_lib(), out)
  }

  out
}




