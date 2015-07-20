#' Set a local drat repo
#'
#' @export
set_local_drat_repo <- function(path = NULL) {
  path <- suppressWarnings(normalizePath(path %||% "~/git/drat"))

  base <- basename(path)
  dir <- dirname(path)

  if (!file.exists(dir)) {
    message("creating base path for drat repo ", bracket(dir))
    dir.create(dir, recursive = TRUE)
  }

  if (!file.exists(path)) {
    message("intialising drat repo ", bracket(path))
    drat::initRepo(base, dir)
  } else {
    if (!is.valid_local_drat_repo(path)) {
      error("local drat repo is not valid ", bracket(path))
    }
  }

  message("setting local drat repo ", bracket(path))
  options(dratRepo = path)
  meta_data$repo_path <- path
}


is.valid_local_drat_repo <- function(path) {
  # FIXME: implement this function
  TRUE
}


## Temporary fix for supporting setting the drat repo via environment variable
## TODO: improve this
is.local_drat_repo_set <- function() {
  drat_repo <- Sys.getenv("DRAT_REPO")
  if (file.exists(drat_repo) && file.info(drat_repo)$isdir) {
    meta_data$repo_path <- drat_repo
  } 
  !is.null(meta_data$repo_path)
}


local_drat_repo <- function() {
  meta_data$repo_path
}
