find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (dir.exists(file.path(current, ".git")) && dir.exists(file.path(current, "data"))) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Project root was not found. Run scripts from this repository or a subdirectory.", call. = FALSE)
    }
    current <- parent
  }
}

PROJECT_ROOT <- find_project_root()

project_file <- function(...) {
  file.path(PROJECT_ROOT, ...)
}

ensure_output_dirs <- function() {
  for (path in c(project_file("results"), project_file("figures"))) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
  }
}

load_required_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing) > 0) {
    stop(
      "Missing R package(s): ",
      paste(missing, collapse = ", "),
      "\nInstall them from the README before running this script.",
      call. = FALSE
    )
  }

  suppressPackageStartupMessages(
    invisible(lapply(packages, library, character.only = TRUE))
  )
}

check_columns <- function(data, required_columns, label) {
  missing <- setdiff(required_columns, names(data))

  if (length(missing) > 0) {
    stop(
      label,
      " is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}
