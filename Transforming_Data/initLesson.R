suppressPackageStartupMessages(library(tidyverse))

# ============================================================================
# POSITRON/ARK COMPATIBILITY PATCH FOR Class: script QUESTIONS
# ============================================================================
.is_ark <- function() {
  !inherits(try(.Call("ps_get_active_request"), silent = TRUE), "try-error")
}

if (.is_ark()) {
  file.edit <- function(..., title = NULL) {
    for (i in seq(sys.nframe() - 1L, 1L, by = -1L)) {
      fr <- sys.frame(i)
      maybe_e <- tryCatch(
        get("e", envir = fr, inherits = FALSE),
        error = function(x) NULL
      )
      if (
        is.environment(maybe_e) &&
          exists("playing", envir = maybe_e, inherits = FALSE) &&
          exists("iptr", envir = maybe_e, inherits = FALSE)
      ) {
        .swirl_e <<- maybe_e
        break
      }
    }
    files <- c(...)
    if (
      length(files) > 0 &&
        requireNamespace("rstudioapi", quietly = TRUE) &&
        isTRUE(rstudioapi::isAvailable())
    ) {
      rstudioapi::navigateToFile(files[[1]])
    }
    invisible(NULL)
  }

  submit <- function() {
    if (
      !exists(".swirl_e", envir = .GlobalEnv) ||
        !is.environment(.GlobalEnv$.swirl_e)
    ) {
      return(invisible())
    }
    e <- .GlobalEnv$.swirl_e
    if (length(attr(e$les, "course_name")) == 0) {
      les_tmp <- e$les
      attr(les_tmp, "course_name") <- "unknown"
      e$les <- les_tmp
    }
    e$playing <- FALSE
    e$script_contents <- readLines(e$script_temp_path, warn = FALSE)
    e$expr <- try(parse(text = e$script_contents), silent = TRUE)
    swirl:::swirl_out("Sourcing your script...", skip_after = TRUE)
    try(source(e$script_temp_path, encoding = "UTF-8"))
    invisible()
  }
} # end .is_ark() patch
