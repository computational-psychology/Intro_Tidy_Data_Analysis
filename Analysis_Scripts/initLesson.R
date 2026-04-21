# ============================================================================
# POSITRON/ARK COMPATIBILITY PATCH FOR Class: script QUESTIONS
#
# In Positron/Ark, the task callback always receives
# base::invisible(base::.ark_last_value) as the expression, regardless of
# what the user typed. This means swirl's uses_func("submit") check always
# fails, so submit() appears to do nothing.
#
# Fix:
#   1. Override file.edit() to capture swirl's internal state environment
#      (e) from the call stack and store it as .swirl_e in globalenv.
#      Also open the file via rstudioapi::navigateToFile().
#   2. Override submit() to perform swirl's do_submit logic directly using
#      the captured .swirl_e, bypassing the broken expr detection.
# ============================================================================

# Only apply the patch in Positron/Ark (identified by ps_get_active_request)
.is_ark <- function() !inherits(try(.Call("ps_get_active_request"), silent = TRUE), "try-error")

if (.is_ark()) {

file.edit <- function(..., title = NULL) {
  # Capture swirl's state environment from the call stack (set by waitUser.script)
  for (i in seq(sys.nframe() - 1L, 1L, by = -1L)) {
    fr      <- sys.frame(i)
    maybe_e <- tryCatch(get("e", envir = fr, inherits = FALSE), error = function(x) NULL)
    if (is.environment(maybe_e) &&
        exists("playing", envir = maybe_e, inherits = FALSE) &&
        exists("iptr",    envir = maybe_e, inherits = FALSE)) {
      .swirl_e <<- maybe_e
      break
    }
  }
  # Open the file in the editor
  files <- c(...)
  if (length(files) > 0 &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      isTRUE(rstudioapi::isAvailable())) {
    rstudioapi::navigateToFile(files[[1]])
  }
  invisible(NULL)
}

submit <- function() {
  if (!exists(".swirl_e", envir = .GlobalEnv) || !is.environment(.GlobalEnv$.swirl_e)) {
    return(invisible())
  }
  e <- .GlobalEnv$.swirl_e
  # Ensure course_name attribute is non-empty (may be missing in test/dev installs)
  # to prevent swirl's testMe() from erroring on: if (oldcourse) { ... }
  if (length(attr(e$les, "course_name")) == 0) {
    les_tmp <- e$les
    attr(les_tmp, "course_name") <- "unknown"
    e$les <- les_tmp
  }
  # Replicate do_submit.default: set state, read script, source it
  e$playing         <- FALSE
  e$script_contents <- readLines(e$script_temp_path, warn = FALSE)
  e$expr            <- try(parse(text = e$script_contents), silent = TRUE)
  swirl:::swirl_out("Sourcing your script...", skip_after = TRUE)
  try(source(e$script_temp_path, encoding = "UTF-8"))
  invisible()
}

} # end .is_ark() patch
