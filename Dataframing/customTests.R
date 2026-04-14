# Put custom tests in this file.

# Uncommenting the following line of code will disable
# auto-detection of new variables and thus prevent swirl from
# executing every command twice, which can slow things down.

# AUTO_DETECT_NEWVAR <- FALSE

# However, this means that you should detect user-created
# variables when appropriate. The answer test, creates_new_var()
# can be used for for the purpose, but it also re-evaluates the
# expression which the user entered, so care must be taken.

# ============================================================================
# CROSS-PLATFORM EXPRESSION ACCESS SHIM
# ============================================================================
# In Positron/Ark, e$expr contains base::.ark_last_value instead of the
# actual expression. This shim provides cross-platform expression access.

# Check if running in Ark/Positron
is_ark <- function() {
  req <- try(.Call("ps_get_active_request"), silent = TRUE)
  !inherits(req, "try-error")
}

# In Ark, expression output (execute_result) may be queued on a background
# I/O thread while swirl's cat() output (stream) runs on the main thread.
# Sleeping briefly inside an answer test gives the I/O thread time to
# dispatch execute_result before stream output from swirl_out() arrives,
# which increases the chance that expression output appears above feedback.
ark_flush <- function() {
  if (is_ark()) {
    flush.console()
    Sys.sleep(0.1)
  }
}

# Get the user's expression as a string (works in both Ark and RStudio)
get_expr_string <- function() {
  if (is_ark()) {
    # Ark/Positron: get from active request
    req <- .Call("ps_get_active_request")
    if (!is.null(req) && !is.null(req$code)) {
      return(req$code)
    }
  } else {
    # RStudio/standard R: get from swirl state
    # Search up the call stack for 'e'
    for (i in 1:sys.nframe()) {
      e <- try(get("e", sys.frame(i)), silent = TRUE)
      if (!inherits(e, "try-error") && !is.null(e$expr)) {
        return(paste(deparse(e$expr), collapse = ""))
      }
    }
  }
  NULL
}

# Get the user's expression as a parsed expression object
get_expr_parsed <- function() {
  expr_string <- get_expr_string()
  if (is.null(expr_string)) return(NULL)
  tryCatch(
    parse(text = expr_string)[[1]],
    error = function(e) NULL
  )
}

# ============================================================================
# SHIMMED ANSWER TESTS FOR CROSS-PLATFORM COMPATIBILITY
# ============================================================================
# These override swirl's built-in expression tests to work in both
# Positron/Ark and RStudio by using get_expr_*() instead of e$expr

#' Test that the user has entered a particular expression.
#' 
#' SHIMMED VERSION: Uses get_expr_parsed() instead of e$expr
expr_identical_to <- function(correct_expression){
  e <- get("e", parent.frame())
  ark_flush()
  
  # Get user's expression using cross-platform method
  expr <- get_expr_parsed()
  if (is.null(expr)) {
    # Fallback to e$expr if get_expr fails (shouldn't happen)
    expr <- e$expr
    if(is.expression(expr)) expr <- expr[[1]]
  }
  
  correct <- parse(text=correct_expression)[[1]]
  results <- expectThat(expr, 
                        is_identical_to_legacy(correct, label=correct_expression),
                        label=deparse(expr))
  if( is(e, "dev") && !results$passed) swirl_out(results$message) 
  return(results$passed)
}

#' Test that the user has entered one of several possible expressions.
#' 
#' SHIMMED VERSION: Uses shimmed expr_identical_to()
any_of_exprs <- function(...){
  e <- get("e", parent.frame())
  ark_flush()
  any(sapply(c(...), function(expr) expr_identical_to(expr)))
}

#' Test that a particular function has been used.
#' 
#' SHIMMED VERSION: Uses get_expr_parsed() instead of e$expr
expr_uses_func <- function(func) {
  e <- get("e", parent.frame())
  ark_flush()
  func <- str_trim(func)
  
  # Get user's expression using cross-platform method
  expr <- get_expr_parsed()
  if (is.null(expr)) {
    expr <- e$expr
  }
  
  results <- expectThat(expr,
                        uses_func(func, label=func), 
                        label=deparse(expr))
  if(is(e,"dev") && !results$passed) swirl_out(results$message)
  return(results$passed)
}

#' Test that the expression itself is of a specific class.
#' 
#' SHIMMED VERSION: Uses get_expr_parsed() instead of e$expr
expr_is_a <- function(class) {
  e <- get("e", parent.frame())
  ark_flush()
  class <- str_trim(class)
  
  # Get user's expression using cross-platform method
  expr <- get_expr_parsed()
  if (is.null(expr)) {
    expr <- e$expr
  }
  
  label <- deparse(expr)
  results <- expectThat(expr, is_a_legacy(class), label=label)
  if(is(e,"dev") && !results$passed) swirl_out(results$message)
  return(results$passed)
}

#' Simple test that user's expression matches expected string exactly
#' 
#' This is a simpler alternative to expr_identical_to() that just
#' compares the string representations directly.
expr_is <- function(correct_expr) {
  user_expr <- get_expr_string()
  if (is.null(user_expr)) {
    return(TRUE)  # Fallback if can't get expression
  }
  identical(user_expr, correct_expr)
}