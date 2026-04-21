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

ark_flush <- function() {
  if (is_ark()) {
    flush.console()
    Sys.sleep(0.05)
  }
}

get_expr_string <- function() {
  if (is_ark()) {
    req <- .Call("ps_get_active_request")
    if (!is.null(req) && !is.null(req$code)) {
      return(req$code)
    }
  } else {
    for (i in 1:sys.nframe()) {
      e <- try(get("e", sys.frame(i)), silent = TRUE)
      if (!inherits(e, "try-error") && !is.null(e$expr)) {
        return(paste(deparse(e$expr), collapse = ""))
      }
    }
  }
  NULL
}

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

expr_identical_to <- function(correct_expression){
  e <- get("e", parent.frame())
  ark_flush()
  expr <- get_expr_parsed()
  if (is.null(expr)) {
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

any_of_exprs <- function(...){
  e <- get("e", parent.frame())
  ark_flush()
  any(sapply(c(...), function(expr) expr_identical_to(expr)))
}

expr_uses_func <- function(func) {
  e <- get("e", parent.frame())
  ark_flush()
  func <- str_trim(func)
  expr <- get_expr_parsed()
  if (is.null(expr)) expr <- e$expr
  results <- expectThat(expr, uses_func(func, label=func), label=deparse(expr))
  if(is(e,"dev") && !results$passed) swirl_out(results$message)
  return(results$passed)
}

expr_is_a <- function(class) {
  e <- get("e", parent.frame())
  ark_flush()
  class <- str_trim(class)
  expr <- get_expr_parsed()
  if (is.null(expr)) expr <- e$expr
  label <- deparse(expr)
  results <- expectThat(expr, is_a_legacy(class), label=label)
  if(is(e,"dev") && !results$passed) swirl_out(results$message)
  return(results$passed)
}

val_equals <- function(correct_val) {
  e <- get("e", parent.frame())
  ark_flush()
  isTRUE(all.equal(e$val, correct_val, check.attributes = FALSE))
}

expr_is <- function(correct_expr) {
  user_expr <- get_expr_string()
  if (is.null(user_expr)) return(TRUE)
  identical(user_expr, correct_expr)
}
