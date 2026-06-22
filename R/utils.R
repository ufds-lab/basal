# I found some functions that may be useful from the saeczi package

#' Extract all matches from a string
#' @noRd
str_extract_all_base <- function(string, pattern) {
  regmatches(string, gregexpr(pattern, string))
}

#' Checking if a parameter input inherits a specific class
#' @noRd
check_inherits <- function(what, ...) {
  opts <- list(...)
  for (i in seq_along(opts)) {
    if (!is.null(opts[[i]])) {
      if (!inherits(opts[[i]], what)) {
        stop(paste0(opts[[i]], " needs to be of class ", what))
      }
    } else {
      stop("unable to check NULL objects")
    }
  }
  invisible(opts)
}

#' Fast aggregation
#' @noRd
agg_stat <- function(vals, nms, .f) {
  agg <- tapply(vals, nms, .f)
  out <- data.frame(nms = names(agg), vals = agg)
  out
}

#' Extract variables (i.e., resposne and covariates) from model formula
#' @noRd
extract_variables <- function(formula) {
  # extract the variables
} 
