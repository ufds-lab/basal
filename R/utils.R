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

#' Apply HT estimators to different domains
#' @noRd
agg_HT <- function(data, res, N, domain, agg_data = NULL) {
  if (is.null(agg_data)) {
    make_aggs <- T
    agg_data <- data[1,]
  }
  if ("BASAL_HT_ESTIMATOR" %in% colnames(data) ||
      "BASAL_HT_SE" %in% colnames(data) ||
      "BASAL_N" %in% colnames(data)) {
    stop("Variables with 'BASAL' prefix are protected. Please rename these.")
  }
  agg_data$`BASAL_HT_ESTIMATOR` <- NA
  agg_data$`BASAL_HT_SE` <- NA
  agg_data$`BASAL_N` <- NA
  
  unique_domains <- unique(data[[domain]])
  for (i in 1:length(unique_domains)) {
    thedomain <- unique_domains[i]
    thedata <- data[data[[domain]] == thedomain,]
    est <- 
      mase::horvitzThompson(y = thedata[[res]],
                      N = N,
                      var_est = T,
                      messages = F)
    if (make_aggs) {
      agg_data[i,] <- c(
        lapply(thedata, function(x) {
          if (is.numeric(x)) {
            return (mean(x))
          } else {
            levels <- unique(x)
            if (length(levels) != 1) {
              warning(
                "Detected multiple levels in non-numeric data. Arbitrarily choosing a value."
              )
            }
            return (x[1])
          }
        }),
        NA, NA, NA # add NA at the end for BASAL_HT_ESTIMATOR, BASAL_HT_SE, and BASAL_N
      )
    }
    
    agg_data[agg_data[[domain]] == thedomain,"BASAL_HT_ESTIMATOR"] <- est$pop_mean
    agg_data[agg_data[[domain]] == thedomain,"BASAL_HT_SE"] <- sqrt(est$pop_mean_var)
    agg_data[agg_data[[domain]] == thedomain,"BASAL_N"] <- nrow(thedata)
#    agg_data[agg_data$domain == thedomain,]$n_zero <- nrow(thedata[(thedata[[res]] == 0),])
  }
  return(agg_data)
}

prop_positive = function(x) {
  return(mean(x == 1))
}
