#' Checking if a parameter input inherits a specific class
#' @noRd
check_inherits <- function(what, ...) {
  opts <- list(...)
  for (i in seq_along(opts)) {
    if (!is.null(opts[[i]])) {
      if (!inherits(opts[[i]], what)) {
        stop("Object ", i, " needs to inherit from class ", what)
      }
    } else {
      stop("unable to check NULL objects")
    }
  }
  invisible(opts)
}

#' @title Apply Direct Estimators to data
#' 
#' @description 
#' Apply direct estimators to different domains. Currently only the Horvitz-Thompson
#' is supported
#' 
#' @param data Data to compute the direct estimators on
#' 
#' @param response Variable to estimate
#' 
#' @param population_size population size 
#' (i.e., the number of (possibly unsampled)) plots/units/individuals
#' 
#' @param domain_name The name of the domain to compute direct estimators across
#' 
#' @returns A dataframe with values for auxiliary variables, as well as new values
#' \itemize{
#'  \item \code{DIR_MEAN_ESTIMATOR}: The direct estimate for the average of the response
#'  \item \code{DIR_MEAN_SE}: The estimated variance of the above estimate
#'  \item \code{DIR_SUM_ESTIMATOR}: The direct estimate for the sum of the response
#'  \item \code{DIR_SUM_SE}: The estimated variance of the above estimate
#'  \item \code{DIR_N}: The number of observations of the response
#' }
#' 
#' @export
aggregate_data <- function(
    data, response, domain_name, population_size = NULL, population_data = NULL, ...
) {
  # one day we would like to pass additional arguments to mase for things like
  # survey weights and whatnot
  # could be nice to allow the user to pick the direct estimator
  
  ready_data <- prepare_agg_data(
    data, response, population_size, domain_name, population_data, ...
  )
  
  auto_agg = ready_data$auto_agg
  data = ready_data$data
  population_data = ready_data$population_data
  agg_data = ready_data$agg_data
  population_size = ready_data$population_size

  unique_domains <- unique(data[[domain_name]])
  
  warning_list = list()
  for (i in 1:length(unique_domains)) {
    thedomain <- unique_domains[i]
    thedata <- data[data[[domain_name]] == thedomain,]
    thepopdata <- population_data[population_data[[domain_name]] == thedomain,]
    est <-
      mase::horvitzThompson(y = thedata[[response]],
                      N = population_size,
                      var_est = T,
                      messages = F,
                      ...)
    agg_data[i,] <- c(
      lapply(colnames(thepopdata), function(name) {
        x = thedata[,name]
        if (is.numeric(x)) {
          return (mean(x))
        } else {
          levels <- unique(x)
          if (length(levels) != 1) {
            warning_list <- union(
              warning_list,
              paste0(
                "Detected multiple levels in non-numeric data in column ", name, 
                ". Arbitrarily choosing the value ", x[1], "."
              )
            )
          }
          return (x[1])
        }
      }),
      NA, NA, NA, NA, NA 
      # add NA at the end for 
      #BASAL_HT_ESTIMATOR, BASAL_HT_SE, BASAL_SUM_ESTIMATOR, BASAL_SUM_SE, and BASAL_N
    )
    
    # so we don't include multiples of a warning
    lapply(warning_list, function(warning) {warning(warning)})
    
    agg_data[agg_data[[domain_name]] == thedomain,"DIR_MEAN_ESTIMATOR"] <- est$pop_mean
    agg_data[agg_data[[domain_name]] == thedomain,"DIR_MEAN_SE"] <- sqrt(est$pop_mean_var)
    agg_data[agg_data[[domain_name]] == thedomain,"DIR_SUM_ESTIMATOR"] <- est$pop_total
    agg_data[agg_data[[domain_name]] == thedomain,"DIR_SUM_SE"] <- sqrt(est$pop_total_var)
    agg_data[agg_data[[domain_name]] == thedomain,"DIR_N"] <- nrow(thedata)
#    agg_data[agg_data$domain == thedomain,]$n_zero <- nrow(thedata[(thedata[[response]] == 0),])
  }
  
  vec <- agg_data$DIR_MEAN_SE
  names(vec) <- agg_data[[domain_name]]
  
  population_data$DIR_MEAN_SE <- vec[population_data[[domain_name]]]

  vec[1:length(vec)] <- agg_data$DIR_SUM_SE
  population_data$DIR_SUM_SE <- vec[population_data[[domain_name]]]
  
  population_data = population_data[!is.na(population_data$DIR_MEAN_SE),]
  
  return(list(
    aggregate_obs = agg_data,
    aggregate_pop = population_data
  ))
}

#' Prepare data for aggregation
#' @noRd
prepare_agg_data <- function (
    data, response, population_size, domain_name, population_data, ...
) {
  if (!is.null(population_data)) {
    if (is.null(population_size)) {
      population_size = nrow(population_data)
    }
  } else if (is.null(population_size)) {
    stop("Must provide either population data or population size.")
  }
  
  if (!is.null(population_data)) {
    auto_agg <- FALSE
    domains_1 <- unique(data[[domain_name]])
    domains_2 <- unique(population_data[[domain_name]])
    if (length(setdiff(domains_1, domains_2)) != 0) {
      if (setdiff(colnames(population_data), colnames(data)) != 0) {
        warning("Missing domains ", 
                setdiff(domains_1, domains_2),
                " from the population data. Removing these columns from observed data.")
        data <- data[data[[domain_name]] %in% domains_2,]
      } else {
        warning("Missing domains ",
                setdiff(domains_1, domains_2),
                " from the population data. Using existing values in the observed data.")
        population_data <- rbind(
          population_data,
          data[data[[domain]] %in% setdiff(domains_1, domains_2), colnames(population_data)]
        )
      }
    }
    if (length(setdiff(domains_2, domains_1)) != 0) {
      warning("Domains present in population data that aren't present in ",
              "observed data. Removing these observations")
      population_data <- population_data[population_data[[domain]] %in% domains_1,]
    }
  } else {
    auto_agg <- TRUE
    population_data <- data[,colnames(data) != response]
  }
  agg_data <- population_data[1,]
  if ("DIR_MEAN_ESTIMATOR" %in% colnames(data) ||
      "DIR_MEAN_SE" %in% colnames(data) ||      
      "DIR_SUM_ESTIMATOR" %in% colnames(data) ||
      "DIR_SUM_SE" %in% colnames(data) ||
      "DIR_N" %in% colnames(data)) {
    stop("Variables with 'DIR' prefix are protected. Please rename these.")
  }
  agg_data$`DIR_MEAN_ESTIMATOR` <- NA
  agg_data$`DIR_SUM_ESTIMATOR` <- NA
  agg_data$`DIR_MEAN_SE` <- NA
  agg_data$`DIR_SUM_SE` <- NA
  agg_data$`DIR_N` <- NA
  
  if (auto_agg) {
    warning(
      "Aggregating sample data for auxiliary variables. If you want to use ",
      "averages of population data for auxiliary variables, ",
      "aggregate the data first using `prepare_agg_data()`, ",
      "and then use this as the training data in fit()."
    )
  }
  
  return (list(
    auto_agg = auto_agg,
    data = data,
    population_data = population_data,
    agg_data = agg_data,
    population_size = population_size
  ))
}

#' @title default_ncores
#' @description
#' Number of CPU cores we may use
#' copied (with slight modifications) from \{eulerr\}, https://github.com/jolars/eulerr
#'
#' Collects the core-count limits we trust and returns the smallest, never less
#' than one. This mirrors the (much more elaborate) min-of-signals design of
#' `parallelly::availableCores()`, but only the durable, non-platform-specific
#' signals: the detected core count, `R CMD check`'s `_R_CHECK_LIMIT_CORES_`
#' (capped at two), and `OMP_THREAD_LIMIT`. We deliberately do not parse cgroup
#' quotas or HPC scheduler variables.
#'
#' @return A positive integer scalar.
#' @keywords internal
default_ncores <- function() {
  n_cores <- parallel::detectCores(logical = TRUE)
  caps <- if (is.na(n_cores)) 1L else as.integer(n_cores)

  # `R CMD check --as-cran` sets `_R_CHECK_LIMIT_CORES_`; the CRAN check farm
  # sets `OMP_THREAD_LIMIT`. Both cap how many cores we may use.
  if (nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_"))) {
    caps <- c(caps, 2L)
  }
  omp <- suppressWarnings(as.integer(Sys.getenv("OMP_THREAD_LIMIT", "")))
  if (!is.na(omp)) {
    caps <- c(caps, omp)
  }

  return (max(1L, min(caps)/2))
}

#' @title proportion of positive
#' @description
#' 
#' Computes the proportion of positive observations in a. binomial trial `x`.
#' @param x Realization of binomial trials.
#' 
#' @param success The value which is considered a success
#' 
#' @returns A positive integer scalar, the proportionof successes in `x`.
#' @keywords internal
prop_positive = function(x, success = 1) {
  return(mean(x == success))
}

#' @title Entropy
#' @description
#' Compute entropy of a bernoulli trial with estimated probability computed from 
#' binomial trials with realizations `x`.
#' 
#' @param x Realization of binomial trials.
#' 
#' @param success The value which is considered a success
#' 
#' @returns A positive integer scalar, indicating the entropy of the bernoulli trial.
entropy = function (x, success = 1) {
  p = prop_positive(x, success)
  H <- -p * log(p) - (1-p) * log(1-p)
  H[is.nan(H)] <- 0
  return (H)
}

#' upper quantile for 95% CI
lower_ci_quantile = function(x, ...) {
  stats::quantile(x, 0.025, ...)
}

#' lower quantile for 95% CI
upper_ci_quantile = function(x, ...) {
  stats::quantile(x, 0.975, ...)
}
