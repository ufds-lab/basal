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

#' @title Make Variable Transformation
#' @description
#' Specify a variable transformation to map the response values through.
#' Can help improve model fitness with highly skewed (for example) distributions
#' of responses. Predictions are transformed back for point statistic estimates.
#'
#' @param variable_transform Function to transform the response.
#' @param inv_transform Left-inverse of \code{variable_transform} i.e.,
#' \code{inv_transform(transform(x)) == x}.
#' @param eval_grid Grid of values to ensure that the \code{transform} and
#' \code{inv_transform} are consistent with each other.
#' @param override Logical flag to indicate that the check on \code{eval_grid}
#' should be ignored.
#' @param tol Tolerance for testing equality under numerical imprecision. If any
#' of \code{variable_transform(inv_transform(eval_grid)}, are more than
#' \code{tol} from \code{eval_grid}, then there will be no error.
#' @param ... Not used currently.
#'
#' @details
#' Variable transformations can be a good way to improve a model by
#' transforming the response. Common transformations include log and root-type
#' transformations.
#'
#' Before model fitting, the data will be transformed via
#' \code{variable_transform}. Then, after posterior estimation but before
#' aggregation and computing summaries, predictions will be back-transformed
#' via \code{inv_transform}.
#'
#' By default, \code{transform} will ensure that \code{inv_transform}
#' is a (one-sided) inverse of \code{variable_transform} by ensuring that
#' \code{inv_transform(variable_transform(eval_grid))} is the same as
#' \code{eval_grid}, however this behavior can be circumvented with the
#' \code{override} flag.
#'
#' @examples
#' # here are two common transformations
#' root_trans <- transform(
#'   sqrt,
#'   function(y) {y^2}
#' )
#'
#' log_trans <- transform(
#'   log,
#'   exp
#' )
#'
#' # we can also use a different grid
#' grid_trans <- transform(
#'   sin,
#'   asin,
#'   eval_grid = seq(-pi/2, pi/2, 0.1),
#' )
#'
#' # the eval_grid can be ignored (this is not recommended)
#' bad_trans <- transform(
#'   function(y) {0},
#'   function(y) {0},
#'   override = TRUE
#' )
#'
#' @return Object of type \code{basal_transformation}.
#' @export
transform <- function(
  variable_transform,
  inv_transform,
  eval_grid = 0:512,
  override = FALSE,
  tol = 1e-10,
  ...
) {
  transformation <- list(
    transform = variable_transform,
    inv_transform = inv_transform
  )
  validate_transformation(transformation, eval_grid, override, tol)

  class(transformation) <- "basal_transformation"
  return(transformation)
}

#' @noRd
validate_transformation <- function(transformation,
                                    eval_grid, override, tol) {
  if (!is.null(transformation)) {
    check_inherits("list", transformation)
    check_inherits(
      "function",
      transformation$transform, transformation$inv_transform
    )

    if (!override) {
      if (any(abs(
        transformation$inv_transform(
          transformation$transform(eval_grid)
        ) - eval_grid) >= tol
      )) {
        stop(paste0(
          "variable_transform$inv_transform isn't an inverse of ",
          "variable_transform$transform on `eval_grid`. To bypass the check ",
          "create your variable transformation with:\n`create_variable_",
          "transformation(transform, inv_transform, override = TRUE)`."
        ))
      }
    }
  }
}


#' @title Apply Direct Estimators to data
#'
#' @description
#' Apply direct estimators to different domains using \code{mase} as a backend.
#'
#' @param data Data to compute the direct estimators on.
#' @param response Variable to estimate.
#' @param domain_name The name of the domain to compute direct
#' estimators across.
#' @param population_data a dataframe of the population data, used for computing
#' aggregates of auxiliary variables.
#' @param population_size "A numeric value of the population size. If
#' \code{NULL}, it is estimated with the sum of the inverse of the
#' \[\code{sample_weights}\]" ([mase::horvitzThompson()]).
#' If both \code{population_size} and \code{sample_weights} are \code{NULL},
#' \code{population_size} is taken to be the number of rows in
#' \code{population_data}.
#' @param sample_weights "A numeric vector of inclusion probabilities for each
#' sampled unit in \code{data}." ([mase::horvitzThompson()]).
#' @param estimate Type of estimate; supported for values "mean" and "sum".
#' @param method Type of direct estimate; supported for values "HT", for
#' Horvitz-Thompson and "PS" for Poast-Stratified.
#' @param PS_strata name of strata used for post-stratification; ignored if
#' method isn't "PS".
#' @param mase_messages logical. If true, prints all messages from \code{mase}.
#' Disabled by default to avoid repeated messages from repeated calls to
#' \code{mase} functions.
#' @param ... to pass extra parameters to \code{mase::horvitzThompson} or
#' \code{mase::postStrat}. See Details.
#'
#' @details
#' The \code{...} parameter can be used to pass additional arguments to the
#' backend used to compute the direct estimates. The arguments being used are,
#' for \code{mase::horvitzThompson}:
#' \itemize{
#'  \item \code{y},
#'  \item \code{pi},
#'  \item \code{N},
#'  \item \code{var_est},
#'  \item \code{messages}
#' }
#' and for for \code{mase::postStratified}:
#' \itemize{
#'  \item \code{y},
#'  \item \code{xsample},
#'  \item \code{xpop},
#'  \item \code{pi},
#'  \item \code{N},
#'  \item \code{var_est},
#'  \item \code{messages}
#' }
#' any other arguments to the direct estimator functions in \code{mase} may be
#' passed to \code{...}.
#'
#' @seealso [mase::horvitzThompson()], [mase::postStrat()]
#'
#' @returns Two data frames \code{aggregate_obs} and \code{aggregate_pop}.
#' They each have an additional column response_se, where response is replaced
#' with the value for \code{response}.
#'
#' In addition, in \code{aggregate_obs}, the column for \code{response}
#' is replaced with the direct estimate of the response (but using the same
#' name). Furthermore, all the other columns are replaced with averages
#' (across \code{domain_name}) of the columns in the population data when they
#' are numeric or a modal response when they are categorical.
#'
#' @examples
#' # HT estimator
#' aggregate(
#'   oregon_sample, "biomass",
#'   "county", oregon_population
#' )
#'
#' # PS estimator
#' aggregate(oregon_sample, "biomass",
#'   "county", oregon_population,
#'   PS_strata = "county", method = "PS"
#' )
#'
#' # use population size
#' aggregate(oregon_sample, "biomass", "county",
#'   population_size = 1e4, oregon_population
#' )
#'
#' # use sampling weights
#' aggregate(oregon_sample, "biomass", "county",
#'   sample_weights = rbeta(nrow(oregon_sample), 1, 1),
#'   oregon_population
#' )
#' @export
aggregate <- function(
  data, response, domain_name, population_data,
  population_size = NULL, sample_weights = NULL,
  estimate = "mean", method = "HT", PS_strata = NULL, mase_messages = FALSE, ...
) {
  check_agg_data_args(
    data, response, domain_name, population_data, population_size,
    sample_weights, estimate, method, PS_strata, ...
  )

  ready_data <- prepare_agg_data(
    data, response, domain_name, population_data,
    population_size, sample_weights, estimate, method, PS_strata, ...
  )

  data <- ready_data$data
  population_data <- ready_data$population_data
  agg_data <- ready_data$agg_data
  population_size <- ready_data$population_size

  unique_domains <- unique(data[[domain_name]])

  response_se <- paste0(response, "_se")
  warning_list <- list() # since we have to compute the estimator
  # length(unique_domains) times, there can be a lot of repeated warnings
  for (i in 1:length(unique_domains)) {
    thedomain <- unique_domains[i]
    thedata <- data[data[[domain_name]] == thedomain, ]
    thepopdata <- population_data[population_data[[domain_name]] == thedomain, ]
    if (method == "HT") {
      est <-
        mase::horvitzThompson(
          y = thedata[[response]],
          N = population_size,
          pi = sample_weights[data[[domain_name]] == thedomain],
          var_est = TRUE,
          messages = mase_messages,
          ...
        )
    } else {
      est <-
        mase::postStrat(
          y = thedata[[response]],
          N = population_size,
          pi = sample_weights,
          xsample = thedata[[PS_strata]],
          xpop = thepopdata[[PS_strata]],
          var_est = TRUE,
          messages = mase_messages,
          ...
        )
    }
    if (estimate == "mean") {
      est_val <- est$pop_mean
      if (is.null(est$pop_mean_var) || is.nan(est$pop_mean_var)) {
        stop("Error: Undefined variance estimate.")
      } else if (est$pop_mean_var < 0) {
        warning("Observed negative estimated variance")
      }
      est_se <- sqrt(est$pop_mean_var)
    } else {
      est_val <- est$pop_total
      if (is.null(est$pop_total_var) || is.nan(est$pop_total_var)) {
        stop("Error: Undefined variance estimate.")
      } else if (est$pop_total_var < 0) {
        warning("Observed negative estimated variance")
      }
      est_se <- sqrt(est$pop_total_var)
    }

    agg_data[i, ] <- c(
      lapply(
        setdiff(colnames(thepopdata), c(response, response_se)),
        function(name) {
          x <- thepopdata[, name]
          if (is.numeric(x)) {
            return(mean(x))
          } else {
            names(sort(table(x), decreasing = TRUE))[1]
          }
        }),
      est_val, est_se
    )

    # so we don't include multiples of a warning
    lapply(warning_list, function(warning) {
      warning(warning)
    })
  }

  vec <- agg_data[[response_se]]
  names(vec) <- agg_data[[domain_name]]

  population_data[[response_se]] <-
    vec[as.factor(population_data[[domain_name]])]

  population_data <-
    population_data[, -which(colnames(population_data) == response)]
  return(structure(
    list(
      aggregate_obs = agg_data,
      aggregate_pop = population_data
    ),
    class = "basal_data_aggregate"
  ))
}

#' Verify the arguments for aggregate()
#' @noRd
check_agg_data_args <- function(
  data, response, domain_name, population_data,
  population_size, sample_weights, estimate, method, PS_strata, ...
) {
  if (is.null(data) ||
        (!inherits(data, "data.frame") &&
           inherits(try(as.data.frame(data)), "try-error"))) {
    stop("data must be a data.frame, or must be coercible to a data.frame")
  }

  if (is.null(population_data) ||
        (!inherits(population_data, "data.frame") &&
           inherits(try(as.data.frame(population_data)), "try-error"))) {
    stop("data must be a data.frame, or must be coercible to a data.frame")
  }

  if (!method %in% c("HT", "PS")) {
    stop("method must be one of \"HT\" or \"PS\".")
  }

  if (!estimate %in% c("mean", "sum")) {
    stop("estimate must be one of \"mean\" or \"sum\".")
  }

  check_inherits("character", response, domain_name, estimate, method)

  if (!is.null(population_size)) {
    check_inherits("numeric", population_size)
  }
  if (!is.null(sample_weights)) {
    check_inherits("numeric", sample_weights)
  }

  opts <- list(...)
  names(opts) <- ...names()
  if (method == "HT") {
    restricted <- c("y", "pi", "N", "var_est")
    s <- setdiff(restricted, names(opts))
    if (length(s) != length(restricted)) {
      stop("Argument(s) ", setdiff(restricted, s), " cannot be provided in ...")
    }
  } else if (method == "PS") {
    restricted <- c("y", "xsample", "xpop", "pi", "N", "var_est")
    s <- setdiff(restricted, names(opts))
    if (length(s) != length(restricted)) {
      stop("Argument(s) ", setdiff(restricted, s), " cannot be provided in ...")
    }
  }

  if (!(domain_name %in% colnames(data))) {
    stop("The specified domain_name ", domain_name,
         " does not correspond to any columns in data.")
  } else if (!(domain_name %in% colnames(population_data))) {
    stop("The specified domain_name ", domain_name,
         " does not correspond to any columns in population_data.")
  }
  if (!is.null(PS_strata)) {
    if (!(PS_strata %in% colnames(data))) {
      stop("PS_strata ", PS_strata, " not found in data.")
    }
    if (!(PS_strata %in% colnames(population_data))) {
      stop("PS_strata ", PS_strata, " not found in population_data.")
    }
  } else if (method == "PS") {
    stop("Must provide PS_strata if using post-stratified estimators.")
  }

  if (!is.null(population_size) && population_size <= 0) {
    stop("Population size must be a non-negative value.")
  }
  if (!is.null(sample_weights) &&
        (any((sample_weights <= 0) | (sample_weights > 1)) ||
           length(sample_weights) != nrow(data))) {
    if (any(sample_weights <= 0)) {
      stop("sample_weights must be positive")
    } else if (any(sample_weights > 1)) {
      stop("sample_weights must be at most 1.")
    } else {
      stop("there should be one sampling weight per row of data.")
    }
  }
}

#' Prepare data for aggregation
#' @noRd
prepare_agg_data <- function(
  data, response, domain_name, population_data,
  population_size, sample_weights, estimate, method, PS_strata, ...
) {
  if (is.null(population_size) && is.null(sample_weights)) {
    population_size <- nrow(population_data)
  }

  common_cols <-
    intersect(colnames(data), unique(c(colnames(population_data), response)))
  data <- data[, common_cols]
  population_data <- population_data[, setdiff(common_cols, response)]

  domains_1 <- unique(data[[domain_name]])
  domains_2 <- unique(population_data[[domain_name]])
  if (length(setdiff(domains_1, domains_2)) != 0) {
    warning(
      "Missing domains ", setdiff(domains_1, domains_2), " from the population",
      " data. Adding existing observations to the observed data."
    )
    population_data <- rbind(
      population_data,
      data[data[[domain_name]] %in% setdiff(domains_1, domains_2),
           colnames(population_data)]
    )
  }
  if (length(setdiff(domains_2, domains_1)) != 0) {
    warning(
      "Domains present in population data that aren't present in ",
      "observed data. Removing these observations"
    )
    population_data <-
      population_data[population_data[[domain_name]] %in% domains_1, ]
  }
  if (paste0(response, "_se") %in% colnames(population_data)) {
    warning("Variable `", response,
            "_se` overwritten as standard error of the direct estimate.")
  }
  population_data[[response]] <- NA
  population_data[[paste0(response, "_se")]] <- NA
  agg_data <- population_data[1, ]

  return(list(
    data = data,
    population_data = population_data,
    agg_data = agg_data,
    population_size = population_size
  ))
}

#' @title default_ncores
#' @description
#' Number of CPU cores we may use
#' copied (with slight modifications) from \{eulerr\},
#' https://github.com/jolars/eulerr. If not "R CMD check --as-cran", then the
#' number of available cores divided by two, otherwise at most two cores.
#'
#' @return An integer indicating the number of cores.
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

  return(max(1L, min(caps) / 2))
}

#' @title proportion of positive
#' @description
#'
#' Computes the proportion of positive (successful) observations in a
#' binomial trial \code{x}.
#' @param x Realization of binomial trials.
#'
#' @param success The value which is considered a success
#'
#' @returns A positive integer scalar, the proportionof successes in \code{x}.
#' @keywords internal
prop_positive <- function(x, success = 1) {
  mean(x == success)
}

#' @title Entropy
#' @description
#' Compute entropy of a sequence of bernoulli trials, with estimated probability
#' computed from binomial trials \code{x}.
#'
#' @param x Realization of binomial trials.
#'
#' @param success The value which is considered a success.
#'
#' @returns A positive integer scalar, indicating the entropy of the
#' bernoulli trial.
#'
#' @examples
#'
#' x = rbinom(20, 1, 0.5)
#' entropy(x)
#'
#' @export
entropy <- function(x, success = 1) {
  p <- prop_positive(x, success)
  H <- -p * log(p) - (1 - p) * log(1 - p)
  H[is.nan(H)] <- 0
  return(H)
}

#' @rdname CI_quantiles
#' @export
lower_ci_quantile <- function(alpha = 0.95, q = NULL, ...) {
  if (is.null(q)) {
    q <- (1 - alpha) / 2
  }
  function(x, ...) {
    stats::quantile(x, q, ...)
  }
}

#' @rdname CI_quantiles
#' @export
upper_ci_quantile <- function(alpha = 0.95, q = NULL, ...) {
  if (is.null(q)) {
    q <- (1 - alpha) / 2
  }
  function(x, ...) {
    stats::quantile(x, 1 - q, ...)
  }
}

#' Function wrappers for CIs
#' @description
#' Provides functions to compute the lower or upper quantile for an
#' (100 * alpha)% credible interval.
#' @param alpha Level of the CI (i.e., \code{alpha = 0.95} => 95% CI).
#' Only used when \code{q} is not provided.
#' @param q Alternative to \code{alpha}; compute the \code{q}-th lower/upper
#' quantile.
#' @param ... further arguments to be passed to [quantile()].
#' @details If \code{q} is not provided, then the functions return the
#' ((1-\code{alpha})/2)-th quantile (lower) or (1-(1-\code{alpha})/2)-th
#' quantile (upper) so that this provides a central \code{alpha}-level CI.
#'
#' If \code{q} is provided, then the returned functions return the \code{q}-th
#' or (1-\code{q})-th quantile.
#' @returns a function \code{function(x, ...)} which returns the lower or
#' upper quantile of an alpha-level CI given values \code{x}.
#'
#' @examples
#' l95 <- lower_ci_quantile()
#' u95 <- upper_ci_quantile()
#' x <- 0:20
#'
#' l95(x)
#' u95(x)
#'
#' # we can get the same result by specifying the quantiles with q
#' l95 <- lower_ci_quantile(q = 0.025)
#' u95 <- upper_ci_quantile(q = 0.025)
#'
#' l95(x)
#' u95(x)
#'
#' # or we can get wrappers for noncentral CIs. Here's a left 95% CI
#' left_l95 <- lower_ci_quantile(q = 0)
#' right_l95 <- upper_ci_quantile(q = 0.05)
#'
#' left_l95(x)
#' right_l95(x)
#'
#' @name CI_quantiles
NULL
