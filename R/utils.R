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
#' Apply Horvitz-Thompson (or Post-Stratified) estimators to different domains.
#' 
#' @param data Data to compute the direct estimators on
#' 
#' @param res Variable to estimate
#' 
#' @param N population size
#' 
#' @param domain Domains to compute the direct estimators over
#' 
#' @param agg_data Dataset with aggregated observations for other variables, or
#' `NULL` to aggregate `data`.
#' 
#' @keywords internal
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
  agg_data$`BASAL_HT_SUM_ESTIMATOR` <- NA
  agg_data$`BASAL_HT_SE` <- NA
  agg_data$`BASAL_HT_SUM_SE` <- NA
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
        NA, NA, NA, NA, NA # add NA at the end for BASAL_HT_ESTIMATOR, BASAL_HT_SE, and BASAL_N
      )
    }

    agg_data[agg_data[[domain]] == thedomain,"BASAL_HT_ESTIMATOR"] <- est$pop_mean
    agg_data[agg_data[[domain]] == thedomain,"BASAL_HT_SE"] <- sqrt(est$pop_mean_var)
    agg_data[agg_data[[domain]] == thedomain,"BASAL_HT_SUM_ESTIMATOR"] <- est$pop_total
    agg_data[agg_data[[domain]] == thedomain,"BASAL_HT_SUM_SE"] <- sqrt(est$pop_total_var)
    agg_data[agg_data[[domain]] == thedomain,"BASAL_N"] <- nrow(thedata)
#    agg_data[agg_data$domain == thedomain,]$n_zero <- nrow(thedata[(thedata[[res]] == 0),])
  }
  return(agg_data)
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

#' @noRd
validate_single_stage_spec <- function(spec, auxiliary_variables, response_name) {
  if (spec$model_type == "custom") {
    if (is.null(spec$formula) || is.null(spec$level)) {
      stop("Must provide a formula and level for custom models.")
    }
    spec$response_name <- NULL
    spec$auxiliary_variables <- NULL
    spec$default_model_data <- NULL
  } else if (spec$model_type != "aux_spec") {
    if (is.null(spec$domain_name) ||
        is.null(response_name) ||
        is.null(auxiliary_variables)) {
      stop("Must provide domain, response, and auxiliary variable names for pre-set models.")
    }
    if (spec$model_type == "BHF") spec$level <- "unit"
    if (spec$model_type == "FH") spec$level <- "area"
    spec$formula <- NULL;

    spec$default_model_data <- list(
      response_name = response_name,
      domain_name = spec$domain_name,
      auxiliary_variables = auxiliary_variables
    )
  }
  return (spec)
}

#' @noRd
validate_GLM_two_stage_spec = function(spec, response_name, auxiliary_variables) {
  if (spec$family$family != "bernoulli") {
    spec$family <- brms::bernoulli()
  }
  if (spec$model_type == "custom") {
    if (is.null(spec$formula) ||
        is.null(spec$level)) {
      message(paste0(
        "Missing values for formula and/or level will be filled by the first stage"
      ))
    }
  } else {
    if (spec$model_type == "BHF") spec$level <- "unit"
    if (spec$model_type == "FH") spec$level <- "area"
    if (spec$level == "area") {
      stop("Can't specify area-level models for the second stage.")
    }
    if (is.null(spec$domain_name) ||
        is.null(auxiliary_variables) ) {
      message(paste0(
        "One or both of domain_name or auxiliary_variables are ",
        "missing. These will be inherited from the first stage"
      ))
    }
    if (!is.null(response_name)) {
      message(paste0(
        "Response variables cannot be specified for the GLM, the response ",
        "will be whether or not the response in the response model is nonzero."
      ))
    }
    spec$default_model_data <- list(
      response_name = "BASAL_NONZERO_INDICATOR",
      domain_name = spec$domain_name,
      auxiliary_variables = auxiliary_variables
    )
  }
  
  return (spec)
}

#' @noRd
validate_transformation <- function(transformation) {
  if (!is.null(transformation)) {
    check_inherits("list", transformation)
    check_inherits("function",
                   transformation$transform, transformation$inv_transform)

    identity = TRUE
    for (i in 0:512) { # somewhat arbitrary points, we want zero to be included though
      # we only check that inv_transform is a left inverse of transform, because 
      # we never go the other way
      identity = identity &&
        round(transformation$inv_transform(transformation$transform(i)),10) == i
    }
    if (!identity) {
      stop(paste0(
        "variable_transform$inv_transform isn't a right-inverse of ",
        "variable_transform$transform on 0:512."
      ))
    }
  }
}

validate_second_stage <- function(spec, auxiliary_variables) {
  if (spec$level == "area") {
    message("Can't fit area-level models to zero observations, re-specifying as a unit-level.")
    spec$level <- "unit"
    if (!is.null(spec$obs_variability)) {
      stop("Can't re-specify model as unit-level. Must provide un-aggregated data")
    }
    spec$level <- "unit"
    spec$obs_variability <- NULL
    if (spec$model_type == "FH") {
      spec$model_type <- "BHF"
    }
  }
  
  # if there is no specification, then inherit from response model
  if (is.null(spec$second_stage_spec)) {
    message("Specification for second stage inherited from this level")
    spec$second_stage_spec <- specify(
      formula = spec$formula,
      level = spec$level,
      model = spec$model_type,
      obs_variability = NULL,
      domain_name = spec$domain_name,
      response_name = spec$response_name,
      auxiliary_variables = auxiliary_variables,
      variable_transform = NULL,
      family = brms::bernoulli(),
      model_stage = spec$model_stage,
      specifying_second_stage_model = TRUE,
      second_stage_spec = NULL
    )
  } else {
    # if there is a specification for the GLM, ensure it has all parts
    # and fill in what we can from the response model
    if (spec$second_stage_spec$model_type != spec$model_type) {
      if (spec$second_stage_spec$model_type != "custom") {
        if ((is.null(spec$second_stage_spec$default_model_data$domain_name) &&
             is.null(spec$domain_name)) ||
            (is.null(spec$second_stage_spec$default_model_data$auxiliary_variables) &&
             is.null(auxiliary_variables))) {
          stop(paste0(
            "Did not specify domain or auxiliary variables in ",
            "the second stage model. This model is custom and you have not ",
            "specified domain_name or auxiliary_variables. Please further ",
            "specify the second stage, set second stage to custom, ",
            "or set these variables here"
          ))
          
        }  else {
          if (!is.null(spec$domain_name) && 
              is.null(spec$second_stage_spec$default_model_data$domain_name)) {
            spec$second_stage_spec$default_model_data$domain_name <- spec$domain_name
            spec$second_stage_spec$domain_name <- spec$domain_name
          }
          if (!is.null(auxiliary_variables) &&
              is.null(spec$second_stage_spec$default_model_data$auxiliary_variables)) {
            spec$second_stage_spec$default_model_data$auxiliary_variables <- auxiliary_variables
          }
        }
      } else {
        if (is.null(spec$second_stage_spec$formula)) {
          message(paste0(
            "Can't inherit formula from model type ", spec$model_type, ". Setting second ",
            "stage model type to ", spec$model_type, ". To use custom second stage, set ",
            "the formula in the second stage."
          ))
          spec$second_stage_spec$model_type <- spec$model_type
        }
      }
    }
    if (spec$second_stage_spec$model_type == spec$model_type) {
      if (spec$second_stage_spec$model_type == "custom") {
        if (is.null(spec$second_stage_spec$formula)) {
          spec$second_stage_spec$formula <- spec$formula
        }
        if (is.null(spec$second_stage_spec$level)) {
          spec$second_stage_spec$level <- spec$level
        }
      } else if (spec$second_stage_spec$model_type != "custom") {
        if (is.null(spec$second_stage_spec$default_model_data$auxiliary_variables)) {
          spec$second_stage_spec$default_model_data$auxiliary_variables <- auxiliary_variables
        }
        if (is.null(spec$second_stage_spec$default_model_data$domain_name)) {
          spec$second_stage_spec$default_model_data$domain_name <- spec$domain_name
          spec$second_stage_spec$domain_name <- spec$domain_name
        }
      }
    }
  }
  
  validate_single_stage_spec(
    spec = spec$second_stage_spec,
    auxiliary_variables = auxiliary_variables,
    response_name = "BASAL_NONZERO_INDICATOR"
  )
  
  return (spec)
}

# fit_helpers.R

#' Accept inputs for fitting a model
#'
#' @param spec a
#'
#' @param data a
#'
#' @param chains a
#'
#' @param iter a
#'
#' @param burn_in a
#'
#' @param thin a
#'
#' @param engine a
#'
#' @return a
#'
#' @noRd
validate_fit_inputs <- function(spec,
                                data,
                                chains,
                                iter,
                                burn_in,
                                thin,
                                engine) {
  
  check_inherits("basal_spec", spec)
  check_inherits("data.frame", data)
  check_inherits("numeric", chains, iter, burn_in, thin)
  
  if (length(engine) != 1 || !is.character(engine) || is.na(engine)) {
    stop("`engine` must be a single character string.")
  }
  if (engine != "brms") {
    stop("Sorry, only `engine = \"brms\"` is currently supported.")
  }
  mcmc_args <- list(chains = chains, iter = iter, burn_in = burn_in, thin = thin)
  bad_length <- names(mcmc_args)[lengths(mcmc_args) != 1]
  if (length(bad_length) > 0) {
    stop(
      "The following argument", if (length(bad_length) > 1) "s must" else "must",
      " have length one: ", paste0("`", bad_length, "`", collapse = ", "),"."
    )
  }
  mcmc_args <- unlist(mcmc_args)
  non_finite <- names(mcmc_args)[!is.finite(mcmc_args)]
  if (length(non_finite) > 0) {
    stop(
      "The following argument", if (length(non_finite) > 1) "s are" else " is",
      " not finite: ", paste0("`", non_finite, "`", collapse = ", "), "."
    )
  }
  non_integer <- names(mcmc_args)[mcmc_args != floor(mcmc_args)]
  if (length(non_integer) > 0) {
    stop(
      "The following argument", if (length(non_integer) > 1) "s must" else " must",
      " be integer-valued: ", paste0("`", non_integer, "`", collapse = ", "), "."
    )
  }
  positive_args <- mcmc_args[c("chains", "iter", "thin")]
  non_positive <- names(positive_args)[positive_args <= 0]
  if (length(non_positive) > 0) {
    stop(
      "The following argument", if (length(non_positive) > 1) "s must" else " must",
      " be positive: ", paste0("`", non_positive, "`", collapse = ", "), "."
    )
  }
  if (burn_in < 0) {
    stop("`burn_in` must be non-negative.")
  }
  if (burn_in >= iter) {
    stop("`burn_in` must be smaller than `iter`.")
  }
}


#' Extract the response variable from a BASAL specification
#'
#' @param spec a
#'
#' @return a
#'
#' @noRd
get_fit_response <- function(spec) {
  
  if (is.null(spec$formula)) {
    response <- spec$default_model_data$response_name
  } else if (inherits(spec$formula, "brmsformula")) {
    response <- all.vars(spec$formula$formula[[2]])[1]
  } else {
    response <- all.vars(spec$formula[[2]])[1]
  }
  if (is.null(response) || length(response) != 1 || is.na(response) || response == "") {
    stop("Unable to determine the response variable from `spec`.")
  }
  
  return(response)
}


#' Check variables required by a model formula
#'
#' @param formula a
#'
#' @param data a
#'
#' @return a
#'
#' @noRd
validate_formula_data <- function(formula, data) {
  
  if (inherits(formula, "brmsformula")) {
    variables <- all.vars(formula$formula)
  } else {
    variables <- all.vars(formula)
  }
  variables <- variables[!(variables %in% c("BASAL_HT_SE"))]
  missing <- setdiff(variables, colnames(data))
  if (length(missing) == 1) {
    stop(paste0("Variable ", missing," missing from your data."))
  }
  if (length(missing) > 1) {
    stop(paste0("Variables ", paste0(missing, collapse = ", "), " missing from your data."))
  }
  
  return(variables)
}