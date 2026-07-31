#' @title Prior specification
#' Prior specification is done on an engine-specific basis.
#' For information on setting priors for a particular engine,
#' the help pages for prior specification for those engines will be sufficient.
specify_priors <- function (...) {
  return (list(...))
}

#' Accept inputs for fitting a model
#' @noRd
validate_fit_inputs <- function(spec,
                                data,
                                chains,
                                iter,
                                burn_in,
                                thin,
                                engine,
                                ncores,
                                nthreads) {
  
  parallel = do_parallel_settings(chains, ncores, nthreads)
  ncores = parallel$ncores
  nthreads = parallel$nthreads
  if ((ncores %% 1) != 0) {
    stop(
      "`ncores` must be an integer, but instead ", ncores, " was given."
    )
  }
  if (nthreads != "default" && ((nthreads %% 1) != 0)) {
    stop(
      "`nthreads` must be either \"default\" or an integer. Not ", nthreads, "."
    )
  }

  check_inherits("basal_spec", spec)
  check_inherits("data.frame", data)
  check_inherits("numeric", chains, iter, burn_in, thin)
  
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

#' Add the (possibly) engine-specific family to the second stage logit model
#' @noRd
add_engine_logit_family <- function (spec, engine) {
  if (!is.null(engine$logistic_family)) {
    spec$second_stage_spec$family <- engine$logistic_family
  }
  return (spec)
}

#' Generic; get response from a model specification
#' @noRd
get_fit_response <- function (spec, ...) {
  UseMethod("get_fit_response")
}

#' Generic; get all variables from a model specification
#' @noRd
all_model_vars <- function (spec, ...) {
  UseMethod("all_model_vars")
}

#' Ensure all variables are present in the data
#' @noRd
validate_model_variables <- function(variables, data) {
  variables = unlist(variables)
  missing <- setdiff(variables, colnames(data))
  if (length(missing) == 1) {
    stop("Variable ", missing," missing from your data.")
  }
  
  return (data[, variables])
}

#' Parallel settings for model fitting
#' @noRd
do_parallel_settings <- function(chains, ncores, nthreads) {

  # A thread here is used to speed up within-chain computations.
  # A core is used to run another chain in parallel
  if (nthreads != "default") {
    ncores <- min(ncores, chains)
  } else if (ncores >= 2 * chains) {
    nthreads <- floor(ncores/chains)
    ncores <- chains
  } else {
    nthreads <- 1
  }
  
  return(
    list(
      ncores = ncores,
      nthreads = nthreads
    )
  )
}

#' Generic; build a custom formula to be fit by a backend
#' @noRd
build_custom_formula <- function (spec, ...) {
  UseMethod("build_custom_formula")
}

#' Generic; build a BHF formula to be fit by a backend
#' @noRd
build_bhf_formula <- function (spec, ...) {
  UseMethod("build_bhf_formula")
}

#' Generic; build a FH formula to be fit by a backend
#' @noRd
build_fh_formula <- function (spec, ...) {
  UseMethod("build_fh_formula")
}

#' Build the formula for a model
#' @noRd
build_basal_formula <- function(spec, response) {
  
  if (spec$model_type == "custom") {
    return(
      build_custom_formula(spec = spec, response = response)
    )
  }
  if (spec$model_type == "BHF") {
    return(
      build_bhf_formula(spec = spec)
    )
  }
  if (spec$model_type == "FH") {
    return(
      build_fh_formula(spec = spec, response = response)
    )
  }
  stop("Unsupported model type: ", spec$model_type, ".")
}

#' Compute the scale of a variable
#' @noRd
get_scale <- function (data, variable) {
  var_sd <- sd(data[[variable]], na.rm = TRUE)
  if (is.infinite(var_sd) || var_sd <= 0) {
    stop("Unable to calculate scale of variable ", variable, ".")
  }
  
  return (var_sd)
}

#' Prepare data for an area-Level model
#' @noRd
prepare_area_level_data <- function(spec,
                                    data,
                                    response,
                                    population_size = NULL) {
  
  if (spec$level != "area") {
    return(
      list(
        data = data,
        response = response
      )
    )
  }
  
  # If the user specifies an area-level model and wants automatic aggregation,
  # compute HT estimators and replace the original response with the direct
  # estimator. This applies to both preset FH models and custom area-level models.
  if (is.null(spec$obs_variability)) {
    if (is.null(population_size)) {
      stop(
	"Population size is required for auto-aggregation (computation of ",
	"direct estimator) in area-level models."
      )
    }
    data <- agg_HT(
      data = data,
      res = response,
      N = population_size,
      domain = spec$domain_name
    )
    response <- "BASAL_HT_ESTIMATOR"
  } else {
    obs_var <- spec$obs_variability
    if (is.numeric(obs_var)) {
      if (length(obs_var) != 1 && length(obs_var) != nrow(data)) {
        stop(
          "`obs_variability` must have length one or the same number of rows as data."
        )
      }
      data$BASAL_HT_SE <- obs_var
    } else if (is.character(obs_var)) {
      if (length(obs_var) != 1) {
        stop(
          "`obs_variability` must be a numeric vector or a single column name."
        )
      }
      if (!(obs_var %in% colnames(data))) {
        stop(
          "`obs_variability` must be a vector of standard errors or a column in the data."
        )
      }
      data$BASAL_HT_SE <- data[[obs_var]]
    } else {
      stop(
        "`obs_variability` must be a numeric vector or the name of a column in data."
      )
    }
  }
  if (!("BASAL_HT_SE" %in% colnames(data))) {
    stop("Unable to construct `BASAL_HT_SE` for the area-level model.")
  }
  valid_se <- is.finite(data$BASAL_HT_SE) &
    data$BASAL_HT_SE > 0
  data <- data[valid_se, , drop = FALSE]
  if (nrow(data) == 0) {
    stop(
      "No observations with positive, finite standard errors remain for the area-level model."
    )
  }

  return(
    list(
      data = data,
      response = response
    )
  )
}

#' Prepare data for a two-stage model
#' @noRd
prepare_two_stage_data <- function(data, response, spec) {
  ss_res = try(get_fit_response(spec$second_stage_spec), silent = TRUE)
  if (inherits(ss_res, "try-error")) {
    ss_res = response
  }
  data$BASAL_NONZERO_INDICATOR <- as.numeric(data[[ss_res]] != 0)
  indicator_values <- unique(stats::na.omit(data$BASAL_NONZERO_INDICATOR))
  if (length(indicator_values) < 2) {
    stop("The two-stage zero-inflated model requires zero and nonzero observations.")
  }
  unfiltered_data <- data
  nonzero_data <- data[!is.na(data[[response]]) & data[[response]] != 0,]
  if (nrow(nonzero_data) == 0) {
    stop("No nonzero observations were found for response variable ", response, ".")
  }
  return(
    list(
      unfiltered_data = unfiltered_data,
      nonzero_data = nonzero_data
    )
  )
}

#' Set the response value of a specification
#' @noRd
set_spec_response <- function (spec, response) {
  UseMethod("set_spec_response")
}

#' Fit the second stage of a two-stage model
#' @noRd
fit_second_stage <- function(spec,
                             data,
                             priors = NULL,
                             chains,
                             iter,
                             burn_in,
                             seed,
                             thin,
                             engine,
                             ncores,
                             nthreads,
                             ...) {
  
  if (is.null(spec)) {
    return(NULL)
  }
  
  second_stage_fit <- fit.basal_spec(
    spec = spec,
    data = data,
    population_size = NULL,
    priors = priors,
    second_stage_priors = NULL,
    chains = chains,
    iter = iter,
    burn_in = burn_in,
    seed = seed,
    thin = thin,
    engine = engine,
    ncores = ncores,
    nthreads = nthreads,
    ...
  )
  
  return(second_stage_fit)
}

fit_basal_model = function(spec, ...) {
  UseMethod("fit_basal_model")
}

