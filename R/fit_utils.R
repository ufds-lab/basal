#' @title Prior specification
#' @param ... prior objects to pass to the engine
#' Prior specification is done on an engine-specific basis.
#' For information on setting priors for a particular engine,
#' the help pages for prior specification for those engines will be sufficient.
#' @export
specify_priors <- function (...) {
  return (list(...))
}

#' Accept inputs for fitting a model
#' @noRd
validate_fit_inputs <- function (
    spec,
    data,
    chains,
    iter,
    burn_in,
    thin,
    engine,
    ncores,
    nthreads
) {
  
  if (!spec$valid_spec) {
    stop("Invalid specification; likely caused by passing the second-stage of a logit-ZI model to fit().")
  }
  if (!is.numeric(ncores) || length(ncores) != 1 || 
      !is.finite(ncores) || ncores <= 0 || ncores != floor(ncores)) {
    stop("`ncores` must be a single positive integer, not ", ncores, ".")
  }
  
  if (!(identical(nthreads, "default") || (is.numeric(nthreads) && length(nthreads) == 1 && 
                                           is.finite(nthreads) && nthreads > 0 && nthreads == floor(nthreads)))) {
    stop(
      "`nthreads` must be a single positive integer or \"default\", not ", nthreads, "."
    )
  }

  class_diff = setdiff(c("data.frame", "basal_data_aggregate"), class(data))
  if (length(class_diff) == 2) {
    stop("data must be of class data.frame or basal_data_aggregate. Not ", class(data)[1])
  }
  check_inherits("basal_spec", spec)
  check_inherits("numeric", chains, iter, burn_in, thin)
  
  mcmc_args <- list(chains = chains, iter = iter, burn_in = burn_in, thin = thin)
  bad_length <- names(mcmc_args)[lengths(mcmc_args) != 1]
  if (length(bad_length) > 0) {
    stop(
      "The following argument", if (length(bad_length) > 1) "s" else "",
      " must have length one: ", paste0("`", bad_length, "`", collapse = ", "),"."
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
      "The following argument", if (length(non_integer) > 1) "s" else "",
      " must be integer-valued: ", paste0("`", non_integer, "`", collapse = ", "), "."
    )
  }
  positive_args <- mcmc_args[c("chains", "iter", "thin")]
  non_positive <- names(positive_args)[positive_args <= 0]
  if (length(non_positive) > 0) {
    stop(
      "The following argument", if (length(non_positive) > 1) "s" else "",
      " must be positive: ", paste0("`", non_positive, "`", collapse = ", "), "."
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
    expected_family = engine$logistic_family
  } else {
    stats::binomial()$family
  }
  if (spec$second_stage_spec$family$family != expected_family$family) {
    if (spec$second_stage_spec$family$family != "gaussian") {
      warning("Family for logistic model ignored.")
    }
  }
  spec$second_stage_spec$family <- expected_family
  return (spec)
}

#' computing model response
#' @noRd
get_fit_response <- function(spec, response = NULL) {
  if (is.null(response)) {
    if (is.null(spec$formula)) {
      response <- spec$default_model_data$response_name
    } else {
      if (!is.null(spec$second_stage_spec$formula) && 
          attr(stats::terms(spec$second_stage_spec$formula), which = "response") == 0) {
        stop("Found formula with no resposne")
      }
      response <- all.vars(spec$formula[[2]])[1]
    }
  }
  if (is.null(response) || length(response) != 1 || is.na(response) || response == "") {
    stop("Unable to determine the response variable from `spec`.")
  }
  
  return (response)
}

#' Set the response value of a specification
#' @noRd
set_spec_response <- function (spec, response) {
  if (!is.null(spec$formula)) {
    spec$formula <- stats::update(
      spec$formula,
      stats::reformulate(".", response)
    )
    spec$default_model_data$response_name <- NULL
  } else {
    spec$default_model_data$response_name <- response
  }
  return (spec)
}

#' computing model variables
#' @noRd
all_model_vars <- function (spec) {
  if (!is.null(spec$formula)) {
    model_variables <- list(all.vars(spec$formula))
    if (attr(stats::terms(spec$formula), which = "response") == 0) {
      # if there is no response listed, we say it's NULL
      model_variables <- union(model_variables, list(NULL))
    } else if (!is.null(spec$default_model_data$response_name)) {
      stop("Malformed specification: observed response as a formula and as `response_name`.")
    }
    if (!is.null(spec$default_model_data$domain_name)) {
      model_variables = union(model_variables, list(spec$default_model_data$domain_name))
    }
  } else {
    model_variables <- list(
      spec$default_model_data$response_name,
      spec$default_model_data$domain_name,
      spec$default_model_data$auxiliary_variables
    )
  }
  
  if (!is.null(spec$second_stage_spec)) {
    model_variables <- union(
      model_variables,
      all_model_vars(spec$second_stage_spec)
    )
  }
  if (!is.null(spec$obs_variability)) {
    model_variables <- union(model_variables, spec$obs_variability)
  }

  model_variables <- unique(model_variables)
  
  return (model_variables)
}

#' Ensure all variables are present in the data
#' @noRd
validate_model_variables <- function (variables, data) {
  variables = unlist(variables)
  missing <- setdiff(variables, colnames(data))
  if (length(missing) > 0) {
    stop("Variable", if (length(missing) > 1) "s " else " ", 
         paste0(missing, collapse = ", "), " missing from your data."
    )
  }
  
  return (data[, variables])
}

#' Parallel settings for model fitting
#' @noRd
do_parallel_settings <- function (chains, ncores, nthreads) {
  
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
      build_bhf_formula(spec = spec, response = response)
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
    stop("Auto-aggregation not supported currently.")
    # the below can be buggy, we should ensure it works well if we want it at all
    
#    if (is.null(population_size)) {
#      stop(
#        "Population size is required for auto-aggregation (computation of ",
#        "direct estimator) in area-level models."
#      )
#    }
#    data <- aggregate_data(
#      data = data,
#      response = response,
#      population_size = population_size,
#      domain_name = spec$domain_name
#    )$aggregate_obs
#    response <- "DIR_MEAN_ESTIMATOR"
#    agg_domain <- spec$domain_name
  } else {
    agg_domain <- NULL
    obs_var <- spec$obs_variability
    if (!(obs_var %in% colnames(data))) {
      stop(
        "`obs_variability` must be a vector of standard errors or a column in the data."
      )
    }
    data$DIR_MEAN_SE <- data[[obs_var]]
  }
  if (!("DIR_MEAN_SE" %in% colnames(data))) {
    stop("Unable to construct the estimated variance of the direct estimator, ",
         "`DIR_MEAN_SE` for the area-level model.")
  }
  valid_se <- is.finite(data$DIR_MEAN_SE) &
    data$DIR_MEAN_SE > 0
  data <- data[valid_se, , drop = FALSE]
  if (nrow(data) == 0) {
    stop(
      "No observations with positive, finite standard errors remain for the area-level model."
    )
  }
  
  return(
    list(
      data = data,
      response = response,
      agg_domain = agg_domain
    )
  )
}

#' Prepare data for a two-stage model
#' @noRd
prepare_two_stage_data <- function (data, response, spec) {
  
  ss_res = try(get_fit_response(spec$second_stage_spec), silent = TRUE)
  if (inherits(ss_res, "try-error")) {
    ss_res <- response
  } else if (!(ss_res %in% colnames(data))) {
    warning(
      "Logit response ", ss_res, " not found in data. Defaulting to ",
      "the base-level response ", response, "."
    )
    ss_res <- response
  }
  data$BASAL_NONZERO_INDICATOR <- as.numeric(data[[ss_res]] != 0)
  indicator_values <- unique(stats::na.omit(data$BASAL_NONZERO_INDICATOR))
  if (length(indicator_values) < 2) {
    stop("The two-stage zero-inflated model requires zero and nonzero observations.")
  }
  unfiltered_data <- data
  nonzero_data <- data[!is.na(data[[response]]) & data[[response]] != 0,]
  if (nrow(nonzero_data) == 0) {
    # should be redudant considering the above error check
    stop("No nonzero observations were found for response variable ", response, ".")
  }
  return(
    list(
      unfiltered_data = unfiltered_data,
      nonzero_data = nonzero_data
    )
  )
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

