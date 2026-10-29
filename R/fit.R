#' Fit a Specified Small Area Estimation Model Object
#'
#' @param spec An object of class \code{basal_spec} containing initialized metadata states.
#'
#' @param data A data.frame containing the response variable and all predictor covariates.
#'
#' @param priors Optional prior specification. If \code{NULL}, default priors are supplied.
#' 
#' @param second_stage_priors Optional prior specification for the logistic
#' second-stage model in a two-stage zero-inflated specification. If \code{NULL}, 
#' default priors are supplied.
#'
#' @param chains Numeric integer. Number of MCMC chains.
#'
#' @param iter Numeric integer. Total number of iterations per chain.
#'
#' @param burn_in Numeric integer. Number of burn-in/warmup iterations per chain.
#'
#' @param seed Numeric integer. Seed for random number generation.
#' 
#' @param thin Thinning for the MCMC. The model keeps every one in every \code{thin} observations
#' 
#' @param engine Engine used for fitting model. Can only use \code{engine = "brms"} right now
#' 
#' @param ncores number of cores to use to computed MCMC chains in parallel
#' 
#' @param nthreads number of cores to speed up individual MCMC chains. Chosen by default in conjunction with `ncores`.
#'
#' @param verbose Logical. If \code{TRUE}, fitting output is printed.
#'
#' @param ... Additional arguments passed to the model fitting engine.
#'
#' @return An object of class \code{basal_fit} containing the results.
#'
#' @exportS3Method generics::fit
#' @aliases fit
#'
fit.basal_spec <- function (
    spec,
    data,
    priors = specify_priors(),
    second_stage_priors = NULL,
    chains = 3,
    iter = 5000,
    burn_in = 1000,
    seed = NULL,
    thin = 2,
    engine = engine_brms(),
    ncores = default_ncores(),
    nthreads = "default",
    verbose = TRUE,
    ...
) {
  
  func_call <- match.call()
  
  validate_fit_inputs(
    spec = spec,
    data = data,
    chains = chains,
    iter = iter,
    burn_in = burn_in,
    thin = thin,
    engine = engine,
    nthreads = nthreads,
    ncores = ncores
  )
  
  if (!is.null(spec$second_stage_spec)) {
    spec <- add_engine_logit_family(spec, engine)
  }
  
  spec <- validate_engine(engine, spec)
  
  vars <- all_model_vars(spec)
  res <- get_fit_response(spec)
  
  if (inherits(data, "basal_data_aggregate")) {
    pop_data = data$aggregate_pop
    data = data$aggregate_obs
  } else {
    pop_data = NULL
  }
  
  data <- validate_model_variables(vars, data)
  
  second_stage_fit <- NULL
  unfiltered_data <- NULL
  
  if (!is.null(spec$second_stage_spec) && spec$model_stage == "zi") {
    message(
      "Estimating two models for two-stage model. ",
      "This may take a while..."
    )
    
    two_stage_data <- prepare_two_stage_data(
      data = data,
      response = res,
      spec = spec
    )
    
    spec$second_stage_spec <- set_spec_response(
      spec$second_stage_spec,
      "BASAL_NONZERO_INDICATOR"
    )
    
    second_stage_fit <- fit_second_stage(
      spec = spec$second_stage_spec,
      data = two_stage_data$unfiltered_data,
      priors = second_stage_priors,
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
    
    unfiltered_data <- two_stage_data$unfiltered_data
    data <- two_stage_data$nonzero_data
#  } else if (spec$model_stage == "zi") {
#    stop("Cannot pass the second-stage specification of a ZI model to `fit()`.")
  } else if (!is.null(spec$second_stage_spec)) {
    stop("Malformed specification: observed second-stage specification but no ZI model.")
  }
  
  if (spec$level == "area" && !is.null(spec$variable_transform)) {
    stop(
      "variable_transform is not currently supported for area-level models, ",
      "because transforming direct estimates also requires transforming their ",
      "sampling standard errors."
    )
  }
  
  if (!is.null(spec$variable_transform)) {
    trans <- spec$variable_transform$transform
    data[[res]] <- trans(data[[res]])
    if (!is.null(spec$second_stage_spec)) {
      unfiltered_data[[res]] <- trans(unfiltered_data[[res]])
    }
  }
  
  area_data <- prepare_area_level_data(
    spec = spec,
    data = data,
    response = res
  )
  
  data <- area_data$data
  res <- area_data$response
  agg_domain <- area_data$agg_domain
  
  formula <- build_basal_formula(spec = spec, response = res)
  
  predictors <- vars[!(vars %in% c(res, "DIR_MEAN_SE"))]
  
  parallel_settings <- do_parallel_settings(
    chains = chains,
    ncores = ncores,
    nthreads = nthreads
  )
  
  ncores <- parallel_settings$ncores
  nthreads <- parallel_settings$nthreads
  
  raw_model <- fit_basal_model(
    spec = spec,
    formula = formula,
    data = data,
    priors = priors,
    family = spec$family,
    chains = chains,
    iter = iter,
    burn_in = burn_in,
    seed = seed,
    thin = thin,
    ncores = ncores,
    nthreads = nthreads,
    ...
  )
  
  out <- list(
    call = func_call,
    spec = spec,
    formula = formula,
    data = data,
    pop_data = pop_data,
    model = raw_model,
    params = list(
      response = res,
      predictors = predictors
    ),
    second_stage_fit = second_stage_fit,
    engine = engine
  )
  
  if (!is.null(second_stage_fit)) {
    out$unfiltered_data <- unfiltered_data
  }
  if (!is.null(agg_domain)) {
    out$agg_domain <- agg_domain
  }
  
  return(
    structure(out, class = c(paste0(engine$name, "_fit"), "basal_fit"))
  )
}
