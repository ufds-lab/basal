#' Fit a Specified Small Area Estimation Model Object
#'
#' @param spec An object of class `basal_spec` containing initialized metadata states.
#'
#' @param data A data.frame containing the response variable and all predictor covariates.
#'
#' @param population_size Number of plots like those sampled in data. Necessary
#' for auto-aggregation in area-level models.
#'
#' @param priors Optional prior specification. If `NULL`, default priors are supplied.
#' 
#' @param second_stage_priors Optional prior specification for the Bernoulli
#' second-stage model in a two-stage zero-inflated specification. If `NULL`, 
#' default priors are supplied.
#'
#' @param chains Numeric integer. Number of MCMC chains. Defaults to `3`.
#'
#' @param iter Numeric integer. Total number of iterations per chain. Defaults to `6000`.
#'
#' @param burn_in Numeric integer. Number of burn-in/warmup iterations per chain. Defaults to `2000`.
#'
#' @param seed Numeric integer. Seed for random number generation. Defaults to `1`.
#' 
#' @param thin Thinning for the MCMC. The model keeps every one in every `thin` observations
#' 
#' @param engine Engine used for fitting model. Can only use `engine = "brms"` right now
#' 
#' @param ncores number of cores to use to computed MCMC chains in parallel
#' 
#' @param nthreads number of cores to speed up individual MCMC chains. Chosen by default in conjunction with `ncores`.
#'
#' @param ... Additional arguments passed to the model fitting engine.
#'
#' @return An object of class basal_fit containing the results.
#'
#' @export
#'
fit.basal_spec <- function(spec,
                           data,
                           population_size = NULL,
                           priors = NULL,
                           second_stage_priors = NULL,
                           chains = 3,
                           iter = 5000,
                           burn_in = 1000,
                           seed = NULL,
                           thin = 2,
                           engine = "brms",
                           ncores = default_ncores(),
                           nthreads = "default",
                           ...) {
  
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
  
  res <- get_fit_response(spec)
  second_stage_fit <- NULL
  unfiltered_data <- NULL

  if (!is.null(spec$second_stage_spec)) {
    message(
      "Estimating two models for two-stage model. ",
      "This may take a while..."
    )
    
    two_stage_data <- prepare_two_stage_data(
      data = data,
      response = res
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
    response = res,
    population_size = population_size
  )
  
  data <- area_data$data
  res <- area_data$response
  unaggregated_data <- area_data$full_data
  
  formula_info <- build_basal_formula(spec = spec, response = res)
  formula <- formula_info$formula
  valid_formula <- formula_info$valid_formula
  
  vars <- validate_formula_data(
    formula = formula,
    data = data
  )
  
  predictors <- vars[!(vars %in% c(res, "BASAL_HT_SE"))]
  
  model_priors <- build_basal_priors(
    formula = valid_formula,
    data = data,
    family = spec$family,
    response = res,
    user_priors = priors
  )
  
  parallel_settings <- do_parallel_settings(
    chains = chains,
    ncores = ncores,
    nthreads = nthreads
  )
  
  ncores <- parallel_settings$ncores
  nthreads <- parallel_settings$nthreads
  
  raw_model <- fit_brms_model(
    formula = valid_formula,
    data = data,
    priors = model_priors,
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
    model = raw_model,
    params = list(
      response = res,
      predictors = predictors
    ),
    second_stage_fit = second_stage_fit
  )
  
  if (!is.null(second_stage_fit)) {
    out$unfiltered_data <- unfiltered_data
  }
  
  return(
    structure(out, class = "basal_fit")
  )
}
