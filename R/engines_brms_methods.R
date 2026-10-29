#' @title The \code{brms} engine
#' @description
#' The \code{brms} model allows for fitting most standard models. This includes unit- 
#' and area-level models, with most standard families in GLMs. It also permits
#' Domain-residual-variances.
#' 
#' @return Object of class \code{basal_engine}
#' @export
engine_brms <- function() {
  engine <- list()
  engine$priority = 1
  engine$name <- "brms"
  
  engine$formula_types <- c("formula", "brmsformula")
  engine$level <- c("unit", "area")
  engine$model <- c("custom", "BHF", "FH")
  engine$model_stage = c("single", "zi")
  
  engine$glm_families <- c(
    "gaussian", "bernoulli", "exponential"
  )
  
  engine$DRV <- TRUE
  
  engine$logistic_family <- brms::bernoulli()
  
  class(engine) <- "basal_engine"
  return (engine)
}

#' Produce a second formula to model DRVs for brms
#' @noRd
make_brms_DRV_addition <- function (spec, response) {
  if (!is.null(spec$DRV)) {
    return (formula(paste0("sigma ~ (1 | ", spec$domain_name, ")")))
  } else {
    return (NULL)
  }
}

#' Build a formula for a Battese-Harter-Fuller model
#' a copy of bhf for now, but may change as we add more
#' @exportS3Method basal::build_bhf_formula
#' @noRd
build_bhf_formula.brms_spec <- function (spec, response, ...) {
  formula = build_bhf_formula.default(spec, response, ...)
  
  DRV_addition = make_brms_DRV_addition(spec, response)
  
  if (!is.null(DRV_addition)) {
    return (brms::brmsformula(formula, DRV_addition))
  } else {
    return (brms::brmsformula(formula))
  }
}

#' Build a formula for a Fay-Herriot model
#' @exportS3Method basal::build_fh_formula
#' @noRd
build_fh_formula.brms_spec <- function (spec, response, ...) {
  formula <- build_bhf_formula(spec, response, ...)
  formula$formula <- stats::update(
    formula$formula,
    stats::reformulate(".", ". | se(DIR_MEAN_SE)")
  )
  
  return (formula)
}

#' Build a formula for a custom model
#' @exportS3Method basal::build_custom_formula
#' @noRd
build_custom_formula.brms_spec <- function(spec, response, ...) {
  formula <- spec$formula
  if (!is.null(spec$DRV)) {
    formula <- brms::brmsformula(
      formula,
      make_brms_DRV_addition(spec, response)
    )
  } else {
    formula <- brms::brmsformula(formula)
  }
  
  if (spec$level == "unit") {
    return (formula)
  } else if (spec$level == "area") {
    formula$formula <- stats::update(
      formula$formula,
      stats::reformulate(".", ". | se(DIR_MEAN_SE)")
    ) 
    
    return (formula)
  }
}

#' Fit a BASAL model Using brms
#' @exportS3Method basal::fit_basal_model
#' @noRd
fit_basal_model.brms_spec <- function(
    spec,
    formula,
    data,
    priors,
    family,
    chains,
    iter,
    burn_in,
    seed,
    thin,
    ncores,
    nthreads,
    ...
) {
  if (!is.null(priors) && length(priors) > 1) {
    stop("Can't assign multiple prior objects with brms, concatenate them with ",
         "c(prior(...), prior(...), ..., prior(...))")
  } else if (identical(priors, list())) {
    priors <- brms::default_prior(
      formula, data, family = family
    )
  }
  
  
  brm_args <- list(
    formula = formula,
    data = data,
    prior = priors,
    chains = chains,
    iter = iter,
    thin = thin,
    family = family,
    warmup = burn_in,
    cores = ncores,
    threads = nthreads
  )
  
  if (!is.null(seed)) {
    brm_args$seed <- seed
  }

  extra_args <- list(...)
  intersect_args <- intersect(names(extra_args), names(brm_args))
  if (length(intersect_args) > 0) {
    stop(
      "The following arguments must be supplied through `fit.basal_spec()` ",
      paste(intersect_args, collapse = ", "), "."
    )
  }
  
  brm_args <- c(brm_args, extra_args)
  raw_model <- suppressMessages(
    do.call(brms::brm, brm_args)
  )
  return (raw_model)
}

#' Get posterior predictions
#' @noRd
#' @exportS3Method basal::get_posterior_predict 
get_posterior_predict.brms_fit <- function (
    fit, model, draws, newdata, ...
) {
  return (
    rstantools::posterior_predict(
      model,
      ndraws = draws,
      newdata = newdata,
      ...
    )
  )
}

#' Get posterior expected predictions
#' @noRd
#' @exportS3Method basal::get_posterior_epred
get_posterior_epred.brms_fit <- function(
    fit,
    newdata,
    ndraws,
    allow_new_levels = FALSE,
    ...
) {
  return (
    rstantools::posterior_epred(
      fit$model,
      newdata = newdata,
      ndraws = ndraws,
      allow_new_levels = allow_new_levels,
      ...
    )
  )
}

#' Call `pp_check()` 
#' @exportS3Method basal::get_model_pp_check
#' @noRd
get_model_pp_check.brms_fit <- function (fit, model, nreps, ...) {
  return (bayesplot::pp_check(model, ndraws = nreps, ...))
}

#' Get rhat value for model parameters
#' @noRd
#' @exportS3Method basal::get_model_rhat
get_model_rhat.brms_fit <- function (fit, ...) {
  return (brms::rhat(fit$model, ...))
}

#' Get effective sample size ratio for model parameters
#' @noRd
#' @exportS3Method basal::get_model_neff_ratio
get_model_neff_ratio.brms_fit <- function (fit, ...) {
  return (brms::neff_ratio(fit$model, ...))
}

