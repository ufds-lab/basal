#' @title Validate an engine
#' @description Internal function for ensuring an engine is able to fit a specification
#' noRd
validate_engine <- function(engine, spec) {
  if (!inherits(engine, "basal_engine")) {
    stop("Engines should be specified by a `basal::engine_*()` function.")
  }
  
  name <- engine$name
  if (!(name %in% basal_engines)) {
    stop("Engine ", name, " is not a registered engine with basal. ",
         "Valid engines for `basal` are: ", paste(basal_engines, collapse = ", "), ".")
  }

  if (!(is.null(spec$formula) || class(spec$formula)[1] %in% engine$formula_types)) {
    stop("Cannot fit a custom model using a formula of class ", class(spec$formula)[1],
	 " with engine ", name, ". Can only use formulae of class: ", 
          paste(engine$formula_types, collapse = ","), ".")
  }

  if (!(spec$level %in% engine$level)) {
    stop("Cannot fit a(n) ", spec$level, "-level model with engine ", name, ". Can only fit models with level: ",
         paste(engine$level, collapse = ","), ".")
  }

  if (!(spec$model_type %in% engine$model)) {
    stop("Cannot fit a ", spec$model_type, " model with ", name, ". Can only fit model types: ",
         paste(engine$model, collapse = ","), ".")
  }

  if (!(spec$model_stage %in% engine$model_stage)) {
    stop("Cannot fit models with stage ", spec$model_stage, " with engine ", name, ". Can only fit stages: ",
	 paste(engine$model_stage, collapse = ","), ".")
  }

  if (!(spec$family$family %in% engine$glm_families)) {
    stop("Cannot use family equal to ", spec$family, " with engine ", name, ". Can only use families: ",
	 paste(engine$glm_families, collapse = ","), ".")
  }

  class(spec) <- c(paste0(name, "_spec"), class(spec))
  if (!is.null(spec$second_stage_spec)) {
    spec$second_stage_spec <- validate_engine(engine, spec$second_stage_spec)
  }
  return (spec)
}

#' Get the response formula from a formula
#' @exportS3Method basal::get_response_formula
#' @noRd
get_response_formula.default <- function (formula) {
  return (formula)
}

#' Set the response value of a specification
#' @noRd
#' @exportS3Method basal::set_spec_response
set_spec_response.default <- function (spec, response) {
  if (spec$model_type == "custom") {
    tmp_formula <- formula(paste0(response, " ~ 1"))
    # the formula `formula(~x1 + x2)`, for example, is valid, and in this case
    # the second entry of the formula are the covariates rather than the response
    # so we move them to the standard place in a formula with a response and covariates
    if (length(spec$formula) == 2) {
      spec$formula[[3]] <- spec$formula[[2]] 
    }
    spec$formula[[2]] <- tmp_formula[[2]]
  } else {
    spec$default_model_data$response_name <- response
  }
  return (spec)
}

#' Default method for computing model response
#' @exportS3Method basal::get_fit_response
#' @noRd
get_fit_response.default <- function(spec, response = NULL) {
  if (is.null(response)) {
    if (is.null(spec$formula)) {
      response <- spec$default_model_data$response_name
    } else {
      response <- all.vars(spec$formula[[2]])[1]
    }
  }
  if (is.null(response) || length(response) != 1 || is.na(response) || response == "") {
    stop("Unable to determine the response variable from `spec`.")
  }
  
  return (response)
}

#' Default method for computing model variables
#' @exportS3Method basaal::all_model_vars
#' @noRd
all_model_vars.default <- function (spec, ss = FALSE) {
  if (spec$model_type == "custom") {
    model_variables <- list(all.vars(spec$formula))
    if (length(spec$formula == 2)) {
      # if there is no response listed, we say it's NULL
      model_variables <- union(model_variables, list(NULL))
    }
    # it's unnecessary to include res in the above, but I'm doing it
    # just in case. We will call unique() anyway
  } else {
    model_variables <- list(
      spec$default_model_data$response_name,
      spec$default_model_data$domain_name,
      spec$default_model_data$auxiliary_variables
    )
  }

  if (!is.null(spec$second_stage_spec)) {
    model_variables <- c(
      model_variables,
      all_model_vars(spec$second_stage_spec)
    )
  }
  model_variables <- unique(model_variables)

  return (model_variables)
}


#' Build a formula for a Fay-Herriot model
#' @exportS3Method basal::build_fh_formula
#' @noRd
build_fh_formula.default <- function(spec, response) {
  formula <- stats::formula(paste0(
    response, " ~ ", 
    paste0(spec$default_model_data$auxiliary_variables, collapse = " + "), " + ",
    "(1 | ",  spec$default_model_data$domain_name, ")"
  ))

  return (formula)
}

#' Build a Formula for a BHF Model
#' @exportS3Method basal::build_bhf_formula
#' @noRd
build_bhf_formula.default <- function (spec) {
  formula <- stats::formula(paste0(
    spec$default_model_data$response_name, " ~ ",
    paste0(spec$default_model_data$auxiliary_variables, collapse = " + "), " + ",
    "(1 | ",  spec$default_model_data$domain_name, ")"
  ))
  
  return (formula)
}

#' Build a formula for a custom model
#' @exportS3Method basal::build_custom_formula
#' @noRd
build_custom_formula.default <- function(spec, response) {
  return (spec$formula)
}

#' Default method for creating `basal` priors
#' @exportS3Method basal::build_basal_priors
#' @noRd
build_basal_priors.default <- function (
  spec,
  formula,
  data,
  family,
  response,
  user_priors = NULL
) {
  return (NULL)
}

#' Default method for fitting a model; errors as a default, should not be called
#' This might be unnecessary (with no default, ommitting an implementation will error)
#' but is useful in that this R file is a template for creating an engine
#' @exportS3Method basal::fit_basal_model
#' @noRd
fit_basal_model.default <- function (spec, ...) {
  stop("No method for fitting the bayesian model with engine ", 
       gsub(x = class(spec)[1], pattern = "_spec", replacement = ""),
       ". ")
}

#' Call `pp_check()` 
#' @exportS3Method basal::get_model_pp_check
#' @noRd
get_model_pp_check.default <- function (fit, model, ...) {
  return (bayesplot::pp_check(model, ...))
}

#' Get rhat value for model parameters
#' @noRd
#' @exportS3Method basal::get_model_rhat
get_model_rhat.default <- function (fit, ...) {
  return (bayesplot::rhat(fit$model, ...))
}

#' Get effective sample size ratio for model parameters
#' @noRd
#' @exportS3Method basal::get_model_neff_ratio
get_model_neff_ratio.default <- function (fit, ...) {
  return (bayesplot::rhat(fit$model, ...))
}

#' Get all model variable names
#' @noRd
#' @exportS3Method basal::get_all_variable_names
get_all_variable_names.default <- function (fit, ...) {
  return (names(fit$model$coefficients))
}

#' Get posterior predictions
#' @noRd
#' @exportS3Method basal::get_posterior_predict 
get_posterior_predict.default <- function (
    fit, model, draws, newdata, ...
) {
  return (
    rstantools::posterior_predict(
      model,
      draws = draws,
      newdata = newdata,
      ...
    )
  )
}

#' Get posterior expected predictions
#' @noRd
#' @exportS3Method basal::get_posterior_epred
get_posterior_epred.default <- function(
    fit,
    newdata,
    ndraws,
    allow_new_levels = FALSE,
    ...
) {
  rstantools::posterior_epred(
    fit$model,
    newdata = newdata,
    ndraws = ndraws,
    allow_new_levels = allow_new_levels,
    ...
  )
}

#' Get number of posterior draws
#' @noRd
#' @exportS3Method basal::get_available_draws
get_available_draws.default <- function(
    fit,
    ...
) {
  nrow(as.data.frame(fit$model))
}

#' Get grouping variables
#' @noRd
#' @exportS3Method basal::get_grouping_variables
get_grouping_variables.brms_fit <- function(
    fit,
    ...
) {
  return (names(coef(zi_fit$model)))
}

