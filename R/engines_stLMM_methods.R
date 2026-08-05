#' @title The `stLMM` engine
#' @description
#' The `brms` model allows for fitting standard models. This includes unit- 
#' and area-level models, with most standard families in GLMs. It also permits
#' Domain-residual-variances, and spatio-temporal process terms.
#' 
#' @return Object of class `basal_engine`
#' @export
engine_stLMM <- function() {
  stop("Unable to use stLMM as an engine yet.")
  engine <- list()
  engine$priority = 10
  engine$name <- "stLMM"
  
  engine$formula_types <- c("formula")
  engine$level <- c("unit", "area")
  engine$model <- c("custom", "BHF", "FH")
  engine$model_stage = c("single", "zi")
  
  engine$glm_families <- c(
    "gaussian", "binomial"
  )
  engine$logistic_family <- stats::binomial()
  
  class(engine) <- "basal_engine"
  return (engine)
}

#' Default method for computing model variables
#' @exportS3Method basaal::all_model_vars
#' @noRd
all_model_vars.stLMM <- function (spec, ss = FALSE, ...) {
  warning("Make sure that the user doesn't use any variables for other parts ",
          "of the formula. e.g., using a variable in the global environment ",
          "that has, say, a numeric, for a process term.")
  
  return (
    NextMethod("all_model_vars")
  )
}

#' Build a formula for a Fay-Herriot model
#' @exportS3Method basal::build_fh_formula
#' @noRd
build_fh_formula.stLMM <- function(spec, response, ...) {
  # an area level model is the same as a unit-level except that the residual
  # variance prior gets changed. We will still do auto-aggregation, because then
  # we can add flexibility which we might not be able to add in stLMM, but we 
  # can provide these variance estimates in the prior
  formula <- stats::formula(paste0(
    response, " ~ ", 
    paste0(spec$default_model_data$auxiliary_variables, collapse = " + "), " + ",
    "iid(", spec$default_model_data$domain_name, ")"
  ))
  return (formula)
}

#' Build a Formula for a BHF Model
#' @exportS3Method basal::build_bhf_formula
#' @noRd
build_bhf_formula.stLMM <- build_fh_formula.stLMM

#' Build a formula for a custom model
#' @exportS3Method basal::build_custom_formula
#' @noRd
build_custom_formula.stLMM <- function(spec, response, ...) {
  # we have to replace standard mixed effects specification with the custom stLMM
  # specification (i.e., (1 | id) -> iid(id) or (x | id) -> x:iid(id))
  breaks <- reformulas::findbars_x(spec$formula)
  base_formula_str <- deparse(splitForm(spec$formula)$fixedFormula)
  extra_terms <- list()
  for (term in breaks) {
    term <- paste(term)[2]
    split_term <- unlist(strsplit(term, " "))
    if (length(split_term) != 3) {
      stop("non-standard term: ", paste(term))
    }
    if (split_term[1] == "1") {
      first_term <- ""
    } else {
      first_term <- paste0(split_term[1], ":")
    }
    new_term <- paste0(first_term, "iid(", split_term[3], ")")
    extra_terms <- union(extra_terms,
                         list(new_term))
  }
  
  new_formula <- paste0(
    base_formula_str, " + ", paste0(unlist(extra_terms), collapse = " + ")
  )
  return (stats::formula(new_formula))
}

#' Default method for fitting a model; errors as a default, should not be called
#' This might be unnecessary (with no default, ommitting an implementation will error)
#' but is useful in that this R file is a template for creating an engine
#' @exportS3Method basal::fit_basal_model
#' @noRd
fit_basal_model.stLMM <- function (
    spec,
    formula,
    data,
    priors,
    family,
    chains,
    iters,
    burn_in,
    seed,
    thin,
    ncores,
    nthreads,
    ...
) {
  stop("No method for fitting the bayesian model with engine ", 
       gsub(x = class(spec)[1], pattern = "_spec", replacement = ""),
       ". ")
  stop("Ensure that the residual variance prior gets set for area level models")
  stop("Add default prior for random effects")
  stop("Thinning is currently not supported")
  
  model_fit <- stLMM::stLMM(
    formula,
    data = data,
    family = family,
    chains = chains,
    n_samples  = iters,
    n_report = ceiling(iters/5),
    priors = priors,
    n_omp_threads = ncores,
    chain_control = list(seed = seed),
    ...
  )
}

#' Call `pp_check()` 
#' @exportS3Method basal::get_model_pp_check
#' @noRd
get_model_pp_check.stLMM <- function (fit, model, ...) {
  stop("do this")
}

#' Get rhat value for model parameters
#' @noRd
#' @exportS3Method basal::get_model_rhat
get_model_rhat.stLMM <- function (fit, ...) {
  stop("do this")
}

#' Get effective sample size ratio for model parameters
#' @noRd
#' @exportS3Method basal::get_model_neff_ratio
get_model_neff_ratio.stLMM <- function (fit, ...) {
  stop("do this")
}

#' Get all model variable names
#' @noRd
#' @exportS3Method basal::get_all_variable_names
get_all_variable_names.stLMM <- function (fit, ...) {
  stop("do this")
}

#' Get posterior predictions
#' @noRd
#' @exportS3Method basal::get_posterior_predict 
get_posterior_predict.stLMM <- function (
    fit, model, draws, newdata, ...
) {
  stop("do this")
}

#' Get posterior expected predictions
#' @noRd
#' @exportS3Method basal::get_posterior_epred
get_posterior_epred.stLMM <- function(
    fit,
    newdata,
    ndraws,
    allow_new_levels = FALSE,
    ...
) {
  stop("do this")
}

#' Get number of posterior draws
#' @noRd
#' @exportS3Method basal::get_available_draws
get_available_draws.stLMM <- function(
    fit,
    ...
) {
  nrow(as_mcmc(fit$model))
}

#' Get grouping variables
#' @noRd
#' @exportS3Method basal::get_grouping_variables
get_grouping_variables.stLMM <- function(
    fit,
    ...
) {
  return (names(stats::coef(zi_fit$model)))
}

