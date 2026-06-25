#' Fit a Specified Small Area Estimation Model Object
#'
#' @param object An object of class `basal_spec` containing initialized metadata states.
#'
#' @param data A data.frame containing the response variable and all predictor covariates.
#'
#' @param priors Optional prior specification. If NULL, default priors are supplied.
#'
#' @param chains Numeric integer. Number of MCMC chains. Defaults to 3.
#'
#' @param iter Numeric integer. Total number of iterations per chain. Defaults to 6000.
#'
#' @param burn_in Numeric integer. Number of burn-in/warmup iterations per chain. Defaults to 2000.
#'
#' @param seed Numeric integer. Seed for random number generation. Defaults to 1.
#'
#' @param ... Additional arguments passed to the model fitting engine.
#'
#' @return An object of class `basal_fit` containing the results.
#'
#' @export
fit.basal_spec <- function(object,
                           data,
                           priors = NULL,
                           chains = 3,
                           iter = 6000,
                           burn_in = 2000,
                           seed = NULL,
                           ...) {

  func_call <- match.call()
  

  check_inherits("data.frame", data)
  check_inherits("numeric", chains, iter, burn_in, thin, seed)
  check_inherits("basal_spec", object)
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  
  if (object$auto_aggregate) {
    stop("Compute HT estimates and variances. Add to data and object$model_type")
  }
  if (level == "area") {
    stop("Add area-level models")
  }
  
  if (object$model_type == "custom") {
    if (engine == "brms") {
      valid_formula = brmsformula(object$formula)
    }
  } else if (object$model_type == "BHF") {
    valid_formula = 
      formula(paste0(
        object$default_model_data$response_name, " ~ ",
        paste0(object$default_model_data$auxiliary_variables, collapse = " + "), " + ",
        "(1 | ",  object$default_model_data$domain_name, ")"
      ))
  } else if (object$model_type == "FH") {
    stop("Can't fit this type of model right now.")
  }
  
  vars = all.vars(valid_formula)
  if (length((missing = setdiff(vars, colnames(data)))) != 0) {
    if (length(missing) == 1) {
      stop(paste0("Variable ", missing, " missing from your data."))
    } else {
      stop(paste0("Variables ", paste0(missing, collapse = ", "), 
                  " missing from your data."))
    }
  }
  
  # set la-croix default priors
  # we don't want improper priors on the random effect variances
  # so we can over-estimate these variances by multiplying the total variability
  # of the data by some number (>1). We know that the variability of random effects
  # should be less than the variability of the data, so this shouldn't be 
  # too informative
  
  prior_sd = sd(data[[object$default_model_data$response_name]]) # compute sd
  
  # we modify default priors from brms
  priors = default_prior(
    valid_formula,
    data
  )
  
  #### This isn't the best practice below. Discuss with Wystan
  
  # we want most things to have flat priors to do this, we change default priors 
  # to do this we change every prior which has a nonempty entry to empty (flat)
  priors$prior[priors$prior != "" & priors$source != "user"] = 
    rep("", sum(priors$prior != "" & priors$source != "user"))
  # we also set sd default parameters to half-cauchy priors
  priors$prior[which(priors$class == "sd" & priors$source == "default" & priors$group == "")] = 
    paste0("student_t(3,0,", prior_sd * 3.5, ")")
  
  # then this is just for a user-facing front
  priors$source[which(v$source != "(vectorized)")] = "default (basal)"
  
  raw_model <- suppressMessages(
    brms::brm(
      formula = valid_formula,
      data = data,
      prior = priors,
      chains = chains,
      iter = iter,
      warmup = warmup,
      seed = seed,
      ...
    )
  )

  out <- list(
    call = func_call,
    spec = object,
    formula = final_formula,
    data = working_data,
    model = raw_model
  )

  return(
    structure(out, class = "basal_fit")
  )
}
