#' Fit a Specified Small Area Estimation Model Object
#'
#' @param object An object of class `lacroix_spec` containing initialized metadata states.
#'
#' @param data A data.frame containing the response variable and all predictor covariates.
#'
#' @param priors Optional prior specification. If NULL, default priors are supplied.
#'
#' @param chains Numeric integer. Number of MCMC chains. Defaults to 3.
#'
#' @param iter Numeric integer. Total number of iterations per chain. Defaults to 5000.
#'
#' @param warmup Numeric integer. Number of burn-in/warmup iterations per chain. Defaults to 2000.
#'
#' @param seed Numeric integer. Seed for random number generation. Defaults to 1.
#'
#' @param ... Additional arguments passed to the model fitting engine.
#'
#' @return An object of class `lacroix_fit` containing the results.
#'
#' @export
fit.lacroix_spec <- function(object,
                             data,
                             priors = NULL,
                             chains = 3,
                             iter = 5000,
                             warmup = 2000,
                             seed = 1,
                             ...) {

  func_call <- match.call()

  check_inherits("data.frame", data)
  check_inherits("numeric", chains, iter, warmup, seed)

  model_type <- object$model_type
  level      <- object$level
  working_data  <- data
  final_formula <- object$formula

  # Preset models
  # 1. Unit-level BHF
  if (model_type == "BHF") {

    # Extract the variables metadata container block bundled during specify stage
    bhf_spec <- object$default_model_data
    resp  <- bhf_spec$response
    dom   <- bhf_spec$domain_name
    auxes <- bhf_spec$auxiliary_variables

    if (auxes == "__everything") {
      # If no predictors are supplied, grab all variables excluding target response and domain id
      all_cols <- names(working_data)
      auxes <- all_cols[!(all_cols %in% c(resp, dom))]
    }

    # The standard BHF representation
    # e.g., resp ~ aux1 + aux2 + (1 | domain)
    random_effect <- paste0("(1 | ", dom, ")")
    rhs <- c(auxes, random_effect)
    final_formula <- reformulate(termlabs = rhs, response = resp)
  }

  # We might want to find a better way to set the default prior.
  # For now, we just set a weakly informative prior if 'prior' remain empty.
  if (is.null(priors)) {
    priors <- brms::set_prior("normal(0, 1)", class = "b")
  }

  raw_mcmc_fit <- suppressMessages(
    brms::brm(
      formula = final_formula,
      data    = working_data,
      prior   = priors,
      chains  = chains,
      iter    = iter,
      warmup  = warmup,
      seed    = seed,
      ...
    )
  )

  out <- list(
    call         = func_call,
    spec         = object,
    formula      = final_formula,
    data         = working_data,
    raw_model    = raw_mcmc_fit
  )

  return(
    structure(out, class = "lacroix_fit")
  )
}
