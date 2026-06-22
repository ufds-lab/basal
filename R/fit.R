#' Fit a Specified Small Area Estimation Model
#'
#' @param object: object of class lacroix_spec
#' @param data a
#' @param priors a
#' @param chains a
#' @param iter a
#' @param burn_in a
#' @param seed a
#' @param ... a
#'
#' @return a
#' @export
#'
fit.lacroix_spec <- function(object,
                             data,
                             priors = NULL,
                             chains = 3,
                             iter = 6000,
                             burn_in = 2000,
                             thin = 2,
                             seed = NULL,
                             engine = "brms", # could be c("brms")
                             ...) {

  func_call <- match.call()
  
  if (!is.null(seed)) {
    set.seed(seed)
  }

  check_inherits("data.frame", data)
  check_inherits("numeric", chains, iter, burn_in, thin, seed)

#  {
#    response_var <- object$response_var
#    fixed_effects <- object$fixed_effects
#    domain_level <- object$domain_level
#    model_type <- object$model_type
#    data <- data
#    final_formula <- object$formula
#  } # out-dated, pulling relevant variables from lacroix_spec objects

  # bhf model
  if (engine == "brms") {
    
  }
  
  if (model_type == "bhf") {

    if (is.null(domain_level)) {
      stop("BHF models require a random intercept.")
    }

    if (!grepl("\\|", deparse(final_formula[[3]]))) {
      random_intercept <- paste0("(1 | ", domain_level, ")")
      final_formula <- reformulate(termlabs = c(fixed_effects, random_intercept), response = response_var)
    }
  }

  # A weakly informative default prior
  # LR - Is a standard normal weakly informative? I feel like the default priors might
  # be less informative
  if (is.null(priors)) {
    priors <- brms::prior(normal(0, 1))
  }

  # Fit the model.
  raw_fit <- suppressMessages(
    brms::brm(
      formula = final_formula,
      data = data,
      prior = priors,
      chains = chains,
      iter = iter,
      burn_in = burn_in,
      seed = seed,
      ...
    )
  )

  out <- list(
    call = func_call,
    spec = object,
    formula = final_formula,
    data = data,
    raw_model = raw_fit
  )

  structure(out, class = "lacroix_fit")
}
