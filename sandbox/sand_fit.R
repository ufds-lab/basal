#' Fit a Specified Small Area Estimation Model
#'
#' @param object a
#' @param data a
#' @param priors a
#' @param chains a
#' @param iter a
#' @param warmup a
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
                             iter = 5000,
                             warmup = 3000,
                             seed = 1,
                             ...) {

  func_call <- match.call()

  check_inherits("data.frame", data)
  check_inherits("numeric", chains, iter, warmup, seed)

  response_var <- object$response_var
  fixed_effects <- object$fixed_effects
  domain_level <- object$domain_level
  model_type <- object$model_type
  data <- data
  final_formula <- object$formula

  # bhf
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
      warmup = warmup,
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
