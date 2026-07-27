#' @title The `rstanarm` engine
#' @description
#' The `rstanarm` engine allows for fitting standard unit-level models. It allows
#' both custom and BHF models.
#' 
engine_rstanarm <- function() {
  engine <- list()
  engine$priority = 2
  engine$name <- "rstanarm"
  
  engine$formula_types <- c("formula")
  engine$level <- c("unit")
  engine$model <- c("custom", "BHF")
  engine$model_stage <- c("single", "zi")
  
  engine$glm_families <- c(
    "gaussian", "bernoulli", "exponential", "poisson", "neg_binomial_2"
  )
  
  class(engine) <- "basal_engine"
  return (engine)
}

#' Default method for creating `basal` priors
#' @exportS3Method basal::build_basal_priors
#' @noRd
build_basal_priors.rstanarm_spec <- function (
  spec,
  formula,
  data,
  family,
  response,
  user_priors = NULL
) {
  # there are only a few (4) different priors one can specify for an rstanarm model
    # there is a prior on an intercept (if it exists)
    # there is a prior on regression coefficients
    # there is a prior on the variance
  # I think most of the time the default priors are reasonable,
  # though I think the default (univariate) variance is exponential, and
  # gellman mentioned that gamma family distributions might not be the best
  # for some of these things. So we might change this to half-cauchy
  
  # right now do nothing because I want to see what default priors are like
  return (NULL)
}

#' Fit a BASAL model Using `rstanarm`
#' @exportS3Method basal::fit_basal_model
#' @noRd
fit_basal_model.rstanarm_spec <- function(
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
  model_args <- list(
    formula = formula,
    data = data,
    chains = chains,
    iter = iter,
    thin = thin,
    family = family,
    warmup = burn_in,
    cores = ncores
  )
  
  if (!is.null(seed)) {
    model_args$seed <- seed
  }

  extra_args <- list(...)
  intersect_args <- intersect(names(extra_args), names(model_args))
  if (length(intersect_args) > 0) {
    stop(
      "The following arguments must be supplied through `fit.basal_spec()` ",
      paste(intersect_args, collapse = ", "), "."
    )
  }
  
  full_model_args <- c(model_args, extra_args)
  raw_model <- suppressMessages(
    do.call(rstanarm::stan_glmer, full_model_args)
  )
  return (raw_model)
}
