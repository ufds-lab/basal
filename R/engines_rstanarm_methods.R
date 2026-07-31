#' @title The `rstanarm` engine
#' @description
#' The `rstanarm` engine allows for fitting standard unit-level models. It allows
#' both custom and BHF models.
#' @export
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
  
  model_args <- union(model_args, priors)
  
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
