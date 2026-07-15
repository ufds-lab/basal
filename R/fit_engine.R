# fit_engine.R

#' Parallel settings for model fitting
#'
#' @param chains a
#'
#' @param ncores a
#'
#' @param nthreads a
#'
#' @return a
#'
#' @noRd
do_parallel_settings <- function(chains, ncores, nthreads) {
  
  # A thread here is used to speed up within-chain computations.
  if (ncores >= 2 * chains) {
    nthreads <- max(nthreads, floor(ncores/chains))
    ncores <- chains
  }
  
  return(
    list(
      ncores = ncores,
      nthreads = nthreads
    )
  )
}


#' Fit a BASAL model Using brms
#'
#' @param formula a
#'
#' @param data a
#'
#' @param priors a
#'
#' @param family a
#'
#' @param chains a
#'
#' @param iter a
#'
#' @param burn_in a
#'
#' @param seed a
#'
#' @param thin a
#'
#' @param ncores a
#'
#' @param nthreads a
#'
#' @param ... a
#'
#' @return a
#'
#' @noRd
fit_brms_model <- function(formula,
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
                           ...) {
  
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
  if (length(intersect_args) > 0L) {
    stop(
      "The following arguments must be supplied through `fit.basal_spec()` ",
      paste(intersect_args, collapse = ", "), "."
    )
  }
  
  brm_args <- c(brm_args, extra_args)
  raw_model <- suppressMessages(
    do.call(brms::brm, brm_args)
  )
  return(raw_model)
}