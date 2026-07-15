# fit_two_stage.R

#' Prepare data for a two-stage model
#'
#' @param data a
#'
#' @param response a
#'
#' @return a
#'
#' @noRd
prepare_two_stage_data <- function(data, response) {
  
  if (!(response %in% colnames(data))) {
    stop(paste0("Response variable ", response, " missing from your data."))
  }
  if (any(data[[response]] < 0, na.rm = TRUE)) {
    stop("Zero-inflated models require a non-negative response.")
  }
  data$BASAL_NONZERO_INDICATOR <- as.numeric(data[[response]] > 0)
  indicator_values <- unique(stats::na.omit(data$BASAL_NONZERO_INDICATOR))
  if (length(indicator_values) < 2) {
    stop("The two-stage zero-inflated model requires zero and positive observations.")
  }
  unfiltered_data <- data
  positive_data <- data[!is.na(data[[response]]) & data[[response]] > 0,]
  if (nrow(positive_data) == 0) {
    stop(paste0("No positive observations were found for response variable ", response, "."))
  }
  return(
    list(
      unfiltered_data = unfiltered_data,
      positive_data = positive_data
    )
  )
}


#' Fit the second stage of a two-stage model
#'
#' @param spec a
#'
#' @param data a
#'
#' @param population_size a
#'
#' @param priors a
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
#' @param engine a
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
fit_second_stage <- function(spec,
                             data,
                             priors = NULL,
                             chains,
                             iter,
                             burn_in,
                             seed,
                             thin,
                             engine,
                             ncores,
                             nthreads,
                             ...) {
  
  if (is.null(spec$second_stage_spec)) {
    return(NULL)
  }
  
  second_stage_fit <- fit.basal_spec(
    spec = spec$second_stage_spec,
    data = data,
    population_size = NULL,
    priors = priors,
    second_stage_priors = NULL,
    chains = chains,
    iter = iter,
    burn_in = burn_in,
    seed = seed,
    thin = thin,
    engine = engine,
    ncores = ncores,
    nthreads = nthreads,
    ...
  )
  
  return(second_stage_fit)
}