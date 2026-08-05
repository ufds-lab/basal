#' Posterior estimation of summary statistics
#'
#' @param fit object of type \code{basal_fit}.
#'
#' @param newdata data to predict data on. If \code{NULL}, fit on training data.
#'
#' @param domain (vector of) names of areas to aggregate estimates on. If \code{NULL},
#' aggregate all the data together.
#'
#' @param stat Named vector of function(s) to apply to posterior predictions.
#' 
#' @param aggregation_statistic Statistic to conduct inference on. Statistic will
#' be computed over population data, stratifying by \code{domain}
#'
#' @param ndraws number of draws from the posterior predictive distribution.
#'
#' @param max_preds maximum number of points to make predictions on. Capped to
#' avoid R session crashing. A value of \code{NULL} or \code{Inf} will predict on all.
#'
#' @param seed The seed for random number generation in posterior prediction.
#' 
#' @export
#' 
estimate <- function(
    fit,
    newdata = NULL,
    domain = "BASAL_INHERIT",
    stat = c(
      mean = mean,
      var = var,
      lower_95_ci = lower_ci_quantile,
      upper_95_ci = upper_ci_quantile
    ),
    aggregation_statistic = c(mean = mean),
    ndraws = 1000,
    max_preds = "default",
    seed = NULL
) {
  
  validate_estimate_inputs(
    fit = fit,
    newdata = newdata,
    domain = domain,
    stat = stat,
    aggregation_statistic = aggregation_statistic,
    ndraws = ndraws,
    max_preds = max_preds,
    seed = seed
  )
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  max_preds <- prepare_max_preds(max_preds)
  two_stage <- !is.null(fit$second_stage_fit)
  
  newdata <- prepare_estimate_data(
    fit = fit,
    newdata = newdata,
    two_stage = two_stage
  )
  
  domain_info <- prepare_estimate_domain(
    fit = fit,
    newdata = newdata,
    domain = domain
  )
  
  domain <- domain_info$domain
  newdata <- domain_info$newdata
  
  validate_estimate_draws(
    fit = fit,
    ndraws = ndraws
  )
  
  nd_subset <- subset_prediction_data(
    newdata = newdata,
    max_preds = max_preds
  )
  
  nd_subset <- prepare_area_prediction_data(
    fit = fit,
    newdata = nd_subset,
    domain = domain,
    two_stage = two_stage
  )
  
  post_preds <- get_estimate_predictions(
    fit = fit,
    newdata = nd_subset,
    ndraws = ndraws
  )
  
  if (!is.null(fit$spec$variable_transform)) {
    inv_trans <- fit$spec$variable_transform$inv_transform
    post_preds <- inv_trans(post_preds)
  }
  
  if (two_stage) {
    second_stage_weights <- get_estimate_predictions(
      fit = fit$second_stage_fit,
      newdata = nd_subset,
      ndraws = ndraws
    )
    
    post_preds <- post_preds * second_stage_weights
  }
  
  preds <- aggregate_posterior_predictions(
    newdata = nd_subset,
    post_preds = post_preds,
    domain = domain,
    ndraws = ndraws,
    aggregation_statistic = aggregation_statistic
  )
  
  stat <- prepare_estimate_stats(stat)
  
  ret_preds <- summarize_estimate_predictions(
    preds = preds,
    domain = domain,
    stat = stat,
    aggregation_statistic = aggregation_statistic
  )
  
  ret <- list(
    call = match.call(),
    fit = fit,
    params = list(
      newdata = newdata,
      domain = domain,
      stat = stat,
      aggregation_statistic = aggregation_statistic,
      ndraws = ndraws,
      max_preds = max_preds,
      seed = seed
    ),
    preds = ret_preds,
    raw_rep_preds = preds
  )
  
  return(
    structure(ret, class = "basal_estimate")
  )
}
