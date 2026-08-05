#' Posterior Predictive and Regression checks from fit `basal` model.
#'
#' @param fit object of class `fit` from `fit()`.
#'
#' @param stat extra functions to plot for posterior checks. Multiple can be
#' specified if included in a list (i.e., `stat = c(mean = mean, ecdf = ecdf)`)
#' functions should be named.
#'
#' @param include_base_pp_check Logical determining whether or not to include the
#' empirial density funciton as a posterior predictive check. Defaults to `TRUE`
#'
#' @param draws number of draws for posterior prediction
#'
#' @param trace_plots Logical flag to include trace plots of parameter estimates.
#' Defaults to `FALSE` so that large number of plots (if including many random effects)
#' are not included in the object.
#' 
#' @param two_stage_stat Like `stat`, but used for the second stage of a second-stage model
#' 
#' @param join_two_stage_stat Like `stat` but used for the response aggregated
#' from the multi-stage model.
#'
#' @return An object of class `basal_check`.
#'
#' @export
check <- function(
    fit,
    stat = c(ecdf = stats::ecdf),
    include_base_pp_check = TRUE,
    draws = 50,
    trace_plots = FALSE,
    two_stage_stat = c(proportion_positive = prop_positive),
    join_two_stage_stat = c(joined_ecdf = stats::ecdf)
) {
  
  validate_check_inputs(
    fit = fit,
    draws = draws,
    include_base_pp_check = include_base_pp_check,
    trace_plots = trace_plots
  )
  
  stat <- prepare_check_stats(stat = stat, argument = "stat")
  two_stage_stat <- prepare_check_stats( stat = two_stage_stat, argument = "two_stage_stat")
  
  join_two_stage_stat <- prepare_check_stats(
    stat = join_two_stage_stat,
    argument = "join_two_stage_stat"
  )
  
  has_second_stage <- !is.null(fit$second_stage_fit)
  
  ret <- list(
    call = match.call(),
    pp_checks = list(response = list(), second_stage = list(), joined = list()),
    convergence = list(response = list(), second_stage = NULL),
    params = list(
      draws = draws,
      include_base_pp_check = include_base_pp_check,
      trace_plots = trace_plots
    )
  )
  
  ret$pp_checks$response <- build_pp_checks(
    fit = fit,
    draws = draws,
    stat = stat,
    include_base_pp_check = include_base_pp_check
  )
  
  ret$convergence$response <- build_convergence_checks(
    fit = fit,
    trace_plots = trace_plots
  )
  
  if (has_second_stage) {
    
    ret$pp_checks$second_stage <- build_pp_checks(
      fit = fit$second_stage_fit,
      draws = draws,
      stat = two_stage_stat,
      include_base_pp_check = FALSE
    )
    
    if (!is.null(join_two_stage_stat)) {
      ret$pp_checks$joined <- build_pp_checks(
        fit = fit,
        draws = draws,
        stat = join_two_stage_stat,
        include_base_pp_check = FALSE
      )
    }
    
    ret$convergence$second_stage <- build_convergence_checks(
      fit = fit$second_stage_fit,
      trace_plots = trace_plots
    )
  }
  
  return(
    structure(ret, class = "basal_check")
  )
}
