#' Specify a `basal` prior object
#' 
#' 
#' @return An object of class `BASAL_prior`
#' @export
set_prior <- function (
    spec, engine = engine_brms(),
    ...,
    variance = NULL
) {
  spec <- validate_engine(engine, spec)
  
  vars <- all_model_vars(spec)
  res <- get_fit_response(spec)
  
  priors = list(...)
  names(priors) <- sapply(priors, function (prior) {class(prior)})
  priors$BASAL_variance_prior = formula(paste0("sigma_y ~ ", variance))
  validate_prior_inputs(priors, variance, vars, res)
  
  formula <- build_basal_formula(spec = spec, response = res)
}

#' Validate prior inputs
#' @noRd
validate_prior_inputs <- function (priors, variance, vars, res) {
  for (prior_list in c(priors$BASAL_global_prior, priors$BASAL_ranef_prior)) {
    if (!is.null(prior_list)) {
      for (prior in prior_list) {
        if (prior[[2]] != "Intercept" && !(prior[[2]] %in% vars)) {
          stop("Nonstandard prior specified: ", prior[[2]], " is not a variable in the specification.")
        } else if (prior[[2]] == res) {
          stop("Nonstandard prior specified: ", prior [[2]], " is the response.")
        }
      }
    }
  }
}

#' Specify a prior on global regression parameters
#' 
#' 
#' @return An object of class `BASAL_global_prior`
#' @export
global_prior <- function (..., intercept) {
   slopes = list(...)
   names(slopes) <- sapply(slopes, function(prior) {return(slopes[[2]])})
   slopes$`Intercept` <- formula(paste0("Intercept ~ ", intercept))
   class(slopes) <- "BASAL_global_prior"
   
   return (slopes)
}

#' Specify a prior on random effect parameters
#' 
#' @return An object of class `BASAL_ranef_prior`
#' @export
ranef_prior <- function (..., intercept) {
  out <- global_prior(..., intercept)
  
  class(out) <- "BASAL_ranef_prior"
  return (out)
}


