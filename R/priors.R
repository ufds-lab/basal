#' priors.R
#' 
#' Compute the response scale
#'
#' @param data a
#'
#' @param response a
#'
#' @return a
#'
#' @noRd
get_response_scale <- function(data, response) {
  
  response_sd <- sd(data[[response]], na.rm = TRUE)
  if (is.infinite(response_sd) || response_sd <= 0) {
    stop("Unable to calculate response standard deviation.")
  }
  
  return(response_sd)
}


#' Construct default priors
#'
#' BASAL sets priors supplied by brms and only modifies
#' group-level standard deviation priors where small area
#' estimation provides additional justification, following Gelman (2006).
#'
#' @param formula a
#'
#' @param data a
#'
#' @param family a
#'
#' @param response a
#'
#' @return A `brmsprior` object.
#'
#' @noRd
make_basal_default_priors <- function(
    formula,
    data,
    family,
    response
) {
  
  priors <- brms::default_prior(
    object = formula,
    data = data,
    family = family
  )
  
  sd_mask <- priors$class == "sd" & priors$coef == "Intercept"
  
  if (family$family == "gaussian") {
    response_sd <- get_response_scale(
      data = data,
      response = response
    )
    sd_prior <- paste0("student_t(3, 0, ", 2.5 * response_sd,")")
    priors[sd_mask, ]$prior <- sd_prior
    priors[sd_mask, ]$source <- "default (basal)"
    
  } else if (family$family == "bernoulli") {
    sd_prior <- "student_t(3, 0, 2.5)"
    priors[sd_mask, ]$prior <- sd_prior
    priors[sd_mask, ]$source <- "default (basal)"
  }
  return(priors)
}


#' Build Priors for a BASAL Model
#'
#' @param formula a
#'
#' @param data a
#'
#' @param family a
#'
#' @param response a
#'
#' @param user_priors a
#'
#' @return a
#'
#' @noRd
build_basal_priors <- function(
    formula,
    data,
    family,
    response,
    user_priors = NULL
) {
  
  if (!is.null(user_priors)) {
    if (!inherits(user_priors, "brmsprior")) {
      stop("`priors` must inherit from class 'brmsprior'.")
    }
    # use validate_prior()
    return(user_priors)
  }
  
  priors <- make_basal_default_priors(
    formula = formula,
    data = data,
    family = family,
    response = response
  )
  
  return(priors)
}
