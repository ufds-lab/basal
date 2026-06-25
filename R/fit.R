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
  

  check_inherits("data.frame", data)
  check_inherits("numeric", chains, iter, burn_in, thin, seed)
  check_inherits("lacroix_spec", object)
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  
  if (object$auto_aggregate) {
    stop("Compute HT estimates and variances. Add to data and object$model_type")
  }
  if (level == "area") {
    stop("Add area-level models")
  }
  
  if (object$model_type == "custom") {
    if (engine == "brms") {
      valid_formula = brmsformula(object$formula)
    }
  } else if (object$model_type == "BHF") {
    valid_formula = 
      formula(paste0(
        object$default_model_data$response_name, " ~ ",
        paste0(object$default_model_data$auxiliary_variables, collapse = " + "), " + ",
        "(1 | ",  object$default_model_data$domain_name, ")"
      ))
  } else if (object$model_type == "FH") {
    stop("Can't fit this type of model right now.")
  }
  
  vars = all.vars(valid_formula)
  if (length((missing = setdiff(vars, colnames(data)))) != 0) {
    if (length(missing) == 1) {
      stop(paste0("Variable ", missing, " missing from your data."))
    } else {
      stop(paste0("Variables ", paste0(missing, collapse = ", "), 
                  " missing from your data."))
    }
  }
  
   v = validate_prior(
      prior("(flat)", class = "sd"),
      valid_formula,
      data
  )
   
  validate_prior(
    prior("(flat)", class = ),
    valid_formula,
    data
  )
  
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
