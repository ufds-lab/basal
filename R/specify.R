#' Specify a Small Area Estimation Model
#'
#' @param formula Formula for a model of type 'model = "custom"'. This should be 
#' the standard form of R mixed effect models. e.g., y ~ x1 + (1 | id) + (x2 | county).
#' Or if you intend to use `brms` as an engine, a formula of type `brmsformula`
#' @param level Character string denoting type of model fit. Value "unit" denotes 
#' unit-level (e.g., predictions on the scale of observations in auxiliary data).
#' Value "area" denotes aggregate data (e.g., predicting values of direct estimators)
#' @param model Character string denoting the method of fitting model. Selecting 
#' "Custom" allows for the specification of a formula and level, while selecting
#' FH or BHF does not. If FH or BHF are selected, domain, response, and auxiliary
#' variable names must be provided
#' @param obs_variability The name of a column containing the variance estimates
#' of direct estimates. If left NULL, then the the direct estimator of the response
#' variable in `response_name` will be estimated across the domain in `domain_name`
#' and the model will estimate these estimates
#' @param domain_name The name of a column containing the different domains of
#' observations. 
#' @param response_name The name of the variate to model as the response
#' @param auxiliary_variable Vector containing character vectors with the name of 
#' covariates for the response. Or __everything to use every other variable
#' @return Object of type lacroix_spec
#' @export a
#'
specify <- function(formula = NULL,
                    level = NULL, #should be one of c("area", "unit"),
                    model = "custom", # model should be in c("custom", "FH", "BHF")
                    obs_variability = NULL,
                    domain_name = NULL,
                    response_name = NULL,
                    auxiliary_variables = NULL) {
  
  func_call <- match.call()
  default_model_data <- NULL # default value. Get's overwritten if not using custom model
  
  # we should make model lose caps before this
  if (!(model %in% c("custom", "FH", "BHF"))) {
    stop("Provided model not a listed option")
  } else if (model == "custom") {
    check_inherits("formula", formula)
    if (is.null(formula)) {
      stop("Must provide a formula with a custom model.")
    } else if (is.vector(level) || !(level == "area" || level == "unit")) {
      stop("Level must be a single value either equal to 'area' or 'unit'.")
    } 
    if (level == "unit") {
      if (!is.null(domain_name)) {
        message("Supplied a domain name for a unit-level custom model.",
                "Domain name will be ignored.")
        domain_name = NULL
      }
      if (!is.null(obs_variability)) {
        message("Supplied variability of the observations. This isn't used",
                "use y | se(obs_variability) ~ ...",
                "this message will be cleaned up later")
        obs_variability = NULL
      }
    } else if (level == "area") {
      if (is.null(obs_variability)) { 
        auto_agg = TRUE
        if (is.null(domain_name)) {
          stop("Must supply a domain name for auto aggregation.")
        }
      } else {
        auto_agg = FALSE
      }
    } 
  } else { # model != "custom"
    if (!is.null(formula)) {
      warning("Ignoring supplied formula for pre-set models. Please use custom model",
              "if you want to use a formula, or use 
                'response', 'domain_name', and 'auxiliary_variables' for preset models")
      formula <- NULL
    }
    if (model != "BHF") {
      stop("Other default models not currently supported.")
    }
    if (model == "BHF") {
      if (is.null(level)) {
        level <- "unit"
      } else if (level != "unit") {
        warning("Specified is not equal to \"unit\" on a BHF model. Setting 'level = \"unit\"'.")
        level <- "unit"
      }
      auto_agg <- FALSE
      obs_variability <- NULL
      if (is.null(response_name)) {
        stop("Must supply a response variable.")
      } else if (is.null(domain_name)) {
        stop("Must supply a domain. name for BHF.")
      } else if (is.null(auxiliary_variables)) {
        warning("No auxiliary variables supplied. Using all other variables.")
        auxiliary_variables <- "__everything"
      }
      default_model_data = list(
        response_name = response_name,
        domain_name = domain_name,
        auxiliary_variables = auxiliary_variables
      )
    } else if (model == "FH") {
      stop("Do this later.")
    }
  }
  
  out <- list(
    call = func_call,
    formula = formula,
    level = level,
    model_type = model,
    domain_name = domain_name,
    obs_variability = obs_variability,
    auto_aggregate = auto_agg,
    default_model_data = default_model_data
  )
  
  return(
    structure(out, class = "lacroix_spec")
  )
}

