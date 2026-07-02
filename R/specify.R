#' Specify a Small Area Estimation Model
#'
#' @param formula Formula for a model of type `model = "custom"`. This should be 
#' the standard form of R mixed effect models. e.g., `y ~ x1 + (1 | id) + (x2 | county)`.
#' Or if you intend to use `brms` as an engine, a formula of type `brmsformula`.
#' Defaults to `NULL`.
#' 
#' @param level Character string of value `"unit"` or `"area"` denoting type of 
#' model fit. Value `"unit"` denotes unit-level (e.g., predictions on the scale 
#' of observations in auxiliary data). Value `"area`" denotes aggregate data 
#' (e.g., predicting values of direct estimators).
#' 
#' @param model Character string targeting either user-defined structures or 
#' preset SAE models. Available choices include `"custom"`, `"FH"` (Fay-Herriot),
#' and `"BHF"` (Battese-Harter-Fuller).  Defaults to `"custom"`. If `"FH"` or 
#' `"BHF"` are selected, `domain_name`, `response_name`, and `auxiliary_variable`
#' must be provided for the respective parts of the model.
#' 
#' @param obs_variability The name of a column containing the variance estimates
#' of direct estimates. If left `NULL`, then the the direct estimator of the response
#' variable in `response_name` will be estimated across the domain in `domain_name`
#' and the model will estimate these estimates.
#' 
#' @param domain_name Character string for stratification, representing
#' the areas corresponding to random effects (e.g., `"county"`).
#' 
#' @param response_name Character string specifying the column name of the target
#' response. Only required for preset framework.
#' 
#' @param auxiliary_variable Character vector containing names of covariates for 
#' the response. Defaults to using all other variables if unspecified in preset models.
#' 
#' @param variable_transform a list specifying a variable transformation 
#' `transform` and inverse variable transformation `inv_transform`. These will be
#' evaluated at points c(0,1) to ensure they work correctly. 
#' 
#' @return Object of type basal_spec.
#' 
#' @examples
#' plot_spec <- specify(
#'   formula = obs_biomass ~ evc + evt + (1 | county),
#'   level = "unit",
#'   model = "custom",
#'   variable_transform = list(
#'     transform = function (y) {y^(1/3)},
#'     inv_transform = function (y) {y^3}
#'   )
#' )
#' 
#' # gives equivalent specification to above
#' # this is not the same object though
#' plot_spec_bhf <- specify(
#'   model = "BHF",
#'   domain_name = "county",
#'   response_name = "obs_biomass",
#'   auxiliary_variables = c("evc", "evt")
#' )
#'
#' @export a
specify <- function(formula = NULL,
                    level = NULL, 
                    model = "custom", 
                    obs_variability = NULL,
                    domain_name = NULL,
                    response_name = NULL,
                    auxiliary_variables = NULL,
                    variable_transform = NULL) {
  
  func_call <- match.call()
  default_model_data <- NULL # default value. Get's overwritten if not using custom model
  
  # we should make model lose caps before this
  if (!(model %in% c("custom", "FH", "BHF"))) {
    stop("Provided model not a listed option")
  } else if (model == "custom") {
    check_inherits("formula", formula)
    if (is.null(formula)) {
      stop("Must provide a formula with a custom model.")
    } else if ((length(level) > 1) || !(level == "area" || level == "unit")) {
      stop("Level must be a single value either equal to 'area' or 'unit'.")
    }
    if (level == "unit") {
      if (!is.null(domain_name)) {
        message("Supplied a domain name for a unit-level custom model.",
                "Domain name will be ignored.")
        domain_name <- NULL
      }
      if (!is.null(obs_variability)) {
        message("Supplied variability of the observations. This isn't used",
                "use y | se(obs_variability) ~ ...",
                "this message will be cleaned up later")
        obs_variability <- NULL
      }
    } else if (level == "area") {
      if (is.null(obs_variability)) { 
        if (is.null(domain_name)) {
          stop("Must supply a domain name for auto aggregation.")
        }
      }
    }
  } else { # model != "custom"
    if (!is.null(formula)) {
      warning("Ignoring supplied formula for pre-set models. Please use custom model",
              "if you want to use a formula, or use
                'response', 'domain_name', and 'auxiliary_variables' for preset models")
      formula <- NULL
    }
    if (is.null(response_name)) {
      stop("Must supply a response variable.")
    } else if (is.null(domain_name)) {
      stop("Must supply a domain. name for BHF.")
    } else if (is.null(auxiliary_variables)) {
      warning("No auxiliary variables supplied. Using all other variables.")
      auxiliary_variables <- paste0(". - ", domain_name - response_name)
    }
    
    if (model == "BHF") {
      if (is.null(level)) {
        level <- "unit"
      } else if (level != "unit") {
        warning("Specified level is not equal to \"unit\" on a BHF model. Setting 'level = \"unit\"'.")
        level <- "unit"
      }
      obs_variability <- NULL
      
    } else if (model == "FH") {
      if (is.null(level)) {
        level <- "area"
      } else if (level != "area") {
        warning("Specified level is not equal to `\"area`\" on a FH model. Seeding 'level = \"unit\"'.")
        level <- "area"
      }   
    }
    default_model_data <- list(
      response_name = response_name,
      domain_name = domain_name,
      auxiliary_variables = auxiliary_variables
    )
  }
  
  # check variable transformation
  if (!is.null(variable_transform)) {
    if (!is.list(variable_transform)) {
      stop("variable_transform must be a list.")
    } else if (!is.function(variable_transform$transform)) {
      stop("variable_transform$transform must be a function")
    } else if (!is.function(variable_transform$inv_transform)) {
      stop("variable_transform$inv_transform must be a function")
    } else if (
      variable_transform$inv_transform(variable_transform$transform(0)) != 0 ||
      variable_transform$inv_transform(variable_transform$transform(1)) != 1
    ) {
      stop(paste0(
        "variable_transform$inv_transform isn't a right-inverse of",
        " variable_transform$transform on c(0,1)."
        ))
    }
  }

  out <- list(
    call = func_call,
    formula = formula,
    level = level,
    model_type = model,
    domain_name = domain_name,
    obs_variability = obs_variability,
    default_model_data = default_model_data,
    variable_transform = variable_transform
  )

  return(
    structure(out, class = "basal_spec")
  )
}

