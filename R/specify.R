#' @title specify
#'
#' @description Specify a Small Area Estimation Model
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
#' @param auxiliary_variables Character vector containing names of covariates for
#' the response. Defaults to using all other variables if unspecified in preset models.
#'
#' @param variable_transform a list specifying a variable transformation
#' `transform` and inverse variable transformation `inv_transform`. These will be
#' evaluated at points c(0,1) to ensure they work correctly.
#'
#' @param family GLM family type, specifying error distributions and links. See `help(stats::glm)`
#' 
#' @param model_stage What stage of model to fit. Currently supported for only
#' "single" and "zi" stage models
#' 
#' @param specifying_second_stage_model Boolean indicating whether or not a specification
#' will be used for the second stage of a ZI model. Not necessary but allows partial
#' specifications which will be filled in by the larger specification.
#' 
#' @param second_stage_spec Either `NULL` or an object of type `basal_spec`, 
#' giving a specification for the second stage of a ZI model. The second stage is 
#' the GLM predicting the probability of zero-valued plots.
#' 
#' @return Object of type `basal_spec`.
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
#'   auxiliary_variables = c("evc", "evt"),
#'   variable_transform = list(
#'     transform = function (y) {y^(1/3)},
#'     inv_transform = function (y) {y^3}
#'   )
#' )
#' 
#' # we can specify area level models with `model = "FH"`, or `level = "area"`
#' FH_spec <- specify(
#'   model = "FH",
#'   domain_name = "county",
#'   response_name = "obs_biomass",
#'   auxiliary_variables = c("evc", "evt")
#'  )
#'  
#'  # and we can create an equivalent model manually:
#'  FH_spec_manual <- specify(
#'    obs_biomass ~  evc + evt + (1 | county),
#'    level = "area",
#'    domain = "county"
#'  )
#'  
#'  # We allow zero-inflated models as well
#'  # This will inherit the remainder of the arguments from the response model
#'  GLM_spec <- specify(
#'    model = "BHF",
#'    auxiliary_variables = c("evc", "evt"),
#'    specifying_second_stage = TRUE
#'  )
#'  
#' response_model <- specify(
#'   model = "BHF",
#'   response = "obs_biomass",
#'   auxiliary_variables = "tcc",
#'   domain = "county",
#'   model_stage = "zi",
#'   second_stage_spec = GLM_spec,
#'   variable_transform = list(
#'     transform = function (y) {y^(1/3)},
#'     inv_transform = function (y) {y^3}
#'   )
#' )
#' 
#' @export
specify <- function(formula = NULL,
                    level = NULL,
                    model = "custom",
                    obs_variability = NULL,
                    domain_name = NULL,
                    response_name = NULL,
                    auxiliary_variables = NULL,
                    variable_transform = NULL,
                    family = stats::gaussian(),
                    model_stage = "single",
                    specifying_second_stage_model = FALSE,
                    second_stage_spec = NULL) {

  # If we have a custom model, the user must provide a formula and a level
  # If we have a non-custom model, the user must specify a model (type)
  # For both, there must be response, auxiliary, and domain
  # for BHF, this is it
  # for FH, we can have obs_variability

  {
    func_call <- match.call()

    match.arg(level, c(NULL, "area", "unit")) # not the best practice with NULL here but works nicely
    match.arg(model, c("custom", "FH", "BHF")); stopifnot(!is.null(model))
    match.arg(model_stage, c("single", "zi")); stopifnot(!is.null(model_stage))
    default_model_data <- NULL
  } # housekeeping provided parameters
  
  
  spec <- list(
    call = func_call,
    formula = formula,
    family = family,
    level = level,
    model_type = model,
    domain_name = domain_name,
    obs_variability = obs_variability,
    default_model_data = default_model_data,
    variable_transform = variable_transform,
    model_stage = model_stage,
    second_stage_spec = second_stage_spec
  )

  if (specifying_second_stage_model) {
    # re-code model so coming if statement separates it
    model <- "aux_spec"
    spec$model_stage <- "zi"
  }

  if (model != "aux_spec") {
    spec <- validate_single_stage_spec(spec = spec, 
                                      auxiliary_variables = auxiliary_variables,
                                      response_name = response_name)
  } else if (model == "aux_spec") {
    spec <- validate_GLM_two_stage_spec(spec = spec, 
                                       auxiliary_variables = auxiliary_variables,
                                       response_name = response_name)
  }

  # correct/set variables depending on their level
  if (!is.null(spec$level) && spec$level == "unit" && !is.null(spec$obs_variability)) {
    message("Supplied variability of the observations. This isn't used ",
            "use `y | se(obs_variability) ~ <covariates>`",
            " to specify known variance.")
    spec$obs_variability <- NULL
  }
  if (!is.null(spec$level) &&
      spec$level == "area" &&
      is.null(spec$domain_name) &&
      is.null(spec$obs_variability) &&
      !specifying_second_stage_model) {
    stop(paste0(
      "Must have either a domain name (for auto aggregation) or observed ",
      "variability with area level models"
    ))
  }

  # validate the variable transformation --- make sure we have a left-inverse
  validate_transformation(spec$variable_transform)
  
  if (spec$model_stage == "zi" && !specifying_second_stage_model) {
    spec <- validate_second_stage(spec, auxiliary_variables)
  }
  return(
    structure(spec, class = "basal_spec")
  )
}

