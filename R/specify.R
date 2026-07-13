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
#'   auxiliary_variables = c("evc", "evt")
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
                    family = gaussian(),
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
  } # housekeeping provided parameters

  if (specifying_second_stage_model) {
    # re-code model so coming for loop separates it
    old_model <- model
    model <- "aux_spec"
    model_stage <- "zi"
    default_model_data <- NULL
  }

  if (model == "custom") {
    if (is.null(formula) || is.null(level)) {
      stop("Must provide a formula and level for custom models.")
    }
    default_data_model <- NULL
    response_name <- NULL; auxiliary_variables <- NULL
    default_model_data <- NULL
  } else if (model != "aux_spec") {
    if (is.null(domain_name) ||
        is.null(response_name) ||
        is.null(auxiliary_variables)) {
      stop("Must provide domain, response, and auxiliary variable names for pre-set models.")
    }
    if (model == "BHF") level <- "unit"
    if (model == "FH") level <- "area"
    formula <- NULL;

    default_model_data <- list(
      response_name = response_name,
      domain_name = domain_name,
      auxiliary_variables = auxiliary_variables
    )
  } else if (model == "aux_spec") {
    if (family$family != "bernoulli") {
      family <- brms::bernoulli()
    }
    model <- old_model
    if (model == "custom") {
      if (is.null(formula) ||
          is.null(level)) {
        message(paste0(
          "Missing values for formula and/or level will be filled by the first stage"
        ))
      }
    } else {
      if (model == "BHF") level <- "unit"
      if (model == "FH") level <- "area"
      if (level == "area") {
        stop("Can't specify area-level models for the second stage.")
      }
      if (is.null(domain_name) ||
          is.null(response_name) ||
          is.null(auxiliary_variables) ) {
        message(paste0(
          "One or more of domain_name, response_name, or auxiliary_variables are ",
          "missing. These will be inherited from the first stage"
        ))
      }
      default_model_data <- list(
        response_name = "BASAL_NONZERO_INDICATOR",
        domain_name = domain_name,
        auxiliary_variables = auxiliary_variables
      )
    }
  }

  # condition parameters based on level
  if (!is.null(level) && level == "unit" && !is.null(obs_variability)) {
    message("Supplied variability of the observations. This isn't used ",
            "use `y | se(obs_variability) ~ <covariates>`",
            " to specify known variance.")
    obs_variability <- NULL
  }
  if (!is.null(level) &&
      level == "area" &&
      is.null(domain_name) &&
      is.null(obs_variability) &&
      !specifying_second_stage_model) {
    stop(paste0(
      "Must have either a domain name (for auto aggregation) or observed ",
      "variability with area level models"
    ))
  }

  # check variable transformation
  if (!is.null(variable_transform)) {
    check_inherits("list", variable_transform)
    check_inherits("function",
                   variable_transform$transform, variable_transform$inv_transform)

    identity = TRUE
    for (i in 1:10) {
      identity = identity &&
        round(variable_transform$inv_transform(variable_transform$transform(i)),10) == i
    }
    if (!identity) {
      stop(paste0(
        "variable_transform$inv_transform isn't a right-inverse of",
        " variable_transform$transform on 1:10."
      ))
    }
  }

  if (model_stage == "zi" && !specifying_second_stage_model) {
    if (is.null(second_stage_spec)) {
      message("Specification for second stage inherited from this level")
      second_stage_spec <- specify(
        formula = formula,
        level = level,
        model = model,
        obs_variability = NULL,
        domain_name = domain_name,
        response_name = response_name,
        auxiliary_variables = auxiliary_variables,
        variable_transform = NULL,
        family = brms::bernoulli(),
        model_stage = model_stage,
        specifying_second_stage_model = TRUE,
        second_stage_spec = NULL
      )
    } else {
      if (second_stage_spec$model_type != model) {
        if (second_stage_spec$model_type != "custom") {
          if ((is.null(second_stage_spec$default_model_data$domain_name) &&
               is.null(domain_name)) ||
              (is.null(second_stage_spec$default_model_data$auxiliary_variables) &&
               is.null(auxiliary_variables))) {
            stop(paste0(
              "Did not specify domain or auxiliary variables in ",
              "the second stage model. This model is custom and you have not ",
              "specified domain_name or auxiliary_variables. Please further ",
              "specify the second stage, set second stage to custom, ",
              "or set these variables here"
            ))

          }  else {
            if (!is.null(domain_name) && is.null(second_stage_spec$default_model_data$domain_name)) {
              second_stage_spec$default_model_data$domain_name <- domain_name
              second_stage_spec$domain_name <- domain_name
            }
            if (!is.null(auxiliary_variables) &&
                is.null(second_stage_spec$default_model_data$auxiliary_variables)) {
              second_stage_spec$default_model_data$auxiliary_variables <- auxiliary_variables
            }
          }
        } else {
          if (is.null(second_stage_spec$formula)) {
            message(paste0(
              "Can't inherit formula from model type ", model, ". Setting second ",
              "stage model type to ", model, ". To use custom second stage, set ",
              "the formula in the second stage."
            ))
            second_stage_spec$model_type <- model
          }
        }
      }
      if (second_stage_spec$model_type == model) {
        if (second_stage_spec$model_type == "custom") {
          if (is.null(second_stage_spec$formula)) {
            second_stage_spec$formula <- formula
          }
          if (is.null(second_stage_spec$level)) {
            second_stage_spec$level <- level
          }
        } else if (second_stage_spec$model_type != "custom") {
          if (is.null(second_stage_spec$default_model_data$auxiliary_variables)) {
            second_stage_spec$default_model_data$auxiliary_variables <- auxiliary_variables
          }
          if (is.null(second_stage_spec$default_model_data$domain_name)) {
            second_stage_spec$default_model_data$domain_name <- domain_name
            second_stage_spec$domain_name <- domain_name
          }
        }
      }
    }
    if (level == "area") {
      message("Can't fit area-level models to zero observations, re-specifying as a unit-level.")
      level <- "unit"
      if (!is.null(obs_variability)) {
        stop("Can't re-specify model as unit-level. Must provide un-aggregated data")
      }
      if (model == "FH") {
        model <- "BHF"
      }
    }
  }

  out <- list(
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

  return(
    structure(out, class = "basal_spec")
  )
}

