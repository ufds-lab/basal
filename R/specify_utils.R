#' @noRd
validate_single_stage_spec <- function(spec, auxiliary_variables, response_name) {
  if (spec$model_type == "custom") {
    if (is.null(spec$formula) || is.null(spec$level)) {
      stop("Must provide a formula and level for custom models.")
    }
    spec$response_name <- NULL
    spec$auxiliary_variables <- NULL
    spec$default_model_data <- NULL
  } else {
    if (is.null(spec$domain_name) ||
        is.null(response_name) ||
        is.null(auxiliary_variables)) {
      # make a better error message
      stop("Must provide domain, response, and auxiliary variable names for pre-set models.")
    }
    if (spec$model_type == "BHF") spec$level <- "unit"
    if (spec$model_type == "FH") spec$level <- "area"
    spec$formula <- NULL

    spec$default_model_data <- list(
      response_name = response_name,
      domain_name = spec$domain_name,
      auxiliary_variables = auxiliary_variables
    )
  }
  return (spec)
}

#' @noRd
validate_GLM_two_stage_spec = function(spec, response_name, auxiliary_variables) {
  if (spec$family$family != "bernoulli") {
    spec$family <- brms::bernoulli()
    if (!(spec$family$family %in% c("bernoulli", "gaussian"))) {
      # more informative
      warning("Your model family is being ignored, replaced with bernoulli, for logistic model.")
    }
  }
  if (spec$model_type == "custom") {
    if (is.null(spec$formula) ||
        is.null(spec$level)) {
      message(paste0(
        "Missing values for formula and/or level will be filled by the first stage"
      ))
    }
  } else {
    if (spec$model_type == "BHF") spec$level <- "unit"
    if (spec$model_type == "FH") spec$level <- "area"
    if (spec$level == "area") {
      stop("Can't specify area-level models for the second stage.")
    }
    if (is.null(spec$domain_name) ||
        is.null(auxiliary_variables) ) {
      message(paste0(
        "One or both of domain_name or auxiliary_variables are ",
        "missing. These will be inherited from the first stage"
      ))
    }
    if (!is.null(response_name)) {
      message(paste0(
        "Response variables cannot be specified for the second-stage logistic ",
        "model, the response will be whether or not the response in the ",
        "response model is nonzero."
      ))
    }
    spec$default_model_data <- list(
      response_name = "BASAL_NONZERO_INDICATOR",
      domain_name = spec$domain_name,
      auxiliary_variables = auxiliary_variables
    )
  }
  
  return (spec)
}

#' @noRd
validate_transformation <- function(transformation) {
  if (!is.null(transformation)) {
    check_inherits("list", transformation)
    check_inherits("function",
                   transformation$transform, transformation$inv_transform)

    identity = TRUE
    for (i in 0:512) { # somewhat arbitrary points, we want zero to be included though
      # we only check that inv_transform is a left inverse of transform, because 
      # we never go the other way
      identity = identity &&
        round(transformation$inv_transform(transformation$transform(i)),10) == i
    }
    if (!identity) {
      # make this a better error message
      stop(paste0(
        "variable_transform$inv_transform isn't a right-inverse of ",
        "variable_transform$transform on 0:512. ",
        "Add override = TRUE somewhere, idk"
      ))
    }
  }
}

validate_second_stage <- function(spec, auxiliary_variables) {
  if (spec$level == "area") {
    message("Can't fit area-level models to zero observations, re-specifying as a unit-level.")
    spec$level <- "unit"
    if (!is.null(spec$obs_variability)) {
      stop("Can't re-specify model as unit-level. Must provide un-aggregated data")
    }
    spec$level <- "unit"
    spec$obs_variability <- NULL
    if (spec$model_type == "FH") {
      spec$model_type <- "BHF"
    }
  }
  
  # if there is no specification, then inherit from response model
  if (is.null(spec$second_stage_spec)) {
    message("Specification for second stage inherited from this level")
    spec$second_stage_spec <- specify(
      formula = spec$formula,
      level = spec$level,
      model = spec$model_type,
      obs_variability = NULL,
      domain_name = spec$domain_name,
      response_name = spec$response_name,
      auxiliary_variables = auxiliary_variables,
      variable_transform = NULL,
      family = brms::bernoulli(),
      model_stage = spec$model_stage,
      specifying_second_stage_model = TRUE,
      second_stage_spec = NULL
    )
  } else {
    # if there is a specification for the GLM, ensure it has all parts
    # and fill in what we can from the response model
    if (spec$second_stage_spec$model_type != spec$model_type) {
      if (spec$second_stage_spec$model_type != "custom") {
        if ((is.null(spec$second_stage_spec$default_model_data$domain_name) &&
             is.null(spec$domain_name)) ||
            (is.null(spec$second_stage_spec$default_model_data$auxiliary_variables) &&
             is.null(auxiliary_variables))) {
          stop(paste0(
            "Did not specify domain or auxiliary variables in ",
            "the logistic model. This model is custom and you have not ",
            "specified domain_name or auxiliary_variables. Please further ",
            "specify the second stage, set second stage to custom, ",
            "or set these variables here"
          ))
          
        }  else {
          if (!is.null(spec$domain_name) && 
              is.null(spec$second_stage_spec$default_model_data$domain_name)) {
            spec$second_stage_spec$default_model_data$domain_name <- spec$domain_name
            spec$second_stage_spec$domain_name <- spec$domain_name
          }
          if (!is.null(auxiliary_variables) &&
              is.null(spec$second_stage_spec$default_model_data$auxiliary_variables)) {
            spec$second_stage_spec$default_model_data$auxiliary_variables <- auxiliary_variables
          }
        }
      } else {
        if (is.null(spec$second_stage_spec$formula)) {
          warning(paste0(
            "Can't inherit formula from model type ", spec$model_type, ". Setting second ",
            "stage model type to ", spec$model_type, ". To use custom second stage, set ",
            "the formula in the second stage."
          ))
          spec$second_stage_spec$model_type <- spec$model_type
        }
      }
    }
    if (spec$second_stage_spec$model_type == spec$model_type) {
      if (spec$second_stage_spec$model_type == "custom") {
        if (is.null(spec$second_stage_spec$formula)) {
          spec$second_stage_spec$formula <- spec$formula
        }
        if (is.null(spec$second_stage_spec$level)) {
          spec$second_stage_spec$level <- spec$level
        }
      } else if (spec$second_stage_spec$model_type != "custom") {
        if (is.null(spec$second_stage_spec$default_model_data$auxiliary_variables)) {
          spec$second_stage_spec$default_model_data$auxiliary_variables <- auxiliary_variables
        }
        if (is.null(spec$second_stage_spec$default_model_data$domain_name)) {
          spec$second_stage_spec$default_model_data$domain_name <- spec$domain_name
          spec$second_stage_spec$domain_name <- spec$domain_name
        }
      }
    }
  }
  
  validate_single_stage_spec(
    spec = spec$second_stage_spec,
    auxiliary_variables = auxiliary_variables,
    response_name = "BASAL_NONZERO_INDICATOR"
  )
  
  return (spec)
}
