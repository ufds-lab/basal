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
    if (spec$model_type == "BHF") spec$level <- "unit"
    if (spec$model_type == "FH") spec$level <- "area"
    
    if (!is.null(spec$formula)) {
      spec$model_type <- "custom"
      if (!inherits(spec$formula, "formula")) {
        stop("Must use base-R formula when specifying a formula for a BHF or FH.")
      }
      spec$formula <- 
        formula(paste0(deparse(spec$formula), " + (1 | ", spec$domain_name, ")"))
      return (
        validate_single_stage_spec(spec, auxiliary_variables, response_name)
      )
    }
    
    if (is.null(spec$domain_name)) {
      stop("Must provide domain name for ", spec$model_type, " model.")
    } else if (is.null(auxiliary_variables)) {
      stop("Must provide `auxiliary_variables` or `formula` for ", spec$model_types, " model.")
    } else if (is.null(response_name)) {
      stop("Must provide `response_name` or `formula`.")
    }
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
  # We can't set the family to binomial() automatically because brms *doesn't use
  # a binomial family for logistic regression* (???) Instead, we will have to 
  # set this within fit() after an engine has been chosen
  if (spec$family$family != "gaussian") {
    message("Specified family ignored for logit model. This will automatically ",
            "set within fit(), after an engine has been chosen.")
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
    
    if (!is.null(spec$formula)) {
      spec$model_type <- "custom"
      if (!inherits(spec$formula, "formula")) {
        stop("Must use base-R formula when specifying a formula for a BHF or FH.")
      }
      if (is.null(spec$domain_name)) {
        stop("If you wish to specify a BHF with a formula, provide the domain name. ",
             "The domain cannot otherwise be easily inferred from the response model.")
      }
      spec$formula <- 
        formula(paste0(deparse(spec$formula), " + (1 | ", spec$domain_name, ")"))
      return (
        validate_GLM_two_stage_spec(spec, auxiliary_variables, response_name)
      )
    }
    
    if (is.null(spec$domain_name) ||
        (is.null(auxiliary_variables) &&
         is.null(spec$formula))) {
      message(paste0(
        "One or both of domain or auxiliary variables are ",
        "missing. These will be inherited from the first stage"
      ))
    }
    
    spec$default_model_data <- list(
      response_name = response_name,
      domain_name = spec$domain_name,
      auxiliary_variables = auxiliary_variables
    )
  }
  
  if (spec$level == "area") {
    stop("Can't specify area-level models for the second stage.")
  }
  
  return (spec)
}

#' Get just the response model from a formula
#' Only necessary because of types of formulae which
#' specify multiple different relations at once (e.g., brmsformula)
#' @noRd
get_response_formula <- function (formula) {
  UseMethod("get_response_formula")
}

validate_second_stage <- function(spec, auxiliary_variables) {
  if (spec$level == "area") {
    warning("Can't fit area-level models to zero observations, re-specifying as a unit-level.")
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
      formula = get_response_formula(spec$formula),
      level = spec$level,
      model = spec$model_type,
      obs_variability = NULL,
      domain_name = spec$domain_name,
      response_name = spec$response_name,
      auxiliary_variables = auxiliary_variables,
      variable_transform = NULL,
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
            ((is.null(spec$second_stage_spec$default_model_data$auxiliary_variables) &&
              is.null(auxiliary_variables)) &&
              is.null(spec$second_stage_spec$formula) &&
              is.null(spec$formula))) {
          stop(paste0(
            "Did not specify domain or auxiliary variables in ",
            "the logistic model. This model is not custom but you have not ",
            "specified domain_name or auxiliary_variables (or formula). Please further ",
            "specify the second stage or set these variables in the non-logit specification"
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
          } else if (!is.null(spec$formula) &&
                     is.null(spec$second_stage_spec$formula) &&
                     is.null(spec$second_stage_spec$default_model_data$auxiliary_variables)) {
            spec$second_stage_spec$formula <- spec$formula
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
          spec$second_stage_spec$formula <- get_response_formula(spec$formula)
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
  
  # at this point, the logit specification should pass the test forced 
  # to the single-stage model. This will also re-specify a few models (e.g.,
  # BHF or FH with formulae -> custom model) for ease of use in fit()
  if (spec$second_stage_spec$model_type != "custom") {
    tmp <- spec$second_stage_spec$default_model_data$response_name
    spec$second_stage_spec$default_model_data$response_name <- "BASAL_TMP_VAR"
    spec$second_stage_spec <- validate_single_stage_spec(
      spec$second_stage_spec,
      spec$second_stage_spec$default_model_data$auxiliary_variables,
      "BASAL_TMP_VAR"
    )
    spec$second_stage_spec$default_model_data$response_name <- tmp
  } else {
    spec$second_stage_spec <- validate_single_stage_spec(
      spec$second_stage_spec, NULL, "BASAL_TMP_VAR"
    )
  }
  
  return (spec)
}

#' @title Make Variable Transformation
#' @param transform Function to transform the response
#' @param inv_transform Right inverse of `transform` i.e., `inv_transform(transform(x)) == x`
#' @param eval_grid Grid of values to ensure that the `transform` and `inv_transform` are consistent with each other
#' @param override logical flag to indicate that the check on `eval_grid` should be ignored.
#' 
#' @return Object of type `BASAL_transformation`
#' @export
make_variable_transform <- function(transform,
                                    inv_transform,
                                    eval_grid = 0:512,
                                    override = FALSE) {
  transformation = list(
    transform = transform,
    inv_transform = inv_transform
  )
  validate_transformation(transformation, eval_grid, override)
  
  class(transformation) <- "BASAL_transformation"
  return (transformation)
}

#' @noRd
validate_transformation <- function(transformation,
                                    eval_grid, override) {
  if (!is.null(transformation)) {
    check_inherits("list", transformation)
    check_inherits("function",
                   transformation$transform, transformation$inv_transform)

    if (!override) {
      identity = TRUE
      for (i in eval_grid) { # somewhat arbitrary points, we want zero to be included though
        # we only check that inv_transform is a left inverse of transform, because 
        # we never go the other way
        identity = identity &&
          round(transformation$inv_transform(transformation$transform(i)),10) == i
      }
      if (!identity) {
        stop(paste0(
          "variable_transform$inv_transform isn't a right-inverse of ",
          "variable_transform$transform on `eval_grid` (defaults to 0:512). To bypass the check ",
          "create your variable transformation with: ",
          "`create_variable_transformation(transform, inv_transform, override = TRUE)`."
        ))
      }
    }
  }
}

