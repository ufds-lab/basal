#' Validate the inputs format
#' @noRd
validate_specify_inputs <- function(
  formula,
  domain_name,
  response_name,
  auxiliary_variables,
  obs_variability,
  model
) {
  if (!is.null(domain_name) &&
        (!is.character(domain_name) || any(is.na(domain_name)) ||
           any(domain_name == ""))) {
    stop("`domain_name` must be a non-empty character ",
         "string, a list thereof, or NULL.")
  }
  if (!is.null(response_name) &&
        (!is.character(response_name) || any(is.na(response_name)) ||
           any(response_name == ""))) {
    stop("`response_name` must be a non-empty character string, ",
         "a list thereof, or NULL.")
  }
  if (!is.null(auxiliary_variables) && !inherits(auxiliary_variables, "list") &&
        (!is.character(auxiliary_variables) ||
           any(auxiliary_variables == ""))) {
    stop(
      "`auxiliary_variables` must be a list, a character vector of ",
      "non-empty variable names or NULL."
    )
  }
  if (!is.null(obs_variability) &&
      !(is.character(obs_variability) &&
          length(obs_variability) == 1 &&
          !is.na(obs_variability) &&
          obs_variability != "")
  ) {
    stop("`obs_variability` must be a single non-empty character string.")
  }
  if (!is.null(formula) && !inherits(formula, "formula")) {
    if (!inherits(formula, "list")) {
      stop("Formula must be formula or list, not ", class(formula)[1], ".")
    } else if (!inherits(formula[[1]], "formula")) {
      stop("Formula must be a formula or a list of formulae. A list cannot ",
           "have class ", class(formula[[1]])[1], ".")
    }
  }

  if (intersect(unique(model), names(model_level_lookup)) != unique(model)) {
    stop(
      "model type(s) ",
      paste0(intersect(unique(model), names(model_level_lookup)),
             collapse = ", "),
      "not accepted as valid model type(s)."
    )
  }
}

#' @noRd
create_zi_specs <- function(spec, auxiliary_variables, response_name) {
  spec1 <- spec
  spec2 <- spec
  specs <- list(spec1 = spec1, spec2 = spec2)
  if (!is.null(spec$formula)) {
    if (inherits(spec$formula, "list")) {
      specs$spec1$formula <- specs$spec1$formula[[1]]
      specs$spec2$formula <- specs$spec2$formula[[2]]
    }
  }

  for (i in 1:2) {
    if (length(response_name) > 1) {
      specs[[i]]$default_model_data$response_name <- response_name[i]
    } else {
      specs[[i]]$default_model_data$response_name <- response_name
    }
    if (length(spec$domain_name) > 1) {
      specs[[i]]$default_model_data$domain_name <-
        specs[[i]]$domain_name <-
        spec$domain_name[i]
    }
    if (is.list(auxiliary_variables) && length(auxiliary_variables) > 1) {
      specs[[i]]$default_model_data$auxiliary_variables <-
        auxiliary_variables[[i]]
    } else {
      specs[[i]]$default_model_data$auxiliary_variables <-
        auxiliary_variables
    }
  }

  specs$spec2$obs_variability <- NULL
  specs$spec2$variable_transform <- NULL
  specs$spec2$call <- NULL
  specs$spec2 <- structure(specs$spec2, class = "basal_spec")

  return(specs)
}

#' @noRd
check_zi_spec_integrity <- function(spec) {
  if (!inherits(spec$formula, "formula") && length(spec$formula) > 2) {
    stop("Lists of formulae must be of length at most 2.")
  }
  if (length(spec$response_name) > 2) {
    stop("Lists of response names must be of length at most 2.")
  }
  if (is.list(spec$auxiliary_variables) &&
        length(spec$auxiliary_variables) > 2) {
    stop("Can only provide at most two lists of auxiliary variables.")
  }
}

#' @noRd
check_spec_integrity <- function(spec, auxiliary_variables, response_name) {
  if (length(spec$level) > 1) {
    stop(
      "level must be a single character string... maybe you should ",
      "set `model_stage = \"zi\"`."
    )
  }
  if (length(spec$model) > 1) {
    stop(
      "model must be a single character string... maybe you should ",
      "set `model_stage = \"zi\"`."
    )
  }

  if (!is.null(spec$formula)) {
    if (!inherits(spec$formula, "formula")) {
      if (length(spec$formula) != 1) {
        stop(
          "Expected a single base R formula for this model, but saw a ",
          class(spec$formula)[1], " with length ",
          length(spec$formula),
          " instead... did you mean to specify a zi model?"
        )
      }
      stop("Formulae must be provided as base-R formulae.")
    }
    if (!inherits(spec$formula, "formula")) {
      stop("Formulae must be a formula, not ", spec$formula, ".")
    }
    if (attr(stats::terms(spec$formula), which = "response") &&
          !is.null(response_name)) {
      stop(
        "Overspecified model: Cannot specify a formula with a response, and ",
        "a response name"
      )
    }
  }
  if (!inherits(spec$family, "family")) {
    stop("Family must be a family, not ", spec$family, ".")
  }
  if (!is.null(spec$level) && !inherits(spec$level, "character")) {
    stop("level must be a character, not ", spec$level, ".")
  } else if (!is.null(spec$level) && length(spec$level) > 1) {
    stop("level must be of length one, not ", spec$level, ".")
  }
  if (!inherits(spec$model_type, "character")) {
    stop("model must be a character, not ", spec$model_type, ".")
  } else if (length(spec$model_type) > 1) {
    stop("model must be of length one, not ", spec$model_type, ".")
  } else if (!is.null(model_level_lookup[[spec$model_type]]) &&
               !is.null(spec$level) &&
               model_level_lookup[[spec$model_type]] != spec$level) {
    stop("level corresponding to specified model type does not match.")
  }
  if (!is.null(spec$domain_name) && !inherits(spec$domain_name, "character") &&
        length(spec$domain_name) > 1) {
    stop("domain name must be NULL or a character of length one, not ",
         spec$domain_name, ".")
  }
  if (!is.null(spec$obs_variability) &&
        !inherits(spec$obs_variability, "character")) {
    stop("obs_variability must be NULL or a character")
  }
}

#' @noRd
validate_single_stage_spec <- function(
  spec, auxiliary_variables, response_name
) {
  if (!is.null(spec$formula)) {
    # our formulae should have responses
    if (!attr(stats::terms(spec$formula), which = "response")) {
      if (!is.null(response_name)) {
        # reasonable correction here, otherwise error
        spec$formula <- stats::update(
          spec$formula,
          stats::reformulate(".", response_name)
        )
      } else {
        stop("Response must be specified")
      }
    }
  }

  if (!is.null(spec$DRV)) {
    if (!is.null(spec$domain_name)) {
      spec$DRV <- spec$domain_name
    } else {
      stop("Specified `DRV` but there provided no `domain_name`. ",
           "Please provide `domain_name`.")
    }
  }
  if (spec$model_type == "custom") {
    if (is.null(spec$formula) || is.null(spec$level)) {
      stop("Must provide a formula and level for custom models.")
    }
    if (!is.null(spec$domain_name)) {
      if (is.null(spec$DRV)) {
        stop(
          "Unexpected arg: `domain_name`. If a BHF or FH model is desired, use",
          " `model = \"BHF\"` or `model = \"FH\"`."
        )
      }
    }
  } else {
    if (spec$model_type == "BHF") spec$level <- "unit"
    if (spec$model_type == "FH") spec$level <- "area"

    if (is.null(spec$domain_name)) {
      stop("Must provide domain name for ", spec$model_type, " model.")
    } else if (is.null(auxiliary_variables) && is.null(spec$formula)) {
      stop("Must provide `auxiliary_variables` or `formula` for ",
           spec$model_types, " model.")
    } else if (is.null(response_name) && is.null(spec$formula)) {
      stop("Must provide `response_name` or `formula`.")
    }

    if (spec$level == "area" && !is.null(spec$variable_transform)) {
      stop("Variable transformations are not supported with area-level models.")
    }

    spec$default_model_data <- list(
      response_name = response_name,
      domain_name = spec$domain_name,
      auxiliary_variables = auxiliary_variables
    )
  }

  if (spec$level == "area" && is.null(spec$obs_variability)) {
    if (!is.null(response_name)) {
      spec$obs_variability <- paste0(response_name, "_se")
    } else { # else the response is in the formula
      spec$obs_variability <- paste0(all.vars(spec$formula)[1], "_se")
    }
  }

  spec$valid_spec <- TRUE
  if (!is.null(spec$second_stage_spec)) {
    spec$second_stage_spec <- validate_single_stage_spec(
      spec$second_stage_spec,
      spec$second_stage_spec$default_model_data$auxiliary_variables,
      spec$second_stage_spec$default_model_data$response_name
    )
  }
  return(spec)
}
