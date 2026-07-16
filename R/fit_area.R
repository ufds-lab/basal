# fit_area.R

#' Prepare data for an area-Level model
#'
#' @param spec a
#'
#' @param data a
#'
#' @param response a
#'
#' @param population_size a
#'
#' @return a
#'
#' @noRd
prepare_area_level_data <- function(spec,
                                    data,
                                    response,
                                    population_size = NULL) {
  
  model_response <- response
  full_data <- NULL
  if (spec$level != "area") {
    return(
      list(
        data = data,
        response = model_response,
        full_data = full_data
      )
    )
  }
  if (spec$model_type != "FH") {
    warning("This may not work correctly.")
  }
  
  # If the user specifies an area-level model and wants automatic aggregation,
  # compute HT estimators and replace the original response with the direct
  # estimator. This applies to both preset FH models and custom area-level models.
  if (is.null(spec$obs_variability)) {
    if (is.null(population_size)) {
      stop(
        paste0(
          "Population size is required for auto-aggregation (computation of ",
          "direct estimator) in area-level models."
        )
      )
    }
    full_data <- data
    if (spec$model_type == "FH") {
      
      trim_variables <- unique(c(spec$default_model_data$response_name,
                                 spec$default_model_data$domain_name,
                                 spec$default_model_data$auxiliary_variables))
      missing_variables <- setdiff(trim_variables, names(data))
      if (length(missing_variables) > 0) {
        stop(
          "Variables required for area-level aggregation are missing from data: ",
          paste0(missing_variables, collapse = ", "), "."
        )
      }
      trim_data <- data[, trim_variables]
      
      data <- agg_HT(
        data = trim_data,
        res = spec$default_model_data$response_name,
        N = population_size,
        domain = spec$default_model_data$domain_name
      )
    } else {
      if (is.null(spec$formula)) {
        stop("A formula must be supplied for a custom area-level model.")
      }
      if (inherits(spec$formula, "brmsformula")) {
        formula_variables <- all.vars(spec$formula$formula)
      } else {
        formula_variables <- all.vars(spec$formula)
      }
      
      missing_formula_variables <- setdiff(formula_variables, names(data))
      if (length(missing_formula_variables) > 0) {
        stop(
          "Variables required by the custom formula are missing from data: ",
          paste(missing_formula_svariables, collapse = ", "),"."
        )
      }
      trim_data <- data[, formula_variables]
      
      data <- agg_HT(
        data = trim_data,
        res = response,
        N = population_size,
        domain = spec$domain_name
      )
    }
    model_response <- "BASAL_HT_ESTIMATOR"
  } else {
    obs_var <- spec$obs_variability
    if (is.numeric(obs_var)) {
      if (length(obs_var) != 1 && length(obs_var) != nrow(data)) {
        stop(
          "`obs_variability` must have length one or the same number of rows as data."
        )
      }
      data$BASAL_HT_SE <- obs_var
    } else if (is.character(obs_var)) {
      if (length(obs_var) != 1) {
        stop(
          "`obs_variability` must be a numeric vector or a single column name."
        )
      }
      if (!(obs_var %in% colnames(data))) {
        stop(
          "`obs_variability` must be a vector of standard errors or a column in the data."
        )
      }
      data$BASAL_HT_SE <- data[[obs_var]]
    } else {
      stop(
        "`obs_variability` must be a numeric vector or the name of a column in data."
      )
    }
  }
  if (!("BASAL_HT_SE" %in% colnames(data))) {
    stop("Unable to construct `BASAL_HT_SE` for the area-level model.")
  }
  valid_se <- is.finite(data$BASAL_HT_SE) &
    data$BASAL_HT_SE > 0
  data <- data[valid_se, , drop = FALSE]
  if (nrow(data) == 0) {
    stop(
      "No observations with positive, finite standard errors remain for the area-level model."
    )
  }

  return(
    list(
      data = data,
      response = model_response,
      full_data = full_data
    )
  )
}