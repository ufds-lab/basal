# fit_formula.R

#' Build a formula for a custom model
#'
#' @param spec a
#'
#' @param response a
#'
#' @return a
#'
#' @noRd
build_custom_formula <- function(spec, response) {
  formula <- spec$formula
  if (inherits(formula, "brmsformula")) {
    if (spec$level == "area") {
      stop(
        "Custom area-level models supplied as `brmsformula` objects are not currently supported."
      )
    }
    return(
      list(
        formula = formula$formula,
        valid_formula = formula
    ))
  }
  
  if (spec$level == "unit") {
    
    valid_formula <- brms::brmsformula(formula)
    return(
      list(
        formula = formula,
        valid_formula = valid_formula
      )
    )
  }
  
  if (spec$level == "area") {
    if (spec$family$family == "gaussian") {
      tmp_formula <- stats::as.formula(paste0(response, " | se(BASAL_HT_SE) ~ 1"))

      if (length(all.vars(formula[[2]])) > 1) {
        stop(
          paste0(
            "Cannot fit area-levels with pre-specified brms addition terms.",
            " If you want addition terms, you should pre-aggregate data and fit ",
            "a unit-level model with the addition terms."
          )
        )
      }
    } else {
      tmp_formula <- stats::as.formula(paste0(response, " ~ 1"))
    }
    
    # Inject the synthesized measurement error LHS into the user-supplied custom formula.
    formula[[2]] <- tmp_formula[[2]]
    valid_formula <- brms::brmsformula(formula)
    
    return(
      list(
        formula = formula,
        valid_formula = valid_formula
      )
    )
  }
  stop("`spec$level` must be either \"unit\" or \"area\".")
}

#' Build a Formula for a BHF Model
#'
#' @param spec a
#'
#' @return a
#'
#' @noRd
build_bhf_formula <- function(spec) {
  
  formula <- stats::formula(paste0(
    spec$default_model_data$response_name, " ~ ",
    paste0(spec$default_model_data$auxiliary_variables, collapse = " + "), " + ",
    "(1 | ",  spec$default_model_data$domain_name, ")"
  ))
  
  valid_formula <- brms::brmsformula(formula)
  
  return(
    list(
      formula = formula,
      valid_formula = valid_formula
    )
  )
}


#' Build a formula for a Fay-Herriot model
#'
#' @param spec a
#'
#' @param response a
#' 
#'
#' @return a
#'
#' @noRd
build_fh_formula <- function(spec, response) {
  
  if (spec$family$family == "gaussian") {
    formula <- stats::formula(
      paste0(response, " | se(BASAL_HT_SE) ~ ", paste0(spec$default_model_data$auxiliary_variables, collapse = " + "),
             " + ", "(1 | ", spec$default_model_data$domain_name, ")"
    ))
    
  } else {
    formula <- stats::formula(
      paste0(response, " ~ ", paste0(spec$default_model_data$auxiliary_variables, collapse = " + "),
             " + ", "(1 | ", spec$default_model_data$domain_name,")"
    ))
  }
  valid_formula <- brms::brmsformula(formula)
  return(
    list(
      formula = formula, valid_formula = valid_formula
    )
  )
}


#' Build the formula for a model
#'
#' @param spec a
#'
#' @param response a
#'
#' @return a
#'
#' @noRd
build_basal_formula <- function(spec, response) {
  
  if (spec$model_type == "custom") {
    return(
      build_custom_formula(spec = spec, response = response)
    )
  }
  if (spec$model_type == "BHF") {
    return(
      build_bhf_formula(spec = spec)
    )
  }
  if (spec$model_type == "FH") {
    return(
      build_fh_formula(spec = spec, response = response)
    )
  }
  stop(paste0("Unsupported model type: ", spec$model_type, "."))
}

