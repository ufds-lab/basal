#' @title The `brms` engine
#' @description
#' The `brms` model allows for fitting most standard models. This includes unit- 
#' and area-level models, with most standard families in GLMs. It also permits
#' Domain-residual-variances.
#' 
#' @return Object of class `basal_engine`
#' @export
engine_brms <- function() {
  engine <- list()
  engine$priority = 1
  engine$name <- "brms"
  
  engine$formula_types <- c("formula", "brmsformula")
  engine$level <- c("unit", "area")
  engine$model <- c("custom", "BHF", "FH")
  engine$model_stage = c("single", "zi")
  
  engine$glm_families <- c(
    "gaussian", "bernoulli", "exponential"
  )
  
  class(engine) <- "basal_engine"
  return (engine)
}

#' Extract the response variables
#' @exportS3Method basal::get_fit_response
#' @noRd
get_fit_response.brms_spec <- function(spec, response = NULL) {
  if (is.null(response)) {
    if (inherits(spec$formula, "brmsformula")) {
      response <- all.vars(spec$formula$formula[[2]])[1]
    } 
  }

  return (
    NextMethod("get_fit_response")
  )
}

#' Extract all variables from a model
#' @exportS3Method basal::all_model_vars
#' @noRd
all_model_vars.brms_spec <- function (spec, ss = FALSE) {
  if (spec$model_type == "custom") {
    if (inherits(spec$formula, "brmsformula")) {
      model_variables <- all.vars(spec$formula$formula)
      
      # now recurse through sub-equations, if there are any
      if (!is.null(spec$formula$pforms)) {
        for (sub_form in formula$pforms) {
          sub_vars <- all.vars(sub_form)
          sub_vars <- sub_vars[sub_vars != sub_form[[2]]]
          model_variables <- c(variables, sub_vars)
        }
      }
    } else {
      return (NextMethod("all_model_vars"))
    }
  } else {
    model_variables <- c(
      spec$default_model_data$response_name,
      spec$default_model_data$domain_name,
      spec$default_model_data$auxiliary_variables
    )
  }
  
  if (!is.null(spec$second_stage_spec)) {
    model_variables <- c(
      model_variables,
      all_model_vars(spec$second_stage_spec)
    )
  }
  
  model_variables <- unique(model_variables)
  
  return (model_variables)
}


#' Build a formula for a Fay-Herriot model
#' @exportS3Method basal::build_fh_formula
#' @noRd
build_fh_formula.brms_spec <- function (spec, response) {
  if (spec$family$family == "gaussian") {
    formula <- stats::formula(
      paste0(response, " | se(BASAL_HT_SE) ~ ", paste0(spec$default_model_data$auxiliary_variables, collapse = " + "),
             " + ", "(1 | ", spec$default_model_data$domain_name, ")"
    ))
  }

  return (brms::brmsformula(formula))
}

#' Build a formula for a custom model
#' @exportS3Method basal::build_custom_formula
#' @noRd
build_custom_formula.brms_spec <- function(spec, response) {
  formula <- spec$formula
  if (inherits(formula, "brmsformula")) {
    if (spec$level == "area") {
      stop(
        "Custom area-level models supplied as `brmsformula` objects are not currently supported."
      )
    }
    return (formula)
  }
  
  if (spec$level == "unit") {
    return (brms::brmsformula(formula))
  }
  
  if (spec$level == "area") {
    if (spec$family$family == "gaussian") {
      tmp_formula <- stats::as.formula(paste0(response, " | se(BASAL_HT_SE) ~ 1"))

      if (length(all.vars(formula[[2]])) > 1) {
        stop(
	  "Cannot fit area-levels with pre-specified brms addition terms.",
	  " If you want addition terms, you should pre-aggregate data and fit ",
	  "a unit-level model with the addition terms."
        )
      }
    } else {
      tmp_formula <- stats::as.formula(paste0(response, " ~ 1"))
      warning(
	"Fitting (non-gaussian) area-level GLM. There is no variance parameter in this GLM ",
	"and so the known variance from the direct estimator cannot be used. If there is a ",
	"variance parameter, or the variance can be derived from a second parameter, ",
	"set an informative prior on that parameter."
      )
    }
    
    # Inject the synthesized measurement error LHS into the user-supplied custom formula.
    formula[[2]] <- tmp_formula[[2]]
    return (brms::brmsformula(formula))
  }
  stop("`spec$level` must be either \"unit\" or \"area\".")
}


#' Build `brms` default priors for a `basal` model
#' BASAL sets priors supplied by brms and only modifies
#' group-level standard deviation priors where small area
#' estimation provides additional justification, following Gelman (2006).
#' @return A `brmsprior` object.
#' @exportS3Method basal::build_basal_priors
#' @noRd
build_basal_priors.brms_spec <- function (
    spec,
    formula,
    data,
    family,
    response,
    user_priors = NULL
) {
  
  if (!is.null(user_priors)) {
    if (!inherits(user_priors, "brmsprior")) {
      stop("`priors` must inherit from class 'brmsprior'.")
    }
    # use validate_prior()
    priors <- brms::validate_prior(
      prior = user_priors,
      formula = formula,
      data = data
    )
  } else {
    priors <- brms::default_prior(
      object = formula,
      data = data,
      family = family
    )
  }
  
  default_priors = priors$source != "user"
  
  sd_mask <- priors$class == "sd" & priors$coef == "Intercept" & default_priors
  
  if (family$family == "gaussian") {
    response_sd <- get_scale(
      data = data,
      variable = response
    )
    sd_prior <- paste0("student_t(3, 0, ", 2.5 * response_sd,")")
    priors[sd_mask, ]$prior <- sd_prior
    priors[sd_mask, ]$source <- "default (basal)"
    
  } else if (family$family == "bernoulli") {
    sd_prior <- "student_t(3, 0, 2.5)"
    priors[sd_mask, ]$prior <- sd_prior
    priors[sd_mask, ]$source <- "default (basal)"
  }

  if (priors$source[1] != "user") {
    reg_coef_mask <- priors$class == "b" & priors$coef != "" & default_priors
    default_predictors <- priors$coef[reg_coef_mask]
    pred_sd <- sapply(default_predictors, function(pred) {get_scale(data, pred)})
    
    priors[reg_coef_mask, ]$prior <- paste0("normal(0, ", 2.5 * pred_sd, ")")
    priors[reg_coef_mask, ]$source <- "default (basal)"
  }
  
  intercept = priors$class == "Intercept"
  if (priors$source[intercept] == "default") {
    priors$prior[intercept] = gsub(x = priors$prior[intercept], pattern = "student_t\\(3, ", replacement = "normal(")
    priors$source[intercept] = "default (basal)"
  }
  
  return(priors)
}

#' Fit a BASAL model Using brms
#' @exportS3Method basal::fit_basal_model
#' @noRd
fit_basal_model.brms_spec <- function(
    spec,
    formula,
    data,
    priors,
    family,
    chains,
    iter,
    burn_in,
    seed,
    thin,
    ncores,
    nthreads,
    ...
) {
  brm_args <- list(
    formula = formula,
    data = data,
    prior = priors,
    chains = chains,
    iter = iter,
    thin = thin,
    family = family,
    warmup = burn_in,
    cores = ncores,
    threads = nthreads
  )
  
  if (!is.null(seed)) {
    brm_args$seed <- seed
  }

  extra_args <- list(...)
  intersect_args <- intersect(names(extra_args), names(brm_args))
  if (length(intersect_args) > 0) {
    stop(
      "The following arguments must be supplied through `fit.basal_spec()` ",
      paste(intersect_args, collapse = ", "), "."
    )
  }
  
  brm_args <- c(brm_args, extra_args)
  raw_model <- suppressMessages(
    do.call(brms::brm, brm_args)
  )
  return (raw_model)
}
