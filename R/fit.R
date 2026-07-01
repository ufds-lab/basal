#' Fit a Specified Small Area Estimation Model Object
#'
#' @param spec An object of class `basal_spec` containing initialized metadata states.
#'
#' @param data A data.frame containing the response variable and all predictor covariates.
#'
#' @param population_size Number of plots like those sampled in `data`. Necessary
#' for auto-aggregation in area-level models.
#'
#' @param priors Optional prior specification. If NULL, default priors are supplied.
#'
#' @param chains Numeric integer. Number of MCMC chains. Defaults to 3.
#'
#' @param iter Numeric integer. Total number of iterations per chain. Defaults to 6000.
#'
#' @param burn_in Numeric integer. Number of burn-in/burn_in iterations per chain. Defaults to 2000.
#'
#' @param seed Numeric integer. Seed for random number generation. Defaults to 1.
#'
#' @param ... Additional arguments passed to the model fitting engine.
#'
#' @return An object of class `basal_fit` containing the results.
#'
#' @export
#'
fit.basal_spec <- function(spec,
                           data,
                           population_size = NULL,
                           priors = NULL,
                           chains = 3,
                           iter = 5000,
                           burn_in = 1000,
                           seed = NULL,
                           thin = 2,
                           engine = "brms",
                           ...) {

  func_call <- match.call()


#  check_inherits("data.frame", data)
  check_inherits("numeric", chains, iter, burn_in, thin)
  check_inherits("basal_spec", spec)

  if (!is.null(spec$formula)) {
    res = formula[[2]]
  } else {
    res = spec$default_model_data$response_name
  }
  if (!is.null(spec$variable_transform)) {
    trans = spec$variable_transform$transform
    # weird as.numeric() calls because
    # data[[res]] seems to sometimes produce character vectors
    # (try with FH and auto-aggregation)
    if (!is.null(spec$default_model_data)) {
      data[[res]] = trans(data[[res]])
    } else {
      data[[res]] = trans(data[[res]])
    }
  }

 
  
  if (spec$level == "area") {
    if (spec$model_type != "FH") {
      warning("This may not work correctly.")
    }
    # if the user specifies an area-level model and they want auto-aggregation
    # then we need to compute HT estimators and replace their response with the
    # HT estimates and fix the MSE to be the SE of HT estimators. This is
    # regardless of if they have a custom model or something like a FH
    if (is.null(spec$obs_variability)) {
      if (is.null(population_size)) {
        stop(paste0(
          "Population size is required for auto-aggregation (computation of ",
          "direct estimator) in area-level models."))
      }
      full_data <- data
      trim_data = data[,colnames(data) %in% c(spec$default_model_data$response_name,
                                              spec$default_model_data$domain_name,
                                              spec$default_model_data$auxiliary_variables)]
      data <- agg_HT(trim_data,
                     spec$default_model_data$response_name,
                     population_size,
                     spec$default_model_data$domain_name)
      res = "BASAL_HT_ESTIMATOR"
    } else {
      # We first get obs_variability.
      obs_var <- spec$obs_variability
      if (is.numeric(obs_variability)) {
        data$`BASAL_HT_SE` = obs_variability
      } else if (is.character(obs_variability)) {
        if (!(obs_variability %in% colnames(data))) {
          stop("obs_variability must be a vector of standard errors or a column in the data.")
        }
        colnames(data)[colnames(data) == obs_var] = "BASAL_HT_SE"
      }
    }
  data = data[(data$BASAL_HT_SE != 0 & !is.nan(data$BASAL_HT_SE)),]
  }

  if (spec$model_type == "custom") {
    # Debug: level and tmp_formula.
    if (spec$level == "unit") {
      formula <- spec$formula
      valid_formula <- brmsformula(formula)
    } else if (spec$level == "area") {
      formula <- spec$formula
      tmp_formula <- formula(paste0(
        res, " | se(BASAL_HT_SE) ~ 1"
      ))
      # Now do the addition terms check
      if (length(all.vars(tmp_formula[[2]])) > 1) {
        # I'm unsure if we can allow the user to specify addition terms in an
        # area level model. I'm going to make this illegal.
        stop(paste0(
          "Cannot fit area-levels with pre-specified brms addition terms.",
          " If you want addition terms, you should pre-aggregate data and fit ",
          "a unit-level model with the addition terms."
        ))
      }

      # Inject the synthesized measurement error LHS back into the user's custom formula
      formula[[2]] <- tmp_formula[[2]]
      valid_formula <- brmsformula(formula)
    }
  } else if (spec$model_type == "BHF") {
    formula =
      formula(paste0(
        spec$default_model_data$response_name, " ~ ",
        paste0(spec$default_model_data$auxiliary_variables, collapse = " + "), " + ",
        "(1 | ",  spec$default_model_data$domain_name, ")"
      ))
    valid_formula = brmsformula(formula)
  } else if (spec$model_type == "FH") {
    formula =
      formula(paste0(
        res, "| se(BASAL_HT_SE) ~ ",
        paste0(spec$default_model_data$auxiliary_variables, collapse = " + "), " + ",
        "(1 | ",  spec$default_model_data$domain_name, ")"
      ))
    valid_formula = brmsformula(formula)

    data = data[(data$BASAL_HT_SE != 0 & !is.nan(data$BASAL_HT_SE)),]
  }

  vars = all.vars(formula)
  vars = vars[!(vars %in% c("BASAL_HT_SE"))]
  if (length((missing = setdiff(vars, colnames(data)))) != 0) {
    if (length(missing) == 1) {
      stop(paste0("Variable ", missing, " missing from your data."))
    } else {
      stop(paste0("Variables ", paste0(missing, collapse = ", "),
                  " missing from your data."))
    }
  }

  if (!("res" %in% ls())) {
    res <- formula[[2]]
  }

  if (!is.null(spec$variable_transform)) {
    trans = spec$variable_transform$transform
    # weird as.numeric() calls because
    # data[[res]] seems to sometimes produce character vectors
    # (try with FH and auto-aggregation)
    if (!is.null(spec$default_model_data)) {
      data[[res]] = trans(data[[res]])
    } else {
      data[[res]] = trans(data[[res]])
    }
  }

  # set basal default priors
  # we don't want improper priors on the random effect variances
  # so we can over-estimate these variances by multiplying the total variability
  # of the data by some number (>1). We know that the variability of random effects
  # should be less than the variability of the data, so this shouldn't be
  # too informative

  predictors = vars[vars != res]
  numeric_preds = predictors[sapply(data[,predictors], is.numeric)]

  res_sd = sd(data[[res]]) # compute sd
  pred_sd = sapply(data[,numeric_preds], sd)

  pred_sd_ratio = pred_sd/res_sd

  prior_family = paste0("student_t(3, 0, ", pred_sd_ratio * 2.5, ")")
  res_prior = paste0("student_t(3, 0, ", res_sd * 2.5, ")")

  # we modify default priors from brms
  priors = default_prior(
    valid_formula,
    data
  )

  reg_coef_mask = (priors$class == "b") & (priors$coef %in% numeric_preds)
  priors[reg_coef_mask,]$prior = prior_family
  priors[reg_coef_mask,]$source = "default (basal)"

  pop_levels = unique(priors$group)
  pop_levels = pop_levels[pop_levels != ""]
  non_numeric_preds = predictors[-which(predictors %in% union(pop_levels, numeric_preds))]

  if (length(non_numeric_preds) != 0) {
    reg_coef_mask = (priors$class == "b") & (priors$coef == non_numeric_preds)
    priors[reg_coef_mask,]$prior = res_prior
    priors[reg_coef_mask,]$source = "default (basal)"
  }

  priors[priors$class == "Intercept",]$prior <- res_prior
  priors[priors$class == "Intercept",]$source <- "default (basal)"

  if (is.null(seed)) {
    raw_model <- suppressMessages(
      brms::brm(
        formula = valid_formula,
        data = data,
        prior = priors,
        chains = chains,
        iter = iter,
        thin = thin,
        warmup = burn_in,
        ...
      )
    )
  } else {
    raw_model <- suppressMessages(
      brms::brm(
        formula = valid_formula,
        data = data,
        prior = priors,
        chains = chains,
        iter = iter,
        thin = thin,
        warmup = burn_in,
        seed = seed,
        ...
      )
    )
  }

  out <- list(
    call = func_call,
    spec = spec,
    formula = formula,
    data = data,
    model = raw_model,
    params = list(
      response = res,
      predictors = predictors
    )
  )

  return(
    structure(out, class = "basal_fit")
  )
}
