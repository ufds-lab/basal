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
                           ncores = default_ncores(),
                           nthreads = 1,
                           ...) {
  
  func_call <- match.call()
  
  if (!is.null(spec$second_stage_spec)) {
    message("Estimating two models for two-stage model. This may take a while...")
    if (is.null(spec$formula)) {
      res <- spec$default_model_data$response_name
    } else {
      res <- spec$formula[[2]]
    }
    second_spec_res <- spec$second_stage_spec
    data$BASAL_ZERO_INDICATOR = as.numeric((data[[res]] == 0))
    second_stage_fit <- fit.basal_spec(
      spec$second_stage_spec,
      data,
      population_size = population_size,
      priors = priors,
      chains = chains,
      iter = iter,
      burn_in = burn_in,
      seed = seed,
      thin = thin,
      engine = engine,
      ncores = ncores,
      nthreads = nthreads,
      ...
    )
    data = data[data[[res]] != 0,]
  } else {
    second_stage_fit <- NULL
  }


#  check_inherits("data.frame", data)
  check_inherits("numeric", chains, iter, burn_in, thin)
  check_inherits("basal_spec", spec)

  if (!is.null(spec$formula)) {
    res <- spec$formula[[2]]
  } else {
    res <- spec$default_model_data$response_name
  }
  if (!is.null(spec$variable_transform)) {
    trans <- spec$variable_transform$transform
    # weird as.numeric() calls because
    # data[[res]] seems to sometimes produce character vectors
    # (try with FH and auto-aggregation)
    data[[res]] <- trans(data[[res]])
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
      if (spec$model_type == "FH") {
        trim_data <- data[,c(spec$default_model_data$response_name,
                            spec$default_model_data$domain_name,
                            spec$default_model_data$auxiliary_variables)]
        data <- agg_HT(trim_data,
                       spec$default_model_data$response_name,
                       population_size,
                       spec$default_model_data$domain_name)
      } else {
        stopifnot(!is.null(spec$formula))
        trim_data <- data[,all.vars(spec$formula)]
        data <- agg_HT(trim_data,
                       res,
                       population_size,
                       spec$domain)
      }
<<<<<<< HEAD
      
      browser()
=======

>>>>>>> main
      res <- "BASAL_HT_ESTIMATOR"
    } else {
      # We first get obs_variability.
      obs_var <- spec$obs_variability
      if (is.numeric(obs_var)) {
        data$BASAL_HT_SE <- obs_var
      } else if (is.character(obs_var)) {
        if (!(obs_var %in% colnames(data))) {
          stop("obs_variability must be a vector of standard errors or a column in the data.")
        }
        colnames(data)[colnames(data)== obs_var] <- "BASAL_HT_SE"
      }
    }
  data <- data[(data$BASAL_HT_SE != 0 & !is.nan(data$BASAL_HT_SE)),]
  }

  spec_family   <- spec$family
  formula       <- NULL
  valid_formula <- NULL

  # zi
  if (spec$model_stage == "zi") {
    spec_family <- zero_inflated_normal

    if (spec$model_type == "custom") {
      formula <- spec$formula
<<<<<<< HEAD
      valid_formula <- brmsformula(formula)
    } else if (spec$level == "area" ) {
      if (family$family == "gaussian") {
        formula <- spec$formula
        tmp_formula <- formula(paste0(
          res, " | se(BASAL_HT_SE) ~ 1"
        ))
        # Now do the addition terms check
        if (length(all.vars(formula[[2]])) > 1) {
          # I'm unsure if we can allow the user to specify addition terms in an
          # area level model. I'm going to make this illegal.
          stop(paste0(
            "Cannot fit area-levels with pre-specified brms addition terms.",
            " If you want addition terms, you should pre-aggregate data and fit ",
            "a unit-level model with the addition terms."
          ))
        }
      } else {
        tmp_formula <- formula(paste0(res, " ~ 1"))
=======
      if (!is.null(spec$second_stage_spec) && !is.null(spec$second_stage_spec$formula)) {
        sec_rhs <- spec$second_stage_spec$formula[[3]]
        sec_rhs_str <- deparse(sec_rhs)
        sec_formula <- as.formula(paste0("logitnonzero ~ ", sec_rhs_str))
        valid_formula <- brms::bf(formula) + brms::bf(sec_formula)
      } else {
        valid_formula <- brms::bf(formula)
>>>>>>> main
      }
    } else if (spec$model_type == "BHF") {
      formula_str <- paste0(res, " ~ ", paste0(spec$default_model_data$auxiliary_variables, collapse = " + "),
                              " + (1 | ", spec$default_model_data$domain_name, ")")
      formula <- as.formula(formula_str)
      valid_formula <- brms::bf(formula)
    } else {
      stop("Sorry, zero-inflated models are only available for Custom or BHF Unit-level estimation.")
    }
<<<<<<< HEAD
  } else if (spec$model_type == "BHF") {
    formula <-
      formula(paste0(
        spec$default_model_data$response_name, " ~ ",
        paste0(spec$default_model_data$auxiliary_variables, collapse = " + "), " + ",
        "(1 | ",  spec$default_model_data$domain_name, ")"
      ))
    valid_formula <- brmsformula(formula)
  } else if (spec$model_type == "FH") {
    formula <-
      formula(paste0(
        res, "| se(BASAL_HT_SE) ~ ",
        paste0(spec$default_model_data$auxiliary_variables, collapse = " + "), " + ",
        "(1 | ",  spec$default_model_data$domain_name, ")"
      ))
    if (spec$family$family != "gaussian") {
      formula[[2]] <- formula(paste0(res, " ~ 1"))[[2]]
    }
    valid_formula <- brmsformula(formula)
=======
>>>>>>> main

  } else {

    if (spec$model_type == "custom") {
      if (spec$level == "unit") {
        formula <- spec$formula
        valid_formula <- brmsformula(formula)
      } else if (spec$level == "area") {
        formula <- spec$formula
        tmp_formula <- formula(paste0(res, " | se(BASAL_HT_SE) ~ 1"))
        if (length(all.vars(formula[[2]])) > 1) {
          stop("Cannot fit area-levels with pre-specified brms addition terms.")
        }
        formula[[2]] <- tmp_formula[[2]]
        valid_formula <- brmsformula(formula)
      }
    } else if (spec$model_type == "BHF") {
      formula <- formula(paste0(spec$default_model_data$response_name, " ~ ",
                                paste0(spec$default_model_data$auxiliary_variables, collapse = " + "),
                                " + (1 | ", spec$default_model_data$domain_name, ")"))
      valid_formula <- brmsformula(formula)
    } else if (spec$model_type == "FH") {
      formula <- formula(paste0(res, "| se(BASAL_HT_SE) ~ ",
                                paste0(spec$default_model_data$auxiliary_variables, collapse = " + "),
                                " + (1 | ", spec$default_model_data$domain_name, ")"))
      valid_formula <- brmsformula(formula)
      data <- data[(data$BASAL_HT_SE != 0 & !is.nan(data$BASAL_HT_SE)), ]
    }
  }

  vars <- all.vars(formula)
  vars <- vars[!(vars %in% c("BASAL_HT_SE", "logitnonzero"))]

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

  # set basal default priors
  # we don't want improper priors on the random effect variances
  # so we can over-estimate these variances by multiplying the total variability
  # of the data by some number (>1). We know that the variability of random effects
  # should be less than the variability of the data, so this shouldn't be
  # too informative
  
  # setting priors by modifying the object created by default_priors()
  # is not ideal, although I'm unsure of another way to give default priors 
  # for *everything*

  predictors <- vars[vars != res]
  numeric_preds <- predictors[sapply(data[,predictors], is.numeric)]

  res_sd <- sd(data[[res]]) # compute sd
  if (length(numeric_preds) == 1) {
    pred_sd = sd(data[,numeric_preds])
  } else {
    pred_sd <- sapply(data[,numeric_preds], sd)
  }

  pred_sd_ratio <- pred_sd/res_sd

  prior_family <- paste0("student_t(3, 0, ", pred_sd_ratio * 2.5, ")")
  res_prior <- paste0("student_t(3, 0, ", res_sd * 2.5, ")")

  # we modify default priors from brms
  priors <- default_prior(
    valid_formula,
    data,
    family = spec$family
  )
  
  reg_coef_mask <- (priors$class == "b") & (priors$coef %in% numeric_preds)
  priors[reg_coef_mask,]$prior <- prior_family
  priors[reg_coef_mask,]$source <- "default (basal)"

  pop_levels <- unique(priors$group)
  pop_levels <- pop_levels[pop_levels != ""]
  non_numeric_preds <- predictors[-which(predictors %in% union(pop_levels, numeric_preds))]

  if (length(non_numeric_preds) != 0) {
    reg_coef_mask <- (priors$class == "b") & (priors$coef == non_numeric_preds)
    priors[reg_coef_mask,]$prior <- res_prior
    priors[reg_coef_mask,]$source <- "default (basal)"
  }

  priors[priors$class == "Intercept",]$prior <- res_prior
  priors[priors$class == "Intercept",]$source <- "default (basal)"

  
  if (ncores >= 2 * chains) {
    nthreads <- max(nthreads, floor(ncores/chains))
    ncores <- chains
  }
  
  if (is.null(seed)) {
    raw_model <- suppressMessages(
      brms::brm(
        formula = valid_formula,
        data = data,
        prior = priors,
        chains = chains,
        iter = iter,
        thin = thin,
        family = spec_family,
        stanvars = if (!is.null(spec$model_stage) && spec$model_stage == "zi") get_basal_stanvars() else NULL,
        warmup = burn_in,
        cores = ncores,
        threads = nthreads,
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
        family = spec_family,
        stanvars = if (!is.null(spec$model_stage) && spec$model_stage == "zi") get_basal_stanvars() else NULL,
        warmup = burn_in,
        seed = seed,
        cores = ncores,
        threads = threads,
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
    ),
    second_stage_fit = second_stage_fit
  )

  return(
    structure(out, class = "basal_fit")
  )
}
