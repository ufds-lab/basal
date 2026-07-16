#' Accept inputs for fitting a model
#' @noRd
validate_fit_inputs <- function(spec,
                                data,
                                chains,
                                iter,
                                burn_in,
                                thin,
                                engine,
				ncores,
				nthreads) {
  
  parallel = do_parallell_settings(chains, ncores, nthreads)
  ncores = parallel$ncores
  nthreads = parallel$nthreads
  if (!is.integer(ncores)) {
    stop(
      "`ncores` must be an integer, but instead ", ncores, " was given."
    ))
  }
  if (nthreads != "default" && !is.integer(nthreads)) {
    stop(
      "`nthreads` must be either \"default\" or an integer. Not ", nthreads, "."
    ))
  }

  check_inherits("basal_spec", spec)
  check_inherits("data.frame", data)
  check_inherits("numeric", chains, iter, burn_in, thin)
  
  if (length(engine) != 1 || !is.character(engine) || is.na(engine)) {
    stop("`engine` must be a single character string.")
  }
  if (engine != "brms") {
    stop("Sorry, only `engine = \"brms\"` is currently supported.")
  }
  mcmc_args <- list(chains = chains, iter = iter, burn_in = burn_in, thin = thin)
  bad_length <- names(mcmc_args)[lengths(mcmc_args) != 1]
  if (length(bad_length) > 0) {
    stop(
      "The following argument", if (length(bad_length) > 1) "s must" else "must",
      " have length one: ", paste0("`", bad_length, "`", collapse = ", "),"."
    )
  }
  mcmc_args <- unlist(mcmc_args)
  non_finite <- names(mcmc_args)[!is.finite(mcmc_args)]
  if (length(non_finite) > 0) {
    stop(
      "The following argument", if (length(non_finite) > 1) "s are" else " is",
      " not finite: ", paste0("`", non_finite, "`", collapse = ", "), "."
    )
  }
  non_integer <- names(mcmc_args)[mcmc_args != floor(mcmc_args)]
  if (length(non_integer) > 0) {
    stop(
      "The following argument", if (length(non_integer) > 1) "s must" else " must",
      " be integer-valued: ", paste0("`", non_integer, "`", collapse = ", "), "."
    )
  }
  positive_args <- mcmc_args[c("chains", "iter", "thin")]
  non_positive <- names(positive_args)[positive_args <= 0]
  if (length(non_positive) > 0) {
    stop(
      "The following argument", if (length(non_positive) > 1) "s must" else " must",
      " be positive: ", paste0("`", non_positive, "`", collapse = ", "), "."
    )
  }
  if (burn_in < 0) {
    stop("`burn_in` must be non-negative.")
  }
  if (burn_in >= iter) {
    stop("`burn_in` must be smaller than `iter`.")
  }
}


#' Extract the response variable from a BASAL specification
#' @noRd
get_fit_response <- function(spec) {
  if (is.null(spec$formula)) {
    response <- spec$default_model_data$response_name
  } else if (inherits(spec$formula, "brmsformula")) {
    response <- all.vars(spec$formula$formula[[2]])[1]
  } else {
    response <- all.vars(spec$formula[[2]])[1]
  }
  if (is.null(response) || length(response) != 1 || is.na(response) || response == "") {
    stop("Unable to determine the response variable from `spec`.")
  }
  
  return(response)
}


#' Check variables required by a model formula
#' @noRd
validate_formula_data <- function(formula, data) {
  if (inherits(formula, "brmsformula")) {
    variables <- all.vars(formula$formula)
    if (!is.null(formula[[2]])) {
      for (sub_form in formula[[2]]) {
	sub_vars <- all.vars(sub_form)
	sub_vars <- sub_vars[sub_vars != sub_form[[2]]]
	variables <- c(variables, sub_vars)
      }
    }
  } else {
    variables <- all.vars(formula)
  }
  variables <- variables[!(variables %in% c("BASAL_HT_SE"))]
  missing <- setdiff(variables, colnames(data))
  if (length(missing) == 1) {
    stop("Variable ", missing," missing from your data."))
  }
  if (length(missing) > 1) {
    stop("Variables ", paste0(missing, collapse = ", "), " missing from your data.")
  }
  
  return(variables)
}

#' Parallel settings for model fitting
#' @noRd
do_parallel_settings <- function(chains, ncores, nthreads) {

  # A thread here is used to speed up within-chain computations.
  # A core is used to run another chain in parallel
  if (nthreads != "default") {
    ncores <- min(ncores, chains)
  } else if (ncores >= 2 * chains) {
    nthreads <- floor(ncores/chains)
    ncores <- chains
  } else {
    nthreads <- 1
  }
  
  return(
    list(
      ncores = ncores,
      nthreads = nthreads
    )
  )
}


#' Fit a BASAL model Using brms
#' @noRd
fit_brms_model <- function(formula,
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
                           ...) {
  
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
  return(raw_model)
}


#' Build a formula for a custom model
#' @noRd
build_custom_formula <- function(spec, response) {
  formula <- spec$formula
  if (inherits(formula, "brmsformula")) {
    if (spec$level == "area") {
      stop(
        "Custom area-level models supplied as `brmsformula` objects are not currently supported."
      )
    }
    return(list(
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
	  "Cannot fit area-levels with pre-specified brms addition terms.",
	  " If you want addition terms, you should pre-aggregate data and fit ",
	  "a unit-level model with the addition terms."
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
  stop("Unsupported model type: ", spec$model_type, ".")
}

#' Prepare data for an area-Level model
#' @noRd
prepare_area_level_data <- function(spec,
                                    data,
                                    response,
                                    population_size = NULL) {
  
  response <- response
  full_data <- NULL
  if (spec$level != "area") {
    return(
      list(
        data = data,
        response = response,
        full_data = full_data
      )
    )
  }
  
  # If the user specifies an area-level model and wants automatic aggregation,
  # compute HT estimators and replace the original response with the direct
  # estimator. This applies to both preset FH models and custom area-level models.
  if (is.null(spec$obs_variability)) {
    if (is.null(population_size)) {
      stop(
	"Population size is required for auto-aggregation (computation of ",
	"direct estimator) in area-level models."
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
    response <- "BASAL_HT_ESTIMATOR"
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
      response = response,
      full_data = full_data
    )
  )
}

#' Prepare data for a two-stage model
#' @noRd
prepare_two_stage_data <- function(data, response) {
  
  if (!(response %in% colnames(data))) {
    stop("Response variable ", response, " missing from your data.")
  }
  data$BASAL_NONZERO_INDICATOR <- as.numeric(data[[response]] != 0)
  indicator_values <- unique(stats::na.omit(data$BASAL_NONZERO_INDICATOR))
  if (length(indicator_values) < 2) {
    stop("The two-stage zero-inflated model requires zero and nonzero observations.")
  }
  unfiltered_data <- data
  nonzero_data <- data[!is.na(data[[response]]) & data[[response]] != 0,]
  if (nrow(positive_data) == 0) {
    stop("No nonzero observations were found for response variable ", response, ".")
  }
  return(
    list(
      unfiltered_data = unfiltered_data,
      nonzero_data = nonzero_data
    )
  )
}


#' Fit the second stage of a two-stage model
#' @noRd
fit_second_stage <- function(spec,
                             data,
                             priors = NULL,
                             chains,
                             iter,
                             burn_in,
                             seed,
                             thin,
                             engine,
                             ncores,
                             nthreads,
                             ...) {
  
  if (is.null(spec$second_stage_spec)) {
    return(NULL)
  }
  
  second_stage_fit <- fit.basal_spec(
    spec = spec$second_stage_spec,
    data = data,
    population_size = NULL,
    priors = priors,
    second_stage_priors = NULL,
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
  
  return(second_stage_fit)
}
