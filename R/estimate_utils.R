#' Validate Inputs to estimate()
#' @noRd
#' 
validate_estimate_inputs <- function (
    fit,
    newdata,
    domain,
    stat,
    aggregation_statistic,
    ndraws,
    max_preds,
    seed
) {
  
  check_inherits("basal_fit", fit)
  
  if (!is.null(newdata) && !inherits(newdata, "data.frame") && inherits(try(as.data.frame(newdata)), "try-error")) {
    stop("`newdata` must be a data frame or `NULL`.")
  }
  if (!is.null(domain) && !is.character(domain)) {
    stop("`domain` must be a character vector or `NULL`.")
  }
  if (!is.numeric(ndraws) || length(ndraws) != 1 || !is.finite(ndraws) || ndraws <= 0 || ndraws != floor(ndraws)) {
    stop("`ndraws` must be a single positive integer.")
  }
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 || !is.finite(seed))) {
    stop("`seed` must be a single finite number or `NULL`.")
  }
  if (!is.character(max_preds) && !is.null(max_preds) && !is.numeric(max_preds)) {
    stop("`max_preds` must be \"default\", a positive integer, `Inf`, or `NULL`.")
  }
  if (is.character(max_preds) && (length(max_preds) != 1 || max_preds != "default")) {
    stop("The only character value supported for `max_preds` is \"default\".")
  }
  if (is.numeric(max_preds) && length(max_preds) != 1) {
    stop("`max_preds` must have length one.")
  }
  if (is.numeric(max_preds) && !is.infinite(max_preds) && (!is.finite(max_preds) || 
      max_preds <= 0 || max_preds != floor(max_preds))) {
    stop("`max_preds` must be a positive integer, `Inf`, `NULL`, or \"default\".")
  }
  
  validate_estimate_functions(functions = stat, argument = "stat")
  validate_estimate_functions(functions = aggregation_statistic, argument = "aggregation_statistic")
  
  if (length(aggregation_statistic) != 1) {
    stop("`aggregation_statistic` must currently contain exactly one function.")
  }
}

#' Validate Functions Supplied to estimate()
#'
#' @noRd
validate_estimate_functions <- function (
    functions,
    argument
) {
  
  if (is.null(functions) || length(functions) == 0) {
    stop("`", argument, "` must contain at least one function.")
  }
  
  if (!is.list(functions)) {
    functions <- as.list(functions)
  }
  
  valid_functions <- vapply(functions, is.function, logical(1))
  
  if (!all(valid_functions)) {
    stop("Every element of `", argument, "` must be a function.")
  }
  if (is.null(names(functions)) || any(is.na(names(functions))) || any(names(functions) == "")) {
    stop("`", argument, "` must contain named functions.")
  }
}

#' Prepare Maximum Number of Predictions
#' @noRd
#' 
prepare_max_preds <- function (max_preds) {
  
  if (max_preds == "default") {
    warning(
      "For ease of computation, argument `max_preds = 1e5`. This sub-samples `newdata`. ",
      "To disable or change this, use `max_preds = NULL` or another value."
    )
    return(1e5)
  }
  if (is.null(max_preds) || is.infinite(max_preds)) {
    return(NULL)
  }
  
  return (max_preds)
}

#' Prepare Data for Estimation
#' @noRd
#' 
prepare_estimate_data <- function (
    fit,
    newdata,
    two_stage
) {
  
  if (!is.null(newdata)) {
    return (newdata)
  }
  
  if (two_stage) {
    return (fit$unfiltered_data)
  }
  
  return (fit$data)
}

#' Prepare Domain for Estimation
#'
#' @noRd
prepare_estimate_domain <- function (
    fit,
    newdata,
    domain
) {

  if (is.null(domain)) {
    return(list(domain = domain, newdata = newdata)
    )
  }
  if (any(domain == "BASAL_INHERIT")) {
    newdomain <- infer_estimate_domain(fit)
    if (!(newdomain %in% colnames(newdata))) {
      stop("Domain ", domain, ", inferred as the domain for estimates, is not present in newdata.")
    }
    domain[domain == "BASAL_INHERIT"] <- newdomain

    message("Assuming domain is ", newdomain, ".")
  }

  missing_domains <- setdiff(domain, colnames(newdata))
  if (length(missing_domains) > 0) {
    stop("Provided domain", 
         if (length(missing_domains) > 1) "s are" else " is",
         " not present in newdata: ", paste0("`", missing_domains, "`", collapse = ", "),
         ". If you want estimates over the whole region, set `domain = NULL`."
    )
  }

  return (list(domain = domain, newdata = newdata))
}

#' Infer Domain from a Fitted Model
#' @noRd
#'
infer_estimate_domain <- function (fit) {
  
  if (!is.null(fit$spec$domain_name)) {
    return (fit$spec$domain_name)
  }
  
  if (!is.null(fit$spec$default_model_data) && !is.null(fit$spec$default_model_data$domain_name)) {
    return (fit$spec$default_model_data$domain_name)
  }
  
  group_coefs <- get_grouping_variables(fit)
  group_coefs <- group_coefs[group_coefs != ""]
  
  if (length(group_coefs) == 1) {
    return (group_coefs[1])
  } else {
    stop("Only one domain is currently supported.")
  }
  
  stop(
    "Can't infer domain name from the model specification or parametric form. ",
    "Please provide a column name containing the domains."
  )
}

get_grouping_variables <- function (fit, ...) {
  UseMethod("get_grouping_variables")
}

#' Validate Number of Posterior Draws
#' @noRd
#' 
validate_estimate_draws <- function (
    fit,
    ndraws
) {
  
  available_draws <- get_available_draws(fit)
  if (ndraws > available_draws) {
    warning(
      "ndraws is greater than the number of posterior draws.",
      "Can't request more posterior draws than obtained via MCMC. ",
      "Setting ndraws to ", available_draws, ", the number of MCMC draws.",
      "Increase the number of chains or number of iterations, or decrease thinning for more draws."
    )
    ndraws <- available_draws
  }
  if (!is.null(fit$second_stage_fit)) {
    second_stage_draws <- get_available_draws(fit$second_stage_fit)
    if (ndraws > second_stage_draws) {
      warning(
        "ndraws is greater than the number of posterior draws for the second-stage (logit) model.",
        "Can't request more posterior draws than obtained via MCMC. ",
        "Setting ndraws to ", available_draws, ", the number of MCMC draws.",
        "Increase the number of chains or number of iterations, or decrease thinning for more draws."
      )
      ndraws <- second_stage_draws
    }
  }
}


get_available_draws <- function(fit, ...) {
  UseMethod("get_available_draws")
}

#' Subset Prediction Data
#' @noRd
#' 
subset_prediction_data <- function (
    newdata,
    max_preds
) {
  
  if (is.null(max_preds) || nrow(newdata) <= max_preds) {
    return(newdata)
  }
  return (
    newdata |> dplyr::slice_sample(n = max_preds)
  )
}

#' Prepare Area-Level Prediction Data
#' @noRd
#' 
prepare_area_prediction_data <- function (
    fit,
    newdata,
    domain,
    two_stage
) {
  
  response_area_level <- (fit$spec$level == "area")
  second_stage_area_level <- (two_stage &&
    fit$spec$second_stage_spec$level == "area")
  
  if (!response_area_level && !second_stage_area_level) {
    return(newdata)
  }
  if (length(domain) != 1) {
    stop("Area-level estimation currently supports one domain variable.")
  }
  # the first stage has strictly fewer domains than the second stage, so
  # using the domains in fit$data below is sufficient
  missing_training_domains <- setdiff(unique(newdata[[domain]]), unique(fit$data[[domain]]))
  if (length(missing_training_domains) != 0) {
    warning(
      "Domains not present in training data detected. ",
      "These cannot be reliably estimated and will be excluded."
    )
    newdata <- newdata[!(newdata[[domain]] %in% missing_training_domains), ,drop = FALSE]
  }
  training_se <- fit$data$BASAL_HT_SE
  names(training_se) <- fit$data[[domain]]
  newdata$BASAL_HT_SE <- training_se[newdata[[domain]]]
  
  return (newdata)
}

#' Obtain Posterior Expected Predictions
#'
#' @noRd
get_estimate_predictions <- function (
    fit,
    newdata,
    ndraws
) {
  
  post_preds <- try(
    get_posterior_epred(
      fit,
      newdata = newdata,
      ndraws = ndraws,
      allow_new_levels = FALSE
    ),
    silent = TRUE
  )
  
  if (inherits(post_preds, "try-error")) {
    warning("Estimating on new levels.")
    
    post_preds <- get_posterior_epred(
      fit,
      newdata = newdata,
      ndraws = ndraws,
      allow_new_levels = TRUE
    )
  }
  
  return (t(post_preds))
}

get_posterior_epred <- function(fit, ...) {
  UseMethod("get_posterior_epred")
}


#' Aggregate Posterior Predictions
#'
#' @noRd
aggregate_posterior_predictions <- function (
    newdata,
    post_preds,
    domain,
    ndraws,
    aggregation_statistic
) {
  
  draw_names <- paste0("rep", seq_len(ndraws))
  newdata[, draw_names] <- post_preds
  
  preds <- newdata |>
    dplyr::group_by_at(domain) |>
    dplyr::reframe(dplyr::across(dplyr::all_of(draw_names), aggregation_statistic[[1]])) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(draw_names),
      names_to = "draw",
      values_to = "predicted_stat"
    )
  
  return (preds)
}

#' Prepare Summary Statistics
#'
#' @noRd
prepare_estimate_stats <- function(stat) {
  
  if (!is.list(stat)) {
    stat <- as.list(stat)
  }
  
  og_stat <- stat
  # oftentimes, there will be weird NAs, and having functions which are slightly
  # robust is nice. Simultaneously I don't want to put this onto the user.
  # So I'm going to try to automatically do it, but this only works if users
  # add a ... argument (e.g., funciton(x, ...)), in which case I try inserting
  # an na.rm = TRUE argument
  for (i in seq_along(stat)) {
    
    fun <- og_stat[[i]]
    res <- try(fun(1:3, na.rm = TRUE), silent = TRUE)
    if (!inherits(res, "try-error")) {
      fun_wrapper <- function(thefun) {
        function(x, ...) {
          thefun(x, na.rm = TRUE,...)
        }
      }
      stat[[i]] <- fun_wrapper(og_stat[[i]])
    }
    # the way R works, though, is that upon creating functions, they are promises
    # and so if they access something in the larger environment and later
    # if part of this changes, then the first time they are run
    # they will access this variable at runtime, not define-time.
    # We are evaluating the functions here so they work as hoped
    tmp <- stat[[i]](1:3)
  }
  
  return (stat)
}

#' Summarize Posterior Predictions
#'
#' @noRd
summarize_estimate_predictions <- function (
    preds,
    domain,
    stat,
    aggregation_statistic
) {
  
  aggregation_name <- names(aggregation_statistic)[1]
  
  if (domain != "BASAL_OVERALL") {
    ret_preds <- preds |>
      dplyr::group_by_at(domain) |>
      dplyr::mutate(dplyr::across(predicted_stat, stat)) |>
      dplyr::select(-c(draw, predicted_stat)) |>
      dplyr::ungroup() |>
      unique()
    
    base::colnames(ret_preds) <- sapply(
      colnames(ret_preds),
      function(name) {
        base::gsub("predicted_stat", paste0("predicted_", aggregation_name), x = name
        )
      }
    )
    
  } else {
    ret_preds <- sapply(stat, function(statistic) {statistic(preds$predicted_stat)})
    base::names(ret_preds) <- paste0("predicted_stat_",base::names(ret_preds))
    base::names(ret_preds) <- sapply(
      base::names(ret_preds),
      function(name) {
        base::gsub("predicted_stat", paste0("predicted_", aggregation_name), x = name)
      }
    )
  }
  
  return(ret_preds)
}
