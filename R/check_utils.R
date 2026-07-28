#' Validate
#'
#' @noRd
validate_check_inputs <- function(
    fit,
    draws,
    include_base_pp_check,
    trace_plots
) {
  
  check_inherits("basal_fit", fit)
  
  if (!is.numeric(draws) || length(draws) != 1 ||  !is.finite(draws) || draws <= 0 || draws != floor(draws)) {
    stop("`draws` must be a single positive integer.")
  }
  if (!is.logical(include_base_pp_check) ||length(include_base_pp_check) != 1 || is.na(include_base_pp_check)) {
    stop("`include_base_pp_check` must be either TRUE or FALSE.")
  }
  if (!is.logical(trace_plots) || length(trace_plots) != 1 || is.na(trace_plots)) {
    stop("`trace_plots` must be either TRUE or FALSE.")
  }
  return(invisible(T))
}


#' Prepare Statistics for Posterior Predictive Checks
#' @noRd
#' 
prepare_check_stats <- function(stat, argument = "stat") {
  
  if (is.null(stat)) {
    return(NULL)
  }
  if (!is.list(stat)) {
    stat <- as.list(stat)
  }
  
  valid_functions <- vapply(stat,is.function,logical(1))
  if (!all(valid_functions)) {
    stop("Every element of `", argument, "` must be a function.")
  }
  if (is.null(names(stat)) || any(is.na(names(stat))) || any(names(stat) == "")) {
    warning("`", argument, "` should contain named functions. ", 
            "Unnamed functions will be assigned numeric names."
    )
    
    missing_names <- is.null(names(stat)) | is.na(names(stat)) | names(stat) == ""
    
    if (is.null(names(stat))) {
      names(stat) <- as.character(seq_along(stat))
    } else {
      names(stat)[missing_names] <- as.character(which(missing_names))
    }
  }
  
  return(stat)
}


#' Construct Posterior Predictive Checks for One Model
#' @noRd
#' 
build_pp_checks <- function(
    object,
    draws,
    stat,
    include_base_pp_check = TRUE
) {
  
  checks <- list()
  
  if (include_base_pp_check) {
    checks$epdf <- brms::pp_check(object$model, ndraws = draws) +
      ggplot2::labs(title = paste0("Posterior predictive distributions for epdf of response."))
  }
  
  if (!is.null(stat)) {
    extra_checks <- custom_pp_check(object = object, draws = draws, stat = stat)
    checks[names(extra_checks)] <- extra_checks
  }
  
  return(checks)
}


#' Construct Convergence Diagnostics
#' @noRd
build_convergence_checks <- function(
    object,
    trace_plots = FALSE
) {
  
  convergence <- list()
  convergence$rhat <- brms::rhat(object$model)
  
  if (sum(convergence$rhat > 1.1, na.rm = T) > 0) {
    warning(
      "Possible issue in convergence. ",
      "Check R-hat values with summary() and plots from pairs()."
    )
  }
  
  convergence$neff <- brms::neff_ratio(object$model) * nrow(as.data.frame(object$model))
  if (sum(convergence$neff < 200, na.rm = T) > 0) {
    warning(
      "Possible issue in convergence. ",
      "Check effective sample sizes with summary() and plots from pairs()."
    )
  }
  if (trace_plots) {
    convergence$trace <- build_trace_plots(object = object)
  }
  
  return(convergence)
}


#' Construct Trace Plots
#' @noRd
#' 
build_trace_plots <- function(object) {
  
  parameter_names <- rownames(brms::posterior_summary(object$model))
  trace <- vector(mode = "list", length = length(parameter_names))
  names(trace) <- parameter_names
  
  for (parameter in parameter_names) {
    trace[[parameter]] <- bayesplot::mcmc_trace(object$model,pars = parameter
    )
  }
  
  return(trace)
}

#' @title Custom posterior predictions
#'
#' @param object Object of type `fit.basal_spec`
#'
#' @param draws Number of draws to draw from the posterior predictive distribution
#'
#' @param stat (possible list of) function to apply to draws from posterior
#' predictive distribution
#' 
#' @param joined_two_stage Boolean indicating whether to compute PPD from the joined
#' two stage model
#' 
#' @noRd
#' 
custom_pp_check <- function(
    object,
    draws,
    stat,
    joined_two_stage = FALSE
) {
  
  prediction_data <- get_pp_check_data(
    object = object,
    draws = draws,
    joined_two_stage = joined_two_stage
  )
  
  y <- prediction_data$y
  pp <- prediction_data$pp
  
  y_stats <- lapply(stat, function(fun) {
      fun(y)
    }
  )
  post_checks <- lapply(stat, function(fun) {
      apply(pp, MARGIN = 1,FUN = fun)
    }
  )
  
  plot_list <- vector(mode = "list", length = length(stat))
  names(plot_list) <- names(stat)
  
  for (i in seq_along(stat)) {
    plot_list[[i]] <- build_custom_pp_plot(
      post_data = post_checks[[i]],
      y_stat = y_stats[[i]],
      y = y,
      stat_name = names(stat)[i]
    )
  }
  
  return(plot_list)
}

#' Obtain Data for Posterior Predictive Checks
#'
#' @noRd
get_pp_check_data <- function(
    object,
    draws,
    joined_two_stage = FALSE
) {
  
  if (!joined_two_stage) {
    
    y <- object$data[[object$params$response]]
    
    pp <- brms::posterior_predict(
      object$model,
      ndraws = draws,
      newdata = object$data
    )
    
  } else {
    y <- object$unfiltered_data[[object$params$response]]
    
    occurrence_draws <- brms::posterior_predict(
      object$second_stage_fit$model,
      ndraws = draws,
      newdata = object$unfiltered_data,
      allow_new_levels = TRUE
    )
    response_draws <- brms::posterior_predict(
      object$model,
      ndraws = draws,
      newdata = object$unfiltered_data,
      allow_new_levels = TRUE
    )
    
    pp <- occurrence_draws * response_draws
  }
  
  return(list(y = y, pp = pp))
}

#' Construct a custom posterior predictive plot
#'
#' @noRd
build_custom_pp_plot <- function(
    post_data,
    y_stat,
    y,
    stat_name
) {
  
  if (is.numeric(y_stat)) {
    post_data <- unlist(post_data)
    y_stat <- unlist(y_stat)
    plot <- (
      ggplot2::ggplot() +
        ggplot2::geom_density(ggplot2::aes(x = post_data, color = "y_rep"), linewidth = 0.5) +
        ggplot2::geom_vline(ggplot2::aes(color = "y", xintercept = y_stat)) +
        ggplot2::xlim(
          min(stats::quantile(post_data, 0.01, na.rm = T), y_stat, na.rm = T),
          max(stats::quantile(post_data, 0.99, na.rm = T), y_stat, na.rm = T)
        )
    )
    
  } else if (is.function(y_stat)) {
    plot <- ggplot2::ggplot()
    for (j in seq_along(post_data)) {
      plot <- plot +
        ggplot2::stat_function(fun = post_data[[j]], ggplot2::aes(color = "y_rep"), 
                               linewidth = 0.5, alpha = min(1, 5 / log(length(post_data))))
    }
    
    plot <- plot +
      ggplot2::stat_function(fun = y_stat, ggplot2::aes(color = "y")) +
      ggplot2::xlim(stats::quantile(y, 0.01), stats::quantile(y, 0.99))
    
  } else {
    stop("Statistic `", stat_name, "` returned an unsupported object.")
  }
  
  plot <- plot +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.line.x.bottom = ggplot2::element_line(),
      axis.line.y.left = ggplot2::element_line(),
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    ) +
    ggplot2::scale_color_manual(
      values = c("black", "lightblue"),
      labels = c(y = expression(y), y_rep = expression(y[rep]))
    ) +
    ggplot2::labs(title = paste0("Posterior predictive distribution for ", stat_name,".")
    )
  
  return(plot)
}