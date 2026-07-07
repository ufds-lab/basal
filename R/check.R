#' Posterior Predictive and Regression checks from fit `basal` model.
#'
#' @param fit object of class `fit` from `fit()`.
#'
#' @param stat extra functions to plot for posterior checks. Multiple can be
#' specified if included in a list (i.e., `stat = c(mean = mean, ecdf = ecdf)`)
#' functions should be named.
#'
#' @param include_base_pp_check Logical determining whether or not to include the
#' empirial density funciton as a posterior predictive check. Defaults to `TRUE`
#'
#' @param draws number of draws for posterior prediction
#'
#' @param trace_plots Logical flag to include trace plots of parameter estimates.
#' Defaults to `FALSE` so that large number of plots (if including many random effects)
#' are not included in the object.
#'
#' @export
check.basal_fit <- function(
    fit,
    stat = c(mean = mean, var = var, ecdf = ecdf),
    include_base_pp_check = TRUE,
    draws = 50,
    trace_plots = FALSE,
    two_stage_stat = c(proportion_positive = prop_positive, entropy = entropy)
) {
  message("Assuming engine is brms")
  if (is.null(fit$second_stage_fit)) {
    two_stage_stat <- NULL
  } else {
    names(stat) <- paste("response model:", names(stat))
    names(two_stage_stat) <- paste("logistic model:", names(two_stage_stat))
  }

  ret <- list()
  if (!is.null(stat) && is.null(names(stat))) {
    warning(paste0(
      "Must apply names to your functions. Proceeding while naming",
      " in chronological order (i.e., c(mean, var) -> c(`1` = mean, `2` = var))."
      ))
    names(stat) <- 1:length(stat)
  }

  if (include_base_pp_check) {
    ret$pp_checks$epdf <- pp_check(fit$model, ndraws = draws)
  }
  if (!is.null(stat)) {
    extra_pp_check <- custom_pp_check(
      fit, draws, stat
    )
    for (i in 1:length(stat)) {
      ret$pp_checks[[names(stat)[i]]] <- extra_pp_check[[i]]
    }
  }
  if (!is.null(two_stage_stat)) {
    two_stage_pp_check <- custom_pp_check(
      fit$second_stage_fit, draws, two_stage_stat
    )
    for (i in 1:length(two_stage_stat)) {
      ret$pp_checks[[names(two_stage_stat)[i]]] <- two_stage_pp_check[[i]]
    }
  }

  ret$convergence$rhat <- brms::rhat(fit$model)
  if (sum(ret$convergence$rhat > 1.1, na.rm = T)) {
    warning(
      "Possible issue in convergence. Check rhat values with summary(), and plots form pairs()."
      )
  }
  ret$convergence$neff <- brms::neff_ratio(fit$model) * nrow(as.data.frame(fit$model))
  if (sum(ret$convergence$neff < 200, na.rm = T)) {
    warning(
      "Possible issue in convergence. Check neff values with summary(), and plots from pairs()."
    )
  }
  if (trace_plots) {
    for(var in rownames(brms::posterior_summary(fit$model))) {
      ret$convergence$trace[[var]] <- bayesplot::mcmc_trace(fit$model, pars = var)
    }
  }
  
  return(
    structure(ret, class = "basal_check")
  )
}

#' Custom posterior predictions
#'
#' @param object Object of type `fit.basal_spec`
#'
#' @param draws Number of draws to draw from the posterior predictive distribution
#'
#' @param stat (possible list of) function to apply to draws from posterior
#' predictive distribution
custom_pp_check <- function(
    object,
    draws,
    stat
) {
  y <- object$data[[object$params$response]]
  y_stats <- sapply(stat, function(fun) {fun(y)})
  pp <- posterior_predict(object$model, ndraws = draws)
  post_checks <- sapply(stat, function(fun) {
    apply(pp, MARGIN = 1, FUN = fun)
  })

  plot_list <- sapply(1:length(stat), function(i) {
    post_data <- unlist(post_checks[,i])
    y_stat <- unlist(y_stats[i])
    if (is.numeric(y_stat)) {
      plot <- (
        ggplot() +
        geom_density(aes(x = post_data, color = "y_rep"), linewidth = 0.5) +
        geom_vline(aes(color = "y", xintercept = y_stat)) +
        xlim(min(quantile(post_data, 0.01), y_stat),
             max(quantile(post_data, 0.99), y_stat))
      )
    } else if (is.list(y_stat) && is.function(y_stat[[1]])) {
      plot <- ggplot()
      for (j in 1:length(post_data)) {
        plot <- plot +
          stat_function(fun = post_data[[j]], aes(color = "y_rep"),
                        linewidth = 0.5, alpha = 5/log(length(post_data)))
      }
      plot <- plot +
        stat_function(fun = y_stat[[1]], aes(color = "y")) +
        xlim(quantile(y, 0.01), quantile(y, 0.99))

    }
    plot <- plot +
      theme_minimal() +
      theme(axis.line.x.bottom = element_line(),
            axis.line.y.left = element_line(),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_blank()) +
      scale_color_manual(
        values = c(
          "black",
          "lightblue"
        ), labels = c(
          y = expression(y),
          y_rep = expression(y[rep])
        )
      ) +
      labs(title = paste0(
        "Posterior predictive distribution for ", names(stat)[i], "."
      ))
  })

  names(plot_list) <- names(stat)
  return(plot_list)
}
