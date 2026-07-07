#' Posterior estimation of summary statistics
#'
#' @param fit object of type `basal_fit`.
#'
#' @param newdata data to predict data on. If NULL, fit on training data.
#'
#' @param domain (vector of) names of areas to aggregate estimates on. If `NULL`,
#' aggregate all the data together.
#'
#' @param stat Named vector of function(s) to apply to posterior predictions.
#'
#' @param ndraws number of draws from the posterior predictive distribution.
#'
#' @param max_preds maximum number of points to make predictions on. Capped to
#' avoid R session crashing. A value of `NULL` or `Inf` will predict on all.
#'
#' @param seed The seed for random number generation in posterior prediction.
#' @export
estimate.basal_fit <- function(
    fit,
    newdata = NULL,
    domain = "BASAL_INHERIT",
    stat = c(mean = mean, 
             var = var),
    ndraws = 1000,
    max_preds = 1e5,
    seed = NULL
) {

  two_stage <- !is.null(fit$second_stage_fit)

  if (is.null(newdata)) {
    newdata <- fit$data
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (domain == "BASAL_INHERIT") {
    if (!is.null(fit$spec$domain_name)) {
      domain = fit$spec$domain_name
    } else if(!is.null(fit$spec$default_model_data$domain_name)) {
      domain = fit$spec$default_model_data$domain_name
    } else {
      group_coefs = unique(fit$model$prior[,4])
      group_coefs = group_coefs[group_coefs != ""]
      if (length(group_coefs) == 1) {
        domain = group_coefs[1]
      } else {
        stop(paste0(
          "Can't infer domain name from the model specification or parametric form. ",
          "Please provide a column name containing the domains."
        ))
      }
    }
    if (!(domain %in% colnames(newdata))) {
      stop(paste0(
        "Domain ", domain, ", inferred as the domain for estimates, is not ",
        "present in newdata."
      ))
    } else {
      message("Assuming domain is ", domain, ".")
    }
  }
  
  if (ndraws > nrow(as.data.frame(fit$model))) {
    stop(paste0(
      "Can't estimate more quantities than obtained via MCMC. Increase the ",
      "number of chains or number of iterations."
    ))
  }
  if (is.null(domain)) {
    domain <- "BASAL_OVERALL_DOMAIN"
    newdata[[domain]] <- "overall"
  }
  if (!(domain %in% colnames(newdata))) {
    stop(paste0(
      "Provided domain is not a column in newdata. If you want estimates over",
      " the whole region, set domain = NULL, otherwise set to a column in newdata."
    ))
  }


  predictors <- names(coef(fit$model)) # can be updated
  if (is.null(max_preds)) {
    nd_subset <- newdata
  } else {
    nd_subset <- newdata %>%
      .[sample.int(nrow(.), size = min(nrow(.), max_preds)),]
  }

  if (fit$spec$level == "area" ||
      (two_stage && fit$spec$second_stage_spec$level == "area")) {
    # the first stage has strictly fewer domains than the second stage, so
    # using the domains in fit$data below is sufficient
    setdiff <- setdiff(unique(nd_subset[[domain]]), unique(fit$data[[domain]]))
    if (length(setdiff) != 0) {
      warning(paste0(
        "Domains not present in training data detected. These cannot be reliably",
        " estimated on and will be excluded."
      ))
      nd_subset <- nd_subset[!c(nd_subset[[domain]] %in% setdiff),]
    }

    training_se <- fit$data$BASAL_HT_SE
    names(training_se) <- fit$data[[domain]]
    nd_subset$`BASAL_HT_SE` <- training_se[nd_subset[[domain]]]
  }

  post_preds <- t(posterior_epred(fit$model,
                                  newdata = nd_subset,
                                  ndraws = ndraws,
                                  allow_new_levels = TRUE))

  if (!is.null(fit$spec$variable_transform)) {
    inv_trans <- fit$spec$variable_transform$inv_transform
    post_preds <- inv_trans(post_preds)
  }

  if (two_stage) {
    second_stage_weights = t(
      posterior_epred(fit$second_stage_fit$model,
                      newdata = nd_subset,
                      ndraws = ndraws,
                      allow_new_levels = TRUE)
    )
    post_preds = post_preds * second_stage_weights
  }


  nd_subset[,(ncol(nd_subset)+1):(ncol(nd_subset) + ndraws)] <- post_preds
  colnames(nd_subset)[(ncol(nd_subset)-ndraws+1):(ncol(nd_subset))] <- paste0("rep", 1:ndraws)

  preds <- nd_subset %>%
    group_by_at(domain) %>%
    reframe(across(paste0("rep", 1:ndraws), mean)) %>%
    pivot_longer(2:ncol(.),
                 names_to = "draw",
                 values_to = "pred_mean")

  ret_preds <- preds %>%
    group_by_at(domain) %>%
    mutate(across(pred_mean, stat)) %>%
    dplyr::select(-c(draw, pred_mean)) %>%
    unique()

  ret <- list(
    fit = fit,
    params = list(
      newdata = newdata,
      domain = domain,
      stat = stat,
      ndraws = ndraws
    ),
    preds = ret_preds,
    raw_rep_preds = preds
  )
  
  return (structure(
    structure(ret, class = "basal_estimate")
  ))
}

