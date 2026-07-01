#' Posterior estimation of summary statistics
#' 
#' @param fit object of type `basal_fit`.
#' 
#' @param newdata data to predict data on. If NULL, fit on training data.
#' 
#' @param domain (vector of) names of areas to aggregate estimates on. If `NULL`, aggregate
#' all the data together.
#' 
#' @param stat Named vector of function(s) to apply to posterior predictions.
#' 
#' @param ndraws number of draws from the posterior predictive distribution.
#' 
#' @param max_preds maximum number of points to make predictions on. Capped to 
#' avoid R session crashing. A value of `NULL` or `Inf` will predict on all.
#' 
#' @param seed The seed for random number generation in posterior prediction.
estimate.basal_fit = function(
    fit,
    newdata = NULL,
    domain = NULL,
    stat = c(mean = mean, 
             var = var),
    ndraws = 1000,
    max_preds = 1e5,
    seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  effects = unique(fit$model$prior$group)
  effects = effects[effects != ""]
  predictors = names(coef(fit$model)) # can be updated
  if (is.null(max_preds)) {
    nd_subset = newdata
  } else {
    nd_subset = newdata %>%
      .[sample.int(nrow(.), size = min(nrow(.), max_preds)),]
  }
  
  if (fit$spec$level == "area") {
    setdiff = setdiff(unique(nd_subset[[domain]]), unique(fit$data[[domain]]))
    if (length(setdiff) != 0) {
      warning(paste0(
        "Domains not present in training data detected. These cannot be reliably",
        " estimated on and will be excluded."
      ))
      nd_subset = nd_subset[!c(nd_subset[[domain]] %in% setdiff),]
    }
    
    training_se = fit$data$BASAL_HT_SE
    names(training_se) = fit$data[[domain]]
    nd_subset$`BASAL_HT_SE` = training_se[nd_subset[[domain]]]
  }
  
  post_preds = t(posterior_epred(fit$model, 
                                 newdata = nd_subset, 
                                 ndraws = ndraws,
                                 allow_new_levels = TRUE))
  
  if (!is.null(fit$spec$variable_transform)) {
    inv_trans = fit$spec$variable_transform$inv_transform
    post_preds = inv_trans(post_preds)
  }
  
  
  nd_subset[,(ncol(nd_subset)+1):(ncol(nd_subset) + ndraws)] = post_preds
  colnames(nd_subset)[(ncol(nd_subset)-ndraws+1):(ncol(nd_subset))] = paste0("rep", 1:ndraws)
  
  preds = nd_subset %>%
    group_by_at(domain) %>%
    reframe(across(paste0("rep", 1:ndraws), mean)) %>%
    pivot_longer(2:ncol(.),
                 names_to = "draw",
                 values_to = "pred_mean")
  
  ret_preds = preds %>%
    group_by_at(domain) %>%
    mutate(across(pred_mean, stat)) %>%
    dplyr::select(-c(draw, pred_mean)) %>%
    unique()
  
  ret = list(
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
  
  return (ret)
}
