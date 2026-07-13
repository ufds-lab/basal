#print.basal_spec <- function(spec, ...) {
#  p = function(x) {writeLines(noquote(x))}
#  p("BASAL model specification:")
#  p("")
#  if (spec$model_type == "custom") {
#    p(paste0(spec$level, "-level model with form"))
#    p(spec$formula)
#  } else if (spec$model_stage == "single" ||
#             (spec$model_stage != "single" && 
#             !is.null(spec$second_stage_spec))) {
#    p(paste0(spec$model_stage, " ", spec$model_type, " model with the form"))
#    p(paste0(spec$default_model_data$response_name, " ~ ",
#             paste0(spec$default_model_data$auxiliary_variables, collapse = "+")))
#    p(paste0("and domain, \"", spec$default_model_data$domain_name, "\"."))
#  } else if (spec$model_stage == "zi") {
#    p(paste0("partial specification for two-stage zi model"))
#    p(paste0("modeling zero-valued data with a ", spec$model_type, " model"))
#  }
#}

#' @exportS3Method base::print
print.basal_fit <- function(fit, ...) {
  print(fit$model)
}

#' @exportS3Method graphics::pairs
pairs.basal_fit <- function(fit, ...) {
  data <- fit$data
  data <- data[,c(fit$params$response, fit$params$predictors)]
  data <- data[,sapply(data[1,], is.numeric)]
  browser()
  pairs(data)
}

#' @exportS3Method base::print
print.basal_check <- function(check, ...) {
  print(check$pp_checks)
}

#' @exportS3Method graphics::pairs
pairs.basal_check <- function(check, ...) {
  stop("No method for basal_check with pairs(). Use pairs() with an object of basal_fit.")
}

#' @exportS3Method base::summary
summary.basal_check <- function(check, ...) {
  print(check$convergence)
}

#' @exportS3Method base::print
print.basal_estimate <- function(estimate, ...) {
  print(estimate$preds)
}
