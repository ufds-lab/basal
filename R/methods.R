#' @exportS3Method base::print
print.basal_spec <- function(spec, suppress_header = F, ...) {
  p = function(...) {writeLines(noquote(paste0(...)))}
  if (!suppress_header) {
    p("**BASAL model specification:**")
  }
  if (spec$model_stage == "single") {
    addition = ""
  } else if (spec$model_stage == "zi") {
    addition = "zero-inflated "
  } else {
    stop("unknown ")
  }
  if (spec$model_type == "custom") {
    p(paste0(addition, spec$model_type, " ", spec$level, "-level model of the form"))
  } else {
    p(addition, spec$model_type, " model of the form")
  }
  if (!is.null(spec$default_model_data$response_name) ||
      !is.null(spec$domain_name) ||
      !is.null(spec$default_model_data$auxiliary_variables)) {
    more_args = T
  } else {
    more_args = F
  }
  
  if (!is.null(spec$formula)) {
    print(spec$formula)
    if (more_args) {
      p("**with additional arguments:**")
    }
  }
  if (!is.null(spec$domain_name)) {
    p("Domain: ", spec$domain_name)
  }
  if (!is.null(spec$default_model_data$response_name)) {
    p("Resposne: ", spec$default_model_data$response_name)
  } else {
    p("Response: ", all.vars(spec$formula)[1])
  }
  if (!is.null(spec$default_model_data$auxiliary_variables)) {
    p("Auxiliary variables: ", paste0(spec$default_model_data$auxiliary_variables, collapse = ","))
  }
  
  if (spec$model_stage == "zi" && !is.null(spec$second_stage_spec)) {
    p("")
    p("**and second-stage logistic specification**")
    print(spec$second_stage_spec, suppress_header = T)
  }
}

#' @exportS3Method base::print
print.basal_fit <- function(x, ...) {
  print(x$model)
}

#' @exportS3Method graphics::pairs
pairs.basal_fit <- function(x, ...) {
  data <- x$data
  data <- data[,unique(c(x$params$response, unlist(x$params$predictors)))]
  data <- data[,sapply(data[1,], function (x) {is.numeric(x) || is.factor(x)})]
  graphics::pairs(data)
}

#' @exportS3Method base::print
print.basal_check <- function(x, ...) {
  for (plot in x$pp_checks) {
    if (!is.null(plot)) {
      for (p in plot) {
        print(p)
      }
    }
  }
}

#' @exportS3Method graphics::pairs
pairs.basal_check <- function(x, ...) {
  stop("No method for basal_check with pairs(). Use pairs() with an object of basal_fit.")
}

#' @exportS3Method base::summary
summary.basal_check <- function(object, ...) {
  for (stat in object$convergence) {
    if (!is.null(stat)) {
      print(stat)
    }
  }
}

#' @exportS3Method base::print
print.basal_estimate <- function(x, ...) {
  print(x$preds)
}
