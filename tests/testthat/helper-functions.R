get_many_specs <- function() {
  testthat::skip_on_cran()

  arglist <- list()

  custom_args <- list(
    formula = biomass ~ ppt + (1 | county),
    level = NULL
  )

  custom_args2 <- list(
    formula = c(
      biomass ~ ppt + (1 | county),
      biomass ~ ppt + (1 | county)
    ),
    level = NULL
  )

  custom_args3 <- list(
    formula = ~ ppt + (1 | county),
    level = NULL
  )

  manual_args <- list(
    response_name = "biomass",
    auxiliary_variables = "ppt"
  )

  for (args in list(custom_args, custom_args2, custom_args3, manual_args)) {
    for (model_type in c("custom", "BHF", "FH")) {
      for (domain_name in list(NULL, "county")) {
        for (transform in list(
          NULL,
          basal::transform(
            function(x) {
              x^2
            },
            function(x) {
              sqrt(x)
            }
          )
        )) {
          for (model_stage in c("single", "zi")) {
            for (level in c("unit", "area")) {
              domain_name <- if (model_type != "custom" || level == "area") {
                "county"
              } else {
                NULL
              }
              for (obs_variability in list(NULL, "asdf")) {
                for (DRV in list(TRUE, NULL)) {
                  expecting_error <- FALSE
                  if (!is.null(args$formula)) {
                    if (inherits(args$formula, "formula")) {
                      f <- args$formula
                    } else {
                      f <- args$formula[[1]]
                    }
                    if (attr(stats::terms(f), which = "response") == 0) {
                      expecting_error <- TRUE
                    }
                  }
                  if ((level == "area" && model_type != "BHF") ||
                        model_type == "FH") {
                    if (model_stage == "zi") {
                      expecting_error <- TRUE
                    } else if (!is.null(transform)) {
                      expecting_error <- TRUE
                    }
                  }
                  if (!is.null(level) &&
                        ((model_type == "BHF" && level == "area") ||
                           (model_type == "FH" && level == "unit"))) {
                    expecting_error <- TRUE
                  }
                  if (model_type == "custom") {
                    if (is.null(args$formula)) {
                      expecting_error <- TRUE
                    } else if (!is.null(domain_name) && is.null(DRV)) {
                      expecting_error <- TRUE
                    }
                  } else {
                    if (is.null(domain_name)) {
                      expecting_error <- TRUE
                    }
                  }
                  if (is.list(args$formula) && length(args$formula) > 1 &&
                        model_stage == "single") {
                    expecting_error <- TRUE
                  }
                  if (is.null(args$response_name) &&
                        (!is.list(args$formula) &&
                           !attr(stats::terms(args$formula),
                                 which = "response"))) {
                    expecting_error <- TRUE
                  }
                  if (!is.null(DRV) && is.null(domain_name)) {
                    expecting_error <- TRUE
                  }
                  arglist <- union(arglist, list(list(
                    args = list(
                      formula = args$formula,
                      response_name = args$response_name,
                      auxiliary_variables = args$auxiliary_variables,
                      model = model_type,
                      domain_name = domain_name,
                      transform = transform,
                      model_stage = model_stage,
                      obs_variability = obs_variability,
                      level = level,
                      DRV = DRV
                    ),
                    error = expecting_error
                  )))
                }
              }
            }
          }
        }
      }
    }
  }

  return(arglist)
}
