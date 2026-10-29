get_many_specs <- function () {
  arglist = list()
  
  custom_args = list(
    formula = biomass ~ ppt + (1 | county),
    level = NULL
  )
  
  custom_args2 = list(
    formula = ~ ppt + (1 | county),
    level = NULL
  )
  
  manual_args = list(
    response_name = "biomass",
    auxiliary_variables = "ppt"
  )
  
  for (args in list(custom_args, custom_args2, manual_args)) {
    for (model_type in c("custom", "BHF", "FH")) {
      for (domain_name in list(NULL, "county")) {
        for (transform in list(
          NULL,
          basal::make_variable_transform(
            function (x) {x^2},
            function (x) {sqrt(x)}
          )
        )) {
          for (model_stage in c("single", "zi")) {
            for (sssm in c(TRUE, FALSE)) {
              for (level in c("unit", "area")) {
                domain_name <- if (model_type != "custom" || level == "area") {
                  "county"
                } else {
                  NULL
                }
                for (obs_variability in list(NULL,"asdf")) {
                  for (DRV in list(TRUE, NULL)) {
                    
                    expecting_error = FALSE
                    if (!sssm && !is.null(args$formula) &&
                        (attr(stats::terms(args$formula), which = "response") == 0)) {
                      expecting_error = TRUE
                    }
                    if (((level == "area" && model_type != "BHF") || model_type == "FH")) {
                      if (sssm) {
                        expecting_error = TRUE
                      } else if (!is.null(transform)) {
                        expecting_error = TRUE
                      }
                    } 
                    if (model_type == "custom") {
                      if (is.null(args$formula) && !sssm) {
                        expecting_error = TRUE
                      } else if (!is.null(domain_name) && is.null(DRV)) {
                        expecting_error = TRUE
                      }
                    } else {
                      if (is.null(domain_name) && !sssm) {
                        expecting_error = TRUE
                      }
                    }
                    if (!is.null(DRV) && is.null(domain_name) && !sssm) {
                      expecting_error = T
                    }
                    arglist <- union(arglist, list(list(
                      args = list(
                        formula = args$formula,
                        response_name = args$response_name,
                        auxiliary_variables = args$auxiliary_variables,
                        model = model_type,
                        domain_name = domain_name,
                        variable_transform = transform,
                        model_stage = model_stage,
                        specifying_second_stage_model = sssm,
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
  }
  return (arglist) 
}

test_specify <- function () {
  args = get_many_specs()
  sapply(args, function(l) {
    if (l$error) {
      test = testthat::expect_error
    } else {
      test = testthat::expect_no_error
    }
    t = test(
      do.call(basal::specify, l$args)
    ) |> try()
    if (inherits(t, "try-error")) {
      browser()
    }
  })
}

testthat::test_that("specify handles argument combinations", {
  test_specify()
})

testthat::test_that("preset specifications are correct", {
  
  bhf <- basal::specify(
    model = "BHF",
    response_name = "biomass",
    auxiliary_variables = "ppt",
    domain_name = "county"
  )
  
  testthat::expect_s3_class(bhf, "basal_spec")
  testthat::expect_equal(bhf$model_type, "BHF")
  testthat::expect_equal(bhf$level, "unit")
  testthat::expect_equal(bhf$domain_name, "county")
  testthat::expect_equal(bhf$default_model_data$response_name, "biomass")
  
  fh <- basal::specify(
    model = "FH",
    response_name = "biomass",
    auxiliary_variables = "ppt",
    domain_name = "county",
    obs_variability = "asdf"
  )
  
  testthat::expect_equal(fh$model_type, "FH")
  testthat::expect_equal(fh$level, "area")
})


testthat::test_that("Second-stage specification inherits model information", {
  
  second_stage <- basal::specify(
    model = "BHF",
    specifying_second_stage_model = TRUE
  )
  
  zi_spec <- basal::specify(
    model = "BHF",
    response_name = "biomass",
    auxiliary_variables = "ppt",
    domain_name = "county",
    model_stage = "zi",
    second_stage_spec = second_stage
  )
  
  testthat::expect_s3_class(zi_spec$second_stage_spec, "basal_spec")
  testthat::expect_equal(zi_spec$second_stage_spec$domain_name, "county")
  testthat::expect_equal(zi_spec$second_stage_spec$default_model_data$auxiliary_variables, "ppt")
  testthat::expect_equal(zi_spec$second_stage_spec$level, "unit")
  testthat::expect_equal(zi_spec$second_stage$model_type, "BHF")
})
