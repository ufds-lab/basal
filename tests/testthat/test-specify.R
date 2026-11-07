get_many_specs <- function () {
  arglist = list()
  
  custom_args = list(
    formula = biomass ~ ppt + (1 | county),
    level = NULL
  )
  
  custom_args2 = list(
    formula = c(biomass ~ ppt + (1 | county),
                biomass ~ ppt + (1 | county)),
    level = NULL
  )
  
  custom_args3 = list(
    formula = ~ ppt + ( 1 | county),
    level = NULL
  )
  
  manual_args = list(
    response_name = "biomass",
    auxiliary_variables = "ppt"
  )
  
  for (args in list(custom_args, custom_args2, custom_args3, manual_args)) {
    for (model_type in c("custom", "BHF", "FH")) {
      for (domain_name in list(NULL, "county")) {
        for (transform in list(
          NULL,
          basal::transform(
            function (x) {x^2},
            function (x) {sqrt(x)}
          )
        )) {
          for (model_stage in c("single", "zi")) {
            for (level in c("unit", "area")) {
              domain_name <- if (model_type != "custom" || level == "area") {
                "county"
              } else {
                NULL
              }
              for (obs_variability in list(NULL,"asdf")) {
                for (DRV in list(TRUE, NULL)) {
                  
                  expecting_error = FALSE
                  if (!is.null(args$formula)) {
                    if (inherits(args$formula, "formula")) {
                      f = args$formula
                    } else {
                      f = args$formula[[1]]
                    }
                    if (attr(stats::terms(f), which = "response") == 0) {
                      expecting_error = TRUE
                    }
                  }
                  if (((level == "area" && model_type != "BHF") || model_type == "FH")) {
                    if (model_stage == "zi") {
                      expecting_error = TRUE
                    } else if (!is.null(transform)) {
                      expecting_error = TRUE
                    }
                  }
                  if (!is.null(level) &&
                      ((model_type == "BHF" && level == "area") ||
                       (model_type == "FH" && level == "unit"))) {
                    expecting_error = TRUE
                  }
                  if (model_type == "custom") {
                    if (is.null(args$formula)) {
                      expecting_error = TRUE
                    } else if (!is.null(domain_name) && is.null(DRV)) {
                      expecting_error = TRUE
                    }
                  } else {
                    if (is.null(domain_name)) {
                      expecting_error = TRUE
                    }
                  }
                  if (is.list(args$formula) && length(args$formula) > 1 &&
                      model_stage == "single") {
                    expecting_error = TRUE
                  }
                  if (is.null(args$response_name) && 
                      (!is.list(args$formula) &&
                       !attr(stats::terms(args$formula), which = "response"))) {
                    expecting_error = T
                  }
                  if (!is.null(DRV) && is.null(domain_name)) {
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
  testthat::skip_on_cran()
  
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
  
  zi_spec <- basal::specify(
    model = "BHF",
    response_name = "biomass",
    auxiliary_variables = "ppt",
    domain_name = "county",
    model_stage = "zi"
  )
  
  testthat::expect_s3_class(zi_spec$second_stage_spec, "basal_spec")
  testthat::expect_equal(zi_spec$second_stage_spec$domain_name, "county")
  testthat::expect_equal(zi_spec$second_stage_spec$default_model_data$auxiliary_variables, "ppt")
  testthat::expect_equal(zi_spec$second_stage_spec$level, "unit")
  testthat::expect_equal(zi_spec$second_stage$model_type, "BHF")
})

testthat::test_that("Specification are proper specifications", {
  for (model_stage in c("single", "zi")) {
    testthat::expect_error(basal::specify(
      y ~ x, response_name = "z", level = "unit", 
      model_stage = model_stage
    ))
  }
  
  for (response_name in list("z", c("z", "w"))) {
    for (auxiliary_variables in list(c("x1"), list(c("x1"), c("x2")))) {
      for (domain_name in list(c("id"), c("id1", "id2"))) {
        s = testthat::expect_no_error(basal::specify(
          response_name = response_name,
          domain_name = domain_name,
          auxiliary_variable = auxiliary_variables,
          model = "BHF", model_stage = "zi"
        ))
        if (length(response_name) > 1) {
          testthat::expect_true(
            s$default_model_data$response_name != s$second_stage_spec$default_model_data$response_name
          )
        }
        testthat::expect_true(
          length(s$default_model_data$response_name) == 1 &&
            length(s$second_stage_spec$default_model_data$response_name) == 1
        )
        if (is.list(auxiliary_variables)) {
          testthat::expect_true(
            s$default_model_data$auxiliary_variables != s$second_stage_spec$default_model_data$auxiliary_variables
          )
        }
        if (length(domain_name) > 1) {
          testthat::expect_true(
            s$default_model_data$domain_name != s$second_stage_spec$default_model_data$domain_name
          )
        }
        testthat::expect_true(
          length(s$default_model_data$domain_name) == 1 &&
            length(s$second_stage_spec$default_model_data$domain_name) == 1
        )
      }
    }
  }
})
