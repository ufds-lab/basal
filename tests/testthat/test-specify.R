test_specify <- function () {
  custom_args = list(
    formula = biomass ~ ppt,
    level = NULL
  )
  
  manual_args = list(
    response_name = "biomass",
    auxiliary_variables = "ppt"
  )
  
  for (args in list(custom_args, manual_args)) {
    for (model_type in c("custom", "BHF", "FH")) {
      if (model_type != "custom") {
        domain_name = "evt_class_cd"
      } else {
        domain_name = NULL
      }
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
                "evt_class_cd"
              } else {
                NULL
              }
              expecting_error = FALSE
              if ((level == "area" || model_type == "FH") && sssm && model_type != "BHF") {
                expecting_error = TRUE
              } else if (sssm && !is.null(args$formula) && is.null(domain_name) && model_type != "custom") {
                expecting_error = TRUE
              } else if (model_type == "custom" && is.null(args$formula) && !sssm) {
                expecting_error = TRUE
              } else if ((model_type == "FH" || (level == "area" && model_type != "BHF")) && 
                         is.null(domain_name) && !sssm) {
                expecting_error = TRUE
              } else if ((model_type == "FH" || (level == "area" && model_type != "BHF")) &&
                        !is.null(transform) && (sssm || model_stage != "zi")) {
                expecting_error = TRUE
              }
              if (expecting_error) {
                t = try(testthat::expect_error(
                  basal::specify(
                    formula = args$formula,
                    response_name = args$response_name,
                    auxiliary_variables = args$auxiliary_variables,
                    model = model_type,
                    domain_name = domain_name,
                    variable_transform = transform,
                    model_stage = model_stage,
                    specifying_second_stage_model = sssm,
                    level = level
                  )
                ))
                if (inherits(t, "try-error")) {
                  browser()
                }
              } else {
                t = try(testthat::expect_no_error(
                  basal::specify(
                    formula = args$formula,
                    response_name = args$response_name,
                    auxiliary_variables = args$auxiliary_variables,
                    model = model_type,
                    domain_name = domain_name,
                    variable_transform = transform,
                    model_stage = model_stage,
                    specifying_second_stage_model = sssm,
                    level = level
                  )
                ))
                if (inherits(t, "try-error")) {
                  browser()
                }
              }
            }
          }
        }
      }
    }
  }
}

testthat::test_that("specify handles argument combinations", {
  test_specify()
})

testthat::test_that("preset specifications are correct", {
  
  bhf <- basal::specify(
    model = "BHF",
    response_name = "biomass",
    auxiliary_variables = "ppt",
    domain_name = "evt_class_cd"
  )
  
  testthat::expect_s3_class(bhf, "basal_spec")
  testthat::expect_equal(bhf$model_type, "BHF")
  testthat::expect_equal(bhf$level, "unit")
  testthat::expect_equal(bhf$domain_name, "evt_class_cd")
  testthat::expect_equal(bhf$default_model_data$response_name, "biomass")
  
  fh <- basal::specify(
    model = "FH",
    response_name = "biomass",
    auxiliary_variables = "ppt",
    domain_name = "evt_class_cd"
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
    domain_name = "evt_class_cd",
    model_stage = "zi",
    second_stage_spec = second_stage
  )
  
  testthat::expect_s3_class(zi_spec$second_stage_spec, "basal_spec")
  testthat::expect_equal(zi_spec$second_stage_spec$domain_name, "evt_class_cd")
  testthat::expect_equal(zi_spec$second_stage_spec$default_model_data$auxiliary_variables, "ppt")
  testthat::expect_equal(zi_spec$second_stage_spec$level, "unit")
  testthat::expect_equal(zi_spec$second_stage$model_type, "BHF")
})
