test_specify <- function() {
  args <- get_many_specs()
  sapply(args, function(l) {
    if (l$error) {
      test <- testthat::expect_error
    } else {
      test <- testthat::expect_no_error
    }
    test(
      do.call(basal::specify, l$args)
    )
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
  zi_spec <- basal::specify(
    model = "BHF",
    response_name = "biomass",
    auxiliary_variables = "ppt",
    domain_name = "county",
    model_stage = "zi"
  )

  testthat::expect_s3_class(zi_spec$second_stage_spec, "basal_spec")
  testthat::expect_equal(zi_spec$second_stage_spec$domain_name, "county")
  testthat::expect_equal(
    zi_spec$second_stage_spec$default_model_data$auxiliary_variables, "ppt"
  )
  testthat::expect_equal(zi_spec$second_stage_spec$level, "unit")
  testthat::expect_equal(zi_spec$second_stage$model_type, "BHF")
})

testthat::test_that("Specification are proper specifications", {
  for (model_stage in c("single", "zi")) {
    testthat::expect_error(basal::specify(
      y ~ x,
      response_name = "z", level = "unit",
      model_stage = model_stage
    ))
  }

  for (response_name in list("z", c("z", "w"))) {
    for (auxiliary_variables in list(c("x1"), list(c("x1"), c("x2")))) {
      for (domain_name in list(c("id"), c("id1", "id2"))) {
        s <- testthat::expect_no_error(basal::specify(
          response_name = response_name,
          domain_name = domain_name,
          auxiliary_variable = auxiliary_variables,
          model = "BHF", model_stage = "zi"
        ))
        if (length(response_name) > 1) {
          testthat::expect_true(
            s$default_model_data$response_name !=
              s$second_stage_spec$default_model_data$response_name
          )
        }
        testthat::expect_true(
          length(s$default_model_data$response_name) == 1 &&
            length(s$second_stage_spec$default_model_data$response_name) == 1
        )
        if (is.list(auxiliary_variables)) {
          testthat::expect_true(
            s$default_model_data$auxiliary_variables !=
              s$second_stage_spec$default_model_data$auxiliary_variables
          )
        }
        if (length(domain_name) > 1) {
          testthat::expect_true(
            s$default_model_data$domain_name !=
              s$second_stage_spec$default_model_data$domain_name
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
