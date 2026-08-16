test_fit <- function() {
  
  test_data <- data.frame(
    biomass = c(10, 12, 14, 9, 11, 13, 8, 10, 20, 11, 16, 19),
    ppt = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
    county = factor(rep(c("A", "B", "C", "D"), each = 3))
  )
  
  specs <- list(
    custom = basal::specify(
      biomass ~ ppt + (1 | county),
      level = "unit"
    ),
    
    BHF = basal::specify(
      model = "BHF",
      response_name = "biomass",
      auxiliary_variables = "ppt",
      domain_name = "county"
    )
  )
  
  for (spec in specs) {
    testthat::expect_no_error(
      generics::fit(
        spec,
        data = test_data,
        chains = 1,
        iter = 10,
        burn_in = 5,
        thin = 1,
        seed = 1,
        ncores = 1,
        nthreads = 1,
        refresh = 0
      )
    )
  }
}


testthat::test_that("fit handles basic model specifications", {
  test_fit()
})

testthat::test_that("specifications produce correct brms models", {
  test_data <- data.frame(
    biomass = c(10, 12, 14, 9, 11, 13, 8, 10, 20, 11, 16, 19),
    ppt = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
    county = factor(rep(c("A", "B", "C", "D"), each = 3))
  )
  
  custom_spec <- basal::specify(
    biomass ~ ppt + (1 | county),
    level = "unit"
  )
  
  bhf_spec <- basal::specify(
    model = "BHF",
    response_name = "biomass",
    auxiliary_variables = "ppt",
    domain_name = "county"
  )
  
  custom_fit <- generics::fit(
    custom_spec,
    data = test_data,
    chains = 1,
    iter = 10,
    burn_in = 5,
    thin = 1,
    seed = 1,
    ncores = 1,
    nthreads = 1,
    refresh = 0
  )
  
  bhf_fit <- generics::fit(
    bhf_spec,
    data = test_data,
    chains = 1,
    iter = 10,
    burn_in = 5,
    thin = 1,
    seed = 1,
    ncores = 1,
    nthreads = 1,
    refresh = 0
  )
  
  testthat::expect_s3_class(custom_fit, "basal_fit")
  testthat::expect_s3_class(bhf_fit, "basal_fit")
  testthat::expect_equal(custom_fit$model$model, bhf_fit$model$model)
})


testthat::test_that("fit validates MCMC", {
  
  test_data <- data.frame(
    biomass = c(10, 12, 15, 9, 18, 21),
    ppt = c(1, 2, 3, 1, 2, 3),
    county = factor(rep(c("A", "B"), each = 3))
  )
  
  spec <- basal::specify(
    biomass ~ ppt + (1 | county),
    level = "unit"
  )
  
  testthat::expect_error(
    generics::fit(
      spec,
      data = test_data,
      chains = 0,
      iter = 10,
      burn_in = 5
    ),
    "must be positive"
  )
  
  testthat::expect_error(
    generics::fit(
      spec,
      data = test_data,
      chains = 1,
      iter = 10,
      burn_in = 10
    ),
    "smaller than"
  )
  
  testthat::expect_error(
    generics::fit(
      spec,
      data = test_data,
      chains = 1,
      iter = 10.5,
      burn_in = 5
    ),
    "integer-valued"
  )
})