test_estimate <- function() {
  
  test_data <- data.frame(
    biomass = c(10, 12, 14, 9, 11, 13, 8, 10, 20, 11, 16, 19),
    ppt = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
    county = factor(rep(c("A", "B", "C", "D"), each = 3))
  )
  
  spec <- basal::specify(
    model = "BHF",
    response_name = "biomass",
    auxiliary_variables = "ppt",
    domain_name = "county"
  )
  
  fit <- generics::fit(
    spec,
    data = test_data,
    chains = 1,
    iter = 20,
    burn_in = 5,
    thin = 1,
    seed = 1,
    ncores = 1,
    nthreads = 1,
    refresh = 0
  )
  
  testthat::expect_no_error(
    basal::estimate(
      fit,
      ndraws = 10,
      max_preds = 1000,
      seed = 1
    )
  )

  testthat::expect_no_error(
    basal::estimate(
      fit,
      domain = "county",
      ndraws = 10,
      max_preds = 1000,
      seed = 1
    )
  )

  testthat::expect_no_error(
    basal::estimate(
      fit,
      domain = NULL,
      ndraws = 10,
      max_preds = 1000,
      seed = 1
    )
  )

  testthat::expect_no_error(
    basal::estimate(
      fit,
      newdata = test_data,
      domain = "county",
      ndraws = 10,
      max_preds = 1000,
      seed = 1
    )
  )
  
  testthat::expect_no_error(
    basal::estimate(
      fit,
      domain = "county",
      stat = c(mean = mean, median = median),
      aggregation_statistic = c(median = median),
      ndraws = 10,
      max_preds = 1000,
      seed = 1
    )
  )
  
  testthat::expect_error(
    basal::estimate(
      fit,
      domain = "state",
      ndraws = 10,
      max_preds = 1000
    ),
    "not present in newdata"
  )
}

testthat::test_that("estimate handles basic arguments and domains", {
  test_estimate()
})

testthat::test_that("estimate returns expected domain information", {
  test_data <- data.frame(
    biomass = c(10, 12, 15, 9, 18, 21, 13, 17, 20, 11, 16, 19),
    ppt = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
    county = factor(rep(c("A", "B", "C", "D"), each = 3))
  )
  
  spec <- basal::specify(
    model = "BHF",
    response_name = "biomass",
    auxiliary_variables = "ppt",
    domain_name = "county"
  )
  
  fit <- generics::fit(
    spec,
    data = test_data,
    chains = 1,
    iter = 20,
    burn_in = 5,
    thin = 1,
    seed = 1,
    ncores = 1,
    nthreads = 1,
    refresh = 0
  )
  
  result <- basal::estimate(
    fit,
    domain = "county",
    stat = c(mean = mean),
    aggregation_statistic = c(mean = mean),
    ndraws = 10,
    max_preds = 1000,
    seed = 1
  )
  
  testthat::expect_s3_class(result, "basal_estimate")
  testthat::expect_equal(result$params$domain, "county")
  testthat::expect_equal(result$params$ndraws, 10)
  testthat::expect_equal(result$params$max_preds, 1000)
  testthat::expect_true("county" %in% names(result$preds))
})