test_estimate <- function() {
  
  test_data <- data.frame(
    biomass = c(0,  11, 8, 10, 0, 12, 14, 9, 0, 13, 8, 10, 0, 11, 16, 19),
    ppt = c(2, 1, 2, 3, 2, 2, 3, 1, 1, 3, 1, 2, 0, 1, 2, 3),
    county = factor(rep(c("A", "B", "C", "D"), each = 4))
  )
  
  spec1 <- basal::specify(
    biomass ~ ppt + (1 | county),
    level = "unit"
  )
  
  spec2 <- basal::specify(
    biomass ~ ppt + (1 | county),
    level = "unit",
    model_stage = "zi"
  )
  
  for (engine in list(
    engine_rstanarm(),
    engine_brms()
  )) {
    args <- list(
      spec = spec1,
      data = test_data,
      chains = 1,
      iter = 20,
      burn_in = 5,
      thin = 1,
      seed = 1,
      ncores = 1,
      nthreads = 1,
      engine = engine,
      refresh = 0
    )
    fit1 <- do.call(
      basal::fit,
      args
    )
    
    args$spec <- spec2
    
    fit2 <- do.call(
      basal::fit,
      args
    )
    
    est_data <- test_data
    est_data$dummy = 1
    
    for (fit in list(
      fit1, fit2
    )) {
      testthat::expect_no_error(
        basal::estimate(
          fit,
          ndraws = 10,
          max_preds = 10,
          seed = 1
        )
      )
      
      testthat::expect_no_error(
        basal::estimate(
          fit,
          newdata = est_data,
          ndraws = 10,
          max_preds = 10,
          seed = 1
        )
      )
      
      testthat::expect_no_error(
        basal::estimate(
          fit,
          newdata = est_data,
          domain = "dummy",
          ndraws = 10,
          max_preds = 10,
          seed = 1
        )
      )
      
      testthat::expect_no_error(
        basal::estimate(
          fit,
          newdata = est_data,
          domain = NULL,
          ndraws = 10,
          max_preds = 10,
          seed = 1
        )
      )
      
      testthat::expect_no_error(
        basal::estimate(
          fit,
          stat = c(mean = mean, median = median),
          aggregation_statistic = c(median = median),
          ndraws = 10,
          max_preds = 10,
          seed = 1
        )
      )
      
      testthat::expect_error(
        basal::estimate(
          fit,
          domain = "state",
          ndraws = 10,
          max_preds = 10
        ),
        "not present in newdata"
      )
    }
  }
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
  
  fit <- basal::fit(
    spec,
    data = test_data,
    chains = 1,
    iter = 20,
    burn_in = 5,
    thin = 1,
    seed = 1,
    ncores = 1,
    nthreads = 1,
    engine = basal::engine_rstanarm(),
    refresh = 0
  )
  
  result <- basal::estimate(
    fit,
    domain = "county",
    stat = c(mean = mean),
    aggregation_statistic = c(mean = mean),
    ndraws = 10,
    max_preds = 10,
    seed = 1
  )
  
  testthat::expect_s3_class(result, "basal_estimate")
  testthat::expect_equal(result$params$domain, "county")
  testthat::expect_equal(result$params$ndraws, 10)
  testthat::expect_equal(result$params$max_preds, 10)
  testthat::expect_true("county" %in% names(result$preds))
})
