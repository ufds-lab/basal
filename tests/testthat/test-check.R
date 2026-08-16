test_check <- function() {
  
  test_data <- data.frame(
    biomass = c(10, 12, 14, 9, 11, 13, 8, 10, 20, 11, 16, 19),
    ppt = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
    county = factor(rep(c("A", "B", "C", "D"), each = 3))
  )
  
  spec <- basal::specify(
    biomass ~ ppt + (1 | county),
    level = "unit"
  )
  
  fit <- generics::fit(
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

  testthat::expect_no_error(
    basal::check(
      fit,
      stat = c(mean = mean),
      draws = 2,
      include_base_pp_check = FALSE,
      trace_plots = FALSE
    )
  )

  testthat::expect_no_error(
    basal::check(
      fit,
      stat = c(
        mean = mean,
        median = median
      ),
      draws = 2,
      include_base_pp_check = FALSE,
      trace_plots = FALSE
    )
  )
}

testthat::test_that("check handles basic arguments and statistics", {
  test_check()
})

testthat::test_that("check returns a basal_check object", {
  
  test_data <- data.frame(
    biomass = c(10, 12, 15, 9, 18, 21, 13, 17, 20, 11, 16, 19),
    ppt = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
    county = factor(rep(c("A", "B", "C", "D"), each = 3))
  )
  
  spec <- basal::specify(
    biomass ~ ppt + (1 | county),
    level = "unit"
  )
  
  fit <- generics::fit(
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
  
  result <- basal::check(
    fit,
    stat = c(mean = mean),
    draws = 2,
    include_base_pp_check = FALSE,
    trace_plots = FALSE
  )
  
  testthat::expect_s3_class(result, "basal_check")
  testthat::expect_equal(result$params$draws, 2)
  testthat::expect_false(result$params$include_base_pp_check)
  testthat::expect_false(result$params$trace_plots)
})


testthat::test_that("check validates arguments", {
  fake_fit <- structure(list(), class = "basal_fit")
  testthat::expect_error(basal::check(fake_fit, draws = 0), "positive integer")
  testthat::expect_error(basal::check(fake_fit, include_base_pp_check = "yes"), "TRUE or FALSE")
  testthat::expect_error(basal::check(fake_fit, trace_plots = NA), "TRUE or FALSE")
})