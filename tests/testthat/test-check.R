test_check <- function() {
  
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
  
  for (engine in list(engine_brms(), engine_rstanarm())) {
    args = list(
      spec = spec1,
      data = test_data,
      chains = 1,
      iter = 10,
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
    
    
    for (fit in list(fit1, fit2)) {
      for (stat in list(
        c(mean = mean, median = median),
        c(ecdf = ecdf),
        c(char = function (x) {"this will error on chars"})
      )) {
        for (pp_c in c(TRUE, FALSE)) {
          for (t_p in c(TRUE, FALSE)) {
            for (jtts in list(
              NULL,
              list(ecdf = ecdf),
              list(mean = mean, var = var)
            )) {
              if (is.character(stat[[1]](c(2,3)))) {
                testthat_fun <- testthat::expect_error
              } else {
                testthat_fun <- testthat::expect_no_error
              }
              testthat_fun(
                basal::check(
                  fit,
                  stat = stat,
                  draws = 2,
                  include_base_pp_check = pp_c,
                  trace_plots = t_p,
                  join_two_stage_stat = jtts
                )
              )
            }
          }
        }
      }
    }
  }
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
  
  fit <- basal::fit(
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
