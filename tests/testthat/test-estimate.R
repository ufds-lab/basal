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
    basal::engine_rstanarm(),
    basal::engine_brms()
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
        )
        # not present in newdata
      )
    }
  }
}

testthat::test_that("estimate handles basic arguments and domains", {
  test_estimate()
})

big_test_estimate <- function () {
  test_data <- data.frame(
    biomass = c(0,  11, 8, 10, 0, 12, 14, 9, 0, 13, 8, 10, 0, 11, 16, 19),
    ppt = c(2, 1, 2, 3, 2, 2, 3, 1, 1, 3, 1, 2, 0, 1, 2, 3),
    county = factor(rep(c("A", "B", "C", "D"), each = 4)),
    d = factor(rep(c("A", "B", "C", "D", "E", "F", "G", "H"), each = 2))
  )
  
  custom_args = list(
    formula = biomass ~ ppt,
    level = NULL
  )
  
  manual_args = list(
    response_name = "biomass",
    auxiliary_variables = "ppt"
  )
  
  for (base_args in list(custom_args, manual_args)) {
    for (model_type in c("custom", "BHF", "FH")) {
      args <- base_args
      if (model_type != "custom") {
        domain_name = "county"
      } else {
        domain_name = NULL
        args$formula <- biomass ~ ppt + (1 | county)
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
              if (level == "area" || model_type == "FH") {
                population_size = 1000
              } else {
                population_size = NULL
              }
              domain_name <- if (model_type != "custom" || level == "area") {
                "county"
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
                next
              } else {
                spec <- basal::specify(
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
                for (engine in list(
                  basal::engine_rstanarm(),
                  basal::engine_brms()
                )) {
                  if (!(level %in% engine$level &&
                        model_type %in% engine$model &&
                        model_stage %in% engine$model_stage) ||
                      sssm) {
                    next
                  }
                  model = (
                    suppressWarnings(basal::fit(
                      spec,
                      test_data,
                      chains = 1,
                      burn_in = 2,
                      thin = 1,
                      iter = 4,
                      ncores = 1,
                      nthreads = 1,
                      engine = engine,
                      refresh = 0,
                      population_size = population_size
                    ))
                  )
                  
                  for (stat in list(
                    list(mean = mean),
                    c(mean = mean, var = var, med = median)
                  )) {
                    for (agg_stat in list(
                      list(mean = mean), list(sd = sd), list(med = median)
                    )) {
                      for (domain in list(NULL, "d", "county")) {
                        testthat::expect_no_error(basal::estimate(
                          model, newdata = test_data, ndraws = 2,
                          stat = stat, aggregation_statistic = agg_stat,
                          domain = domain
                        ))
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

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
