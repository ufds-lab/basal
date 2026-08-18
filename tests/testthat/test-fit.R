# stolen from specify check, just going to make sure it works with fitting as well
big_test_fit <- function () {
  test_data <- data.frame(
    biomass = c(0,  11, 8, 10, 0, 12, 14, 9, 0, 13, 8, 10, 0, 11, 16, 19),
    ppt = c(2, 1, 2, 3, 2, 2, 3, 1, 1, 3, 1, 2, 0, 1, 2, 3),
    county = factor(rep(c("A", "B", "C", "D"), each = 4))
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
                  engine_rstanarm(),
                  engine_brms()
                )) {
                  if (!(level %in% engine$level &&
                        model_type %in% engine$model &&
                        model_stage %in% engine$model_stage) ||
                      sssm) {
                    next
                  }
                  testthat::expect_no_error(
                    suppressWarnings(basal::fit(
                      spec,
                      test_data,
                      chains = 1,
                      burn_in = 5,
                      thin = 1,
                      iter = 10,
                      ncores = 1,
                      nthreads = 1,
                      engine = engine,
                      refresh = 0
                    )
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

test_many_specs <- testthat::test_that("fit handles basic model specifications", {
  big_test_fit()
})

test_fit <- function() {
  
  test_data <- data.frame(
    biomass = c(10, 12, 14, 9, 11, 13, 8, 10, 20, 11, 16, 19),
    ppt = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
    county = factor(rep(c("A", "B", "C", "D"), each = 3))
  )
  
  for (engine in list(engine_rstanarm(), engine_brms())) {
    for (model_type in c("BHF", "FH")) {
      if (model_type == "BHF") {
        level <- "unit"
      } else if (model_type == "FH") {
        level <- "area"
      }
      
      if (model_type == "FH") {
        domain_name <- "county"
        population_size = 100
      } else {
        domain_name <- NULL
        population_size <- NULL
      }
      
      specs <- list(
        custom = basal::specify(
          biomass ~ ppt + (1 | county),
          level = level,
          domain_name = domain_name
        ),
        
        BHF = basal::specify(
          model = model_type,
          response_name = "biomass",
          auxiliary_variables = "ppt",
          domain_name = "county"
        ),
        
        semi_custom = basal::specify(
          biomass ~ ppt,
          domain_name = "county",
          model = model_type
        )
      )
      
      if (!(model_type %in% engine$model) ||
          !(level %in% engine$level)) {
        for (spec in names(specs)) {
          testthat::expect_error(
            basal::fit(
              specs[[spec]],
              data = test_data,
              chains = 1,
              iter = 10,
              burn_in = 5,
              thin = 1,
              seed = 1,
              ncores = 1,
              nthreads = 1,
              engine = engine,
              population_size = population_size,
              refresh = 0
            )
          )
        }
        next
      }
      
      fit_list <- list()
      
      for (spec in names(specs)) {
        testthat::expect_no_error(
          fit_list[[spec]] <- basal::fit(
            specs[[spec]],
            data = test_data,
            chains = 1,
            iter = 10,
            burn_in = 5,
            thin = 1,
            seed = 1,
            ncores = 1,
            nthreads = 1,
            engine = engine,
            population_size = population_size,
            refresh = 0
          )
        )
      }
      
      if (engine$name == "brms") {
        model_list <- lapply(
          fit_list,
          function (fit) {
            fit$model$model
          }
        )
        
      } else if (engine$name == "rstanarm") {
        model_list <- lapply(
          fit_list,
          function (fit) {
            fit_list$custom$model$call
          }
        )
      }
      same_models = TRUE
      for (model in model_list) {
        if (!all.equal(model, model_list[[1]])) {
          same_models = FALSE
        }
      }
      testthat::expect_true(same_models)
    }
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
  for (engine in list(engine_rstanarm(), engine_brms())) {
    testthat::expect_error(
      basal::fit(
        spec,
        data = test_data,
        chains = 0,
        iter = 10,
        burn_in = 5,
        engine = engine
      )
      # chains must be positive
    )
    
    testthat::expect_error(
      basal::fit(
        spec,
        data = test_data,
        chains = 1,
        iter = 10,
        burn_in = 10,
        engine = engine
      )
      # burn_in must be smaller than iters
    )
    
    testthat::expect_error(
      basal::fit(
        spec,
        data = test_data,
        chains = 1,
        iter = 10.5,
        burn_in = 5,
        engine = engine
      )
      # iters must be an integer
    )
  }
})
