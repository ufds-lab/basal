# stolen from specify check, just going to make sure it works with fitting as well
get_many_fit_args <- function () {
  arglist = get_many_specs()
  
  ret = lapply(arglist, function (l) {
    if (l$error) {
      return (list(error = TRUE))
    }
    spec <- do.call(basal::specify, l$args)
    
    for (engine in list(
      engine_rstanarm(),
      engine_brms()
    )) {
      expecting_error = FALSE
      if (!((spec$level %in% engine$level) &&
            (spec$model_type %in% engine$model) &&
            (spec$model_stage %in% engine$model_stage)) ||
          !spec$valid_spec) {
        expecting_error = TRUE
      } else if (!is.null(spec$obs_variability) && 
                 spec$level == "area" && 
                 spec$obs_variability != "biomass_se") {
        expecting_error = TRUE
      } else if (!is.null(spec$DRV) && engine$name != "brms") {
        expecting_error = TRUE 
      }
      return (list(spec = spec,
                   engine = engine,
                   error = expecting_error))
        
    }
  })
  
  return (unique(ret))
}

big_test_fit <- function () {
  test_data <- data.frame(
    biomass = c(0,  11, 8, 10, 0, 12, 14, 9, 0, 13, 8, 10, 0, 11, 16, 19),
    ppt = c(2, 1, 2, 3, 2, 2, 3, 1, 1, 3, 1, 2, 0, 1, 2, 3),
    county = factor(rep(c("A", "B", "C", "D"), each = 4))
  )
  
  test_agg_data = aggregate_data(
    test_data, "biomass", "county", test_data, population_size = 1e3
  )
  
  specs = suppressWarnings(suppressMessages(get_many_fit_args()))
  
  sapply(specs, function (l) {
    if (l$error) {
      test = testthat::expect_error
    } else {
      test = testthat::expect_no_error
    }
    if (is.null(l$spec) || 
        l$spec$level == "area") {
      thedata = test_agg_data
    } else {
      thedata = test_data
    }
    
    t = test(
      suppressWarnings(basal::fit(
        l$spec,
        thedata,
        chains = 1,
        burn_in = 2,
        thin = 1,
        iter = 4,
        ncores = 1,
        nthreads = 1,
        engine = l$engine,
        refresh = 0
      )
      ))
  })
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
  
  agg_test_data = basal::aggregate_data(
    test_data, "biomass", "county", test_data, population_size = 1e3
  )
  
  for (engine in list(basal::engine_rstanarm(), basal::engine_brms())) {
    for (model_type in c("BHF", "FH")) {
      if (model_type == "BHF") {
        level <- "unit"
      } else if (model_type == "FH") {
        level <- "area"
      }
      
      if (model_type == "FH") {
        thedata <- agg_test_data
      } else {
        thedata <- test_data
      }
      obs_variability = NULL
      
      specs <- list(
        custom = basal::specify(
          biomass ~ ppt + (1 | county),
          level = level,
          domain_name = NULL,
          obs_variability = obs_variability
        ),
        
        BHF = basal::specify(
          model = model_type,
          response_name = "biomass",
          auxiliary_variables = "ppt",
          domain_name = "county",
          obs_variability = obs_variability
        ),
        
        semi_custom = basal::specify(
          biomass ~ ppt,
          domain_name = "county",
          model = model_type,
          obs_variability = obs_variability
        )
      )
      
      if (!(model_type %in% engine$model) ||
          !(level %in% engine$level)) {
        for (spec in names(specs)) {
          testthat::expect_error(
            basal::fit(
              specs[[spec]],
              data = thedata,
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
        t = testthat::expect_no_error(
          fit_list[[spec]] <- basal::fit(
            specs[[spec]],
            data = thedata,
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
        ) |> try(
          if (inherits(t, "try-error")) {
            browser()
          }
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
      t = testthat::expect_true(same_models) |> try()
      if (inherits(t, "try-error")) {
        browser()
      }
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
  for (engine in list(basal::engine_rstanarm(), basal::engine_brms())) {
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
