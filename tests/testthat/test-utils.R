testthat::test_that("aggregate_data handles it's arguments", {
  data <- list(
    data.frame(
      biomass = c(10, 12, 14, 9, 11, 13, 8, 10, 20, 11, 16, 19),
      ppt = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
      county = factor(rep(c("A", "B", "C", "D"), each = 3)),
      strata = factor(rep(c("E","F","G"), each = 4))
    ),
    NULL,
    "a"
  )
  
  N = nrow(data[[1]])
  
  # I'm testing each argument - that's a lot of tests 
  # (1,8000,000 of them as of last time I updated this comment)!
  # I'm not going to test for all the interactions with the idea that we really 
  # don't expect most of these errors to be a combination type of thing, and just
  # want to make sure they show up in the first place
  for (thedata in data) {
    if (is.null(thedata) || any(thedata != data[[1]])) {
      response_list = c("biomass")
      domain_list = c("county")
      population_data_list = list(data[[1]])
      population_size_list = list(1000)
      sample_weight_list = list(rep(0.1, N))
      estimate_list = list("mean")
    } else {
      response_list = list("biomass", "biomas", 1, NULL)
      domain_list = list("county", 1, NULL)
      population_data_list = union(data,data[[1]] |> dplyr::rename(couty = county))
      population_size_list = list(-1, "a", 0, 1000, NULL)
      sample_weight_list = list(rep(0.1, N), "a", 1, rep(-0.1, N), NULL)
      estimate_list = list("mean", "sum", "hi", 1, NULL)
    }
    for (response in response_list) {
      if (is.null(response) || response != "biomass") {
        domain_list = list("county")
        population_data_list = list(data[[1]])
        population_size_list = list(1000)
        sample_weight_list = list(rep(0.1, N))
      }
      for (domain_name in domain_list) {
        if (is.null(domain_name) || domain_name != "county") {
          population_data_list = list(data[[1]])
          population_size_list = list(1000)
          sample_weight_list = list(rep(0.1, N))
        }
        for (population_data in population_data_list) {
          for (population_size in population_size_list) {
            if (is.null(population_size) || population_size != 1000) {
              sample_weight_list = list(rep(0.1, N))
            }
            for (sample_weights in sample_weight_list) {
              for (estimate in estimate_list) {
                if(is.null(estimate) || estimate %in% c("hi", 1)) {
                  method_list = list("HT", "PS")
                  PS_strata_list = list("strata")
                  extra_args_list = list(NULL)
                } else {
                  method_list = list("HT", "PS", "hi", 1, NULL)
                  PS_strata_list = list("strata", "hi", 1, NULL)
                  extra_arg_list = list(NULL, c(y=2), c(N=1e5, var_est = T), c(fpc = F))
                }
                for (method in method_list) {
                  for (PS_strata in PS_strata_list) {
                    for (extra_args in extra_arg_list) {
                      if (is.null(domain_name) || !is.character(domain_name)) {
                        expecting_error = T
                      } else if (is.null(thedata) || !is.data.frame(thedata)) {
                        expecting_error = T
                      } else if (is.null(response) || !(response %in% colnames(thedata))) {
                        expecting_error = T
                      } else if (is.null(population_data) || !is.data.frame(population_data)) {
                        expecting_error = T
                      } else if (!(domain_name %in% colnames(thedata)) || 
                                 !(domain_name %in% colnames(population_data))) {
                        expecting_error = T
                      } else if (is.null(population_size) || !is.numeric(population_size) || population_size <= 0) {
                        expecting_error = T
                      } else if (is.null(population_size) || !is.numeric(population_size) 
                                 || population_size < 0 || population_size > 1 
                                 || length(population_size) != N) {
                        expecting_error = T
                      } else if (!is.character(estimate) || !match.arg(estimate, c("mean", "sum"))) {
                        expecting_error = T
                      } else if (!is.character(method) || !match.arg(method, c("HT", "PS"))) {
                        expecting_error = T
                      } else if (!is.character(PS_strata) 
                                 || !match.arg(PS_strata, intersect(colnames(thedata), colnames(population_data)))) {
                        expecting_error = T
                      } else if (!is.null(extra_args) && 
                                 intersect(names(extra_args), c("y", "pi", "N", "var_est", "messages"))) {
                        # not including the extra for PS estimator because none of those are in the extra_args list
                        expecting_error = T
                      } else if (method == "PS" && is.null(PS_strata)) {
                        expecting_error = T
                      }
                      
                      if (expecting_error) {
                        test = testthat::expect_error
                      } else {
                        test = testthat::expect_no_error
                      }
                      args = list(
                        data = thedata,
                        response = response,
                        domain_name = domain_name,
                        population_data = population_data,
                        population_size = population_size,
                        sample_weights = sample_weights,
                        estimate = estimate,
                        method = method,
                        PS_strata = PS_strata
                      )
                      if (!is.null(extra_args)) {
                        for (i in 1:length(extra_args)) {
                          args[[names(extra_args)[i]]] = extra_args[[i]]
                        }
                      }
                      
                      t = test(do.call(basal::aggregate_data, args = args)) |> try()
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
    }
  }
})

testthat::test_that("aggregate_data aggregates correctly", {
  data = data.frame(
    biomass = c(10, 12, 14, 9, 11, 13, 8, 10, 20, 11, 16, 19),
    ppt = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
    county = factor(rep(c("A", "B", "C", "D"), each = 3)),
    strata = factor(rep(c("E","F","G"), each = 4))
  )
  
  sub_data = data[data$county == "A",]
  
  ht = mase::horvitzThompson(
    y = sub_data$biomass,
    N = 1e5,
    var_est = T,
    messages = F
  )
  
  a = aggregate_data(data, "biomass", "county", data, population_size = 1e5)
  a = a$aggregate_obs[a$aggregate_obs$county == "A",]
  
  testthat::expect_equal(ht$pop_mean, a$biomass)
  testthat::expect_equal(sqrt(ht$pop_mean_var), a$biomass_se)
  
  a = aggregate_data(data, "biomass", "county", data, population_size = 1e5, estimate = "sum")
  a = a$aggregate_obs[a$aggregate_obs$county == "A",]
  
  testthat::expect_equal(ht$pop_total, a$biomass)
  testthat::expect_equal(sqrt(ht$pop_total_var), a$biomass_se)
  
  # for post-strat we need the strata to be finer than the domain, so I'm swapping
  # the "county" and "strata" variables here
  sub_data = data[data$strata == "E",]
  ps = mase::postStrat(
    sub_data$biomass,
    xsample = sub_data$county,
    xpop = sub_data$county,
    N = 1e5, var_est = T, messages = F
  )
  
  a = aggregate_data(data, "biomass", "strata", data,
                     population_size = 1e5, method = "PS", PS_strata = "county")
  a = a$aggregate_obs[a$aggregate_obs$strata == "E",]
  
  expect_equal(a$biomass, ps$pop_mean)
  expect_equal(a$biomass_se, sqrt(ps$pop_mean_var))
  
  a = aggregate_data(data, "biomass", "strata", data,
                     population_size = 1e5, method = "PS", estimate = "sum", PS_strata = "county")
  a = a$aggregate_obs[a$aggregate_obs$strata == "E",]
  
  expect_equal(a$biomass, ps$pop_total)
  expect_equal(a$biomass_se, sqrt(ps$pop_total_var))
})
  
  