test_specify <- function () {
  custom_args = list(
    formula = biomass ~ ppt,
    level = NULL
  )
  
  manual_args = list(
    response_name = "biomass",
    auxiliary_variables = "ppt"
  )
  
  for (args in list(custom_args, manual_args)) {
    for (model_type in c("custom", "BHF", "FH")) {
      if (model_type != "custom") {
        domain_name = "evt_class_cd"
      } else {
        domain_name = NULL
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
              if (level == "area") {
                domain_name = "evt_class_cd"
              }
              expect_error = FALSE
              if ((level == "area" || model_type == "FH") && sssm && model_type != "BHF") {
                expect_error = TRUE
              } else if (sssm && !is.null(args$formula) && is.null(domain_name) && model_type != "custom") {
                expect_error = TRUE
              } else if (model_type == "custom" && is.null(args$formula) && !sssm) {
                expect_error = TRUE
              } else if ((model_type == "FH" || (model_type == "custom" && level == "area")) && 
                         is.null(domain_name) && !sssm) {
                expect_error = TRUE
              }
              if (expect_error) {
                testthat::expect_error(
                  basal::specify(
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
                )
              } else {
                testthat::expect_no_error(
                  basal::specify(
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
                )
              }
            }
          }
        }
      }
    }
  }
}


