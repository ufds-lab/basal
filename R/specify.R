#' Specify a Small Area Estimation Model
#'
#' @param formula A formula object describing the model design. Required when `model = "custom"`. Defaults to `NULL`.
#'
#' @param level Character string denoting the target spatial level. Available values are `"unit"` and `"area"`.
#'
#' @param model Character string targeting either user-defined structures or preset SAE models. Available choices include
#'        `"custom"`, `"FH"` (Fay-Herriot), and `"BHF"` (Battese-Harter-Fuller).  Defaults to `"custom"`.
#'
#' @param obs_variability Conditional parameter for the variances. When set to `"__auto_aggregation"` for area-level models,
#'        fit() automatically computes the variances.
#'
#' @param domain_level Character string for stratification variable representing the small areas (e.g., `"county"`).
#'
#' @param response Character string specifying the column name of the target forest attribute (e.g., `"obs_biomass"`).
#'        Required for preset frameworks.
#'
#' @param auxiliary_variables Character vector tracking prediction covariates.
#'        Defaults to `"__everything"` for all the other variables if unspecified for presets.
#'
#' @return An object of class `lacroix_spec` containing the outputs.
#'
#' @examples
#' plot_spec <- specify(
#'   formula = obs_biomass ~ evc + evt + (1 | county),
#'   level = "unit",
#'   model = "custom"
#' )
#'
specify <- function(formula = NULL,
                    level = c("area", "unit"),
                    model = "custom", # model should be in c("custom", "FH", "BHF")
                    obs_variability = NULL,
                    domain_name = NULL,
                    response_name = NULL,
                    auxiliary_variables = NULL) {

  func_call <- match.call()
  check_inherits("logical", auto_aggregate)

  default_model_data <- NULL # default value. Get's overwritten if not using custom model

  # we should make model lose caps before this
  if (!(model %in% c("custom", "FH", "BHF"))) {
    stop("Provided model not a listed option")
  } else if (model == "custom") {
    check_inherits("formula", formula)
    if (is.null(formula)) {
      stop("Must provide a formula with a custom model.")
    } else if (is.vector(level) || !(level == "area" || level == "unit")) {
      stop("Level must be a single value either equal to 'area' or 'unit'.")
    }
    if (level == "unit") {
      if (!is.null(domain_name)) {
        message("Supplied a domain name for a unit-level custom model.",
                "Domain name will be ignored.")
        domain_name = NULL
      }
      if (!is.null(obs_variability)) {
        message("Supplied variability of the observations. This isn't used",
                "use y | se(obs_variability) ~ ...",
                "this message will be cleaned up later")
        obs_variability = NULL
      }
    } else if (level == "area") {
      if (obs_variability == "__auto_aggregation") {
        auto_agg = TRUE
        if (is.null(domain_name)) {
          stop("Must supply a domain name for auto aggregation.")
        }
      } else {
        auto_agg = FALSE
        if (!is.null(domain_name)) {
          message("Not using domain name because obs_variability has been provided.")
          domain_name = NULL
        }
        if (is.null(obs_variability)) {
          stop("Must supply 'obs_variability' if not using auto aggregation.")
        }
      }
    }
  } else { # model != "custom"
    if (!is.null(formula)) {
      warning("Ignoring supplied formula for pre-set models. Please use custom model",
              "if you want to use a formula, or use
                'response', 'domain_name', and 'auxiliary_variables' for preset models")
      formula <- NULL
    }
    if (model != "BHF") {
      stop("Other default models not currently supported.")
    }
    if (model == "BHF") {
      if (level != "unit") {
        warning("Specified is not equal to \"unit\" on a BHF model. Setting 'level = \"unit\"'.")
        level <- "unit"
      }
      auto_agg <- NULL
      obs_variability <- NULL
      if (is.null(response)) {
        stop("Must supply a response variable.")
      } else if (is.null(domain_name)) {
        stop("Must supply a domain. name for BHF.")
      } else if (is.null(auxiliary_variables)) {
        warning("No auxiliary variables supplied. Using all other variables.")
        auxiliary_variables <- "__everything"
      }
      default_model_data = list(
        response = response,
        domain_name = domain_name,
        auxiliary_variables = auxiliary_variables
      )
    }
  }

  out <- list(
    call = func_call,
    formula = formula,
    level = level,
    model_type = model,
    domain_name = domain_name,
    obs_variability = obs_variability,
    auto_aggregate = auto_agg,
    default_model_data = default_model_data
  )

  return(
    structure(out, class = "lacroix_spec")
  )
}

