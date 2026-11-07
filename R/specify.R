#' @title specify
#'
#' @description Specify a Small Area Estimation Model
#'
#' @param formula Formula for a model of type \code{model = "custom"}.
#' This should be the standard form of R mixed effect models.
#' e.g., \code{y ~ x1 + (1 | id) + (x2 | county)}.
#'
#' @param level Character string of value \code{"unit"} or \code{"area"}. Value
#' \code{"unit"} denotes unit-level. Value \code{"area"} denotes aggregate data.
#'
#' @param model Character string targeting either user-defined structures or
#' preset SAE models. Available choices include \code{"custom"}, \code{"FH"}
#' (Fay-Herriot), and \code{"BHF"} (Battese-Harter-Fuller). Defaults to
#' \code{"custom"}. If \code{"FH"} or \code{"BHF"} are selected,
#' \code{domain_name}, \code{response_name}, and \code{auxiliary_variable} may
#' be provided for the respective parts of the model.
#'
#' @param domain_name Character string for stratification, representing
#' the areas corresponding to random effects (e.g., \code{"county"}).
#'
#' @param response_name Character string specifying the column name of the
#' target response. Only required for preset framework.
#'
#' @param auxiliary_variables Character vector containing names of covariates
#' for the response. Defaults to using all other variables if unspecified in
#' preset models.
#'
#' @param transform Either an object of class \code{BASAL_transformation} from
#' [transform()] or a list specifying a variable transformation
#' \code{transform} and inverse variable transformation \code{inv_transform}.
#'
#' @param family GLM family type, specifying error distributions and links.
#' See [stats::glm] or [brms::brmsfamily]
#'
#' @param model_stage What stage of model to fit. Currently supported for only
#' "single" and "zi" stage models.
#'
#' @param ... Optional further arguments. See details.
#'
#' @details
#' The \code{...} argument allows for the use of various other
#' options. These include
#'
#' \itemize{
#'  \item \code{obs_var}: Used to fix variance of observations across a
#'  domain for area-level models. Often used when direct estimators may be
#'  pre-computed in an area-level model. This must be a name representing a
#'  column in the data. If specifying an area-level model and this is not set,
#'  then this is set to \code{response_se} where \code{response} is the response
#'  of the model. This agrees with the output of [aggregate()].
#'  \item \code{DRV}: A logical indicating whether a model should be specified
#'  with domain residual variances. May not be supported by all engines.
#' }
#'
#' @return Object of type \code{basal_spec}.
#'
#' @examples
#' transform <- transform(
#'   function(y) {
#'     y^(1 / 3)
#'   },
#'   function(y) {
#'     y^3
#'   }
#' )
#'
#' plot_spec_bhf <- specify(
#'   obs_biomass ~ evc + evt + (1 | county),
#'   level = "unit",
#'   transform = transform
#' )
#'
#' # gives equivalent specification to above
#' # this is not the same object though
#' plot_spec_bhf <- specify(
#'   obs_biomass ~ evc + evt,
#'   domain_name = "county",
#'   model = "BHF",
#'   transform = transform
#' )
#'
#' # gives equivalent specification to above
#' plot_spec_bhf <- specify(
#'   model = "BHF",
#'   domain_name = "county",
#'   response_name = "obs_biomass",
#'   auxiliary_variables = c("evc", "evt"),
#'   transform = transform
#' )
#'
#' # we can specify area level models with `model = "FH"`, or `level = "area"`
#' FH_spec <- specify(
#'   obs_biomass ~ evc + evt,
#'   model = "FH",
#'   # level = "area", # can replace `model = "FH"` above
#'   domain_name = "county"
#' )
#'
#' # We allow zero-inflated models as well
#' response_model <- specify(
#'   c(biomass ~ tcc, biomass ~ tcc + ppt),
#'   model = "BHF",
#'   domain_name = "county",
#'   model_stage = "zi",
#'   transform = transform
#' )
#'
#' @export
specify <- function(
  formula = NULL,
  domain_name = NULL,
  model = "custom",
  level = NULL,
  response_name = NULL,
  auxiliary_variables = NULL,
  transform = NULL,
  family = stats::gaussian(),
  model_stage = "single",
  ...
) {
  # If we have a custom model, the user must provide a formula and a level
  # If we have a non-custom model, the user must specify a model (type)
  # For both, there must be response, auxiliary, and domain
  # for BHF, this is it
  # for FH, we can have obs_var

  func_call <- match.call()

  dotlist <- list(...)
  if (!("obs_var" %in% ...names()) || is.null(dotlist$obs_var)) {
    obs_var <- NULL
  } else {
    obs_var <- dotlist$obs_var
  }
  if (!("DRV" %in% ...names()) || is.null(dotlist$DRV)) {
    DRV <- NULL
  } else {
    DRV <- TRUE
  }
  if (!is.null(level)) {
    level <- match.arg(level, c("area", "unit"))
  }
  model <- match.arg(model, c("custom", "FH", "BHF"))
  model_stage <- match.arg(model_stage, c("single", "zi"))
  opt_args <- list(...)

  validate_specify_inputs(
    formula = formula,
    domain_name = domain_name,
    response_name = response_name,
    auxiliary_variables = auxiliary_variables,
    obs_variability = obs_var,
    model = model
  )
  default_model_data <- NULL

  spec <- list(
    call = func_call,
    formula = formula,
    family = family,
    level = level,
    model_type = model,
    domain_name = domain_name,
    obs_variability = obs_var,
    default_model_data = default_model_data,
    variable_transform = transform,
    model_stage = model_stage,
    second_stage_spec = NULL,
    opt_args = opt_args,
    DRV = DRV,
    valid_spec = FALSE
  )

  if (spec$model_stage == "zi") {
    check_zi_spec_integrity(spec)
    specs <- create_zi_specs(spec, auxiliary_variables, response_name)
    spec <- specs$spec1
    spec$second_stage_spec <- specs$spec2
    check_spec_integrity(
      spec = spec,
      auxiliary_variables = spec$default_model_data$auxiliary_variables,
      response_name = spec$default_model_data$response_name
    )
    check_spec_integrity(
      spec = spec$second_stage_spec,
      auxiliary_variables =
        spec$second_stage_spec$default_model_data$auxiliary_variables,
      response_name = spec$second_stage_spec$default_model_data$response_name
    )
    spec <- validate_single_stage_spec(
      spec = spec,
      auxiliary_variables = spec$default_model_data$auxiliary_variables,
      response_name = spec$default_model_data$response_name
    )
    if (any(spec$level == "area")) {
      stop("Cannot fit area-level zero-inflated models currently.")
    }
  } else if (model != "aux_spec") {
    check_spec_integrity(
      spec = spec,
      auxiliary_variables = auxiliary_variables,
      response_name = response_name
    )
    spec <- validate_single_stage_spec(
      spec = spec,
      auxiliary_variables = auxiliary_variables,
      response_name = response_name
    )
  }

  # correct/set variables depending on their level
  # auto-aggregation related checks
  if (!is.null(spec$level) && spec$level == "unit" &&
        !is.null(spec$obs_variability)) {
    message("Supplied variability of the observations. ",
            "This isn't supported for unit-level models")
    spec$obs_variability <- NULL
  }

  # validate the variable transformation --- make sure we have a left-inverse
  if (!is.null(spec$variable_transform) &&
        !inherits(spec$variable_transform, "BASAL_transformation")) {
    spec$variable_transform <- transform(
      spec$variable_transform$transform,
      spec$variable_transform$inv_transform
    )
  }

  if (spec$level == "area" && !is.null(spec$variable_transform)) {
    stop("Can't specify variable transforms with area-level models")
  }

  return(
    structure(spec, class = "basal_spec")
  )
}
