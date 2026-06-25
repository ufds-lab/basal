#' Specify a Small Area Estimation Model
#'
#' @param formula a
#' @param level a
#' @param model a
#' @param engine a
#' @param auto_aggregate a
#'
#' @return a
#' @export a
#'
specify <- function(formula,
                    level = c("area", "unit"),
                    model = NULL,
                    engine = "brms",
                    auto_aggregate = FALSE) {

  func_call <- match.call()

  check_inherits("formula", formula)
  check_inherits("logical", auto_aggregate)

  if (is.null(level)) {
    if (model == "fh") {
      level <- "area"
    } else {
      level <- "unit"
    }
  }

  if (!(model %in% c("fh", "bhf"))) {
    stop("Invalid 'model'.")
  }

  if (engine != c("brms")) {
    stop("The engine is currently not available.")
  }

  # We want to extract the variables.
  Y <- toString(formula[[2]])
  rhs_str <- deparse(formula[[3]])

  # Check if there is a random intercept
  has_pipe <- grepl("\\|", rhs_str)

  if (has_pipe) {
    # If a pipe exists, split the string structure
    all_words <- unlist(str_extract_all_base(rhs_str, "\\w+|\\|"))

    # Locate the position
    one_idx <- which(all_words == "|")

    # Following '|' is domain level, like county
    domain_level <- all_words[one_idx + 1]

    # Before '|' are fixed effects
    # We also get rid of '1'.
    raw_fixed <- all_words[1:(one_idx - 1)]
    fixed_effects <- raw_fixed[raw_fixed != "1"]

  } else {
    # If no condition is provided, extract directly
    all_words <- unlist(str_extract_all_base(rhs_str, "\\w+"))

    # Accordingly the domain remains undefined at this stage
    domain_level <- NULL
    fixed_effects <- all_words
  }

  # Alignment check
  if (level == "unit" && auto_aggregate) {
    warning("'auto_aggregate' has been reset to FALSE. \n",
            "It is only for area-level models.")
    auto_aggregate <- FALSE
  }

  out <- list(
    call = func_call,
    formula = formula,
    level = level,
    model_type = model,
    engine = engine,
    response_var = Y,
    fixed_effects = fixed_effects,
    domain_level = domain_level,
    auto_aggregate = auto_aggregate
  )

  structure(out, class = "basal_spec")
}
