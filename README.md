# Development Mode

`basal` is still under development. Please us at your own risk!

# `basal`: *Ba*yesian *S*mall *A*rea Estimation *L*ibrary
This package is intended to provide tools for Bayesian modeling within the small area estimation context.

## Modeling Design and Example
The design of `basal` is to use a simple modeling framework for all models. 
Every model starts with a call to `specify()`, detailing various different parts of a model. For example, with the data provided here, 

```r
spec <- specify(biomass ~ ppt, domain = "evt", model = "BHF")
```

is a valid specification for a Battese-Harter-Fuller model. Likewise,

```r
spec <- specify(
  response_name = "biomass", 
  auxiliary_variables = "ppt",
  domain = "evt",
  model = "BHF"
)
```

specifies the same model, and as does

```r
spec <- specify(biomass ~ ppt + (1 | evt), level = "unit")
```

`basal` provides a very flexible modeling framework, and we are actively adding more model types and terms to add to these models. The bulk of the work for a `basal` model, however, is done in `specify()`, and the remaining three steps in a Bayesian analysis can be done quite briefly:

```r
basal_fit <- fit(spec, oregon_sample)
check(basal_fit) 
# inspect the diagnostic plots for agreement between blue and black lines

estimate(basal_fit, oregon_population, domain = "county")
```

This constitutes a Bayesian analysis, but the `check()`'s will likely not show significant fitness of a model. Other options for helping with fitness are, such as variable transformations, or two-staged models, are detailed in the "workflow" vignette.

# Installation
`basal` has not been released to the CRAN, you can download it from github with

```r
remotes::install_github("ufds-lab/basal")
```

Or, to build and install locally, run:

```r
git clone git@github.com:ufds-lab/basal.git
cd basal
R CMD intall .
```
