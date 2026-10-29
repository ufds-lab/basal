#' @rdname oregon_trees
"oregon_sample"

#' @rdname oregon_trees
"oregon_population"

#' @title 
#' Oregon forests survey and population data
#' @description
#' Public data from the US Forest Service's surveys in Oregon state over 10 years.
#' 
#' \code{oregon_sample} represents the results from surveys while 
#' \code{oregon_population} is a (random) subset of 200,000 of the (2,000,000?)
#' possible plots in oregon with remotely sensed attributes across.
#' 
#' @format
#' The columns in \code{oregon_sample} are:
#' \describe{
#'  \item{biomass}{[numeric], dry biomass of trees within an observed region (tons).}
#'  \item{county}{[factor], with levels for counties in oregon}
#'  \item{evt}{[factor], with levels for the \strong{E}xisting \strong{V}egegation \strong{T}ype in the observed region.}
#'  \item{evc}{[numeric], \strong{E}xisting \strong{V}egetation \strong{C}over; proportion of a region covered by vegetation (units?).}
#'  \item{elev}{[numeric], elevation of an observed region (units?).}
#'  \item{ppt}{[numeric], \strong{P}rism-\strong{P}recipi\strong{T}ation within an observed region (units?).}
#' }
#' All variables aside from \code{biomass} and \code{county} are remotely sensed, 
#' and all except for \code{biomass} are in \code{oregon_population}.
#' 
#' 
#' @name oregon_trees
NULL