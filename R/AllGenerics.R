#' Generic function that returns the Marr sample pairs
#'
#' Given a Marr object, this function returns the
#' Marr sample pairs
#' @rdname MarrSamplepairs
setGeneric(name = "MarrSamplepairs", def = function(object) {
            standardGeneric("MarrSamplepairs")
})
#' Generic function that returns the Marr features
#'
#' Given a Marr object, this function returns the
#' Marr features
#' @rdname MarrFeatures
setGeneric(name = "MarrFeatures", def = function(object) {
            standardGeneric("MarrFeatures")
})
#' Generic function that returns the Marr filtered sample pairs
#'
#' Given a Marr object, this function returns the
#' Marr filtered sample pairs
#' @rdname MarrSamplepairsfiltered
setGeneric(name = "MarrSamplepairsfiltered", def = function(object) {
            standardGeneric("MarrSamplepairsfiltered")
})
#' Generic function that returns the Marr filtered features
#'
#' Given a Marr object, this function returns the
#' Marr filtered features
#' @rdname MarrFeaturesfiltered
setGeneric(name = "MarrFeaturesfiltered", def = function(object) {
            standardGeneric("MarrFeaturesfiltered")
})
