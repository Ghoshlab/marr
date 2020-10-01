#' Generic function that returns the marr sample pairs
#'
#' Given a marr object, this function returns the
#' marr sample pairs
#' @rdname marrSamplepairs
setGeneric(name = "marrSamplepairs", def = function(object) {
            standardGeneric("marrSamplepairs")
})
#' Generic function that returns the marr features
#'
#' Given a marr object, this function returns the
#' marr features
#' @rdname marrFeatures
setGeneric(name = "marrFeatures", def = function(object) {
            standardGeneric("marrFeatures")
})
#' Generic function that returns the marr filtered sample pairs
#'
#' Given a marr object, this function returns the
#' marr filtered sample pairs
#' @rdname marrSamplepairsfiltered
setGeneric(name = "marrSamplepairsfiltered", def = function(object) {
            standardGeneric("marrSamplepairsfiltered")
})
#' Generic function that returns the marr filtered features
#'
#' Given a marr object, this function returns the
#' marr filtered features
#' @rdname marrFeaturesfiltered
setGeneric(name = "marrFeaturesfiltered", def = function(object) {
            standardGeneric("marrFeaturesfiltered")
})
