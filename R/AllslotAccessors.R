#' @title Accessors for the 'marrSamplepairs' slot of a marr object.
#'
#' @description Accessors for the 'marrSamplepairs' slot of
#' a marr object.
#'
#' @usage
#' \S4method{marrSamplepairs}{marr}(object)
#'
#' @docType methods
#' @name marrSamplepairs
#' @rdname marrSamplepairs
#' @aliases marrSamplepairs marrSamplepairs,marr-method
#' @param object an object of class \code{marr}.
#'
#' @return The distribution of percent reproducible features
#' (column-wise) per sample pair after applying the maximum
#' rank reproducibility.
#'
#' @export
#'
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_marr <- marr(object = data, pSamplepairs=0.75,
#' pFeatures=0.75, alpha=0.05)
#' marrSamplepairs(data_marr)
#'
setMethod(f = "marrSamplepairs", signature(object = "marr"),
            function(object) {
                        return(object@marrSamplepairs)
            })
#' @title Accessors for the 'marrFeatures' slot of a marr object.
#'
#' @description Accessors for the 'marrFeatures' slot
#' of a marr object.
#'
#' @usage
#' \S4method{marrFeatures}{marr}(object)
#'
#' @docType methods
#' @name marrFeatures
#' @rdname marrFeatures
#' @aliases marrFeatures marrFeatures,marr-method
#' @param object an object of class \code{marr}.
#'
#' @return The distribution of percent reproducible features
#' (row-wise) per feature after applying the maximum rank
#' reproducibility.
#'
#' @export
#'
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_marr <- marr(object = data, pSamplepairs=0.75,
#' pFeatures=0.75, alpha=0.05)
#' marrFeatures(data_marr)
#'
setMethod(f = "marrFeatures", signature(object = "marr"),
            function(object) {
                        return(object@marrFeatures)
            })
#' @title Accessors for the 'marrSamplepairsfiltered' slot of a marr object.
#'
#' @description Accessors for the 'marrSamplepairsfiltered'
#' slot of a marr object.
#'
#' @usage
#' \S4method{marrSamplepairsfiltered}{marr}(object)
#'
#' @docType methods
#' @name marrSamplepairsfiltered
#' @rdname marrSamplepairsfiltered
#' @aliases marrSamplepairsfiltered marrSamplepairsfiltered,marr-method
#' @param object an object of class \code{marr}.
#'
#' @return The percent of reproducible features based on a
#' threshold value after applying maximum rank reproducibility.
#'
#' @export
#'
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_marr <- marr(object = data, pSamplepairs=0.75,
#' pFeatures=0.75, alpha=0.05)
#' marrSamplepairsfiltered(data_marr)
#'
setMethod(f = "marrSamplepairsfiltered", signature(object = "marr"),
            function(object) {
                        return(object@marrSamplepairsfiltered)
            })
#' @title Accessors for the 'marrFeaturesfiltered' slot of a marr object.
#'
#' @description Accessors for the 'marrFeaturesfiltered'
#' slot of a marr object.
#'
#' @usage
#' \S4method{marrFeaturesfiltered}{marr}(object)
#'
#' @docType methods
#' @name marrFeaturesfiltered
#' @rdname marrFeaturesfiltered
#' @aliases marrFeaturesfiltered marrFeaturesfiltered,marr-method
#' @param object an object of class \code{marr}.
#'
#' @return  The percent of reproducible sample pairs based on a
#' threshold value after applying maximum rank reproducibility.
#'
#' @export
#'
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_marr <- marr(object = data, pSamplepairs=0.75,
#' pFeatures=0.75, alpha=0.05)
#' marrFeaturesfiltered(data_marr)
#'
setMethod(f = "marrFeaturesfiltered", signature(object = "marr"),
            function(object) {
                        return(object@marrFeaturesfiltered)
            })
