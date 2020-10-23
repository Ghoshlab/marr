#' @title Accessors for the 'MarrSamplepairs' slot of a Marr object.
#'
#' @description Accessors for the 'MarrSamplepairs' slot of
#' a Marr object.
#'
#' @usage
#' \S4method{MarrSamplepairs}{Marr}(object)
#'
#' @docType methods
#' @name MarrSamplepairs
#' @rdname MarrSamplepairs
#' @aliases MarrSamplepairs MarrSamplepairs,Marr-method
#' @param object an object of class \code{Marr}.
#'
#' @return The distribution of percent reproducible features
#' (column-wise) per sample pair after applying the maximum
#' rank reproducibility.
#'
#' @export
#'
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_Marr <- Marr(object = data, pSamplepairs=0.75,
#'                   pFeatures=0.75, alpha=0.05)
#' MarrSamplepairs(data_Marr)
#'
setMethod(f = "MarrSamplepairs", signature(object = "Marr"),
            function(object) {
                        return(object@MarrSamplepairs)
            })
#' @title Accessors for the 'MarrFeatures' slot of a Marr object.
#'
#' @description Accessors for the 'MarrFeatures' slot
#' of a Marr object.
#'
#' @usage
#' \S4method{MarrFeatures}{Marr}(object)
#'
#' @docType methods
#' @name MarrFeatures
#' @rdname MarrFeatures
#' @aliases MarrFeatures MarrFeatures,Marr-method
#' @param object an object of class \code{Marr}.
#'
#' @return The distribution of percent reproducible sample pairs
#' (row-wise) per feature after applying the maximum rank
#' reproducibility.
#'
#' @export
#'
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_Marr <- Marr(object = data, pSamplepairs=0.75,
#'                   pFeatures=0.75, alpha=0.05)
#' MarrFeatures(data_Marr)
#'
setMethod(f = "MarrFeatures", signature(object = "Marr"),
            function(object) {
                        return(object@MarrFeatures)
            })
#' @title Accessors for the 'MarrSamplepairsfiltered' slot of a Marr object.
#'
#' @description Accessors for the 'MarrSamplepairsfiltered'
#' slot of a Marr object.
#'
#' @usage
#' \S4method{MarrSamplepairsfiltered}{Marr}(object)
#'
#' @docType methods
#' @name MarrSamplepairsfiltered
#' @rdname MarrSamplepairsfiltered
#' @aliases MarrSamplepairsfiltered MarrSamplepairsfiltered,Marr-method
#' @param object an object of class \code{Marr}.
#'
#' @return The percent of reproducible features based on a
#' threshold value after applying maximum rank reproducibility.
#'
#' @export
#'
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_Marr <- Marr(object = data, pSamplepairs=0.75,
#'                   pFeatures=0.75, alpha=0.05)
#' MarrSamplepairsfiltered(data_Marr)
#'
setMethod(f = "MarrSamplepairsfiltered", signature(object = "Marr"),
            function(object) {
                        return(object@MarrSamplepairsfiltered)
            })
#' @title Accessors for the 'MarrFeaturesfiltered' slot of a Marr object.
#'
#' @description Accessors for the 'MarrFeaturesfiltered'
#' slot of a Marr object.
#'
#' @usage
#' \S4method{MarrFeaturesfiltered}{Marr}(object)
#'
#' @docType methods
#' @name MarrFeaturesfiltered
#' @rdname MarrFeaturesfiltered
#' @aliases MarrFeaturesfiltered MarrFeaturesfiltered,Marr-method
#' @param object an object of class \code{Marr}.
#'
#' @return  The percent of reproducible sample pairs based on a
#' threshold value after applying maximum rank reproducibility.
#'
#' @export
#'
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_Marr <- Marr(object = data, pSamplepairs=0.75,
#'                   pFeatures=0.75, alpha=0.05)
#' MarrFeaturesfiltered(data_Marr)
#'
setMethod(f = "MarrFeaturesfiltered", signature(object = "Marr"),
            function(object) {
                        return(object@MarrFeaturesfiltered)
            })
