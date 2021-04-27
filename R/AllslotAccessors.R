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

#' @title Accessors for the 'MarrData' slot of a Marr object.
#'
#' @description Accessors for the 'MarrData'
#' slot of a Marr object.
#'
#' @usage
#' \S4method{MarrData}{Marr}(object)
#'
#' @docType methods
#' @name MarrData
#' @rdname MarrData
#' @aliases MarrData MarrData,Marr-method
#' @param object an object of class \code{Marr}.
#'
#' @return  Original data object passed to \code{Marr}
#'
#' @export
#'
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_Marr <- Marr(object = data, pSamplepairs=0.75,
#'                   pFeatures=0.75, alpha=0.05)
#' MarrData(data_Marr)
#'
setMethod(f = "MarrData", signature(object = "Marr"),
          function(object) {
              return(object@MarrData)
          })

#' @title Accessors for the 'MarrPSamplepairs' slot of a Marr object.
#'
#' @description Accessors for the 'MarrPSamplepairs'
#' slot of a Marr object.
#'
#' @usage
#' \S4method{MarrPSamplepairs}{Marr}(object)
#'
#' @docType methods
#' @name MarrPSamplepairs
#' @rdname MarrPSamplepairs
#' @aliases MarrPSamplepairs MarrPSamplepairs,Marr-method
#' @param object an object of class \code{Marr}.
#'
#' @return  Value of \code{pSamplepairs} argument passed 
#' to \code{Marr}
#'
#' @export
#'
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_Marr <- Marr(object = data, pSamplepairs=0.75,
#'                   pFeatures=0.75, alpha=0.05)
#' MarrPSamplepairs(data_Marr)
#'
setMethod(f = "MarrPSamplepairs", signature(object = "Marr"),
          function(object) {
              return(object@MarrPSamplepairs)
          })

#' @title Accessors for the 'MarrPFeatures' slot of a Marr object.
#'
#' @description Accessors for the 'MarrPFeatures'
#' slot of a Marr object.
#'
#' @usage
#' \S4method{MarrPFeatures}{Marr}(object)
#'
#' @docType methods
#' @name MarrPFeatures
#' @rdname MarrPFeatures
#' @aliases MarrPFeatures MarrPFeatures,Marr-method
#' @param object an object of class \code{Marr}.
#'
#' @return  Value of \code{MarrPFeatures} argument passed 
#' to \code{Marr}
#'
#' @export
#'
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_Marr <- Marr(object = data, pSamplepairs=0.75,
#'                   pFeatures=0.75, alpha=0.05)
#' MarrPFeatures(data_Marr)
#'
setMethod(f = "MarrPFeatures", signature(object = "Marr"),
          function(object) {
              return(object@MarrPFeatures)
          })

#' @title Accessors for the 'MarrAlpha' slot of a Marr object.
#'
#' @description Accessors for the 'MarrAlpha'
#' slot of a Marr object.
#'
#' @usage
#' \S4method{MarrAlpha}{Marr}(object)
#'
#' @docType methods
#' @name MarrAlpha
#' @rdname MarrAlpha
#' @aliases MarrAlpha MarrAlpha,Marr-method
#' @param object an object of class \code{Marr}.
#'
#' @return  Value of \code{alpha} argument passed to \code{Marr}
#'
#' @export
#'
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_Marr <- Marr(object = data, pSamplepairs=0.75,
#'                   pFeatures=0.75, alpha=0.05)
#' MarrAlpha(data_Marr)
#'
setMethod(f = "MarrAlpha", signature(object = "Marr"),
          function(object) {
              return(object@MarrAlpha)
          })

#' @title Accessors for the 'MarrFeatureVars' slot of a Marr object.
#'
#' @description Accessors for the 'MarrFeatureVars'
#' slot of a Marr object.
#'
#' @usage
#' \S4method{MarrFeatureVars}{Marr}(object)
#'
#' @docType methods
#' @name MarrFeatureVars
#' @rdname MarrFeatureVars
#' @aliases MarrFeatureVars MarrFeatureVars,Marr-method
#' @param object an object of class \code{Marr}.
#'
#' @return Value of \code{featureVars} passed to \code{Marr}. NULL
#' if \code{featureVars} was left blank
#'
#' @export
#'
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_Marr <- Marr(object = data, pSamplepairs=0.75,
#'                   pFeatures=0.75, alpha=0.05)
#' MarrFeatureVars(data_Marr)
#'
setMethod(f = "MarrFeatureVars", signature(object = "Marr"),
          function(object) {
              return(object@MarrFeatureVars)
          })