#' @title the marr class
#'
#' @description  Objects of this class store
#' needed information to work with a
#' marr object
#'
#' @slot marrSamplepairs marr sample pairs
#' @slot marrFeatures marr features
#' @slot marrSamplepairsfiltered marr sample pairs post filtering
#' @slot marrFeaturesfiltered marr metabolites post filtering
#'
#' @return \code{marrSamplepairs} returns the distribution of
#' percent reproducible  features (column-wise) per sample pair,
#' \code{marrFeatures} returns the  distribution of percent reproducible
#' sample pairs (row-wise) per feature,
#' \code{marrSamplepairsfiltered} returns the percent of reproducible
#' features based on a threshold value and
#' \code{marrFeaturesfiltered} returns the percent of reproducible
#' sample pairs based on a threshold value
#'
#' @name marr-class
#' @import methods
#' @exportClass marr
#' @aliases marr-class
#'
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_marr <- marr(object = data, pSamplepairs=0.75,
#' pFeatures=0.75, alpha=0.05)
#'
setClass(Class = "marr", slot = list(marrSamplepairs = "numeric",
            marrFeatures = "array", marrSamplepairsfiltered = "numeric",
            marrFeaturesfiltered = "numeric"))
#' @param marr
#'
#' @importFrom utils head
#' @importFrom utils tail
setMethod("show", "marr", function(object) {
            cat("marr: Maximum Rank reproducibility\n")
            cat("   marrSamplepairs (length =", length(object@marrSamplepairs),
                        "):", "\n")
            cat(c(head(round(object@marrSamplepairs, 3), n = 3),
                        "...", tail(round(object@marrSamplepairs, 3),
                                    n = 3)), "\n")
            cat("   marrFeatures (length =", length(object@marrFeatures),
                        "):", "\n")
            cat(c(head(round(object@marrFeatures, 3), n = 3),
                        "...", tail(round(object@marrFeatures, 3),
                                    n = 3)), "\n")
})
