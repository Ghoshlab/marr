#' @title S4 Class union
#' @description Class union allowing \code{MarrData} slot to be a data.frame or 
#' Summarized Experiment
#' @name data.frameORSummarizedExperiment
#' @importFrom methods setClassUnion
setClassUnion("data.frameORSummarizedExperiment",
              c("data.frame", "SummarizedExperiment"))

#' @title S4 Class union
#' @description Class union allowing \code{MarrFeatureVars} slot to be a vector or 
#' NULL
#' @name vectorORNull
#' @importFrom methods setClassUnion
setClassUnion("vectorORNull",
              c("vector", "NULL"))

#' @title the Marr class
#'
#' @description  Objects of this class store
#' needed information to work with a
#' Marr object
#'
#' @slot MarrSamplepairs Marr sample pairs
#' @slot MarrFeatures Marr features
#' @slot MarrSamplepairsfiltered Marr sample pairs post filtering
#' @slot MarrFeaturesfiltered Marr metabolites post filtering
#' @slot MarrData Original data object passed to \code{Marr}
#' @slot MarrPSamplepairs Value of \code{pSamplepairs} argument passed 
#' to \code{Marr}
#' @slot MarrPFeatures Value of \code{pFeatures} argument passed to \code{Marr}
#' @slot MarrAlpha Value of \code{alpha} argument passed to \code{Marr}
#' @slot MarrFeatureVars Value of \code{featureVars} passed to \code{Marr}. NULL
#' if \code{featureVars} was left blank
#'
#' @return \code{MarrSamplepairs} returns the distribution of
#' percent reproducible  features (column-wise) per sample pair,
#' \code{MarrFeatures} returns the  distribution of percent reproducible
#' sample pairs (row-wise) per feature,
#' \code{MarrSamplepairsfiltered} returns the percent of reproducible
#' features based on a threshold value and
#' \code{MarrFeaturesfiltered} returns the percent of reproducible
#' sample pairs based on a threshold value
#'
#' @name Marr-class
#' @import methods
#' @importFrom dplyr mutate_if
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#' @exportClass Marr
#' @aliases Marr-class
#'
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_Marr <- Marr(object = data, pSamplepairs=0.75,
#'                   pFeatures=0.75, alpha=0.05)
#'
setClass(Class = "Marr", 
         slot = list(MarrSamplepairs = "data.frame",
                     MarrFeatures = "data.frame",
                     MarrSamplepairsfiltered = "numeric",
                     MarrFeaturesfiltered = "numeric",
                     MarrData = "data.frameORSummarizedExperiment",
                     MarrPSamplepairs = "numeric",
                     MarrPFeatures = "numeric",
                     MarrAlpha = "numeric",
                     MarrFeatureVars = "vectorORNull"))

#' @importFrom utils head
#' @importFrom utils tail
setMethod("show", "Marr", function(object) {
    
    samplePairs <- MarrSamplepairs(object) %>%
        mutate_if(is.numeric, round, digits = 3)
    features <- MarrFeatures(object) %>%
        mutate_if(is.numeric, round, digits = 3)
    
    cat("Marr: Maximum Rank Reproducibility\n")
    cat(c("MarrSamplepairs (length =", nrow(samplePairs), "):", "\n"))
    print.data.frame(head(samplePairs, n = 3))
    cat("...\n")
    cat(c("MarrFeatures (length =", nrow(features), "):", "\n"))
    print.data.frame(head(features, n = 3))
    cat("...\n")
})
