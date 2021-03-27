#' @title Filter by Maximum Rank Reproducibility
#'
#' @description Filters \code{Marr} object according to the Maximum Rank 
#' Reproducibility of the features, samples pairs, or both. Features are removed
#' if their reproducibility per sample pair is less than \code{pFeatures}.
#' Samples are removed if their sample pair reproducibility per feature is less
#' than \code{pSamplepairs} for all pairings of that sample and the other
#' samples in the set.
#'
#' @param object a Marr object from \code{Marr}
#' @param by String specifying which reproducibility values to filter by. 
#' Options include "features" to filter features according to their 
#' reproducibility, "samplePairs" to filter samples according to the 
#' reproducibility of sample pairs, or "both" to filter both features and sample
#'  pairs according to their respective reproducibility. Default is "both".
#' 
#' @return A \code{data.frame} or \code{SummarizedExperiment}, the same as the
#' data structure initially input into the \code{Marr} function
#' 
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_Marr <- Marr(object = data, pSamplepairs=0.75,
#'                   pFeatures=0.75, alpha=0.05)
#' MarrFilterData(data_Marr, by = "both")
#'
#' @export
MarrFilterData <- function(object, by = c("both", "features", "samplePairs")) {
    by <- match.arg(by)
    
    if (!is(object, "Marr")) {
        stop("object must be class Marr")
    }
    
    if (is.data.frame(object@MarrData)) {
        return <- .dfFilter(object, by)
    }
    
    if (is(object@MarrData, "SummarizedExperiment")) {
        return <- .seFilter(object, by)
    }
    
    return
}

#' @importFrom magrittr %>%
#' @importFrom rlang .data
.dfFilter <- function(object, by) {
    if (by == "both" | by == "features") {
        pFeatures <- object@MarrPFeatures
        featuresToKeep <- 
            which(object@MarrFeatures$reproducibility > pFeatures*100)
    } else {
        featuresToKeep <- seq(to = nrow(object@MarrData))
    }
    
    if (by == "both" | by == "samplePairs") {
        pSamplePairs <- object@MarrPSamplepairs
        featureVars <- object@MarrFeatureVars
        
        originalSamples <- object@MarrData %>%
            colnames()
        
        samplePairs <- object@MarrSamplepairs %>%
            filter(.data$reproducibility > pSamplePairs*100) %>%
            select(!.data$reproducibility)
        
        if (is.null(featureVars)) {
            featureVarIndex <- NULL
        } else {
            featureVarIndex <- which(originalSamples %in% featureVars)
        }
        
        samplesToKeep <- 
            c(featureVarIndex,
              which(originalSamples %in% samplePairs$sampleOne |
                        originalSamples %in% samplePairs$sampleTwo))
    } else {
        samplesToKeep <- seq(to = ncol(object@MarrData))
    }
    
    return <- object@MarrData[featuresToKeep, samplesToKeep]
}

#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom rlang .data
.seFilter <- function(object, by) {
    if (by == "both" | by == "features") {
        pFeatures <- object@MarrPFeatures
        featuresToKeep <- 
            which(object@MarrFeatures$reproducibility > pFeatures*100)
    } else {
        featuresToKeep <- seq(to = nrow(object@MarrData))
    }
    
    if (by == "both" | by == "samplePairs") {
        pSamplePairs <- object@MarrPSamplepairs
        originalSamples <- object@MarrData %>%
            colnames()
        
        samplePairs <- object@MarrSamplepairs %>%
            filter(.data$reproducibility > pSamplePairs*100) %>%
            select(!.data$reproducibility)
        
        samplesToKeep <- which(originalSamples %in% samplePairs$sampleOne |
                                   originalSamples %in% samplePairs$sampleTwo)
    } else {
        samplesToKeep <- seq(to = ncol(object@MarrData))
    }
    
    return <- object@MarrData[featuresToKeep, samplesToKeep]
}