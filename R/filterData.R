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
#' @return A list of \code{data.frame}'s or a \code{SummarizedExperiment}. 
#' If a \code{data.frame} was originally input into the \code{Marr} function,
#'  a list with three elements, \code{filteredData}, 
#' \code{removedSamples}, and \code{removedFeatures}, will be returned. If a
#' \code{SummarizedExperiment} was originally input, output will be a 
#' \code{SummarizedExperiment} with the assay filtered and with two metadata
#' objects, \code{removedSamples} and \code{removedFeatures} added.
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
        featuresToKeep <- object@MarrFeatures$reproducibility > pFeatures*100
    } else {
        featuresToKeep <- rep(TRUE, times = nrow(object@MarrData))
    }
    
    originalSamples <- object@MarrData %>%
        colnames()
    featureVars <- object@MarrFeatureVars
    
    if (by == "both" | by == "samplePairs") {
        pSamplePairs <- object@MarrPSamplepairs
        
        samplePairs <- object@MarrSamplepairs %>%
            filter(.data$reproducibility > pSamplePairs*100) %>%
            select(!.data$reproducibility)
        
        samplesToKeep <- c(originalSamples %in% samplePairs$sampleOne |
                               originalSamples %in% samplePairs$sampleTwo |
                               originalSamples %in% featureVars)
        
    } else {
        samplesToKeep <- rep(TRUE, times = ncol(object@MarrData))
    }
    
    filteredData <- object@MarrData[featuresToKeep, samplesToKeep]
    removedSamples <- object@MarrData[, (!samplesToKeep | 
                                             originalSamples %in% featureVars)]
    removedFeatures <- object@MarrData[!featuresToKeep, ]
    
    if(ncol(filteredData) == length(featureVars) | nrow(filteredData) == 0) { 
        filteredData <- NULL 
    }
    if(ncol(removedSamples) == length(featureVars)) { removedSamples <- NULL }
    if(nrow(removedFeatures) == 0) { removedFeatures <- NULL }
    
    return(list("filteredData" = filteredData,
                "removedSamples" = removedSamples,
                "removedFeatures" = removedFeatures))
}

#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom S4Vectors metadata
#' @importFrom S4Vectors metadata<-
.seFilter <- function(object, by) {
    if (by == "both" | by == "features") {
        pFeatures <- object@MarrPFeatures
        featuresToKeep <- object@MarrFeatures$reproducibility > pFeatures*100
    } else {
        featuresToKeep <- rep(TRUE, times = nrow(object@MarrData))
    }
    
    if (by == "both" | by == "samplePairs") {
        pSamplePairs <- object@MarrPSamplepairs
        featureVars <- object@MarrFeatureVars
        
        originalSamples <- object@MarrData %>%
            colnames()
        
        samplePairs <- object@MarrSamplepairs %>%
            filter(.data$reproducibility > pSamplePairs*100) %>%
            select(!.data$reproducibility)
        
        samplesToKeep <- c(originalSamples %in% samplePairs$sampleOne |
                               originalSamples %in% samplePairs$sampleTwo |
                               originalSamples %in% featureVars)
        
    } else {
        samplesToKeep <- rep(TRUE, times = ncol(object@MarrData))
    }
    
    filteredData <- object@MarrData[featuresToKeep, samplesToKeep]
    removedSamples <- object@MarrData[, !samplesToKeep]
    removedFeatures <- object@MarrData[!featuresToKeep, ]
    
    if(ncol(filteredData) == 0 | nrow(filteredData) == 0) { 
        filteredData <- NULL 
    }
    if(ncol(removedSamples) == 0) { removedSamples <- NULL }
    if(nrow(removedFeatures) == 0) { removedFeatures <- NULL }
    
    return <- object@MarrData[featuresToKeep, samplesToKeep]
    metadata(return)$removedSamples <- removedSamples
    metadata(return)$removedFeatures <- removedFeatures
        
    return
}