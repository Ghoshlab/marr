#' @title Marr
#'
#' @description This function applies an Rcpp-based implementation of a
#' computationally efficient method for assessing reproducibility in
#' high-throughput experiments, called the the Marr procedure. This
#' function also defines the Marr class and constructor.
#'
#' @param object an object which is a \code{matrix} or
#' \code{data.frame} with features (e.g. metabolites or genes) on
#' the rows and samples as the columns. Alternatively,
#' a user can provide a \code{SummarizedExperiment} object
#' and the \code{assay(object)} will be used as input
#' for the Marr procedure.
#' @param pSamplepairs (Optional) a threshold value that lies
#' between 0 and 1, used to assign a feature to be reproducible
#' based on the reproducibility output of the sample pairs per feature.
#' Default is 0.75.
#' @param pFeatures (Optional) a threshold value that lies between
#' 0 and 1, used to assign a sample pair to be reproducible based
#' on the reproducibility output of the features per sample pair.
#' Default is 0.75.
#' @param alpha (Optional) level of significance to control
#' the False Discovery Rate (FDR).
#' Default is 0.05.
#' @param featureVars (Optional) Vector of the columns which identify features.
#' If a `SummarizedExperiment` is used for `data`, row variables will be used.
#' @export
#'
#' @return A object of the class \code{Marr} that
#' contains a numeric vector of the Marr sample pairs in
#' the \code{MarrSamplepairs} slot, a numeric vector of the Marr
#' features in the \code{MarrFeatures} slot, a numeric value of
#' the Marr filtered features in the \code{MarrSamplepairsfiltered} slot,
#' and a numeric value of the Marr filtered sample pairs in the
#' \code{MarrFeaturesfiltered} slot.
#' @details
#' marr (Maximum Rank Reproducibility) is a nonparametric approach,
#' which assesses reproducibility in high-dimensional biological
#' replicate experiments. Although it
#' was originally developed for RNASeq data it can be
#' applied across many different high-dimensional biological data
#' including MassSpectrometry based Metabolomics and ChIPSeq. The Marr
#' procedure uses a maximum rank statistic to identify reproducible signals
#' from noise without making any distributional assumptions of reproducible
#' signals. This procedure can be easily applied to a variety of
#' measurement types since it employs a rank scale.
#'
#' This function computes the distributions of percent reproducible
#' sample pairs (row-wise) per feature and percent reproducible  features
#' (column-wise) per sample pair, respectively. Additionally,
#' it also computes the percent of reproducible
#' sample pairs and features based on a threshold value.
#
#' See the vignette for more details.
#'
#' @aliases Marr
#'
#' @docType methods
#' @name Marr
#' @importFrom SummarizedExperiment assays
#' @importFrom SummarizedExperiment assay
#' @importFrom SummarizedExperiment colData
#' @importFrom SummarizedExperiment rowData
#' @importFrom dplyr select
#' @importFrom utils combn
#' @importFrom magrittr %>%
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_Marr <- Marr(object = data, pSamplepairs=0.75,
#'                   pFeatures=0.75, alpha=0.05)
#' data("msprepCOPD")
#' data_Marr_COPD <- Marr(object = msprepCOPD, pSamplepairs=0.75,
#'                        pFeatures=0.75, alpha=0.05)
#'
#' @references
#' Philtron, D., Lyu, Y., Li, Q. and Ghosh, D., 2018.
#' Maximum Rank Reproducibility: A Nonparametric Approach to
#' Assessing Reproducibility in Replicate Experiments.
#' Journal of the American Statistical Association, 113(523),
#' pp.1028-1039.
#'
#' @rdname Marr
#' @export
Marr <- function(object, pSamplepairs = 0.75, pFeatures = 0.75,
            alpha = 0.05, featureVars = NULL) {
    
    originalObject <- object

    if (is.data.frame(object)) {
        if(!is.null(featureVars)) {
            featureColumns <- object %>%
                select(featureVars)
            object <- object %>%
                select(-featureVars) %>%
                as.matrix()
        } else {
            object <- as.matrix(object)
            featureColumns <- NULL
        }
        samplePairNames <- combn(colnames(object), 2)
    } else if (is(object, "SummarizedExperiment")) {
        featureColumns <- rowData(object)
        samplePairNames <- combn(rownames(colData(object)), 2)
        object <- assay(object)
    } else if (is(object, "matrix")) {
        if (!is.null(featureVars)) {
            featureColumns <- as.data.frame(object) %>%
                select(featureVars)
            object <- as.data.frame(object) %>%
                select(-featureVars) %>%
                as.matrix()
        } else {
            featureColumns <- NULL
        }
        originalObject <- as.data.frame(object)
        samplePairNames <- combn(colnames(as.data.frame(object)), 2)
    } else {
        stop("The class of the object must be a matrix,
                data.frame or SummarizedExperiment")
    }
    
    if (any(is.na(object))) {
        stop("Object must not contains NAs.")
    }

    Marrutils <- MarrProc(object, alpha)
    samplepairs <- Marrutils$samplepairs
    features <- Marrutils$features
    filSamplepairs <- (length(which(samplepairs > (pSamplepairs *
                100))) * 100)/choose(dim(object)[2], 2)
    filFeatures <- (length(which(features > (pFeatures *
                100))) * 100)/dim(object)[1]
    
    results <- new("Marr")
    results@MarrSamplepairs <- data.frame(sampleOne = 
                                              samplePairNames[1, ],
                                          sampleTwo = 
                                              samplePairNames[2, ],
                                          reproducibility = samplepairs)
    if(is.null(featureColumns)) {
        results@MarrFeatures <- data.frame(reproducibility = features)
    } else {
        results@MarrFeatures <- data.frame(featureColumns, 
                                           reproducibility = features)
    }
    results@MarrSamplepairsfiltered <- filSamplepairs
    results@MarrFeaturesfiltered <- filFeatures
    results@MarrData <- originalObject
    results@MarrPFeatures <- pFeatures
    results@MarrPSamplepairs <- pSamplepairs
    results@MarrAlpha <- alpha
    results@MarrFeatureVars <- featureVars
    
    return(results)
}