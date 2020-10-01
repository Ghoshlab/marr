#' @title marr
#'
#' @description This function applies an Rcpp-based implementation of a
#' computationally efficient method for assessing reproducibility in
#' high-throughput experiments, called the the marr procedure. This
#' function also defines the marr class and constructor.
#'
#' @param object an object which is a \code{matrix} or
#' \code{data.frame} with features (e.g. metabolites or genes) on
#' the rows and samples as the columns. Alternatively,
#' a user can provide a \code{SummarizedExperiment} object
#' and the \code{assay(object)} will be used as input
#' for the marr procedure.
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
#' @export
#'
#' @return A object of the class \code{marr} that
#' contains a numeric vector of the marr sample pairs in
#' the \code{marrSamplepairs} slot, a numeric vector of the marr
#' features in the \code{marrFeatures} slot, a numeric value of
#' the marr filtered features in the \code{marrSamplepairsfiltered} slot,
#' and a numeric value of the marr filtered sample pairs in the
#' \code{marrFeaturesfiltered} slot.
#' @details
#' marr (Maximum Rank Reproducibility) is a nonparametric approach,
#' which assesses reproducibility in high-dimensional biological
#' replicate experiments. Although it
#' was originally developed for RNASeq data it can be
#' applied across many different high-dimensional biological data
#' including MassSpectrometry based Metabolomics and ChIPSeq. The marr
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
#' @aliases marr
#'
#' @docType methods
#' @name marr
#' @importFrom SummarizedExperiment assays
#' @importFrom SummarizedExperiment assay
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_marr <- marr(object = data, pSamplepairs=0.75,
#' pFeatures=0.75, alpha=0.05)
#' data("msprepCOPD")
#' data_marr_COPD <- marr(object = msprepCOPD, pSamplepairs=0.75,
#' pFeatures=0.75, alpha=0.05)
#'
#' @references
#' Philtron, D., Lyu, Y., Li, Q. and Ghosh, D., 2018.
#' Maximum Rank Reproducibility: A Nonparametric Approach to
#' Assessing Reproducibility in Replicate Experiments.
#' Journal of the American Statistical Association, 113(523),
#' pp.1028-1039.
#'
#' @rdname marr
#' @export
marr <- function(object, pSamplepairs = 0.75, pFeatures = 0.75,
            alpha = 0.05) {
            if (!any(is(object, "matrix") | is(object, "data.frame") |
                        is(object, "SummarizedExperiment"))) {
                        stop("The class of the object must be a matrix,
                        data.frame or SummarizedExperiment")
            }
            if (is.data.frame(object)) {
                        object <- as.matrix(object)
            }
            if (is(object, "SummarizedExperiment")) {
                        object <- assay(object)
            }
            if (any(is.na(object))) {
                        stop("Object must not contains NAs.")
            }
            marrutils <- marrProc(object, alpha = 0.05)
            samplepairs <- marrutils$samplepairs
            features <- marrutils$features
            filSamplepairs <- (length(which(samplepairs > (pSamplepairs *
                        100))) * 100)/choose(dim(object)[2], 2)
            filFeatures <- (length(which(features > (pFeatures *
                        100))) * 100)/dim(object)[1]
            results <- new("marr")
            results@marrSamplepairs <- samplepairs
            results@marrFeatures <- features
            results@marrSamplepairsfiltered <- filSamplepairs
            results@marrFeaturesfiltered <- filFeatures
            return(results)
}
