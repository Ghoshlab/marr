#' @title Plot percent reproducible features per sample pair
#' for pairwise replicates from \code{Marr} function.
#'
#' @description This function plots a histogram
#' showing the sample pairs along the y-axis
#' and percent reproducible features per sample pair on
#' the x-axis.
#'
#' @param object a Marr object from \code{Marr}
#' @param xLab label for x-axis.
#' Default is 'Percent reproducible features
#'  per sample pair for pairwise replicates'.
#' @param yLab label for y-axis. Default is 'Sample pair'
#' @return A histogram will be created showing the
#' sample pairs along the y-axis and percent reproducible features
#' per sample pair on the x-axis.
#'
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 xlab ylab
#' @export
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_Marr <- Marr(object = data, pSamplepairs=0.75,
#'                   pFeatures=0.75, alpha=0.05)
#' MarrPlotSamplepairs(data_Marr)
#'
MarrPlotSamplepairs <- function(  object,
        xLab = "Percent reproducible features per sample pair",
        yLab = "Sample pair") {
        w = object@MarrSamplepairs
        df11 = data.frame(w)
        ggplot(df11, aes(x = w))+
        geom_histogram(binwidth=2,color = "black",
        fill = "white") + ylab(yLab) + xlab(xLab)
}
#' @title Plot percent reproducible sample pairs per feature
#' for pairwise replicates from \code{Marr} function.
#'
#' @description This function plots a histogram
#' showing the features along the y-axis
#' and percent reproducible sample pairs per feature on
#' the x-axis.
#'
#' @param object a Marr object from \code{Marr}
#' @param xLab label for x-axis.
#' Default is 'Percent reproducible sample pairs
#' per feature for pairwise replicates'.
#' @param yLab label for y-axis. Default is 'Feature'
#' @return A histogram will be created showing the
#' features along the y-axis and percent reproducible sample pairs
#' per feature on the x-axis.
#'
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 xlab ylab
#' @export
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_Marr <- Marr(object = data, pSamplepairs=0.75,
#'                   pFeatures=0.75, alpha=0.05)
#' MarrPlotFeatures(data_Marr)
#'
MarrPlotFeatures <- function(  object,
        xLab = "Percent reproducible sample pairs per feature",
        yLab = "Feature") {
        ww = object@MarrFeatures
        df12 = data.frame(ww)
        ggplot(df12, aes(x = ww))+
        geom_histogram(binwidth=2,color = "black",
        fill = "white") + ylab(yLab) + xlab(xLab)
}
