#' @title marrProc
#'
#' @description This function is a helper function that
#' computes distributions of reproducible sample pairs per feature
#' and reproducible features per sample pair for the function
#' \code{marr}.
#'
#' @param object an object which is a \code{matrix} or
#' \code{data.frame} with features (e.g. metabolites or genes) on
#' the rows and samples as the columns. Alternatively,
#' a user can provide a \code{SummarizedExperiment} object
#' and the \code{assay(object)} will be used as input
#' for the marr procedure.
#' @param alpha (Optional) level of significance to control
#' the False Discovery Rate (FDR).
#' Default is 0.05.
#'
#' @return A list of percent reproducible statistics including
#' \item{samplepairs}{the distribution of
#' percent reproducible  features (column-wise) per sample pair}
#' \item{features}{the distribution of
#' percent reproducible  sample pairs (row-wise) per feature}
#'
#' @aliases marrProc
#'
#' @docType methods
#' @examples
#' data <- matrix(rnorm(2400), nrow=200, ncol=12)
#' data_marrProc <- marrProc(object=data, alpha = 0.05)
#'
#' @rdname marrProc
#' @export
marrProc <- function(object, alpha = 0.05) {
        datranks <- array(0, dim = c(dim(object)[2], dim(object)[1]))
        for (i in seq_len(dim(object)[2])) {
                datranks[i, ] <- rank(-(object[, c(i)]),
                ties.method = "average")
        }
        datMaxRankij <- array(0, dim = c(dim(object)[2],
                                            dim(object)[2], dim(object)[1]))
        rall <- k <- array(0, dim = c(dim(object)[2], dim(object)[2]))
        for (i in seq_len(dim(object)[2])) {
                for (j in i:dim(object)[2]) {
                        datMaxRankij[i, j, ] <- apply(cbind(datranks[i, ],
                        datranks[j,]), 1, max)
                }
        }
        countall = c(0)
        for (i in seq_len(dim(object)[2])) {
                for (j in i:dim(object)[2]) {
                        if (i < j) {
                                listMaRR <- .MaRR(datMaxRankij[i, j, ],
                                alpha = 0.05)
                                rall[i, j] <- length(listMaRR[[5]])
                                mall_rep_i <- listMaRR[[5]]
                                posall <- array(0, dim = c(dim(object)[1]))
                                posall[mall_rep_i] <- rep(1, length(mall_rep_i))
                                countall = countall + (posall == 1) * 1
                        }
                }
        }
        dim1 <- dim(object)[2]
        rall_new <- upper.tri(rall, diag = FALSE)
        rall2 <- rall[rall_new]
        samplepairs <- (rall2 * 100)/dim(object)[1]
        features <- (countall * 100)/(choose(dim1, 2))
        list(samplepairs = samplepairs, features = features)
}
