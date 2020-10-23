#' @useDynLib Marr
#' @importFrom Rcpp evalCpp
#' @exportPattern '^[[:alpha:]]+'
NULL

# .estFDR: an internal function to produce a vector of estimated
# false discovery rats based on a vector of maximum rank statistics
# and a khat.
# INPUTS: maxRank = a vector of maximum rank statistics
#         khat = a value of khat calculated from the argmin of
# .mySS (Rcpp internal function in MarrUtility.cpp)
# OUTPUTS: a vector of estimated FDR values for each potential
# threshold Nhat=1,...,n as described in Philtron et al., 2018

.estFDR = function(khat, maxRank) {
            n = length(maxRank)
            Nhat = (khat + 1):n
            Q.khat = sum(maxRank <= khat)
            RNhat <- .RNhatcpp(khat, maxRank)
            indicateR = as.numeric(RNhat > 0)
            RNhat[indicateR == 0] = 1
            temp = Nhat - khat
            numer = temp * temp
            denom = (n - khat) * RNhat
            FDRNhatkhat = (numer/denom) * indicateR
            return(c(rep(0, khat), FDRNhatkhat))
}

# .MaRR: an internal function that performs the Marr procedure based
#        on a vector of maximum rank statistics.
# INPUTS: maxRank = a vector of maximum rank statistics
#         cutoff = a value between 0 and 1 that provides
#                  the maximum allowed value for pi-hat.
#         alpha = desired level of FDR control
#         khat.to.zero = TRUE/FALSE, whether or not to set k-hat to zero
# for mFDR (marginal FDR) calculation (recommended for very small pi1)
# OUTPUTS: khat = n*pi-hat, discrete estimate of where irreproducible
#                 signals begin
#          Nhat = estimated cut-off for maximum ranks that will control
#                 FDR at level alpha
#          estFdr = the estimated fdr value for each of potential N-hat
#          SS = vector of values for SS loss function evaluated
#               at i/n =0, 1/n, 2/n,..., 1
#          whichSig = vector of indices for maximum ranks declared to
#                     be reproducible
.MaRR = function(maxRank, cutoff = 0.9, alpha = 0.05,
            khat.to.zero = FALSE) {
            maxx = floor(cutoff * length(maxRank))
            mySS = .getSS(maxRank)
            khat = which(mySS[seq_len(maxx)] == min(mySS[seq_len(maxx)])) -
                        1
            if (khat.to.zero == TRUE) {
                        khat = 0
            }
            tempFdr = .estFDR(khat, maxRank)
            Nhat = max(which(tempFdr <= alpha))
            whichSig = which(maxRank <= Nhat)
            return(list(Nhat = Nhat, khat = khat, estFdr = tempFdr,
                        SS = mySS, whichSig = whichSig))
}
