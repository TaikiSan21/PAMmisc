#' @title Calculate the Wigner-Ville Transform of a Signal
#'
#' @description Calculates the Wigner-Ville transform a signal. By default, the
#'   signal will be zero-padded to the next power of two before computing the
#'   transform, and creates an NxN matrix where N is the zero-padded length.
#'   Note that this matrix can get very large for larger N, consider shortening
#'   longer signals.
#'
#' @details This code mostly follows Pamguard's Java code for computing the
#'   Wigner-Ville and Hilbert transforms.
#'
#' @param signal input signal waveform
#' @param n the size of the output, if missing will be the next power of two
#'   from the length of the input signal (recommended)
#'   ## NO ITS NOT ITS THE LENGTH OF THE SIGNAL AND CHANGING N DOESNT CHAGNE T
#'   ## TIME LENGTH ONLY FREQUENCY LENGTH
#' @param sr the sample rate of the data
#' @param plot logical flag whether or not to plot the result
#'
#' @return a list with three items. \code{tfr}, the real values of the wigner
#'   transform as an nxn matrix. \code{f} and \code{t} the values of the frequency
#'   and time axes.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom stats fft
#' @importFrom graphics axis image
#' @export
#'
wignerTransform <- function(signal, n, sr, plot=FALSE) {
    analytic <- toAnalytic(signal)[1:length(signal)] # size changed during toAnalytic function
    conjAnalytic <- Conj(analytic)
    if(missing(n)) {
        n <- length(analytic)
    }

    nRow <- n # nFreq bins
    nCol <- length(analytic) # nTimesteps

    tfr <- matrix(0, nRow, nCol)

    for(iCol in 1:nCol) {
        taumax <- min(iCol-1, nCol-iCol, round(nRow/2)-1)
        tau <- -taumax:taumax
        indices <- (nRow + tau) %% nRow + 1
        # * .5 in PG?
        tfr[indices, iCol] <- analytic[iCol+tau] * conjAnalytic[iCol-tau]

        tau <- round(nRow/2)
        if(iCol + tau <= nCol &&
           iCol - tau >= 1) {
            # PG is like this, wv.wge is just the same fucking thing???
            tfr[tau+1, iCol] <- (analytic[iCol+tau] * conjAnalytic[iCol-tau] +
                                     analytic[iCol-tau] * conjAnalytic[iCol+tau])/2
        }
    }
    tfr <- apply(tfr, 2, fft)
    result <- list(tfr=Re(tfr), t=1:nCol/sr, f=sr/2*1:nRow/nRow)
    if(plot) {
        image(t(result$tfr), xaxt='n', yaxt='n', ylab='Frequency (kHz)', xlab = 'Time (ms)')
        axis(1, at = 1:4/4, labels = round(1e3*max(result$t)*1:4/4, 3))
        axis(2, at = 1:4/4, labels = round(max(result$f)*1:4/4/1e3, 1))
    }
    result
}

toAnalytic <- function(signal) {
    # Get analytic signal using Hilbert transform
    len <- length(signal)
    # next power of 2 to zero pad
    newLen <- nextExp2(len)
    newSignal <- c(signal, rep(0, newLen-len))
    hMult <- get1221(newLen)
    fft(fft(newSignal) * hMult, inverse = TRUE) # possibly scale by len???? only done in real version in PG
}

nextExp2 <- function(x) {
    # Take either the length as an int, or length of thingy
    if(length(x) != 1) x <- length(x)
    logTwo <- log2(x)
    ceil <- ceiling(logTwo)
    # bitwShiftL(1, ceil)
    2 ^ ceil
}

get1221 <- function(x) {
    c(1,
      rep(2, x/2 - 1),
      1,
      rep(0, x/2 - 1))
}
