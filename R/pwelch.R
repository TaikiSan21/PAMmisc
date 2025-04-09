#' @title Estimate Power Spectral Density Using Welch's Method
#'
#' @description Estimates the power spectral density (PSD) of an
#'   input signal using Welch's method. This should function
#'   similarly to the Matlab function pwelch, but results may
#'   not be identical. Breaks the input signal into (usually)
#'   overlapping frames and averages the resulting PSD estimates
#'
#' @param x input signal, either a numeric vector, \link[tuneR]{Wave},
#'   \link[tuneR]{WaveMC}, or \code{audioSample} object. Can also be
#'   a path to a wav file and it will be read in
#' @param nfft length of FFT window to use for individual frames
#' @param noverlap number of samples each frame should overlap
#' @param sr sample rate of data, only necessary if \code{x} is a vector
#' @param window window to apply, must be a vector of length \code{nfft}.
#'   If \code{NULL} (default), then a \link[signal]{hamming} window will
#'    be used
#' @param demean method of demeaning the signal, one of \code{'long'},
#'   \code{'short'}, or \code{'none'}. Long subtracts the mean of the
#'   entire signal \code{x}, short subtracts the mean of each individual
#'   frame, none does no mean subtraction.
#' @param channel channel number to analyse, ignored if \code{x} is a vector
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @return returns a list with items \code{spec}, the PSD estimate of the
#'   input signal, and \code{freq}, the frequency values (Hz) at
#'   each value of \code{spec}
#'
#' @examples
#' # wav example is synthetic echolocation clicks at 4kHz
#' wavFile <- system.file('extdata/testWav.wav', package='PAMmisc')
#' psd <- pwelch(wavFile, nfft=1e3, noverlap=500, demean='long')
#' plot(x=psd$freq, y=10*log10(psd$spec), type='l')
#'
#' @export
#'
#' @importFrom purrr reduce
#' @importFrom fftw planFFT FFT
#' @importFrom signal hamming
#'
pwelch <- function(x, nfft, noverlap=0, sr=NULL, window=NULL, demean=c('long', 'short', 'none'), channel=1) {
    if(is.character(x) && file.exists(x)) {
        # x <- readWave(x, toWaveMC=TRUE)
        x <- fastReadWave(x, header=FALSE)
    }
    if(inherits(x, 'Wave')) {
        sr <- x@samp.rate
        bitFactor <- 2^(x@bit - 1)
        if(channel == 1) {
            x <- x@left / bitFactor
        } else {
            x <- x@right / bitFactor
        }
    }
    if(inherits(x, 'WaveMC')) {
        sr <- x@samp.rate
        bitFactor <- 2^(x@bit - 1)
        x <- x@.Data[, channel] / bitFactor
    }
    if(inherits(x, 'audioSample')) {
        sr <- x$rate
        x <- x[channel, ]
    }
    demean <- match.arg(demean)
    if(demean == 'long') {
        x <- x - mean(x)
    }
    if(is.null(sr)) {
        stop('Must provide "sr" if "x" is a vector')
    }
    if(is.null(window) || length(window) != nfft) {
        window <- hamming(nfft)
    }
    if(noverlap < 1) {
        noverlap <- noverlap * nfft
    }
    hop <- (nfft - noverlap)
    x <- chunk(x, nfft, hop)
    nFrames <- length(x)
    if(length(x[[nFrames]]) < nfft) {
        x[[nFrames]] <- c(x[[nFrames]], rep(0, nfft - length(x[[nFrames]])))
    }

    ffPlan <- planFFT(nfft)
    doShort <- demean == 'short'
    x <- lapply(x, function(w) {
        if(doShort) {
            w <- w - mean(w)
        }
        w <- w * window
        spec <- FFT(w, plan=ffPlan)
        Re(spec * Conj(spec))
    })
    power <- reduce(x, `+`)
    isEven <- (nfft %% 2) == 0
    nPsd <- (nfft %/% 2) + 1
    if(isEven) {
        power <- power[1:nPsd] + c(0, power[nfft:(nPsd+1)], 0)
    } else {
        power <- power[1:nPsd] + c(0, power[nfft:(nPsd+1)])
    }
    scale <- sr * sum(window^2) * nFrames
    power <- power / scale
    frequency <- (seq_along(power)-1) / nfft * sr
    list(spec=power, freq=frequency)
}

# from stack overflow - break a vector "x" into a list of vectors of size "n"
chunk <- function(x, length, hop=length) {
    mapply(function(a, b) (x[a:b]),
           seq.int(from=1, to=length(x)-length+1, by=hop),
           pmin(seq.int(from=1, to=length(x)-length+1, by=hop)+(length-1), length(x)),
           SIMPLIFY=FALSE)
}
