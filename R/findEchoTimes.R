#' @title Find Estimated Echo Times
#' 
#' @description Finds the estimated times of echoes in a waveform clip. This 
#'   function was developed to estimate the time of a surface reflected echo
#'   of echolocation clicks of deep diving marine mammals. The times of echoes
#'   are estimated by finding peaks in the autocorrelation of a signal.
#' 
#' @param wav waveform to find echoes in. Can be a numeric vector, 
#'   \link[tuneR]{Wave}, or \link[tuneR]{WaveMC} class object
#' @param sr sample rate of the waveform, if \code{wav} is a \code{Wave}
#'   or \code{WaveMC} object it will use the \code{samp.rate} slot
#' @param filter filter to apply to \code{wav}, a vector of two numbers
#'   specifying the lower and upper bounds of the filter in Hz. A first
#'   value of \code{0} means no highpass filter is applied, a second value
#'   greater than \code{sr/2} means no lowpass filter is applied.
#' @param clipLen length of clip (seconds) to analyse for echoes, 
#'   measured from start of \code{wav}
#' @param peakMin minimum magnitude of autocorrelation value to be 
#'   considered a possible peak
#' @param minTime minimum allowed echo time (seconds), this should be large
#'   enough to avoid correlating the original pulse with itself
#' @param maxTime maximum allowed echo time (seconds)
#' @param channel if \code{wav} has multiple channels, channel to use
#' @param n the number of potential echoes to return, times with the 
#'   \code{n} highest autocorrelation magnitude will be returned
#' @param plot logical flag to create plot, will create a two-panel plot
#'   of the waveform (top) and the autocorrelation (bottom). Points of 
#'   the selected candidate echo times are also drawn
#' @param plotText optional text to plot on the upper waveform plot
#' 
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#' 
#' @return a list with elements \code{mag}, \code{time} and \code{wav}
#' 
#' @importFrom graphics points text
#' 
#' @export
#' 
findEchoTimes <- function(wav, 
                       sr=NULL,
                       filter=NULL,
                       clipLen=.03,
                       peakMin=.01, 
                       minTime=.001, 
                       maxTime=NULL,
                       channel=NULL,
                       n=3,
                       plot=TRUE, 
                       plotText=NULL) {
    if(is.character(wav)) {
        wav <- readWave(wav, toWaveMC = TRUE, from=0, to=clipLen, units='seconds')
    }
    if(inherits(wav, 'Wave')) {
        sr <- wav@samp.rate
        if(is.null(channel) || channel == 1) {
            wav <- wav@left / 2^(wav@bit - 1)
        } else {
            wav <- wav@right / 2^(wav@bit - 1)
        }
    }
    if(inherits(wav, 'WaveMC')) {
        sr <- wav@samp.rate
        if(is.null(channel)) {
            channel <- ncol(wav@.Data)
        }
        wav <- wav@.Data[, channel] / 2^(wav@bit - 1)
    }
    if(is.null(sr)) {
        stop('Must provide sample rate')
    }
    wav <- wav[1:(clipLen * sr)]
    if(is.null(filter)) {
        filter <- c(0, sr / 2)
    }
    # assume hp only for length 1 filter
    if(length(filter) == 1) {
        filter <- c(filter, sr/2)
    }
    tooBig <- filter > sr/2
    if(any(tooBig)) {
        warning('Filter values higher than Nyquist found, they will be set to Nyquist')
        filter[tooBig] <- sr/2
    }
    if(filter[1] != 0) {
        highPass <- butter(8, filter[1] * 2 / sr, type='high')
        wav <- signal::filter(highPass, wav)
    }
    if(filter[2] < sr/2) {
        lowPass <- butter(8, filter[2] * 2 / sr, type='low')
        wav <- signal::filter(lowPass, wav)
    }
    # just want numeric, filter converts to "ts"
    if(inherits(wav, 'ts')) {
        wav <- wav@.Data
    }
    corrWav <- ffwAcf(wav)
    mfreq <- mean(filter)
    # demodulation apparently helps the autocorr
    z <- demod(corrWav, mfreq, sr)
    peaks <- peakfinder(abs(z), peakMin)
    peaks$peakTime <- (peaks$peakLoc-1)/sr
    if(is.null(maxTime)) {
        maxTime <- clipLen
    }

    isWithin <- peaks$peakTime > minTime &
        peaks$peakTime < maxTime
    
    peaks$peakMag <- peaks$peakMag[isWithin]
    peaks$peakLoc <- peaks$peakLoc[isWithin]
    peaks$peakTime <- peaks$peakTime[isWithin]
    peakSort <- sort(peaks$peakMag, index.return=TRUE, decreasing=TRUE)
    topN <- peakSort$ix[1:n]
    
    if(plot) {
        if(all((par()$mfrow == 1))) {
            oldmf <- par(mfrow=c(2, 1))
            on.exit(par(oldmf), add=TRUE)
            thisPlotIx <- c(1,1)
        } else {
            thisPlotIx <- par()$mfg[1:2]
        }
        oldMar <- par(mar=c(1.5, 2, 2, 1))
        on.exit(par(oldMar), add=TRUE)
        plot(x=(0:(length(wav)-1))/sr, y=wav, type='l')
        wavPeak <- which.max(abs(wav))
        points(x=wavPeak/sr + peaks$peakTime[topN], y=rep(0, n), col='red', pch=16)
        points(x=wavPeak/sr + peaks$peakTime[topN[1]], y=c(0), col='green', pch=16)
        lines(x=rep(wavPeak/sr + maxTime, 2), y=c(-1, 1), col='blue', lty=3)
        if(!is.null(plotText)) {
            text(x=.005, y=max(wav) * .88, plotText, pos=4)
        }        
        par(mar=c(2, 2, 1.5, 1))
        par(mfg=thisPlotIx + c(1, 0))
        plot(x=0:(length(z)-1)/sr, y=abs(z), type='l', xlim=c(0, .02), xaxs='i', yaxs='i')
        points(x=peaks$peakTime[topN], y=peaks$peakMag[topN], col='red')
        points(x=peaks$peakTime[topN[1]], y=peaks$peakMag[topN[1]], col='green')
        lines(x=c(minTime, minTime), y=c(0, 1), lty=3, col='black')
        lines(x=c(maxTime, maxTime), y=c(0, 1), lty=3, col='blue')
    }
    list(mag=peaks$peakMag[topN], time=peaks$peakTime[topN], wav=wav)
}

#' @importFrom signal filtfilt butter
# implementation of MATLAB's demod function
demod <- function(y, Fc, Fs, method='am') {
    if(Fc >= Fs/2) {
        stop('Invalid carrier frequency (higher than Nyquist)')
    }
    
    len <- length(y) # change to be by matrix
    if(method %in% c('am', 'amdsb-sc', 'amdsb-tc', 'amssb')) {
        t <- seq(from=0, to=(len-1)/Fs, by=1/Fs)
        x_in <- y * cos(2*pi*Fc*t)
        bfilt <- butter(5, Fc*2/Fs)
        x_in <- filtfilt(bfilt, x_in)
        # x_in <- EndEffect(bfilt, x_in)
    }
    x_in
}

#' @importFrom fftw FFT IFFT
# fast implementation of autocorrelation using FFT
ffwAcf <- function(x) {
    n <- length(x)
    fx <- FFT(c(x, rep(0, n)))
    x2 <- IFFT(abs(fx)^2)
    abs(x2[1:n]/x2[1])
}

# Finds thea peaks in a series of values using local derivative.
#   Based on original MATLAB from 2015 by Nathanael C. Yoder.
 
# @param x0 series of values to find peaks in
# @param thresh level above the minimum to be counted as a peak
# @param extrema \code{1} for maximum \code{-1} for minimum
# @param plot logical flag to plot results

# @return a list with elements \code{peakLoc} giving the indices of the
#   peaks and \code{peakMag} giving the magnitude of the peaks
#
peakfinder <- function(x0, thresh=NULL, extrema=1, plot=FALSE) {
    if(!is.vector(x0)) {
        stop('PEAKFINDER:Input\nThe input data must be a vector')
    }
    if(is.null(x0) || length(x0) == 0) {
        return(list(peakLoc=NULL, peakMag=NULL))
    }
    if(is.complex(x0)) {
        warning('PEAKFINDER:NotReal\nAbsolute value of data will be used')
        x0 <- abs(x0)
    }
    if(is.null(thresh)) {
        thresh <- (max(x0) - min(x0)) / 4
    } else if(!is.numeric(thresh) || is.complex(thresh)) {
        thresh <- (max(x0) - min(x0)) / 4
        warning('PEAKFINDER:VectorThresh\n',
                'The threshold must be a real scalar. A threshold of ',
                round(thresh, 4), ' will be used.')
    } else if(length(thresh) > 1) {
        warning('PEAKFINDER:VectorThresh\n',
                'The threshold must be a scalar. The first threshold in the vector will be used.')
        thresh <- thresh[1]
    }
    extrema <- sign(extrema) # should only be 1 or -1
    if(extrema == 0) {
        stop('PEAKFINDER:ZeroMaxima\n',
             'Either 1 (for maxima) or -1 (for minima) must be input for extrema')
    }
    x0 <- x0 * extrema # make it so always finding max
    dx0 <- diff(x0) # find derivative
    eps <- 1e-16 
    dx0[dx0 == 0] <- -1*eps # this is so we findthe first of repeated values
    ind <- which(dx0[1:(length(dx0)-1)] * dx0[2:length(dx0)] < 0) + 1 # find where sign changes
    
    # include endpoints in potential peaks and valleys
    x <- c(x0[1], x0[ind], x0[length(x0)])
    ind <- c(1, ind, length(x0))
    
    # x only has the peaks, valleys, and endpoints
    len <- length(x)
    minMag <- min(x)
    
    if(len > 2) { # function with peaks and valleys
        # set initial parameters for loop
        tempMag <- minMag
        foundPeak <- FALSE
        leftMin <- minMag
        
        # deal with first point a little differently since tacked it on
        # calculate the sign of the derivative isnce we tacked the first point
        # on it does not necessarily alternate like the rest.
        signDx <- sign(diff(x[1:3]))
        if(signDx[1] <= 0) { # first point is larger or equal to the second
            ii <- 0
            if(signDx[1] == signDx[2]) { # want alternating signs
                x <- x[-2]
                ind <- ind[-2]
                len <- len-1
            }
        } else { # first point smaller than second
            ii <- 1
            if(signDx[1] == signDx[2]) { # want alternating signs
                x <- x[-1]
                ind <- ind[-1]
                len <- len-1
            }
        }
        
        # prealloc max number of maxima
        maxPeaks <- ceiling(len/2)
        peakLoc <- rep(0, length(maxPeaks))
        peakMag <- peakLoc
        cInd <- 1
        
        # loop through extrema which should be peaks and then valleys
        while(ii < len) {
            ii <- ii + 1 # this is a peak
            # reset peak finding if we had a peak and the next peak is bigger
            # than the last or the left min was small enough to reset.
            if(foundPeak && 
               (x[ii] > peakMag[length(peakMag)] || 
                leftMin < peakMag[length(peakMag)]-thresh)) {
                tempMag <- minMag
                foundPeak <- FALSE
            }
            
            #make sure we dont iterate past the length of our vector
            if(ii == len) {
                break
            }
            
            #found new peak that was larger than temp mag and threshold larger
            # than the minimum to its left
            if(x[ii] > tempMag && x[ii] > leftMin + thresh) {
                tempLoc <- ii
                tempMag <- x[ii]
            }
            
            ii <- ii + 1 # move onto the valley
            # come down at least thresh from peak
            if(!foundPeak && tempMag > thresh + x[ii]) {
                foundPeak <- TRUE
                leftMin <- x[ii]
                peakLoc[cInd] <- tempLoc
                peakMag[cInd] <- tempMag
                cInd <- cInd + 1
            } else if(x[ii] < leftMin) { #new left minima
                leftMin <- x[ii]
            }
        }
        
        # check end point
        if(x[length(x)] > tempMag &&
           x[length(x)] > leftMin + thresh) {
            peakLoc[cInd] <- len
            peakMag[cInd] <- x[length(x)]
            cInd <- cInd + 1
        } else if(!foundPeak && tempMag > minMag) { #check if we still need to add the last point
            peakLoc[cInd] <- tempLoc
            peakMag[cInd] <- tempMag
            cInd <- cInd + 1
        }
        
        # create output
        peakInds <- ind[peakLoc[1:(cInd-1)]]
        peakMag <- peakMag[1:(cInd-1)]
    } else {
        peakMag <- max(x)
        xInd <- which.max(x)
        if(peakMag > minMag + thresh) {
            peakInds <- ind[xInd]
        } else {
            peakMag <- NULL
            peakInds <- NULL
        }
    }
    
    # change sign of data if was finding minima
    if(extrema < 0) {
        peakMag <- -peakMag
        x0 <- -x0
    }
    if(plot) {
        plot(1:length(x0), x0, type='l')
        points(peakInds, peakMag)
    }
    list(peakLoc=peakInds, peakMag=peakMag)
}
