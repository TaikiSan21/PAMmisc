#' @title Find Peaks and Troughs in a Spectrum
#'
#' @description Finds up to three peaks in a spectrum, as well as the troughs
#'   between those peaks.
#'
#' @details The first peak is the frequency with the highest dB level (first and
#'   last frequency points are ignored). Then this uses a very simple algorithm to
#'   find second and third peaks
#'   in a spectrum. Peak candidates are identified with a few simple steps:
#'   \describe{
#'     \item{Step 1}{Use a local average of (\code{smooth}) points to smooth the
#'   spectrum.}
#'     \item{Step 2}{Check if a point is larger than both its neighbors.}
#'     \item{Step 3}{Check if points are within the frequency range specified by
#'   \code{freqBounds}. Points must be at least f1 kHz away from the frequency
#'   , but no further than f2 kHz away.}
#'     \item{Step 4}{Check if points are above the minimum dB level specified by
#'   \code{dbMin}.}
#'   }
#'   From the remaining points the point with the highest dB level is selected
#'   as the second peak, then the frequency range filter of Step 3 is applied
#'   again around this second peak before attempting to find a third peak. If
#'   no second or third peak is found (ie. no values fall within the specified
#'   frequency and dB search ranges), then it will be set to 0. The trough
#'   values are set as the frequency with the lowest dB level between any peaks
#'   that were found. The trough values will be 0 for any peaks that were not
#'   found.
#'
#'   If you are unsure of what levels to specify for \code{freqBounds} and
#'   \code{dbMin}, setting \code{plot=TRUE} will show a visualization of the
#'   search range and selected peaks so you can easily see if the selected
#'   parameters are capturing the behavior you want.
#' @param spec the spectrum of a signal, the first column must be frequency in
#'   kilohertz, the second column must be dB
#' @param freqBounds a two element vector specifying the frequency range around
#'   the highest peak to search for a second/third peak. Units are in kHz, a
#'   value of c(f1, f2) requires a second peak to be at least f1 kHz away from
#'   the first peak, but no further than f2 kHz away.
#' @param dbMin minimum dB level for second / third peaks, relative to maximum
#'   dB. Any points lower than this dB level will not be considered a candidate
#'   peak.
#' @param smooth the amount to smooth the spectrum before attempting to find
#'   second / third peaks. Uses a simple local average, smooth is the total
#'   number of points to use. A value of 1 applies no smoothing.
#' @param plot logical flag to plot image of peak/trough locations on spectrum.
#'   Useful for finding appropriate settings for freqBounds and dbMin
#'
#' @return a dataframe with the frequencies (in kHz) of up to 3 peaks and 2
#'   troughs between those peaks. Also reports the peak-to-peak distance. Any
#'   peaks / troughs that were not able to be found (based on \code{freqBounds}
#'   and \code{dbMin} parameters) will be 0.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#'
#' clickWave <- createClickWave(signalLength = .1, clickLength = 1000, clicksPerSecond = 200,
#'                              frequency = 3e3, sampleRate = 10e3)
#' peakTrough(seewave::spec(clickWave, plot=FALSE), plot=TRUE)
#'
#' @importFrom dplyr filter mutate
#' @importFrom magrittr %>%
#' @importFrom RcppRoll roll_mean
#' @import ggplot2
#' @export
#'
peakTrough <- function(spec, freqBounds=c(10, 30), dbMin=-15, smooth=5, plot=FALSE) {
    # Default values to return if we dont find other peaks
    if(max(spec[, 1] > 1e3)) {
        message(paste0('Expected kHz, but frequency units appear to be in hertz.',
                       'Converting before calculation, note that result is in kHz.'))
        spec[, 1] <- spec[, 1] / 1e3
    }
    specNa <- is.na(spec[, 2])
    if(any(specNa)) {
        spec[specNa, 2] <- min(spec[, 2], na.rm=TRUE)
    }
    peak2 <- 0; peak2dB <- dbMin
    trough <- 0; troughdB <- dbMin
    peak3 <- 0; peak3dB <- dbMin
    trough2 <- 0; trough2dB <- dbMin

    # normalizing dB level
    spec[,2] <- spec[,2] - max(spec[,2], na.rm = TRUE)
    extend <- floor(smooth/2)
    spec[,2] <- roll_mean(c(rep(spec[1,2], extend), spec[,2], rep(spec[nrow(spec), 2], extend)), smooth)

    # find peak but not first and last
    wherePeak <- which.max(spec[-1*c(1, nrow(spec)), 2]) + 1
    peak <- spec[wherePeak, 1]
    peakdB <- spec[wherePeak, 2]

    if(length(peak)==0) { # Not sure how this would happen, just in case
        peak <- 0; peakdB <- dbMin
    }
    before <- spec[c(1, 1:nrow(spec)-1), 2]
    after <- spec[c(2:nrow(spec), nrow(spec)), 2]
    isPeak <- (spec[, 2] > before) & (spec[, 2] >= after)
    inRange1 <- ((spec[, 1] >= (peak + freqBounds[1])) & (spec[, 1] <= (peak + freqBounds[2]))) |
        ((spec[, 1] <= (peak - freqBounds[1])) & (spec[, 1] >= (peak - freqBounds[2])))
    notPeak1 <- spec[, 1] != peak
    inDbRange <- spec[, 2] >= dbMin

    peak2Spec <- spec[isPeak & inRange1 & notPeak1 & inDbRange, ]
    if(length(peak2Spec)==2) {
        peak2Spec <- matrix(peak2Spec, ncol=2)
    }

    if(nrow(peak2Spec) > 0) {
        wherePeak2 <- which.max(peak2Spec[, 2])
        peak2 <- peak2Spec[wherePeak2, 1]
        peak2dB <- peak2Spec[wherePeak2, 2]

        inRange2 <- (peak2Spec[, 1] >= (peak2 + freqBounds[1])) |
            (peak2Spec[, 1] <= (peak2 - freqBounds[1]))
        notPeak2 <- peak2Spec[, 1] != peak2
        peak3Spec <- peak2Spec[inRange2 & notPeak2, ]
        if(length(peak3Spec)==2) {
            peak3Spec <- matrix(peak3Spec, ncol=2)
        }

        if(nrow(peak3Spec) > 0) {
            wherePeak3 <- which.max(peak3Spec[, 2])
            peak3 <- peak3Spec[wherePeak3, 1]
            peak3dB <- peak3Spec[wherePeak3, 2]
        }
    }
    allPeaks <- sort(c(peak, peak2, peak3))
    allPeaks <- allPeaks[allPeaks != 0]

    # Find troughs based on num of non-zero peaks. Change nothing if only 1.
    if(length(allPeaks)==2) {
        inTrough <- (spec[, 1] > allPeaks[1]) & (spec[, 1] < allPeaks[2])
        troughMat <- spec[inTrough, ]
        if(length(troughMat) == 2) {
            troughMat <- matrix(troughMat, ncol = 2)
        }
        whereTrough <- which.min(troughMat[, 2])
        trough <- troughMat[whereTrough, 1]
        troughdB <- troughMat[whereTrough, 2]
    } else if(length(allPeaks)==3) {
        inTrough <- (spec[, 1] > allPeaks[1]) & (spec[, 1] < allPeaks[2])
        troughMat <- spec[inTrough, ]
        if(length(troughMat) == 2) {
            troughMat <- matrix(troughMat, ncol = 2)
        }
        whereFirst <- which.min(troughMat[, 2])
        first <- troughMat[whereFirst, 1]
        firstdB <- troughMat[whereFirst, 2]

        inTrough2 <- (spec[, 1] > allPeaks[2]) & (spec[, 1] < allPeaks[3])
        troughMat <- spec[inTrough2, ]
        if(length(troughMat) == 2) {
            troughMat <- matrix(troughMat, ncol = 2)
        }
        whereSecond <- which.min(troughMat[, 2])
        second <- troughMat[whereSecond, 1]
        seconddB <- troughMat[whereSecond, 2]
        # Want lowest trough to be labelled "trough", not "trough2"
        if(firstdB <= seconddB) {
            trough <- first
            troughdB <- firstdB
            trough2 <- second
            trough2dB <- seconddB
        } else {
            trough <- second
            troughdB <- seconddB
            trough2 <- first
            trough2dB <- firstdB
        }
    }
    # If 2nd/3rd peaks are 0, make this 0.
    peakToPeak2 <- ifelse(peak2==0, 0, abs(peak-peak2))
    peakToPeak3 <- ifelse(peak3==0, 0, abs(peak-peak3))
    peak2ToPeak3 <- ifelse((peak3==0) | (peak2==0), 0, abs(peak2-peak3))

    if(plot) {
        # I DONT THINK THIS WORKS WITH RECTS - CHECK LOGIC LATER
        specDf <- data.frame(Freq = spec[, 1], dB = spec[, 2])
        freqLines <- sort(peak + c(freqBounds, -1*freqBounds))
        graphDf <- data.frame(Freq = c(peak, peak2, peak3, trough, trough2),
                              dB = c(max(specDf$dB), peak2dB, peak3dB, troughdB, trough2dB),
                              Type = c('Highest Peak', 'Second Peak', 'Third Peak', 'Trough / Notch', 'Trough / Notch'))
        g <- ggplot() + geom_line(data=specDf, aes_string(x='Freq', y='dB')) +
            geom_vline(xintercept=freqLines, color='goldenrod1') +
            geom_hline(yintercept=dbMin, color='blue') +
            geom_point(data=graphDf, aes_string(x='Freq', y='dB', color='Type'), size=3) +
            coord_cartesian(xlim=range(specDf$Freq), ylim=range(specDf$dB)) +
            scale_x_continuous(breaks=seq(0,500,20)) +
            labs(title='Finding Peaks and Troughs', x='Frequency (kHz)', y='Relative dB') +
            ## FIX ME
            geom_rect(aes(xmin=c(freqLines[1], freqLines[3]), xmax=c(freqLines[2], freqLines[4]), ymin=dbMin, ymax=0, fill='a'), alpha=.1) +
            geom_rect(aes(xmin=c(-5, freqLines[2], freqLines[4]), xmax=c(freqLines[1], freqLines[3], 150), ymin=dbMin, ymax=0, fill='b'), alpha=.15) +
            geom_rect(aes(xmin=c(freqLines[1], freqLines[3]), xmax=c(freqLines[2], freqLines[4]), ymin=-130, ymax=dbMin, fill='c'), alpha=.1) +
            ### END FIX
            scale_fill_manual(values=c('green', 'blue', 'yellow'), labels=c('Range to Search', 'dB Range', 'Frequency Range'), name='') +
            theme(plot.title=element_text(hjust=.5))
        suppressWarnings(print(g))
    }
    tryCatch({
        # data.frame(peak = peak, peak2 = peak2, peak3 = peak3,
        #            trough = trough, trough2 = trough2,
        #            peakToPeak2 = peakToPeak2, peakToPeak3 = peakToPeak3, peak2ToPeak3 = peak2ToPeak3)
        result <- list(peak = peak, peak2 = peak2, peak3 = peak3,
             trough = trough, trough2 = trough2,
             peakToPeak2 = peakToPeak2, peakToPeak3 = peakToPeak3, peak2ToPeak3 = peak2ToPeak3)
        resLen <- sapply(result, length)
        for(i in which(resLen == 0)) {
            result[[i]] <- NA
        }

        structure(result, row.names=c(NA, -1), class='data.frame')
    },
    error = function(e) {
        message('peakTrough failed with error:\n', e$message)
        data.frame(peak = NA, peak2 = NA, peak3 = NA,
                   trough = NA, trough2 = NA,
                   peakToPeak2 = NA, peakToPeak3 = NA, peak2ToPeak3 = NA)
    })
}
