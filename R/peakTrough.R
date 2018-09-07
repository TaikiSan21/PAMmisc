#' @title Find Peaks and Troughs in a Spectrum
#'
#' @description Finds up to three peaks in a spectrum, as well as the troughs
#'   between those peaks.
#'
#' @details This uses a very simple algorithm to find second and third peaks
#'   in a spectrum. Peak candidates are identified with a few simple steps.
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
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom dplyr filter mutate
#' @importFrom magrittr %>%
#' @import ggplot2
#' @export
#'
peakTrough <- function(spec, freqBounds=c(10, 30), dbMin=-15, smooth=5, plot=FALSE) {
    # Default values to return if we dont find other peaks
    peak2 <- 0; peak2dB <- dbMin
    trough <- 0; troughdB <- dbMin
    peak3 <- 0; peak3dB <- dbMin
    trough2 <- 0; trough2dB <- dbMin

    # normalizing dB level
    spec[,2] <- spec[,2] - max(spec[,2])
    specDf <- data.frame(Freq=spec[,1], dB=runAvg(spec[,2], l=smooth)) # Smooth with local average, nearest 5
    wherePeak <- which.max(specDf$dB)
    peak <- specDf$Freq[wherePeak]
    peakdB <- specDf$dB[wherePeak]

    if(length(peak)==0) { # Not sure how this would happen, just in case
        peak <- 0; peakdB <- dbMin
    }

    peak2Df <- specDf %>%
        mutate(before = c(specDf$dB[1], specDf$dB[1:(nrow(specDf)-1)]),
               after = c(specDf$dB[2:nrow(specDf)], specDf$dB[nrow(specDf)]),
               isPeak = (dB > before) & (dB >= after)) %>%
        filter(isPeak,
               ((Freq >= (peak + freqBounds[1])) & (Freq <= (peak + freqBounds[2]))) |
                   ((Freq <= (peak - freqBounds[1])) & (Freq >= (peak - freqBounds[2]))),
               Freq != peak,
               dB >= dbMin)

    if(nrow(peak2Df) > 0) {
        wherePeak2 <- which.max(peak2Df$dB)
        peak2 <- peak2Df$Freq[wherePeak2]
        peak2dB <- peak2Df$dB[wherePeak2]
        peak3Df <- peak2Df %>%
            filter((Freq >= (peak2 + freqBounds[1])) |
                       (Freq <= (peak2 - freqBounds[1])),
                   Freq != peak2)

        if(nrow(peak3Df) > 0) {
            wherePeak3 <- which.max(peak3Df$dB)
            peak3 <- peak3Df$Freq[wherePeak3]
            peak3dB <- peak3Df$dB[wherePeak3]
        }
    }
    allPeaks <- sort(c(peak, peak2, peak3))
    allPeaks <- allPeaks[allPeaks != 0]

    # Find troughs based on num of non-zero peaks. Change nothing if only 1.
    if(length(allPeaks)==2) {
        troughDf <- filter(specDf, Freq > allPeaks[1], Freq < allPeaks[2])
        whereTrough <- which.min(troughDf$dB)
        trough <- troughDf$Freq[whereTrough]
        troughdB <- troughDf$dB[whereTrough]
    } else if(length(allPeaks)==3) {
        troughDf <- filter(specDf, Freq > allPeaks[1], Freq < allPeaks[2])
        whereFirst <- which.min(troughDf$dB)
        first <- troughDf$Freq[whereFirst]
        firstdB <- troughDf$dB[whereFirst]
        troughDf <- filter(specDf, Freq > allPeaks[2], Freq < allPeaks[3])
        whereSecond <- which.min(troughDf$dB)
        second <- troughDf$Freq[whereSecond]
        seconddB <- troughDf$dB[whereSecond]
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
        freqLines <- sort(peak + c(freqBounds, -1*freqBounds))
        graphDf <- data.frame(Freq = c(peak, peak2, peak3, trough, trough2),
                              dB = c(max(specDf$dB), peak2dB, peak3dB, troughdB, trough2dB),
                              Type = c('Highest Peak', 'Second Peak', 'Third Peak', 'Trough / Notch', 'Trough / Notch'))
        g <- ggplot() + geom_line(data=specDf, aes(x=Freq, y=dB)) +
            geom_vline(xintercept=freqLines, color='goldenrod1') +
            geom_hline(yintercept=dbMin, color='blue') +
            geom_point(data=graphDf, aes(x=Freq, y=dB, color=Type), size=3) +
            coord_cartesian(xlim=range(specDf$Freq), ylim=range(specDf$dB)) +
            scale_x_continuous(breaks=seq(0,140,20)) +
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
    data.frame(peak = peak, peak2 = peak2, peak3 = peak3,
               trough = trough, trough2 = trough2,
               peakToPeak2 = peakToPeak2, peakToPeak3 = peakToPeak3, peak2ToPeak3 = peak2ToPeak3)
}

runAvg <- function(x, l=5) {
    if(l > length(x)) {
        rep(mean(x, na.rm=TRUE), length(x))
    } else {
        sapply(seq_along(x), function(s) {
            if(s <= (l-1)/2) {
                mean(x[1:(s+(l-1)/2)], na.rm=TRUE)
            } else if((s+(l-1)/2) > length(x)) {
                mean(x[(s-(l-1)/2):length(x)], na.rm=TRUE)
            } else {
                mean(x[(s-(l-1)/2):(s+(l-1)/2)], na.rm=TRUE)
            }
        })
    }
}
