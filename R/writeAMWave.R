#' @title Write Amplitude Modulated Waveform
#'
#' @description Write a wave file for a synthesized amplitude modulated call
#'
#' @param fileName name of the file to write. If missing, the file be named
#'   usign signalLength, modFrequency, frequency, and sampleRate
#' @param outDir directory to write wave files to
#' @param signalLength length of signal to create in seconds
#' @param modFrequency modulation frequency in Hz of the amplitude modulation
#' @param frequency frequency of the AM call
#' @param sampleRate sample rate for the wave file to create
#' @param window window constants for applying the amplitude modulation. See details.
#' @param silence silence to pad before and after signal in seconds
#' @param gainFactor scaling factor between 0 and 1. Low numbers are recommended (default 0.1)
#'
#' @details Amplitude modulated signals are modelled as an ideal sinusoid multiplied by a
#'   window function. The window function is an offset sinusoid with frequency equal to the
#'   modulation frequency:
#'  \deqn{W = .5 + .45 * sin(2 \pi mf t)}
#'  See \code{example(writeAMWave)} for a plot showing how this works.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#' # Visualisation of modelled AM wave
#' signal <- sin(2*pi*100*(1:1000)/1000)
#' window <- .55 + .45 * sin(2*pi*15*(1:1000)/1000)
#' par(mfrow=c(3,1))
#' plot(signal, type='l')
#' plot(window, type='l')
#' plot(window*signal, type='l')
#'
#' @importFrom tuneR Wave writeWave normalize
#' @export
#'
writeAMWave <- function(fileName, outDir, signalLength, modFrequency, frequency,
                        sampleRate, window = c(.55, .45), silence=c(0,0), gainFactor = .1) {
    wav <- createAMWave(signalLength, modFrequency, frequency, sampleRate, window,
                        silence, gainFactor)
    if(missing(fileName)) {
        fileName <- paste0(signalLength, 's_', modFrequency, 'modfreq_',
                           round(frequency / 1000, 0), 'khz_', round(sampleRate / 1000, 0), 'khzsr.wav')
    }
    if(missing(outDir)) {
        return(writeWave(wav, fileName, extensible = FALSE))
    }
    outDir <- gsub('[\\\\/]*$', '', outDir)
    if(!dir.exists(outDir)) {
        cat('Creating directory', outDir)
        dir.create(outDir)
    }
    fileName <- paste0(outDir,'/', fileName)
    writeWave(wav, fileName, extensible = FALSE)
}

#' @rdname writeAMWave
#'
createAMWave <- function(signalLength, modFrequency, frequency, sampleRate,
                         window = c(.55, .45), silence=c(0,0), gainFactor = .1) {
    t <- 0 : ((signalLength * sampleRate) - 1)
    tone <- sin(2 * pi * frequency * t / sampleRate)
    amWindow <- (window[1] + window[2]*sin(2 * pi * modFrequency * t / sampleRate))
    tone <- c(rep(0, silence[1]*sampleRate), tone * amWindow, rep(0, silence[2]*sampleRate))
    wav <- normalize(Wave(left=tone, samp.rate=sampleRate, bit=16), unit='16', level=gainFactor)
    wav
}
