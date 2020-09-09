#' @title Write Click Waveform
#'
#' @description Write a wave file for a synthesized delphinid click
#'
#' @param fileName name of the file to write. If missing, the file be named
#'   usign signalLength, clickLength, clicksPerSecond, frequency, and sampleRate
#' @param outDir directory to write wave files to
#' @param signalLength length of signal to create in seconds
#' @param clickLength length of each click in microseconds
#' @param clicksPerSecond number of clicks per second
#' @param frequency frequency of the clicks
#' @param sampleRate sample rate for the wave file to create
#' @param silence silence to pad before and after signal in seconds
#' @param gainFactor scaling factor between 0 and 1. Low numbers are recommended (default 0.1)
#'
#' @details This code is based on Matlab code by Julie Oswald (2004). Clicks are
#'   simulated as an exponentially damped sinusoid.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @return \code{writeClickWave} invisibly returns the file name, \code{createClickWave}
#'   returns a \linkS4class{Wave} class object
#'
#' @examples
#'
#' tmpFile <- file.path(tempdir(), 'tempWav.wav')
#' writeClickWave(tmpFile, signalLength = 1, clickLength = 100, clicksPerSecond = 200,
#'                frequency = 30000, sampleRate = 100000)
#' file.remove(tmpFile)
#' clickWave <- createClickWave(signalLength = 1, clickLength = 100, clicksPerSecond = 200,
#'                              frequency = 30e3, sampleRate = 100e3)
#'
#' @importFrom tuneR Wave writeWave normalize bind
#' @export
#'
writeClickWave <- function(fileName, outDir, signalLength, clickLength, clicksPerSecond,
                           frequency, sampleRate, silence=c(0,0), gainFactor = .1) {

    wav <- createClickWave(signalLength, clickLength, clicksPerSecond, frequency,
                           sampleRate, silence, gainFactor)
    if(missing(fileName)) {
        fileName <- paste0(sum(signalLength, silence) * length(frequency), 's_',
                           round(clickLength, 0), 'cl_',
                           paste0(clicksPerSecond, collapse = '-'), 'cps_',
                           paste0(round(frequency / 1000, 0), collapse = '-'), 'khz_',
                           round(sampleRate / 1000, 0), 'khzsr.wav')
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
    invisible(fileName)
}

#' @rdname writeClickWave
#' @export
#'
createClickWave <- function(signalLength, clickLength, clicksPerSecond,
                            frequency, sampleRate, silence=c(0,0), gainFactor = .1) {
    nFreq <- length(frequency)
    nCps <- length(clicksPerSecond)
    if(nCps == 1 &&
       nFreq == 1) {
        clickLength <- clickLength / 1e6 # convert from micros
        clickPeriod <- 1 / clicksPerSecond
        if(clickPeriod < clickLength) {
            stop('Click Period is less than Click Length')
        }
        t <- 0 : ((round(clickLength * sampleRate, 0)) - 1)
        tone <- sin(2 * pi * frequency * t / sampleRate)
        gain <- exp(-t/16)
        tone <- tone * gain
        # iciSilence <- clickPeriod - clickLength
        iciSilence <- rep(0, clickPeriod * sampleRate - length(tone))
        tone <- rep_len(c(tone, iciSilence), sampleRate * signalLength)
        tone <- c(rep(0, silence[1]*sampleRate), tone, rep(0, silence[2]*sampleRate))
        wav <- normalize(Wave(left=tone, samp.rate=sampleRate, bit=16), unit='16', level=gainFactor)
        return(wav)
    }
    if(nCps < nFreq) {
        clicksPerSecond <- rep_len(clicksPerSecond, nFreq)
    }
    if(nFreq < nCps) {
        frequency <- rep_len(frequency, nCps)
    }
    do.call(bind, lapply(seq_along(frequency), function(i) {
        createClickWave(signalLength, clickLength, clicksPerSecond[i],
                        frequency[i], sampleRate, silence, gainFactor)
    }))
}
