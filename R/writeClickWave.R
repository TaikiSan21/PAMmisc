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
#' @importFrom tuneR Wave writeWave normalize
#' @export
#'
writeClickWave <- function(fileName, outDir, signalLength, clickLength, clicksPerSecond,
                           frequency, sampleRate, silence=c(0,0), gainFactor = .1) {
    wav <- createClickWave(signalLength, clickLength, clicksPerSecond, frequency,
                           sampleRate, silence, gainFactor)
    if(missing(fileName)) {
        clickLength <- clickLength / 1e6 # convert from micros
        fileName <- paste0(signalLength, 's_', clickLength * 1e6, 'cl_', clicksPerSecond, 'cps_',
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

#' @rdname writeClickWave
#' @export
#'
createClickWave <- function(signalLength, clickLength, clicksPerSecond,
                            frequency, sampleRate, silence=c(0,0), gainFactor = .1) {
  clickLength <- clickLength / 1e6 # convert from micros
  clickPeriod <- 1 / clicksPerSecond
  if(clickPeriod < clickLength) {
    stop('Click Period is longer than Click Length')
  }
  t <- 0 : (clickLength * sampleRate)
  tone <- sin(2 * pi * frequency * t / sampleRate)
  gain <- exp(-t/16)
  tone <- tone * gain
  iciSilence <- clickPeriod - clickLength
  iciSilence <- rep(0, iciSilence * sampleRate)
  tone <- rep(c(tone, iciSilence), signalLength/clickPeriod)
  tone <- c(rep(0, silence[1]*sampleRate), tone, rep(0, silence[2]*sampleRate))
  wav <- normalize(Wave(left=tone, samp.rate=sampleRate, bit=16), unit='16', level=gainFactor)
  wav
}
