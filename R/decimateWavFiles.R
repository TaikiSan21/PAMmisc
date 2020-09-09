#' @title Decimate Wave Files
#'
#' @description Decimate a folder of .wav files or a single .wav file
#'   to a new sample rate.
#'
#' @param inDir directory of wave files to decimate. Can also be a single .wav file.
#' @param outDir directory to write wave files to
#' @param newSr sample rate to decimate the files to
#'
#' @details This code is based on R code written by Jay Barlow.
#'
#' @return Invisibly returns the names of all files that were successfully
#'   decimated
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#'
#' origDir <- file.path(tempdir(), 'origSR')
#' decDir <- file.path(tempdir(), 'decSR')
#' writeClickWave('origWav.wav', outDir=origDir, signalLength = 1, clickLength = 100,
#'                clicksPerSecond = 200, frequency = 20000, sampleRate = 100000)
#' decWavs <- decimateWavFiles(origDir, decDir, 50000)
#' file.remove(paste0(origDir, 'origWav.wav'))
#' file.remove(decWavs)
#'
#' @importFrom tuneR readWave writeWave
#' @importFrom seewave bwfilter resamp
#' @importFrom utils choose.dir setTxtProgressBar txtProgressBar
#' @export
#'
decimateWavFiles <- function(inDir, outDir, newSr) {
    if(missing(inDir)) {
        cat('Please choose an input folder.\n')
        inDir <- choose.dir()
    }
    if(missing(outDir)) {
        cat('Please choose an output folder.\n')
        outDir <- choose.dir()
    }
    outDir <- gsub('[\\\\/]*$', '', outDir)
    if(!dir.exists(outDir)) {
        cat('Creating directory', outDir)
        dir.create(outDir)
    }
    if(missing(newSr)) {
        newSr <- as.integer(readline(prompt='What sample rate would you like to decimate to?\n'))
    }
    # Check if just 1 file
    if(grepl('\\.wav$', inDir) &&
       file.exists(inDir)) {
        files <- inDir
    } else {
        files <- list.files(inDir, pattern='\\.wav$', recursive=FALSE, full.names=TRUE)
    }
    error <- rep(FALSE, length(files))
    cat('Decimating', length(files), 'wav files...\n')
    pb <- txtProgressBar(min = 0, max = length(files), style = 3)
    for(i in seq_along(files)) {
        inWave <- try(readWave(files[i]), silent=TRUE)
        if(length(inWave)==1) {
            error[i] <- TRUE
            next
        }
        outWave <- bwfilter(inWave, n=2, from=5, to=1.2*(newSr/2), output='Wave')
        outWave <- resamp(outWave, g=newSr, output='Wave')
        outWave@left <- round(outWave@left)
        writeWave(outWave, filename=paste0(outDir, '/LF_', basename(files[i])), extensible=FALSE)
        setTxtProgressBar(pb, value = i)
    }
    if(sum(error) > 0) {
        warning('Error trying to decimate file(s):\n',
            paste(basename(files[error]), collapse=', '))
    }
    invisible(paste0(outDir, '/LF_', basename(files))[!error])
}
