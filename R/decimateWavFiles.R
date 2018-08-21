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
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom tuneR readWave writeWave
#' @importFrom seewave bwfilter resamp
#' @importFrom utils choose.dir
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
    for(i in seq_along(files)) {
        inWave <- try(tuneR::readWave(files[i]), silent=TRUE)
        if(length(inWave)==1) {
            error[i] <- TRUE
            next
        }
        outWave <- seewave::bwfilter(inWave, n=2, from=5, to=1.2*(newSr/2), output='Wave')
        outWave <- seewave::resamp(outWave, g=newSr, output='Wave')
        tuneR::writeWave(outWave, filename=paste0(outDir, '/LF_', files[i]), extensible=FALSE)
    }
    if(sum(error) > 0) {
        warning('Error trying to decimate file(s):\n',
            paste(basename(files[error]), collapse=', '))
    }
}
