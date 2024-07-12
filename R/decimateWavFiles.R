#' @title Decimate Wave Files
#'
#' @description Decimate a folder of .wav files or a single .wav file
#'   to a new sample rate.
#'
#' @param inDir directory of wave files to decimate. Can also be a single .wav file.
#' @param outDir directory to write wave files to
#' @param newSr sample rate to decimate the files to
#' @param progress logical flag to show progress bar
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
#' # one 20kHz wav file is included in package test data
#' origDir <- system.file('extdata', package='PAMmisc')
#' decDir <- file.path(tempdir(), 'decSR')
#' decWavs <- decimateWavFiles(origDir, decDir, 10000)
#' file.remove(decWavs)
#'
#' @importFrom tuneR readWave writeWave
#' @importFrom seewave bwfilter resamp
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom tcltk tk_choose.dir
#' @export
#'
decimateWavFiles <- function(inDir, outDir, newSr, progress=TRUE) {
    if(missing(inDir)) {
        cat('Please choose an input folder.\n')
        inDir <- tk_choose.dir(caption='Select input folder.')
    }
    if(missing(outDir)) {
        cat('Please choose an output folder.\n')
        outDir <- tk_choose.dir(caption='Select output folder.')
    }
    outDir <- gsub('[\\\\/]*$', '', outDir)
    if(!dir.exists(outDir)) {
        # cat('Creating directory', outDir)
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
    if(progress) {
        cat('Decimating', length(files), 'wav files...\n')
        pb <- txtProgressBar(min = 0, max = length(files), style = 3)
    }
    for(i in seq_along(files)) {
        inWave <- try(readWave(files[i]), silent=TRUE)
        inBit <- inWave@bit
        if(length(inWave)==1) {
            error[i] <- TRUE
            next
        }
        outWave <- bwfilter(inWave, n=2, from=5, to=1.2*(newSr/2), output='Wave')
        outWave <- resamp(outWave, g=newSr, output='Wave')
        # sometimes outWave ends up bigger than inWave which can cause writing issues,
        # just rescale to inWave size if this happens
        # relScale <- max(abs(outWave@left)) / max(abs(inWave@left))
        relScale <- max(abs(outWave@left)) / (2^(inBit-1) - 1)
        outWave@left <- outWave@left / max(1, relScale)
        outWave@left <- round(outWave@left)
        outWave@bit <- inBit
        writeWave(outWave, filename=paste0(outDir, '/LF_', basename(files[i])), extensible=FALSE)
        if(progress) {
            setTxtProgressBar(pb, value = i)
        }
    }
    if(sum(error) > 0) {
        warning('Error trying to decimate file(s):\n',
                paste(basename(files[error]), collapse=', '))
    }
    invisible(paste0(outDir, '/LF_', basename(files))[!error])
}
