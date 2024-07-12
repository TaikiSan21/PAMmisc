#' @title Load Wave File
#'
#' @description loads data from a WAVE file
#'
#' @param where file to load data from
#' @param from starting point to load data from (seconds)
#' @param to end point to read data to (seconds), \code{NA} to read til end
#' @param header logical flag to read only header information
#'
#' @return returns an object of the class \code{audioSample} as loaded from
#'   the WAVE file, or if \code{header=TRUE} a named list with the sample.rate,
#'   num channels, bit rate, and sample length
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @details WAVE is a RIFF (Resource Interchange File Format) widely used for
#'   storage of uncompressed audio data. It is often identified by the
#'   extension .WAV on DOS-legacy systems (such as Windows). Although
#'   WAVE files may contain compressed data, the above functions only
#'   support plain, uncompressed PCM data.
#'
#'   This function was originally written Simon Urbanek, all credit for the bulk
#'   of the C programming goes to him. Adapted by Taiki Sakai to add additional
#'   features and fix some bugs. See additional license comments in file.c
#'
#' @export
#'
fastReadWave <- function(where, from=0, to=NA_real_, header=FALSE) {
    from <- as.numeric(from)
    to <- as.numeric(to)
    result <- .Call(load_wave_file, where, from, to, as.integer(header), PACKAGE="PAMmisc")
    if(header) {
        names(result) <- c('sample.rate', 'channels', 'bits', 'samples')
    }
    result
}
