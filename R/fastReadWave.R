#' @title Load Wave File
#'
#' @description loads data from a WAVE file
#'
#' @param where file to load data from
#' @param from starting point to load data from (seconds)
#' @param to end point to read data to (seconds), \code{NA} to read til end
#' @param header logical flag to read only header information
#' @param toWaveMC logical flag to return a \link[tuneR]{WaveMC} object
#'
#' @return returns an object of the class \code{audioSample} as loaded from
#'   the WAVE file, or if \code{header=TRUE} a named list with the sample.rate,
#'   num channels, bit rate, and sample length. \code{audioSample} objects
#'   store the wav data as a matrix wth one row for every channel and
#'   attributes "rate" and "bits"
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
#' @importFrom tuneR WaveMC
#' @export
#'
fastReadWave <- function(where, from=0, to=NA_real_, header=FALSE, toWaveMC=FALSE) {
    from <- as.numeric(from)
    to <- as.numeric(to)
    result <- .Call(load_wave_file, where, from, to, as.integer(header), PACKAGE="PAMmisc")
    if(header) {
        names(result) <- c('sample.rate', 'channels', 'bits', 'samples')
        return(result)
    }
    if(length(result) != result$len) {
        result <- result[1:result$len]
    }
    if(is.null(dim(result))) {
        dim(result) <- c(1, length(result))
    }
    if(toWaveMC) {
        bit <- result$bits
        sr <- result$rate
        result <- t(unclass(result))
        dim <- dim(result)
        result <- as.integer(result * 2^(bit-1))
        dim(result) <- dim
        return(
            WaveMC(data=result, samp.rate=sr, bit=bit)
        )
    }
    result
}

#' @export
#'
`[.audioSample` <- function(x, ..., drop = FALSE) {
    y <- NextMethod("[")
    attr(y, "rate") <- attr(x, "rate", TRUE)
    attr(y, "bits") <- attr(x, "bits", TRUE)
    # want original asamp output to always be a matrix
    # once we extract a single channel drop this assumption
    # for consistence access results, same as after wav@left for Wave
    if(!is.null(dim(y))) {
        class(y) <- class(x)
    }
    y
}
#' @export
#'
`$.audioSample` <- function(x, name) attr(x, name)
