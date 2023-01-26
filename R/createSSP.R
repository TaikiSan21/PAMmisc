#' @title Create Sound Speed Profiles
#'
#' @description Creates sound speed profiles (Depth vs Sound Speed) using temperature
#'   and salinity data downloaded from HYCOM data servers
#'
#' @param x a data.frame with columns \code{UTC}, \code{Longitude}, and
#'   \code{Latitude} to create sound speed profiles for
#' @param f the frequency (Hz) to generate the profile for
#' @param nc netcdf file containing salinity and temperature data at depth, if
#'   \code{NULL} (default) these will be downloaded from HYCOM servers
#' @param ncVars names of the salinity and temperature variables (in that order)
#'   in your netcdf file, only change these if you are providing your own file
#'   to \code{nc}
#' @param dropNA logical flag to drop NA values from soundspeeed profile from outputs.
#'   SSP will be calculated up to the maximum depth at each coordinate, which can vary.
#'   Setting this option to \code{FALSE} ensures that outputs are the same length for
#'   each coordinate, which can be useful
#' @param progress logical flag to show progress bar for SST download
#' @param \dots additional arguments to pass to \link{matchEnvData}
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @return a list with one element for each row of \code{x}, each element is a list
#'   containing \code{speed}, the sound speed (m/s), and \code{depth} (m)
#'
#' @examples
#' \dontrun{
#' # examples not run because they require internet connection
#' coords <- data.frame(UTC=as.POSIXct('2014-07-15 01:00:00', tz='UTC'),
#'                      Longitude = -119, Latitude = 33)
#' ssp <- createSSP(coords)
#' plot(x=ssp[[1]]$speed, y=-ssp[[1]]$depth, type='l')
#' }
#'
#' @importFrom seewave wasp
#' @export
#'
createSSP <- function(x, f=30e3, nc=NULL, ncVars=c('salinity', 'water_temp'), dropNA=TRUE, progress, ...) {
    if(!all(c('UTC', 'Longitude', 'Latitude') %in% names(x))) {
        warning('Need UTC, Longitude, and Latitude columns')
        return(NULL)
    }
    if(is.null(nc)) {
        nc <- PAMmisc::hycomList
        nc$varSelect <- nc$vars %in% ncVars
    }
    result <- vector('list', length=nrow(x))
    x <- matchEnvData(x, nc=nc, raw=TRUE, depth=NULL, progress=progress, ...)
    for(i in seq_along(result)) {
        ssp <- wasp(f=f, t=x[[i]][[ncVars[2]]], s=x[[i]][[ncVars[1]]], d=x[[i]]$matchDepth, medium='sea')
        if(dropNA) {
            keepers <- !is.na(ssp$c)
            result[[i]] <- list(speed=ssp$c[keepers], depth=x[[i]]$matchDepth[keepers])
        } else {
            result[[i]] <- list(speed=ssp$c, depth=x[[i]]$matchDepth)
        }
    }
    result
}
