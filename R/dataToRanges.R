#' @title Create List of the Ranges of Coordinates
#'
#' @description Creates a named list with the ranges of Longitude, Latitude,
#'   and Time (UTC) data for use in functions like \link{formatURL}. Can also
#'   specify an amount to buffer the min and max values by for each coordinate
#'
#' @param data a data frame with longitude, latitude, and time (UTC) columns
#' @param buffer a vector of the amount to buffer the min and max values of
#'   Longitude, Latitude, and UTC by (in that order)
#'
#' @return a list with the ranges of coordinates for Longitude, Latitude, and
#'   UTC. Ranges are listed as c(left, right), so if your data spans across the
#'   dateline
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#'
#' gps <- data.frame(Latitude = c(32, 32.1, 32.2, 32.2, 32.2),
#'                   Longitude = c(-110, -110.1, -110.2, -110.3, -110.4),
#'                   UTC = as.POSIXct(c('2000-01-01 00:00:00', '2000-01-01 00:00:10',
#'                                      '2000-01-01 00:00:20', '2000-01-01 00:00:30',
#'                                      '2000-01-01 00:00:40')))
#' dataToRanges(gps)
#'
#' dataToRanges(gps, buffer = c(.05, .05, 86400))
#'
#' @export
#'
dataToRanges <- function(data, buffer = c(0, 0, 0)) {
    oldNames <- colnames(data)
    newNames <- standardCoordNames(oldNames)
    colnames(data) <- newNames
    if(!all(c('UTC', 'Latitude', 'Longitude') %in% newNames)) {
        stop('Must have columns UTC, Longitude, and Latitude in your data.')
    }
    if(!inherits(data$UTC, 'POSIXct')) {
        stop('Please convert date column to POSIXct first.')
    }
    # if weve crossed the dateline longitude range is different
    crossDateline <- checkDateline(data)
    if(crossDateline) {
        longRange <- c(
            min(data$Longitude[data$Longitude > 0]) - buffer[1],
            max(data$Longitude[data$Longitude < 0]) + buffer[1]
        )
    } else {
        # hmm... if we subtract past -180 or add past 180 thats a problem
        # but second one only if we are actually a 180 not a 360
        longRange <- range(data$Longitude) + buffer[1] * c(-1, 1)
    }

    list(
        Longitude = longRange,
        Latitude = range(data$Latitude) + buffer[2] * c(-1, 1),
        UTC = range(data$UTC) + buffer[3] * c(-1, 1)
    )
}
