#' @title Get a Map Fitted to Data
#'
#' @description Return a ggmap object centered on the middle of the positions
#'    data. In online mode will download from Google Maps, will start zoomed
#'    in then gradually zoom out until all points fit unless force is TRUE. In
#'    offline mode will return a map just large enough to contain all the
#'    positions data.
#'
#' @param positions data frame containing columns \code{Latitude} and \code{Longitude}
#' @param source the source for the map. Can be either \code{google}, \code{stamen}, or
#'   \code{offline}. \code{google} will load the smallest Google Map that fits the data.
#'   \code{stamen} will load a 'toner-lite' style stamen map. Both \code{stamen} and
#'   \code{offline} will load a map slightly larger than the range of the data. Google
#'   Maps option requires setting up a Google API, please search ggmap help for more info.
#' @param bounds optional, the boundaries of a box that must be within the output plot.
#'   Used to extend the plotted area, especially useful for the offline mode. Specified as
#'   the lat / long coordinates of the lower left and upper right corners.
#' @param zoom zoom level used for the map. An integer, see zoom description in
#'    get_map function of ggmap
#' @param force flag whether or not to force a specific zoom level instead of
#'   finding it automatically
#' @param center the location to center the map on. If \code{NULL}, will use the
#'   mean of the range of the data. If not \code{NULL} it must be a named vector
#'   with values \code{lon} and \code{lat}
#' @param quiet \code{FALSE} to report zoom level used
#'
#' @return a ggmap object, fitted to the coordinates of the positions data
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#'
#' library(ggplot2)
#' data <- data.frame(Longitude = c(-117, -118), Latitude = c(32, 33.5))
#' \donttest{
#' # Online mode, gets smallest Google Map that fits data
#' map <- getFittedMap(data)
#' map
#' map + geom_point(data = data, aes(x = Longitude, y = Latitude))
#'
#' # Offline mode, fits map closely to data
#' map <- getFittedMap(data, mode = 'offline')
#' map
#' map + geom_point(data = data, aes(x = Longitude, y = Latitude))
#'
#' # Offline mode, expand bounds for nicer looking map
#' map <- getFittedMap(data, mode = 'offline', bounds = c(32, -120, 34, -117))
#' map
#' map + geom_point(data = data, aes(x = Longitude, y = Latitude))
#' }
#' @import ggplot2
#' @importFrom ggmap get_map ggmap get_stamenmap
#' @export
#'
getFittedMap <- function(positions, source = 'stamen', bounds=NULL, zoom=14, force=FALSE, center=NULL, quiet=TRUE) {
    latOptions <- c('Latitude', 'latitude', 'lat')
    lonOptions <- c('Longitude', 'longitude', 'lon')
    hasLat <- latOptions[which(latOptions %in% colnames(positions))]
    hasLon <- lonOptions[which(lonOptions %in% colnames(positions))]
    if(length(hasLat) != 1 ||
       length(hasLon) != 1) {
        stop('positions data must have one "Latitude" and one "Longitude" column.')
    }
    positions <- positions[, c(hasLat, hasLon)]
    # We cant automatically map near the poles yet, just stop for now.
    poleThresh <- 70
    if(max(abs(positions[[hasLat]])) > poleThresh) {
        stop('It looks like you are near one of the poles. Automatic mapping not yet supported here. Sorry!')
    }
    # If we tried to zoome out this far something bad happened.
    if(zoom<2) {
        stop('Cannot use Zoom 0 or 1. Check coordinates for errors.')
    }
    # Get our map with smallest bounding box of data, or specified box
    positions <- fixDateline(positions, hasLon)
    if(is.null(bounds)) {
        boundLong <- range(positions[[hasLon]])
        boundLat <- range(positions[[hasLat]])
    } else {
        boundLong <- bounds[c(2,4)]
        boundLat <- bounds[c(1,3)]
    }

    if(is.null(center)) {
        center <- c(lon=mean(boundLong), lat=mean(boundLat))
    }
    # Try downloading up to three times - fails quite often, but will work on second try
    if(!offline) {
        map <- NULL
        nTries <- 3
        map <- switch(source,
                      'google' = .getGoogleMap(center = center, zoom = zoom),
                      'stamen' = .getStamenMap(bbox = c(left = boundLong[1],
                                                        bottom = boundLat[1],
                                                        right = boundLong[2],
                                                        top = boundLat[2])),
                      'offline' = .getOfflineMap(positions),
                      .getOfflineMap(positions))
        suppressMessages(
            for(t in 1:nTries) {
                try(
                    switch(source,
                           'google' = map <- get_map(location = center, zoom=zoom),
                           'stamen' = map <- get_stamenmap(bbox = c(left = boundLong[1],
                                                                    bottom = boundLat[1],
                                                                    right = boundLong[2],
                                                                    top = boundLat[2]),
                                                           maptype = 'toner-lite', zoom = zoom), # ZOOM IZ BROAK
                           NULL),
                    silent = TRUE)
                if(!is.null(map)) break
            }
        )
        # If still null we couldnt download, so switch to offline mode
        if(is.null(map)) {
            warning('Unable to download map, switching to offline mode.')
            return(getFittedMap(positions, zoom=zoom, quiet=quiet, source='offline'))
        }
        # Checking if all points are within map range. If not, zoom out 1.
        mapRange <- attr(map, 'bb')
        if(!force & (
            boundLong[1] < mapRange[2] |
            boundLong[2] > mapRange[4] |
            boundLat[1] < mapRange[1] |
            boundLat[2] > mapRange[3])) {
            # statement mostly useful for debugging
            # cat('Zoom level', zoom, 'is too close. Trying', zoom-1,'. \n')
            return(getFittedMap(positions, zoom=zoom-1, quiet=quiet, source = source))
        }
        if(!quiet) {
            cat('Zoom level', zoom, 'being used. \n')
        }
        return(ggmap(map))
    }
    # If we're in offline, or if above loop didn't succeed
    if(offline || is.null(map)) {
        coastdata <- readRDS(system.file('coastlines/coastlines.RData', package='PAMmisc'))
        basemap <- ggplot() + geom_map(data=coastdata, map=coastdata, aes(map_id=id), fill='#d0e3b4', color='black') +
            theme(panel.grid = element_blank(), panel.background = element_rect(fill='#A3CCFF'))
        # If no specific box given, need to extend the x axis a bit or it looks weird later
        if(is.null(bounds)) {
            longRange <- boundLong[2]-boundLong[1]
            boundLong[1] <- boundLong[1] - longRange * .1
            boundLong[2] <- boundLong[2] + longRange * .1
        }
        bnds <- data.frame(lllat = boundLat[1], lllong = boundLong[1],
                           urlat = boundLat[2], urlong = boundLong[2])
        # We cant plot directly on this map, coord_map breaks the fill, but we need the aspect ratio.
        coordmap <- basemap + geom_rect(data=bnds, aes(xmin=lllong, xmax=urlong, ymin=lllat, ymax=urlat), fill=NA) +
            coord_map()

        maprange <- ggplot_build(coordmap)$layout$panel_params[[1]]
        aspect <- with(maprange, (y.proj[2]-y.proj[1])/(x.proj[2]-x.proj[1]))
        # Add a blank rectangle to extend plotted range
        map <- basemap +
            theme(aspect.ratio = aspect) +
            geom_rect(data=bnds, aes(xmin=lllong, xmax=urlong, ymin=lllat, ymax=urlat), fill=NA)
        # geom_rect(aes(xmin=boundLong[1], xmax=boundLong[2], ymin=boundLat[1], ymax=boundLat[2]), fill=NA)
        return(map)
    }
}

.getGoogleMap <- function(center, zoom, force, boundLong, boundLat, quiet) {
    map <- NULL
    nTries <- 3
    suppressMessages(
        for(i in seq_along(nTries)) {
            try(
                map <- get_map(location = center, zoom = zoom),
                silent = TRUE
            )
            if(!is.null(map)) break
        }
    )
    if(is.null(map)) {
        return(NULL)
    }
    mapRange <- attr(map, 'bb')
    if(!force & (
        boundLong[1] < mapRange[2] |
        boundLong[2] > mapRange[4] |
        boundLat[1] < mapRange[1] |
        boundLat[2] > mapRange[3])) {
        # statement mostly useful for debugging
        # cat('Zoom level', zoom, 'is too close. Trying', zoom-1,'. \n')
        return(.getGoogleMap(center = center, zoom=zoom-1, force=force, quiet=quiet))
    }
    if(!quiet) {
        cat('Zoom level', zoom, 'being used. \n')
    }
    return(ggmap(map))
}

