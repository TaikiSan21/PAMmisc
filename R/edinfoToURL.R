#' @title Create a URL for Downloading Data from a edinfo Object
#'
#' @description Creates a properly formatted URL (see \link{formatURL}) from
#'   a datalist either from the package's recommended sources or an ERDDAP
#'   dataset id
#'
#' @param edinfo a edinfo class object, either from \link{getEdinfo} or created by
#'   \link{erddapToEdinfo}
#' @param ranges list of ranges for Longitude, Latitude, and UTC. Must be a
#'   named list with a vector of min/max values for each of the three dimensions
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @export
#'
edinfoToURL <- function(edinfo, ranges) {
    # can be a dataset id, convert first
    if(is.character(edinfo)) {
        edinfo <- erddapToEdinfo(edinfo)
    }
    if(is.null(edinfo$varSelect)) {
        # list vars avail, loop through each to set T/F
        edinfo <- varSelect(edinfo)
    }
    if(sum(edinfo$varSelect) == 0) {
        stop('No variables selected for download.')
    }
    if('Depth' %in% names(edinfo$limits) &&
       !('Depth' %in% names(ranges))) {
        ranges$Depth <- edinfo$limits$Depth
    }
    if(inherits(ranges$UTC, 'POSIXct') &&
       identical(edinfo$limits$UTC, c(1, 365))) {
        ranges$UTC <- lubridate::yday(ranges$UTC)
    }
    # WHY DOES ERDDAP SOMETIMES HAVE BACKWARDS COORDS
    # Only found for latitude so far, but including lon just in case
    for(d in c('Latitude', 'Longitude')) {
        if(edinfo$spacing[[d]] < 0) {
            ranges[[d]] <- rev(ranges[[d]])
        }
    }
    formatURL(base = edinfo$base,
              dataset = edinfo$dataset,
              fileType = edinfo$fileType,
              vars = edinfo$vars[edinfo$varSelect],
              ranges = ranges,
              style = edinfo$source)
}
