#' @title Read Tracks from a GPX File
#'
#' @description Read in a GPX file and convert the tracks to a dataframe
#'
#' @param x a path to a .gpx file
#'
#' @return a dataframe with columns \code{Latitude}, \code{Longitude},
#'   \code{UTC}, and \code{Name}
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#'
#' gpxFile <- system.file('extdata', 'GPX.gpx', package='PAMmisc')
#' gpxData <- readGPXTrack(gpxFile)
#' str(gpxData)
#'
#' @importFrom xml2 read_xml xml_find_all xml_text xml_attr xml_find_first xml_ns_strip
#' @importFrom dplyr bind_rows
#' @export
#'
readGPXTrack <- function(x) {
    if(is.character(x)) {
        if(!file.exists(x)) {
            stop('File ', x, ' does not exist')
        }
        xml <- read_xml(x)
    } else if(inherits(x, 'xml_document')) {
        xml <- x
    }
    xml <- xml_ns_strip(xml)
    tracks <- xml_find_all(xml, '//trk')
    trackOut <- vector('list', length=length(tracks))
    for(i in seq_along(trackOut)) {
        oneTrack <- list(Name = xml_text(xml_find_first(tracks[i], '//name')))
        points <- xml_find_all(tracks[i], './/trkpt')
        oneTrack$Latitude <- as.numeric(xml_attr(points, 'lat'))
        oneTrack$Longitude <- as.numeric(xml_attr(points, 'lon'))
        oneTrack$UTC <- xml_text(xml_find_all(points, './/time'))
        oneTrack$Name <- rep(oneTrack$Name, length(oneTrack$UTC))
        trackOut[[i]] <- oneTrack
    }
    trackOut <- bind_rows(trackOut)
    trackOut$UTC <- as.POSIXct(trackOut$UTC, format='%Y-%m-%dT%H:%M:%SZ', tz='UTC')
    trackOut
}