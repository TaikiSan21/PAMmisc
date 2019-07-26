#' @title Mark Straight Path Segments in GPS Track
#'
#' @description This function attempts to mark portions of a GPS track where a
#'   ship is travelling in a straight line by comparing the recent average
#'   heading with a longer term average heading. If these are different, then the
#'   ship should be turning. Note this currently does not take in to account time,
#'   only number of points
#'
#' @param gps gps data with column Heading
#' @param nSmall number of points to average to get ship's current heading
#' @param nLarge number of points to average to get ship's longer trend heading
#' @param thresh the amount which \code{nSmall} and \code{nBig} should differ by to
#'   call this a turn
#' @param plot logical flag to plot result
#'
#' @details
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom RcppRoll roll_meanr
#' @export
#'
 straightPath<- function(gps, nSmall = 10, nLarge = 60, thresh = 10, plot = FALSE) {
    smallLag <- roll_meanr(gps$Heading, n = nSmall, fill = NA)
    bigLag <- roll_meanr(gps$Heading, n = nLarge, fill = NA)
    gps$diff <- bigLag - smallLag
    gps$straight <- gps$diff < thresh
    if(plot) {
        plot(x=gps$Longitude, y=gps$Latitude, col=gps$straight, type='l')
    }
    gps[-c(1:(nLarge-1)),]
}
