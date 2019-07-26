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
#' @importFrom ggplot2 ggplot aes geom_path scale_color_manual
#' @export
#'
 straightPath<- function(gps, nSmall = 10, nLarge = 60, thresh = 10, plot = FALSE) {
    smallLag <- roll_meanr(gps$Heading, n = nSmall, fill = NA)
    bigLag <- roll_meanr(gps$Heading, n = nLarge, fill = NA)
    gps$timeDiff <- gps$UTC - c(gps$UTC[1], gps$UTC[1:(nrow(gps)-1)])
    gps$timeGroup <- as.factor(cumsum(gps$timeDiff > 10))
    gps$headDiff <- bigLag - smallLag
    gps$straight <- gps$headDiff < thresh
    gps <- gps[-c(1:(nLarge-1)),]
    if(plot) {
        # strt <- gps[gps$straight, ]
        # turn <- gps[!gps$straight, ]
        # plot(x=strt$Longitude, y=strt$Latitude, col='darkgreen', type='l')
        # lines(x=turn$Longitude, y=turn$Latitude, col='red', lwd = 2)
        myPlot <- ggplot(gps, aes(x=Longitude, y=Latitude, col=straight)) +
            geom_path(aes(group=timeGroup), size = 1.3) +
            scale_color_manual(values = c('red', 'darkgreen'))
            # scale_size_manual(values = c(2, 1))
        print(myPlot)
        }
    gps
}
