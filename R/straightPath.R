#' @title Mark Straight Path Segments in GPS Track
#'
#' @description This function attempts to mark portions of a GPS track where a
#'   ship is travelling in a straight line by comparing the recent average
#'   heading with a longer term average heading. If these are different, then the
#'   ship should be turning. Note this currently does not take in to account time,
#'   only number of points
#'
#' @param gps gps data with columns Heading and UTC (POSIX format)
#' @param nSmall number of points to average to get ship's current heading
#' @param nLarge number of points to average to get ship's longer trend heading
#' @param thresh the amount which \code{nSmall} and \code{nBig} should differ by to
#'   call this a turn
#' @param plot logical flag to plot result, \code{gps} must also have columns
#'   Latitude and Longitude
#'
#' @details
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom RcppRoll roll_meanr roll_sumr
#' @importFrom ggplot2 ggplot aes geom_path scale_color_manual
#' @export
#'
straightPath<- function(gps, nSmall = 10, nLarge = 60, thresh = 10, plot = FALSE) {
    gps$realHead <- cos(gps$Heading * pi / 180)
    gps$imHead <- sin(gps$Heading * pi / 180)
    smallLag <- Arg(complex(real=roll_sumr(gps$realHead, n=nSmall, fill=NA),
                            imaginary=roll_sumr(gps$imHead, n = nSmall, fill = NA))) * 180 / pi
    bigLag <- Arg(complex(real=roll_sumr(gps$realHead, n=nLarge, fill=NA),
                          imaginary=roll_sumr(gps$imHead, n = nLarge, fill = NA))) * 180 / pi

    gps$timeDiff <- gps$UTC - c(gps$UTC[1], gps$UTC[1:(nrow(gps)-1)])

    # this >10 is so that we dont connect big jumps if there's a disconnect in the gps track
    # trying >30 because 10 broke i tup too much
    # so if theres ever a jump of more than 30 seconds it wont connect those 30s apart pieces
    gps$timeGroup <- as.factor(cumsum(gps$timeDiff > 30))
    gps$headDiff <- (bigLag - smallLag) %% 360
    gps$headDiff <- ifelse(gps$headDiff > 180, gps$headDiff - 360, gps$headDiff)
    gps$straight <- abs(gps$headDiff) < thresh
    gps <- gps[-c(1:(nLarge-1)),]

    # why did i do this timeGroup thing??? and Im not sure its actually doing whatever i thought
    # it was doing in the first place
    # its for the colors in geom_path to work, bruh
    if(plot) {
        myPlot <- ggplot(gps, aes(x=Longitude, y=Latitude)) +
            geom_path(aes(group=timeGroup, col=straight), size = 1.3) +
            scale_color_manual(values = c('red', 'darkgreen'))
        # scale_size_manual(values = c(2, 1))
        print(myPlot)
    }
    gps
}
