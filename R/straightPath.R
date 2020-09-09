#' @title Mark Straight Path Segments in GPS Track
#'
#' @description This function attempts to mark portions of a GPS track where a
#'   ship is traveling in a straight line by comparing the recent average
#'   heading with a longer term average heading. If these are different, then the
#'   ship should be turning. Note this currently does not take in to account time,
#'   only number of points
#'
#' @param gps gps data with columns Longitude, Latitude, and UTC (POSIX format).
#'   Usually this has been read in from a Pamguard database, in which case columns
#'   Heading and Speed will also be used.
#' @param nSmall number of points to average to get ship's current heading
#' @param nLarge number of points to average to get ship's longer trend heading
#' @param thresh the amount which \code{nSmall} and \code{nBig} should differ by to
#'   call this a turn
#' @param plot logical flag to plot result, \code{gps} must also have columns
#'   Latitude and Longitude
#'
#' @return the original dataframe \code{gps} with an added logical column
#'   \code{straight} indicating which portions are approximately straight
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#'
#' gps <- data.frame(Latitude = c(32, 32.1, 32.2, 32.2, 32.2),
#'                   Longitude = c(-110, -110.1, -110.2, -110.3, -110.4),
#'                   UTC = as.POSIXct(c('2000-01-01 00:00:00', '2000-01-01 00:00:10',
#'                                      '2000-01-01 00:00:20', '2000-01-01 00:00:30',
#'                                      '2000-01-01 00:00:40')),
#'                   Heading = c(320, 320, 270, 270, 270),
#'                   Speed = c(.8, .8, .5, .5, .5))
#'
#' straightPath(gps, nSmall=1, nLarge=2)
#'
#' straightPath(gps, nSmall=1, nLarge=4)
#'
#' @importFrom RcppRoll roll_meanr roll_sumr
#' @importFrom ggplot2 ggplot aes geom_path scale_color_manual
#' @importFrom geosphere bearing
#' @export
#'
straightPath<- function(gps, nSmall = 10, nLarge = 60, thresh = 10, plot = FALSE) {
    if(is.null(gps$Heading) ||
       any(is.na(gps$Heading))) {
        # stop('Heading data missing. Tell Taiki to make it work without heading data.')
        head <- bearing(matrix(c(gps$Longitude[1:(nrow(gps)-1)], gps$Latitude[1:(nrow(gps)-1)]), ncol=2),
                           matrix(c(gps$Longitude[2:(nrow(gps))], gps$Latitude[2:(nrow(gps))]), ncol=2)) %% 360
        gps$Heading <- c(head, head[length(head)])
    }

    # if(is.null(gps$Speed) ||
    #    any(is.na(gps$Speed))) {
    #     stop('Speed data is missing. Tell Taiki to make it work withou speed data.')
    # }
    gps$realHead <- cos(gps$Heading * pi / 180)
    gps$imHead <- sin(gps$Heading * pi / 180)
    smallLag <- Arg(complex(real=roll_sumr(gps$realHead, n=nSmall, fill=NA),
                            imaginary=roll_sumr(gps$imHead, n = nSmall, fill = NA))) * 180 / pi
    bigLag <- Arg(complex(real=roll_sumr(gps$realHead, n=nLarge, fill=NA),
                          imaginary=roll_sumr(gps$imHead, n = nLarge, fill = NA))) * 180 / pi

    gps$timeDiff <- gps$UTC - c(gps$UTC[1], gps$UTC[1:(nrow(gps)-1)])
    # 1 knot = .51444 m/s, not currently using distance
    # gps$distApprox <- as.numeric(gps$timeDiff) * gps$Speed * .51444
    # this >10 is so that we dont connect big jumps if there's a disconnect in the gps track
    # trying >30 because 10 broke i tup too much
    # so if theres ever a jump of more than 30 seconds it wont connect those 30s apart pieces
    gps$timeGroup <- as.factor(cumsum(gps$timeDiff > 30))
    gps$headDiff <- (bigLag - smallLag) %% 360
    gps$headDiff <- ifelse(gps$headDiff > 180, gps$headDiff - 360, gps$headDiff)
    gps$straight <- abs(gps$headDiff) < thresh
    # gps <- gps[-c(1:(nLarge-1)),]

    # why did i do this timeGroup thing??? and Im not sure its actually doing whatever i thought
    # it was doing in the first place
    # its for the colors in geom_path to work, bruh
    if(plot) {
        # myPlot <- ggplot(gps, aes(x=Longitude, y=Latitude)) +
        #     geom_path(aes(group=timeGroup, col=straight), size = 1.3) +
        #     scale_color_manual(values = c('red', 'darkgreen'))
        gpsEnds <- gps[c(1, nrow(gps)), ]
        gpsEnds$Type <- c('Start', 'End')
        myPlot <- ggplot(gps, aes_string(x='Longitude', y='Latitude')) +
            geom_point(data=gpsEnds, aes_string(x='Longitude', y='Latitude', shape='Type', col='straight'), size=4) +
            geom_path() +
            geom_path(aes_string(group='timeGroup', col='straight'), size = 1.3) +
            # geom_path(aes(col=straight), arrow=arrow()) +
            scale_color_manual(limits=c(TRUE, FALSE), values = c('darkgreen','red')) +
            scale_shape_manual(limits=c('Start', 'End'), values = c(16,7)) +
            guides(color=guide_legend(override.aes=list(shape=32), title='Straight'),
                   shape=guide_legend(title='Endpoint', override.aes=list(color=ifelse(gpsEnds$straight, 'darkgreen', 'red'))))
        print(myPlot)

    }
    gps
}
