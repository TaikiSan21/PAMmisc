#' @title plotPresGrid
#'
#' @description Creates a grid plot of the presence or density of detections across time
#'   where the x-axis is the hour of the day and the y-axis is the date
#'
#' @param x a data.frame of detections, must have a column \code{UTC} that contains the
#'   time of detection as a POSIXct object in UTC timezone
#' @param start the beginning datetime of the plot, if \code{NULL} will be set to the minimum
#'   time in \code{x}
#' @param end the ending datetime of the plot, if \code{NULL} will be set to the maximum
#'   time in \code{x}
#' @param timeBin the unit of time for each rectangle in the grid, must be one of "hour", "minute",
#'   "30min", or "15min"
#' @param type one of either "presence" or "density". If "density", then boxes will be colored according
#'   to the number of detections in each \code{timeBin} will be plotted. If "presence", then each box
#'   will be colored by \code{fill}
#' @param gps (optional) if not \code{NULL}, a data.frame of GPS coordinates covering the date range
#'   of \code{x}. These are used to calculate sunrise and sunset information which is shown as a
#'   shaded dark region in the background of the plot. The data.frame must have columns "UTC",
#'   "Latitude", and "Longitude". If columns "Latitude" and "Longitude" are present in \code{x},
#'   then these values will be used and you do not need to provide separate GPS data here
#' @param fill the fill color for the boxes, only used if \code{type} is "presence"
#' @param color the outline color for the boxes, only used if \code{type} is "presence"
#' @param cmap the colormap to use for the boxes, only used if \code{type} is "density"
#' @param title if \code{TRUE}, a title will automatically created. If any other value, that will be
#'   used for the title of the plot.
#'
#' @returns a ggplot2 object
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#'
#' df <- data.frame(UTC = as.POSIXct(runif(1e2, min=0, max=7*24*3600),
#'                                   origin='1970-01-01 00:00:00', tz='UTC'))
#' plotPresGrid(df, type='presence', timeBin='hour')
#' plotPresGrid(df, type='density', timeBin='hour')
#' plotPresGrid(df, type='density', timeBin='30min')
#' gps <- data.frame(UTC = as.POSIXct('1970-01-01 00:00:00', tz='UTC'),
#'                  Latitude=32.4,
#'                  Longitude = -118)
#' plotPresGrid(df, gps=gps, timeBin='hour')
#'
#' @export
#'
#' @importFrom lubridate floor_date hour period minute
#' @importFrom suncalc getSunlightTimes
#' @importFrom data.table setDT setDF setkeyv data.table
#' @importFrom viridisLite viridis
#' @import ggplot2
#' @import dplyr
#'
plotPresGrid <- function(x, start=NULL, end=NULL,
                         timeBin=c('hour', 'minute', '30min', '15min'),
                         type=c('presence', 'density'), gps=NULL, fill='blue', color=NA,
                         cmap=viridis(25), title=TRUE) {
    if(is.null(start)) {
        start <- floor_date(min(x$UTC), unit='day')
    }
    if(is.null(end)) {
        end <- floor_date(max(x$UTC), unit='day') + period(1, 'day')
    }
    if(is.null(gps) &&
       all(c('UTC', 'Latitude', 'Longitude') %in% colnames(x))) {
        gps <- x[c('UTC', 'Latitude', 'Longitude')]
    }
    if(is.null(gps)) {
        g <- ggplot()
    } else {
        sun_df <- makeSunriseDf(start, end, gps)
        g <- makeSunriseBackground(sun_df, start, end)
    }
    g <- g +
        scale_x_continuous(expand=c(0,0), limits=c(0, 24)) +
        scale_y_datetime(expand=c(0, 0), limits=c(start - period(0, 'day'), end + period(1, 'day'))) +
        labs(x='Hour')
    switch(match.arg(timeBin),
           'hour' = {
               x$plot_min = hour(x$UTC)
               x$plot_max = hour(x$UTC) + 1
               tlab <- 'Hour'
           },
           'minute' = {
               x$plot_min = hour(x$UTC) + minute(x$UTC)/60
               x$plot_max = x$plot_min + 1/60
               tlab <- 'Minute'
           },
           '30min' = {
               x$plot_min = hour(x$UTC) + floor(minute(x$UTC) / 30) * .5
               x$plot_max = x$plot_min + .5
               tlab <- '30 Minutes'
           },
           '15min' = {
               x$plot_min = hour(x$UTC) + floor(minute(x$UTC)/15) * .25
               x$plot_max = x$plot_min + .25
               tlab <- '15 Minutes'
           }
    )
    x$day <- floor_date(x$UTC, 'day')
    type <- match.arg(type)
    if(type == 'presence') {
        x <- select(x, all_of(c('plot_min', 'plot_max', 'day'))) %>%
            distinct()
        g <- g +
            geom_rect(data=x, aes(xmin=.data$plot_min, xmax=.data$plot_max, ymin=.data$day, ymax=.data$day + period(1, 'day')), fill=fill, col=color)
    }
    if(type == 'density') {
        x <- group_by(x, .data$day, .data$plot_min, .data$plot_max) %>%
            summarise(count=n(), .groups='keep')
        g <- g +
            geom_rect(data=x, aes(xmin=.data$plot_min, xmax=.data$plot_max, ymin=.data$day, ymax=.data$day + period(1, 'day'), fill=count)) +
            scale_fill_gradientn(colors=cmap)
        tlab <- paste0('Calls/', tlab)
    }
    if(isTRUE(title)) {
        title <- paste0('Call ', oneUp(type), ' (', tlab, ')')
    }
    g + ggtitle(title)
}

makeSunriseDf <- function(start, end, gps) {
    gpsCols <- c('UTC', 'Latitude', 'Longitude')
    if(!all(gpsCols %in% colnames(gps))) {
        stop('gps must have columns UTC, Latitude, and Longitude')
    }
    gps <- gps[gpsCols]
    sun_df <- data.table(UTC = seq(from=start, to=end, 'day'))
    setkeyv(sun_df, 'UTC')
    setDT(gps)
    setkeyv(gps, 'UTC')
    sun_df <- gps[sun_df, roll='nearest']
    setDF(sun_df)
    sun_df$Date <- as.Date(sun_df$UTC)
    sun_df$sunrise <- NA
    sun_df$sunset <- NA
    for(i in 1:nrow(sun_df)) {
        sun <- getSunlightTimes(sun_df$Date[i], sun_df$Latitude[i], sun_df$Longitude[i], keep=c('sunrise', 'sunset'))
        sun_df$sunrise[i] <- hour(sun$sunrise) + minute(sun$sunrise)/60
        sun_df$sunset[i] <- hour(sun$sunset) + minute(sun$sunset)/60
    }
    sun_df
}

makeSunriseBackground <- function(sun_df, start, end) {
    sun_df$rise_after <- sun_df$sunrise > sun_df$sunset
    g <- ggplot()
    if(any(sun_df$rise_after)) {
        g <- g +
            geom_rect(data=sun_df[sun_df$rise_after,],
                      aes(xmin=.data$sunset, xmax=.data$sunrise, ymin=.data$UTC, ymax=.data$UTC + period(1, 'day')), fill='lightgray')
    }
    if(any(!sun_df$rise_after)) {
        g <- g +
            geom_rect(data=sun_df[!sun_df$rise_after,],
                      aes(xmin=0, xmax=.data$sunrise, ymin=.data$UTC, ymax=.data$UTC+period(1, 'day')), fill='lightgray') +
            geom_rect(data=sun_df[!sun_df$rise_after,],
                      aes(xmin=.data$sunset, xmax=24, ymin=.data$UTC, ymax=.data$UTC+period(1, 'day')), fill='lightgray')
    }
    g
}
