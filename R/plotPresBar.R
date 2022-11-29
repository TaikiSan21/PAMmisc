#' @title plotPresBar
#'
#' @description Creates a bar plot of the presence or density of detections across time
#'
#' @param x a data.frame of detections, must have a column \code{UTC} that contains the
#'   time of detection as a POSIXct object in UTC timezone
#' @param start the beginning datetime of the plot, if \code{NULL} will be set to the minimum
#'   time in \code{x}
#' @param end the ending datetime of the plot, if \code{NULL} will be set to the maximum
#'   time in \code{x}
#' @param timeBin the unit of time for each bar in the plot, must be one of "hour", "day",
#'   "week", or "month"
#' @param type one of either "presence" or "density". If "density", then the total number of
#'   detections in each \code{timeBin} will be plotted. If "presence", then you must also set
#'   \code{presBin} to define the time scale for binning detections
#' @param presBin one of either "hour", "day", "week", or "minute", and must be a smaller unit
#'   of time than \code{timeBin}. This defines the timescale for counting presence. For example,
#'   \code{timeBin} of "day" and \code{presBin} of "hour" will plot the number of hours in each day
#'   that have detections.
#' @param by (optional) if not \code{NULL}, specifies the name of a column in \code{x} to split and
#'   color the bars by
#' @param title if \code{TRUE}, a title will automatically created. If any other value, that will be
#'   used for the title of the plot.
#' @param fill the fill color for the bars, only used if \code{by} is \code{NULL}, otherwise bars are
#'   colored by species using the default \link{ggplot2} palette
#' @param format date format if \code{UTC} column of \code{x} is a character
#'
#' @returns a ggplot2 object
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#'
#' df <- data.frame(UTC = as.POSIXct(runif(1e2, min=0, max=7*24*3600),
#'                                   origin='1970-01-01 00:00:00', tz='UTC'),
#'                  label = sample(letters[1:3], 1e3, replace=TRUE))
#' plotPresBar(df, type='presence', timeBin='day', presBin='hour')
#' plotPresBar(df, type='density', timeBin='day')
#' plotPresBar(df, type='density', timeBin='day', by='label')
#'
#' @export
#'
#' @importFrom lubridate floor_date hour period minute
#' @import ggplot2
#' @import dplyr
#'
plotPresBar <- function(x, start=NULL, end=NULL,
                        timeBin=c('day', 'week', 'month', 'hour'),
                        type=c('presence', 'density'),
                        presBin = c('hour', 'day', 'week', 'minute'),
                        by=NULL, title=TRUE, fill='grey35',
                        format = c('%m/%d/%Y %H:%M:%S', '%m-%d-%Y %H:%M:%S',
                                   '%Y/%m/%d %H:%M:%S', '%Y-%m-%d %H:%M:%S')) {
    type <- match.arg(type)
    timeBin <- match.arg(timeBin)
    if(!'UTC' %in% colnames(x)) {
        stop('"x" must have column UTC')
    }
    if(is.character(x$UTC) ||
       is.factor(x$UTC)) {
        x$UTC <- parseToUTC(as.character(x$UTC), format=format, tz='UTC')
    }
    x$timeBin <- floor_date(x$UTC, unit=timeBin)

    if(is.null(start)) {
        start <- min(x$UTC)
        start <- start - period(1, units=timeBin)
    }
    if(is.null(end)) {
        end <- max(x$UTC)
        end <- end + period(1, units=timeBin)
    }
    start <- floor_date(start, unit=timeBin)
    end <- floor_date(end, unit=timeBin) + period(1, timeBin)

    if(type=='presence') {
        presBin <- match.arg(presBin)
        if(period(1, presBin) / period(1, timeBin) >= 1) {
            stop('Cannot create count of ', presBin, ' per ', timeBin)
        }
        x$presBin <- floor_date(x$UTC, unit=presBin)
        x <- select(x, any_of(c('presBin', 'timeBin', by)))
        x <- distinct(x)
        ylab <- paste0(oneUp(presBin), 's')
    }
    if(type == 'density') {
        ylab <- 'Calls'
    }
    tlab <- paste0(ylab, '/', oneUp(timeBin))
    g <- ggplot() +
        labs(x='Date', y=paste0('Count (', ylab, ')')) +
        scale_y_continuous(expand=expansion(mult=c(0,.05))) +
        scale_x_datetime(limits=c(start, end), expand=c(0,0))
    if(is.null(by)) {
        g <- g +
            geom_bar(data=x, aes_string(x='timeBin'), fill=fill)
    } else {
        g <- g +
            geom_bar(data=x, aes_string(x='timeBin', fill=by))
    }
    if(isTRUE(title)) {
        title <- paste0('Call ', oneUp(type), ' (', tlab, ')')
        if(!is.null(by)) {
            title <- paste0(title, ' by "', by, '"')
        }
    }
    g + ggtitle(title)
}

oneUp <- function(s) {
    paste0(toupper(substring(s, 1, 1)), substring(s, 2))
}