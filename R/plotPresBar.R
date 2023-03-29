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
#' @param bin string identifying how to bin detections. Acceptable time units are
#'   \code{c('minute', 'hour', 'day', 'week', 'month')}. For presence, \code{bin}
#'   should be of the form \code{'unit1/unit2'}, e.g. \code{'hour/day'} will show
#'   the hours per day with detections. For call density, \code{bin} is a single
#'   time unit, e.g. \code{'hour'} will show the number of calls per hour. Call
#'   density can also be specified as \code{'call/hour'}. Note that plural forms
#'   of all units are accepted.
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
#'                  label = sample(letters[1:3], 1e2, replace=TRUE))
#' # hours per day with detections
#' plotPresBar(df, bin='hour/day')
#' # calls per day - these options are identical
#' plotPresBar(df, bin='day')
#' plotPresBar(df, bin='call/day')
#' plotPresBar(df, bin='calls/day')
#' # calls per day, colored by 'label'
#' plotPresBar(df, bin='day', by='label')
#'
#' @export
#'
#' @importFrom lubridate floor_date hour period minute
#' @import ggplot2
#' @import dplyr
#'
plotPresBar <- function(x, start=NULL, end=NULL,
                        bin = 'hour/day',
                        by=NULL, title=TRUE, fill='grey35',
                        format = c('%m/%d/%Y %H:%M:%S', '%m-%d-%Y %H:%M:%S',
                                   '%Y/%m/%d %H:%M:%S', '%Y-%m-%d %H:%M:%S')) {
    binChoice <- c('call', 'minute', 'hour', 'day', 'week', 'month')
    binSplit <- strsplit(bin, '/')[[1]]
    binSplit <- gsub('s$', '', binSplit)
    switch(length(binSplit),
           '1' = {
               type <- 'density'
               timeBin <- match.arg(binSplit, binChoice)

           },
           '2' = {
               if(binSplit[1] == 'call') {
                   type <- 'density'
                   timeBin <- match.arg(binSplit[2], binChoice)
               } else {
                   type <- 'presence'
                   timeBin <- match.arg(binSplit[2], binChoice)
                   presBin <- match.arg(binSplit[1], binChoice)
               }
           }
    )
    # type <- match.arg(type)
    # timeBin <- match.arg(timeBin)
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
        # presBin <- match.arg(presBin)
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