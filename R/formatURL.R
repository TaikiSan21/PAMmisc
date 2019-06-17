#' @title Format URL for Downloading Environmental Data
#'
#' @description This function creates a properly formatted URL for downloading
#'   environmental data. Users can either provide the base URL, dataset ID, and
#'   other parameters on their own or select from a list of curated options (see
#'   ?CHECKMYDATASHITYO for details on this list).
#'
#' @param base the base URL to download from
#' @param dataset the dataset identifier to download
#' @param fileType the fileType to download
#' @param vars a vector of the names of variables to download
#' @param source the source from which the data will be downloaded. This argument
#'   is used for formatting purposes and does not affect the base URL.
#' @param bounds a list of the bounds for the data to download. For erddap and
#'   hycom data this should be a list with three vectors containing the min/max
#'   values for time, latitude, and longitude in that order. For example:
#'   \code{list(c(timeMin, timeMax), c(latMin, latMax), c(lonMin, lonMax))}. All
#'   time values must be POSIXct.
#' @param stride stride interval for downloading data. A value of 1 (default)
#'   gets every data point, 2 gets every other data point, etc. The same value
#'   is applied to all dimensions.
#'
#' @details
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom lubridate with_tz
#' @export
#'
formatURL <- function(base=NULL, dataset, fileType, vars, source = c('erddap', 'hycom'), bounds, stride=1) {
    if(is.null(base)) {
        # choose from our list
        # Check compatibility of provided bounds and selected bounds
        edChoose <- menu(title = 'Select an environmental dataset:',
                         choices = names(envDatalist))
        if(edChoose == 0) stop('No dataset selected.')
        base <- envDatalist[[edChoose]]
        # Choose variables here?
    }
    if('edInfo' %in% class(base)) {
        baseData <- edMatchTime(base, bounds[[1]])
        # get args from list
        dataset <- baseData$dataset
        fileType <- baseData$fileType
        vars <- names(baseData$vars)[baseData$vars] # vars is a logical vector to select from available vars
        source <- baseData$source
        base <- baseData$base
    }
    if(length(dataset) > 1) {
        result <- vector('character', length = length(dataset))
        startTime <- bounds[[1]][1]
        for(i in seq_along(dataset)) {
            thisDataset <- dataset[i]
            thisBounds <- bounds
            thisBounds[[1]][1] <- startTime
            endTime <- min(baseData$range$time[[i]][2], bounds[[1]][2])
            thisBounds[[1]][2] <- endTime
            startTime <- endTime + baseData$resolution$time
            result[i] <- formatURL(base=base, dataset = thisDataset, fileType = fileType, vars = vars,
                                   source = source, bounds = thisBounds, stride=stride)
        }
        return(result)
    }
    # Check in bounds warn and truncate here

    # At some point here check the base / dataset for ones that need to be read different E/W of 180
    # and make longitude 0-360 or -180 180
    switch(source,
           'erddap' = formatURL_erddap(base=base, dataset=dataset, fileType=fileType,
                                       vars=vars, bounds=bounds, stride=stride),
           'hycom' = formatURL_hycom(base=base, dataset=dataset, fileType=fileType,
                                     vars=vars, bounds=bounds, stride=stride),
           stop('I dont know how to deal with source', source, '.')
    )
}

#' @rdname formatURL
#' @export
#'
formatURL_erddap <- function(base, dataset, fileType='nc', vars, bounds, stride=1) {
    allBounds <- formatBounds_erddap(bounds, stride, html=FALSE)
    paste0(base,
           dataset,
           '.', fileType, '?',
           paste0(vars, allBounds, collapse=',')
    )
}

#' @rdname formatURL
#' @export
#'
formatURL_hycom <- function(base, dataset, fileType='netcdf', vars, bounds, stride=1) {
    allBounds <- formatBounds_hycom(bounds, stride)
    paste0(base,
           dataset,
           '?', paste0('var=', vars, collapse = '&'),
           allBounds,
           '&vertCoord=',
           '&accept=', fileType)
}

formatBounds_hycom <- function(bounds, stride=1, html=TRUE) {
    # time / lat[lo-hi] / long[left-right]
    paste0('&north=', bounds[[2]][2],
           '&west=', bounds[[3]][1],
           '&east=', bounds[[3]][2],
           '&south=', bounds[[2]][1],
           '&horizStride=', stride,
           '&time_start=', posixTo8601(bounds[[1]][1], html=html),
           '&time_end=', posixTo8601(bounds[[1]][2], html=html),
           '&timeStride=', stride)
}

formatBounds_erddap <- function(bounds, stride=1, html=FALSE) {
    # Change times to proper format
    paste0(sapply(bounds, function(x) {
        if('POSIXct' %in% class(x)) {
            x <- posixTo8601(x, html=html)
        }
        paste0('[(',
               x[1],
               '):', stride, ':(',
               x[2],
               ')]')
    }
    ), collapse = '')
}

posixTo8601 <- function(date, html = FALSE) {
    if(!('POSIXct' %in% class(date))) stop('Date must be POSIXct')
    result <- format(with_tz(date, 'UTC'), format = '%Y-%m-%dT%H:%M:%SZ')
    if(html) {
        gsub(':', '%3A', result)
    } else {
        result
    }
}

edMatchTime <- function(base, times) {
    # times vector of start/stop POSIXct UTC
    dataset <- base$dataset
    if(length(dataset) == 1) {
        return(base)
    }
    # browser()
    first <- which(sapply(base$range$time, function(x) (times[1] <= x[2]) & (times[1] >= x[1])))
    last <- which(sapply(base$range$time, function(x) (times[2] >= x[1]) & (times[2] <= x[2])))
    if(length(first) == 0 ||
       length(last) == 0) {
        stop('Time range is not in the range of this dataset.')
    }
    # if they are in same sets, just pick one (boundary cases)
    if(identical(first, last)) {
        first <- first[1]
        last <- last[1]
    }
    # if they are in more than one, only need most extreme
    first <- max(first)
    last <- min(last)
    base$dataset <- base$dataset[first:last]
    base$range <- lapply(base$range, function(x) x[first:last])
    base
}
