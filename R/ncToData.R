#' @title Match Data From a Netcdf File
#'
#' @description Extracts all variables from a netcdf file matching Longitude,
#'   Latitude, and UTC coordinates in given dataframe
#'
#' @param data dataframe containing Longitude, Latitude, and UTC to extract matching
#'   variables from the netcdf file
#' @param nc name of a netcdf file
#' @param buffer vector of Longitude, Latitude, and Time (seconds) to buffer around
#'   each datapoint. All values within the buffer will be used to report the mean,
#'   median, and standard deviation
#' @param FUN a vector or list of functions to apply to the data. Default is to apply
#'   mean, median, and standard deviation calculations
#' @param raw logical flag to return only the raw values of the variables. If \code{TRUE}
#'   the output will be changed to a list with length equal to the number of data points.
#'   Each item in the list will have separate named entries for each variable that will have
#'   all values within the given buffer and all values for any Z coordinates present.
#' @param keepMatch logical flag to keep the matched coordinates, these are useful to make sure
#'   the closest point is actually close to your XYZT
#' @param progress logical flag to show progress bar for matching data
#' @param depth depth values (meters) to use for matching, overrides any \code{Depth} column
#'   in the data or can be used to specify desired depth range when not present in data.
#'   Variables will be summarised over the range of these depth values. \code{NULL}
#'   uses all available depth values
#' @param verbose logical flag to show warning messages for possible coordinate mismatch
#'
#' @return original dataframe with three attached columns for each variable in the netcdf
#'   file, one for each of mean, median, and standard deviation of all values within the buffer
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#'
#' data <- data.frame(Latitude = 32, Longitude = -117,
#'                    UTC = as.POSIXct('2005-01-01 00:00:00', tz='UTC'))
#' nc <- system.file('extdata', 'sst.nc', package='PAMmisc')
#' # default calculates mean
#' ncToData(data, nc = nc)
#' # calculate mean, median, and sd
#' ncToData(data, nc=nc, FUN=c(mean, median, sd), buffer = c(.01, .01, 86400))
#' # custom function
#' meanPlusOne <- function(x) {
#'     mean(x, na.rm=TRUE) + 1
#' }
#' ncToData(data, nc=nc, FUN=c(mean, meanPlusOne))
#'
#' @importFrom ncdf4 nc_open nc_close ncvar_get
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom purrr transpose
#'
#' @export
#'
ncToData <- function(data, nc, buffer = c(0,0,0), FUN = c(mean),
                     raw = FALSE, keepMatch=TRUE, progress=TRUE, depth=0, verbose=TRUE) {
    nc <- nc_open(nc)
    on.exit(nc_close(nc))
    nc <- romsCheck(nc)
    oldNames <- colnames(data)
    colnames(data) <- standardCoordNames(oldNames)
    # this finds the name of the longitude coordinate from my list that matches lon/long/whatever to Longitude
    nc180 <- ncIs180(nc)
    data180 <- dataIs180(data)

    if(nc180 != data180) {
        data <- to180(data, inverse = !nc180)
    }
    # for each variable, make the ncvar_get call which needs start and count#
    # these are XYZT if Z is present, -1 count means get all
    # OPTION TO ONLY GET A CERTAIN Z VALUE LIKE DEPTH FIRST VALUE SOMEHOW
    dropVar <- c('latitude', 'longitude', 'month', 'day', 'year', 'lon', 'lat')
    varNames <- names(nc$var)[!(names(nc$var) %in% dropVar)]
    names(nc$dim) <- standardCoordNames(names(nc$dim))
    if('Depth' %in% names(nc$dim)) {
        matchDims <- c('matchLong', 'matchLat', 'matchTime', 'matchDepth')
    } else {
        matchDims <- c('matchLong', 'matchLat', 'matchTime')
    }
    allVar <- vector('list', length = length(varNames) + length(matchDims))
    names(allVar) <- c(varNames, matchDims)
    for(v in seq_along(allVar)) {
        allVar[[v]] <- vector('list', length = nrow(data))
    }
    reqDims <- names(nc$dim)[names(nc$dim) %in% c('Longitude', 'Latitude', 'UTC')]
    if(!all(reqDims %in% names(data))) {
        stop('data must have columns ', paste0(reqDims, collapse=', '))
    }

    if(progress) {
        cat('Matching data...\n')
        pb <- txtProgressBar(min=0, max=nrow(data), style=3)
    }
    for(v in varNames) {
        names(nc$var[[v]]$dim) <- names(nc$dim)[nc$var[[v]]$dimids + 1]
    }
    for(i in 1:nrow(data)) {
        varData <- getVarData(data[i,], nc=nc, var=varNames, buffer = buffer,
                              depth=depth, verbose=verbose)
        for(v in varNames) {
            allVar[[v]][[i]] <- varData[[v]]
        }
        for(c in matchDims) {
            allVar[[c]][[i]] <- varData[[c]]
        }
        if(progress) {
            setTxtProgressBar(pb, value=i)
        }
    }
    if(raw) {
        return(transpose(allVar))
    }

    # if its a single function make a list for looping
    if(is.list(FUN) &&
       is.null(names(FUN))) {
        names(FUN) <- as.character(substitute(FUN))[-1]
    } else if(is.function(FUN)) {
        tmpName <- as.character(substitute(FUN))
        FUN <- list(FUN)
        names(FUN) <- tmpName
    }
    fnames <- names(FUN)

    for(v in varNames) {
        # data[[paste0(v, '_mean')]] <- sapply(allVar[[v]], function(x) mean(x, na.rm=TRUE))
        # # these two are currently much slower, may want option for just mean? or option for providing
        # # funciton or list of functions and it appends that name???
        # data[[paste0(v, '_median')]] <- sapply(allVar[[v]], function(x) median(x, na.rm=TRUE))
        # data[[paste0(v, '_stdev')]] <- sapply(allVar[[v]], function(x) sd(x, na.rm=TRUE))
        for(f in seq_along(FUN)) {
            if(fnames[f] %in% c('mean', 'median', 'sd')) {
                # FUN[[f]] <- function(x) FUN[[f]](x, na.rm=TRUE)
                data[[paste0(v, '_', fnames[f])]] <- sapply(allVar[[v]], function(x) FUN[[f]](x, na.rm=TRUE))
            } else {
                data[[paste0(v, '_', fnames[f])]] <- sapply(allVar[[v]], FUN[[f]])
            }
        }
    }
    if(keepMatch) {
        for(c in matchDims) {
            data[[paste0(c, '_mean')]] <- sapply(allVar[[c]], function(x) mean(x, na.rm=TRUE))
        }
        if(!is.null(data$matchTime_mean) &&
           !(all(is.na(data$matchTime_mean))) &&
           max(abs(data$matchTime_mean)) > 365) {
            data$matchTime_mean <- as.POSIXct(data$matchTime_mean, origin = '1970-01-01 00:00:00', tz='UTC')
        }
    }
    data <- to180(data, inverse=!data180)
    # change back to whatever they were using
    colnames(data)[1:length(oldNames)] <- oldNames
    data
}

getVarData <- function(data, nc, var, buffer, depth=NULL, verbose=TRUE) {
    xIx <- dimToIx(data$Longitude, nc$dim$Longitude, buffer[1], verbose)
    yIx <- dimToIx(data$Latitude, nc$dim$Latitude, buffer[2], verbose)
    hasT <- 'UTC' %in% names(nc$dim)
    if(hasT) {
        tIx <- dimToIx(data$UTC, nc$dim$UTC, buffer[3], verbose)
        tVals <- ncTimeToPosix(nc$dim$UTC$vals[tIx$ix], units = nc$dim$UTC$units)
    } else {
        tVals <- NA
    }
    hasZ <- 'Depth' %in% names(nc$dim)
    if(hasZ) {
        if(is.null(depth)) {
            if('Depth' %in% colnames(data)) {
                # no buffer for depth
                zIx <- dimToIx(data$Depth, nc$dim$Depth, 0, verbose=FALSE)
                zVals <- nc$dim$Depth$vals[zIx$ix]
            } else {
                zIx <- list(start=1, count=-1)
                zVals <- nc$dim$Depth$vals
            }
        } else {
            zIx <- dimToIx(depth, nc$dim$Depth, 0, verbose=FALSE)
            zVals <- nc$dim$Depth$vals[zIx$ix]
        }
    }
    result <- vector('list', length = length(var) + 3)
    names(result) <- c(var, 'matchLong', 'matchLat', 'matchTime')
    for(v in var) {
        # create start and count for each var, depends if they use T and Z dims
        start <- c(xIx$start, yIx$start)
        count <- c(xIx$count, yIx$count)
        thisVar <- nc[['var']][[v]]
        hasZ <- 'Depth' %in% names(thisVar$dim)
        if(hasZ) {
            start <- c(start, zIx$start)
            count <- c(count, zIx$count)
        }
        hasT <- 'UTC' %in% names(thisVar$dim)
        if(hasT) {
            start <- c(start, tIx$start)
            count <- c(count, tIx$count)
        }
        result[[v]] <- ncvar_get(nc, varid=v, start=start, count=count)
    }
    result$matchLong <- nc$dim$Longitude$vals[xIx$ix]
    result$matchLat <- nc$dim$Latitude$vals[yIx$ix]
    result$matchTime <- tVals
    if(hasZ) {
        result$matchDepth <- zVals
    }
    result
}
