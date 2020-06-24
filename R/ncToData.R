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
#' @param raw logical flag to return only the raw values of the variables. If \code{TRUE}
#'   the output will be changed to a list with length equal to the number of data points.
#'   Each item in the list will have separate named entries for each variable that will have
#'   all values within the given buffer and all values for any Z coordinates present.
#' @param quiet logical flag to show warning messages for possible coordinate mismatch
#'
#' @return original dataframe with three attached columns for each variable in the netcdf
#'   file, one for each of mean, median, and standard deviation of all values within the buffer
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom ncdf4 nc_open nc_close ncvar_get
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom purrr transpose
#'
#' @export
#'
ncToData <- function(data, nc, buffer = c(0,0,0), raw = FALSE, quiet=FALSE) {
    nc <- nc_open(nc)
    on.exit(nc_close(nc))
    nc <- romsCheck(nc)
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
    allVar <- vector('list', length = length(varNames) + 3)
    names(allVar) <- c(varNames, 'matchLong', 'matchLat', 'matchTime')
    for(v in seq_along(allVar)) {
        allVar[[v]] <- vector('list', length = nrow(data))
    }
    for(i in 1:nrow(data)) {
        for(v in varNames) {
            varData <- getVarData(data[i,], nc=nc, var=v, buffer = buffer, quiet=quiet)
            allVar[[v]][[i]] <- varData$var
        }
        for(c in c('matchLong', 'matchLat', 'matchTime')) {

            allVar[[c]][[i]] <- varData[[c]]
        }
    }
    if(raw) {
        return(transpose(allVar))
    }
    allVar <- bind_rows(
        lapply(purrr::transpose(allVar), function(i) {
            result <- vector('list', length = 3 * length(i))
            names(result) <- paste0(rep(names(i), each = 3),
                                    c('_mean', '_median', '_stdev'))
            for(v in seq_along(i)) {
                result[(v-1)*3 + 1] <- mean(i[[v]], na.rm = TRUE)
                result[(v-1)*3 + 2] <- median(i[[v]], na.rm = TRUE)
                result[(v-1)*3 + 3] <- sd(i[[v]], na.rm = TRUE)
            }
            bind_cols(result)
        })
    )
    if(max(abs(allVar$matchTime_mean)) > 365) {
        allVar$matchTime_mean <- as.POSIXct(allVar$matchTime_mean, origin = '1970-01-01 00:00:00', tz='UTC')
        allVar$matchTime_median <- as.POSIXct(allVar$matchTime_median, origin = '1970-01-01 00:00:00', tz='UTC')
    }
    cbind(data, allVar)
}

getVarData <- function(data, nc, var, buffer, quiet=FALSE) {
    names(nc$dim) <- standardCoordNames(names(nc$dim))
    thisVar <- nc[['var']][[var]]
    thisVar$dim <- nc$dim[thisVar$dimids + 1]
    xIx <- dimToIx(data$Longitude, thisVar$dim$Longitude, buffer[1], quiet)
    yIx <- dimToIx(data$Latitude, thisVar$dim$Latitude, buffer[2], quiet)
    tIx <- dimToIx(data$UTC, thisVar$dim$UTC, buffer[3], quiet)
    hasZ <- any(!(names(thisVar$dim) %in% c('Longitude', 'Latitude', 'UTC')))
    if(hasZ) {
        start <- c(xIx$start, yIx$start, 1, tIx$start)
        count <- c(xIx$count, yIx$count, -1, tIx$count)
    } else {
        start <- c(xIx$start, yIx$start, tIx$start)
        count <- c(xIx$count, yIx$count, tIx$count)
    }
    tVals <- ncTimeToPosix(thisVar$dim$UTC$vals[tIx$ix], units = thisVar$dim$UTC$units)
    list(var = ncvar_get(nc, varid = var, start = start, count = count),
         matchLong = thisVar$dim$Longitude$vals[xIx$ix],
         matchLat = thisVar$dim$Latitude$vals[yIx$ix],
         matchTime = tVals
    )
}
