#' @title Match Data From an Existing Netcdf File or Download and Match
#'
#' @description Extracts all variables from a netcdf file matching Longitude,
#'   Latitude, and UTC coordinates in given dataframe
#'
#' @param data dataframe containing Longitude, Latitude, and UTC to extract matching
#'   variables from the netcdf file
#' @param nc name of a netcdf file
#' @param var (optional) vector of variable names
#' @param buffer vector of Longitude, Latitude, and Time (seconds) to buffer around
#'   each datapoint. All values within the buffer will be used to report the mean,
#'   median, and standard deviation
#' @param fileName (optional) file name to save downloaded nc file to. If not provided,
#'   then no nc files will be stored, instead small temporary files will be downloaded
#'   and then deleted. This can be much faster, but means that the data will need to be
#'   downloaded again in the future. If \code{fileName} is provided, then the function
#'   will attempt to download a single nc file covering the entire range of your data.
#'   If your data spans a large amount of time and space this can be problematic.
#' @param \dots other parameters to pass to \link{ncToData}
#'
#' @return original dataframe with three attached columns for each variable in the netcdf
#'   file, one for each of mean, median, and standard deviation of all values within the buffer
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @name matchEnvData
#' @export
#'
setGeneric('matchEnvData',
           function(data, nc=NULL, var=NULL, buffer=c(0,0,0), fileName = NULL, ...) standardGeneric('matchEnvData')
)

#' @rdname matchEnvData
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom rerddap cache_delete
#' @export
#'
setMethod('matchEnvData', 'data.frame', function(data, nc=NULL, var=NULL, buffer=c(0,0,0), fileName = NULL, ...) {
    # First just get an edinfo
    if(is.null(nc)) {
        nc <- browseEdinfo(var=var)
    }
    if(is.character(nc) &&
       !file.exists(nc)) {
        nc <- try(erddapToEdinfo(nc, chooseVars = TRUE))
        if(inherits(nc, 'try-error')) {
            stop(paste0(nc, ' must be a valid nc file or erddap dataset id.'))
        }
    }

    # if pointing to an ncfile, just do that
    if(is.character(nc) &&
       file.exists(nc)) {
        return(ncToData(data=data, nc=nc, buffer=buffer, ...))
    }
    if(!inherits(nc, 'edinfo')) {
        stop(paste0(nc, ' must be a valid nc file or erddap dataset id.'))
    }

    # no filename provided means dont download all, do smaller
    if(is.null(fileName)) {
        result <- vector('list', length = nrow(data))
        cat('Downloading data...\n')
        pb <- txtProgressBar(min=1, max = length(result), style=3)
        for(i in seq_along(result)) {
            ncData <- downloadEnv(data=data[i, ], edinfo = nc, buffer = buffer)
            on.exit(rerddap::cache_delete(basename(ncData)), add=FALSE)
            result[[i]] <- ncToData(data=data[i, ], nc=ncData, buffer=buffer, ...)
            setTxtProgressBar(pb, value = i)
        }
        return(bind_rows(result))
    }

    # file name provided means get big all at once
    if(!grepl('\\.nc$', fileName)) {
        fileName <- paste0(fileName, '.nc')
    }
    ncData <- downloadEnv(data=data, edinfo = nc, fileName = fileName, buffer = buffer)
    return(ncToData(data=data, nc=ncData, buffer=buffer, ...))
})
