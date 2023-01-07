# converts nc time dimension values to posix , either giving
# units explicitly or just passing the whole time dimension
ncTimeToPosix <- function(vals, units) {
    # if sending the whole dimension extract bits
    if(is.list(vals) &&
       all(c('units', 'vals') %in% names(vals))) {
        units <- vals$units
        vals <- vals$vals
    }
    if(units == 'hours since 2000-01-01 00:00:00') {
        return(as.POSIXct(vals * 3600, origin = '2000-01-01 00:00:00', tz='UTC'))
    }
    if(units == 'seconds since 1970-01-01T00:00:00Z') {
        return( as.POSIXct(vals, origin = '1970-01-01 00:00:00', tz='UTC'))
    }
    if(units == 'seconds since 1981-01-01 00:00:00') {
        return( as.POSIXct(vals, origin = '1981-01-01 00:00:00', tz='UTC'))
    }
    if(units == 'count') {
        return(vals)
    }
    if(units == 'posix') {
        return(vals)
    }
    if(units == 'hours since 1950-01-01') {
        return(as.POSIXct(vals * 3600, origin = '1950-01-01', tz='UTC'))
    }
    stop('Dont know how to deal with time with units ', units)
}

#
#' @importFrom lubridate yday
#'
dimToIx <- function(data, dim, buffer=0, verbose=TRUE) {
    if(!verbose) {
        return(suppressWarnings(dimToIx(data, dim, buffer, verbose=TRUE)))
    }
    if(buffer > 0) {
        data <- c(min(data) - buffer, data, max(data) + buffer)
    }
    if(dim$name %in% c('time', 'UTC', 'dayOfYear') ||
       inherits(data, 'POSIXct')) {
        dim$vals <- ncTimeToPosix(dim)
        if(dim$name == 'dayOfYear' &&
           inherits(data, 'POSIXct')) {
            data <- yday(data)
        }
    }
    # prob want to keep actual distance?
    ix <- sapply(data, function(x) which.min(abs(dim$vals - x)))
    # start <- which.min(abs(dim$vals - (min(data) - buffer)))
    diff <- abs(data - dim$vals[ix])
    # get start and count values for te ncvar_get call
    # start <- min(ix) - buffer
    start <- min(ix)
    if(start < 1) {
        warning('Buffer for "', dim$name, '" made desired value less than minimum of dimension range. Using lowest value.')
        start <- 1
    }

    # end <- max(ix) + buffer
    end <- max(ix)
    # end <- which.min(abs(dim$vals - (max(data) + buffer)))
    if(end > dim$len) {
        warning('Buffer for "', dim$name, '" made desired value greater than maximum of dimension range. Using highest value.')
        end <- dim$len
    }
    count <- end - start + 1
    maxDiff <- max(diff)
    if(inherits(maxDiff, 'difftime')) {
        diffMsg <- paste0(round(as.double(maxDiff, units='days'), 1), ' days')
        maxDiff <- as.double(maxDiff, units='secs')
    } else {
        diffMsg <- round(maxDiff, 3)
    }
    # browser()
    # check if any matched data appear to be out of bounds. if dimension has only one value, can only report
    # the diffrence if non-zero. this can be fine - what if we only have one month of monthly data, so all of
    # our times are off by up to 15 days.
    # if multiple values in dim, we can find the stride and see if we are out of bounds by more than one
    # stride and warn people, this means should prob increase lims of data request
    if(length(dim$vals) == 1 &&
       maxDiff > 0) {
        warnMsg <- paste0('Data are matched to the nearest value in the netcdf file, dimension ', dim$name,
                          ' had values up to ', diffMsg, ' apart from the nearest value present in the file.')
        warning(warnMsg)
    } else if(length(dim$vals) > 1) {
        dimStride <- abs(dim$vals[2]-dim$vals[1])
        # browser()
        if(inherits(dimStride, 'difftime')) {
            dimStride <- as.double(dimStride, units='secs')
        }
        # dimStride <- round(dimStride, 4)
        if(maxDiff / dimStride > 1) {
            warnMsg <- paste0('Data are matched to the nearest value in the netcdf file, dimension ', dim$name,
                              ' had values up to ', diffMsg, ' apart from the nearest value present in the file.',
                              ' It is possible that a closer match could be made with more data.')
            warning(warnMsg)
        }
    }
    list(ix=start:end, start=start, count=count, diff=diff)
}

# make everything Latitude Longitude UTC, see getCoordNameMatch() for conversions
standardCoordNames <- function(names) {
    nameDf <- getCoordNameMatch()
    for(i in seq_along(names)) {
        if(!(tolower(names[i]) %in% nameDf$current)) {
            next
        }
        names[i] <- nameDf[nameDf$current == tolower(names[i]), 'standard']
    }
    names
}

to180 <- function(x, inverse = FALSE) {
    if(inverse) {
        return(to360(x, inverse = FALSE))
    }
    if(is.data.frame(x) ||
       is.list(x)) {
        tmp <- x$Longitude %% 360
        tmp <- ifelse(tmp > 180, tmp - 360, tmp)
        x$Longitude <- tmp
        return(x)
    }
    tmp <- x %% 360
    tmp <- ifelse(tmp > 180, tmp - 360, tmp)
    tmp
}

to360 <- function(x, inverse = FALSE) {
    if(inverse) {
        return(to180(x, inverse = FALSE))
    }
    if(is.data.frame(x) ||
       is.list(x)) {
        x$Longitude <- x$Longitude %% 360
        return(x)
    }
    x %% 360
}

# check if an nc file has longitude in 180 or 360 coords
#' @importFrom ncdf4 ncatt_get
#'
ncIs180 <- function(nc) {
    names(nc$dim) <- standardCoordNames(names(nc$dim))
    if(is.null(nc$dim$Longitude)) {
        stop('Couldnt find a "Longitude" coordinate in this NC file.')
    }
    dataIs180(nc$dim$Longitude$vals)
}

dataIs180 <- function(data) {
    if(is.data.frame(data) ||
       is.list(data)) {
        data <- data$Longitude
    }
    if(any(data > 180)) {
        return(FALSE)
    }
    if(any(data < 0)) {
        return(TRUE)
    }
    TRUE
}

# just holds this dataframe so i can see it / add to it easily instead of storing it as an rdata
getCoordNameMatch <- function() {
    data.frame(
        current = c('lon', 'long', 'lat', 'time', 'longitude', 'latitude', 'utc', 'date', 'dayofyear', 'altitude', 'depth', 'level', 'lev'),
        standard = c('Longitude', 'Longitude', 'Latitude', 'UTC', 'Longitude', 'Latitude', 'UTC', 'UTC', 'UTC', 'Depth', 'Depth', 'Depth', 'Depth'),
        stringsAsFactors = FALSE
    )
}

# check that data is within limits of the dataset you want to pull, it will either
# replace these wiith acceptable min/max or remove them if outside
checkLimits <- function(data, edi, replace=FALSE, verbose=TRUE) {
    limits <- edi$limits
    if(!is.null(edi$isCurrent) &&
       isTRUE(edi$isCurrent)) {
        limits$UTC[2] <- nowUTC()
    }
    # make both same 180 status and save original status to reconvert later
    data180 <- dataIs180(data)
    limit180 <- dataIs180(limits$Longitude)
    data <- to180(data, inverse = !limit180)
    # helper checks on dimension
    checkOneLim <- function(dat, lim, dim, replace=FALSE, verbose=TRUE) {
        # check for 1-365 data, cant be out of bounds of that
        if(inherits(dat[[dim]], 'POSIXct') &&
           identical(lim[[dim]], c(1, 365))) {
            return(dat)
        }
        lowCheck <- dat[[dim]] >= lim[[dim]][1]
        highCheck <-  dat[[dim]] <= lim[[dim]][2]
        inLim <- lowCheck & highCheck
        if(all(inLim)) {
            return(dat)
        }
        # handle replacement or not
        repMsg <- ifelse(replace, 'replaced with the min or max value allowed.', 'excluded.')
        wrnMsg <- paste0(sum(!inLim), ' out of ', length(inLim),
                         ' data points are outside the range of dimension ',
                         dim, ', these will be ', repMsg)
        if(verbose) {
            warning(wrnMsg, call. = FALSE)
        }
        if(!replace) {
            return(dat[inLim, ])
        }
        if(any(!lowCheck)) {
            # dat[!lowCheck, dim] <- lim[[dim]][1]
            dat[[dim]][!lowCheck] <- lim[[dim]][1]
            if(all(!lowCheck)) {
                dat[[dim]][length(dat[[dim]])] <- lim[[dim]][1] + edi$spacing[[dim]]
            }
        }
        if(any(!highCheck)) {
            # dat[!highCheck, dim] <- lim[[dim]][2]
            dat[[dim]][!highCheck] <- lim[[dim]][2]
            if(all(!highCheck)) {
                dat[[dim]][1] <- lim[[dim]][2] - edi$spacing[[dim]]
            }
        }
        dat
    }
    for(d in names(limits)) {
        data <- checkOneLim(data, limits, d, replace, verbose)
    }
    to180(data, inverse = !data180)
}

# does creating of temp directories/files if you need, adds a suffix to a file
# if you need
#' @importFrom hoardr hoard
#'
fileNameManager <- function(fileName=NULL, suffix=NULL) {
    if(is.null(fileName)) {
        tempDir <- hoard()$cache_path_set('PAMmisc')
        if(!dir.exists(tempDir)) {
            dir.create(tempDir, recursive = TRUE)
        }
        fileName <- paste0(tempDir, '/TEMPFILE.nc')
    }
    if(!is.null(suffix)) {
        fileName <- gsub('(.*)(\\.nc$)', paste0('\\1_', suffix, '\\2'), fileName)
    }
    if(!dir.exists(dirname(fileName))) {
        dir.create(dirname(fileName), recursive = TRUE)
    }
    fileName
}

checkDateline <- function(data) {
    names(data) <- standardCoordNames(names(data))
    data <- to180(data)
    # check diff signs, then make sure we arent around the 0 transition
    (sign(max(data$Longitude)) != sign(min(data$Longitude))) &&
        ((max(data$Longitude) >= 90) || (min(data$Longitude) <= -90))
}

# if buffer values are less than the individual spacing in a datset bump
# them up to that. this ensures that when you download data you have at least
# one datapoint to each side of your data so you can always get closest one
# (ie it usually only grabs between range given, so if your max is close to the
# next one it wont get that)
bufferToSpacing <- function(buffer, edinfo) {
    # buffer is XYT long lat time
    if(buffer[1] < abs(edinfo$spacing$Longitude)) {
        buffer[1] <- abs(edinfo$spacing$Longitude)
    }
    if(buffer[2] < abs(edinfo$spacing$Latitude)) {
        buffer[2] <- abs(edinfo$spacing$Latitude)
    }
    if(!is.null(edinfo$spacing$UTC) &&
       buffer[3] < edinfo$spacing$UTC) {
        buffer[3] <- edinfo$spacing$UTC
    }
    buffer
}

romsCheck <- function(nc) {
    if(!all(c('year', 'month', 'day', 'lon', 'lat') %in% names(nc$var))) {
        return(nc)
    }
    names(nc$dim) <- standardCoordNames(names(nc$dim))
    if(identical(nc$dim$Longitude$vals, 1:nc$dim$Longitude$len)) {
        nc$dim$Longitude$vals <- ncvar_get(nc, 'lon')[, 1]
    }
    if(identical(nc$dim$Latitude$vals, 1:nc$dim$Latitude$len)) {
        nc$dim$Latitude$vals <- ncvar_get(nc, 'lat')[1, ]
    }
    y <- ncvar_get(nc, 'year')
    m <- ncvar_get(nc, 'month')
    d <- ncvar_get(nc, 'day')
    nc$dim$UTC$vals <- as.POSIXct(paste0(y, '-', m, '-', d), tz='UTC')
    nc$dim$UTC$units <- 'posix'
    # whichDimVar <- names(nc$var) %in% c('lon', 'lat', 'year', 'month', 'day')
    # nc$var <- nc$var[!whichDimVar]
    nc
}

nowUTC <- function() {
    now <- Sys.time()
    attr(now, 'tzone') <- 'UTC'
    now
}

estDownloadSize <- function(x, edi, verbose=FALSE) {
    spacing <- edi$spacing
    nLats <- floor(diff(range(x$Latitude, na.rm=TRUE)) / spacing$Latitude) + 3
    nLongs <- floor(diff(range(x$Longitude, na.rm=TRUE)) / spacing$Longitude) + 3
    if(is.null(spacing$UTC) || is.na(spacing$UTC)) {
        nTimes <- 1
    } else {
        nTimes <- floor(as.numeric(difftime(max(x$UTC), min(x$UTC), units='secs')) / spacing$UTC) + 3
    }
    if(is.null(spacing$Depth) || is.na(spacing$Depth)) {
        nDepths <- 1
    } else {
        if('Depth' %in% colnames(x) &&
           edi$source != 'hycom') {
            nDepths <- floor(diff(range(x$Depth, na.rm=TRUE)) / spacing$Depth) + 3
        } else {
            nDepths <- floor(diff(edi$limits$Depth) / spacing$Depth) + 3
        }
    }
    if(verbose) {
        cat('Lats :', nLats, '\nLongs:', nLongs, '\nTimes:', nTimes, '\nDepths:', nDepths)
    }
    size <- nLats * nLongs * nTimes * nDepths * 8 / 1e6 * sum(edi$varSelect)
    list(size=size, biggest=c('Latitude', 'Longitude', 'UTC')[which.max(c(nLats, nLongs, nTimes))])
}

planDownload <- function(x, edi, last=0, thresh=50) {
    if(nrow(x) == 1) {
        return(last + 1)
    }
    estSize <- estDownloadSize(x, edi)
    if(estSize$size <= thresh) {
        return(rep(last + 1, nrow(x)))
    }
    whichLow <- x[[estSize$biggest]] <= mean(range(x[[estSize$biggest]], na.rm=TRUE))
    isNa <- is.na(whichLow)
    lows <- planDownload(x[whichLow & !isNa, ], edi, last, thresh)
    highs <- planDownload(x[!whichLow & !isNa, ], edi, max(lows), thresh)
    out <- rep(NA, nrow(x))
    out[whichLow & !isNa] <- lows
    out[!whichLow & !isNa] <- highs
    out[isNa] <- -1
    out
}
