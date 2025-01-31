# hycomFunctions

library(lubridate)
library(dplyr)
library(PAMmisc)
library(patchwork)

# INPUT x is an array output pulled from a HYCOM grid, pulled with buffer=c(.16, .16)
# this results in a length of 5 for .08 spacing or 9 for .04 spacing
# reorganizes these down to a 3x3 grid of +- .08 spacing applying averaging
# for the .04 grid
# OUTPUT a 3x3xD array, coerces a 2d array to MxNx1

make33Grid <- function(x) {
    dim(x) <- c(dim(x), rep(1, 3-length(dim(x))))
    dims <- dim(x)
    if(dims[1] == 3 &&
       dims[2] == 3) {
        return(x)
    }
    # 16 buffer returns 5 means .08 grid
    if(dims[1] == 5) {
        x <- x[2:4, , , drop=FALSE]
        dims <- dim(x)
    }
    if(dims[2] == 5) {
        x <- x[, 2:4, , drop=FALSE]
        dims <- dim(x)
    }
    # if a .04 grid, we do averaging of .08 +- .04 *.5
    if(dims[1] == 9) {
        new <- array(NA, dim=c(3, dims[2:3]))
        new[1, , ] <- (.5 * x[2, ,] + .5 * x[4, ,] + x[3, , ]) / 2
        new[2, , ] <- (.5 * x[4, ,] + .5 * x[6, ,] + x[5, , ]) / 2
        new[3, , ] <- (.5 * x[6, ,] + .5 * x[8, ,] + x[7, , ]) / 2
        x <- new
        dims <- dim(x)
    }
    # print(dims)
    if(dims[2] == 9) {
        new <- array(NA, dim=c(dims[1], 3, dims[3]))
        # browser()
        new[, 1, ] <- (.5 * x[, 2, ] + .5 * x[, 4, ] + x[, 3, ]) / 2
        new[, 2, ] <- (.5 * x[, 4, ] + .5 * x[, 6, ] + x[, 5, ]) / 2
        new[, 3, ] <- (.5 * x[, 6, ] + .5 * x[, 8, ] + x[, 7, ]) / 2
        x <- new
    }
    x
}

# returns center value of 3x3 grid, if NA then mean of all

centerMean <- function(x) {
    if(length(dim(x)) == 3) {
        x <- x[, , 1]
    }
    out <- x[2, 2]
    if(is.na(out)) {
        return(mean(x, na.rm=TRUE))
    }
    out
}

# calculates ILD given temperatures and depths using Surface-0.5C method

calcIld <- function(t, depth) {
    if(is.na(t[1])) {
        return(NA)
    }
    t0 <- t[1]
    tml <- t0 - 0.5
    kmax <- length(t[!is.na(t)])
    dmax <- max(depth[!is.na(t)])
    temp1 <- spline(depth[1:kmax], t[1:kmax], n=dmax, xmin=0, xmax=dmax,
                    method='natural')[[2]][1:dmax]
    k <- (which(temp1 < tml))[1]
    max(0, k-1 + (tml-temp1[k-1]) / (temp1[k]-temp1[k-1]))
}

# converts to 3x3 grid and calculates mean/SD for all env params
calcEKParams <- function(x, ekNames=TRUE) {
    if(is.list(x) &&
       is.null(names(x))) {
        return(bind_rows(lapply(x, calcEKParams, ekNames=ekNames)))
    }
    x$water_temp <- make33Grid(x$water_temp)
    x$water_u <- make33Grid(x$water_u[, , 1])
    x$water_v <- make33Grid(x$water_v[, , 1])
    x$salinity <- make33Grid(x$salinity[, , 1])
    x$surf_el <- make33Grid(x$surf_el)
    x$ild <- apply(x$water_temp, c(1, 2), function(t) {
        calcIld(t, x$matchDepth)
    })
    result <- list(
        sst_mean = centerMean(x$water_temp[, , 1]),
        sst_sd = sd(x$water_temp[, , 1], na.rm=TRUE),
        sss_mean = centerMean(x$salinity),
        sss_sd = sd(x$salinity, na.rm=TRUE),
        u_mean = centerMean(x$water_u),
        u_sd = sd(x$water_u, na.rm=TRUE),
        v_mean = centerMean(x$water_v),
        v_sd = sd(x$water_v, na.rm=TRUE),
        ild_mean = centerMean(x$ild),
        ild_sd = sd(x$ild, na.rm=TRUE),
        ssh_mean = centerMean(x$surf_el),
        ssh_sd = sd(x$surf_el, na.rm=TRUE)
    )
    if(ekNames) {
        names(result) <- c(
            'sst.mean',
            'sst.SD',
            'sss.mean',
            'sss.SD',
            'u.mean',
            'u.SD',
            'v.mean',
            'v.SD',
            'ild.mean',
            'ild.SD',
            'ssh.mean',
            'ssh.SD'
        )
    }
    result
}
# INPUT
# data - a dataframe with UTC, Latitude, and Longitude
# folder - folder to store downloads for this project. .nc and log files will live here
# tz - timezone of your dataset, must match something in OlsonNames()
# log - name of log file, this can be left as NULL and will use the folder name
# buffer - amount to extend coordinates in "data", in order of Lon, Lat, UTC (seconds)
# retry - if FALSE, will only download days that have not been attempted. If TRUE will
#         also download days that were attempted and failed
# timeout - timeout max duration for download, can be increased if repeatedly reaching limit
# hyList - list of HYCOM datasets, don't change without Taiki's help
# progress - logical flag to show progress bar

# OUTPUT
# writes log file and NetCDF files for full days covering the range of coordinates/times
# in "data". If a download fails, you can run this again pointing to the same folder
# and it will resume. To retry failed downloads, run with retry=TRUE.

loadLog <- function(folder) {
    logName <- paste0(basename(folder), '_', 'HYCOMLog.csv')
    logName <- file.path(folder, logName)
    if(!file.exists(logName)) {
        return(NULL)
    }
    log <- read.csv(logName, stringsAsFactors = FALSE)
    log$day <- as.POSIXct(log$day, format='%Y-%m-%d', tz='UTC')
    log
}

downloadHYCOM <- function(data=NULL, folder,
                          tz='UTC',
                          log=NULL, buffer=c(0.16, 0.16, 0),
                          mode=c('segment', 'full'),
                          retry=TRUE, timeout=600, hyList=PAMmisc::hycomList,
                          hour=NULL, hourTz='America/Los_Angeles',
                          frequency=1,
                          progress=TRUE) {
    if(!dir.exists(folder)) {
        dir.create(folder)
    }
    mode <- match.arg(mode)
    data <- ekToStandardFormat(data, tz=tz)
    logDf <- dataToLog(data, mode=mode, buffer=buffer, frequency=frequency)

    if(is.null(log)) {
        log <- paste0(basename(folder), '_', 'HYCOMLog.csv')
        log <- file.path(folder, log)
    }

    if(file.exists(log)) {
        oldLog <- read.csv(log, stringsAsFactors = FALSE)
        oldLog$day <- as.POSIXct(oldLog$day, format='%Y-%m-%d', tz='UTC')
        logDf <- bind_rows(oldLog, logDf[!logDf$day %in% oldLog$day, ])
        logDf <- arrange(logDf, day)
    }

    if(retry) {
        toTry <- !logDf$succeeded
    } else {
        toTry <- !logDf$attempted
    }

    if(!any(toTry)) {
        cat('No days to try to download.')
        if(isFALSE(retry)) {
            cat(' Did you mean to run with "retry=TRUE" ?')
        }
        return(logDf)
    }

    tried <- rep(FALSE, nrow(logDf))

    on.exit({
        nTried <- sum(tried)
        nSucceeded <- sum(logDf$succeeded[tried])
        nPlanned <- sum(toTry)
        write.csv(logDf, file = log, row.names = FALSE)
        cat('\nSucceeded on ', nSucceeded,
            ' out of ', nTried, ' attempts ',
            '(out of ', nPlanned, ' planned attempts)', sep='')
    })

    if(progress) {
        pb <- txtProgressBar(min=0, max=sum(toTry), style=3)
        ix <- 0
    }

    for(i in which(toTry)) {
        logDf$attempted[i] <- TRUE
        tried[i] <- TRUE
        dlDf <- data.frame(Latitude = c(logDf$minLat[i], logDf$maxLat[i]),
                           Longitude = c(logDf$minLong[i], logDf$maxLong[i]),
                           UTC = c(logDf$day[i], logDf$day[i] + 24 * 3600))
        if(!is.null(hour) &&
           mode == 'full') {
            dlDf$UTC <- dlDf$UTC[1] + hour * 3600
            dlDf$UTC <- with_tz(force_tz(dlDf$UTC, tzone=hourTz), tzone='UTC')
        }
        whichHy <- PAMmisc:::whichHycom(dlDf, hyList)[1]
        thisHy <- hyList$list[[whichHy]]
        thisExpt <- gsub('.*(expt_[0-9\\.X]*).*', '\\1', thisHy$dataset)
        thisHy <- varSelect(thisHy, select = c(T, T, T, T, T))
        # if segment we store cruiseNum, so add to name here
        if('cruiseNum' %in% colnames(logDf)) {
            prefix <- paste0(logDf$cruiseNum[i], '_')
        } else {
            prefix <- ''
        }
        filename <- paste0(prefix,
                           'HYCOM_',
                           thisExpt,
                           '_',
                           format(logDf$day[i], format='%Y-%m-%d'),
                           '_',
                           format(logDf$day[i]+24*3600, format='%Y-%m-%d'),
                           '.nc')
        filename <- file.path(folder, filename)
        thisFile <- tryCatch({
            result <- downloadEnv(dlDf, edinfo = thisHy, buffer=buffer, fileName = filename,timeout = timeout, progress = FALSE)
            result
        },
        warning = function(w) {
            logDf$fail_message[i] <<- w$message
            FALSE
        },
        error = function(e) {
            logDf$fail_message[i] <<- w$message
            FALSE
        })

        if(progress) {
            ix <- ix + 1
            setTxtProgressBar(pb, value=ix)
        }
        if(isFALSE(thisFile)) {
            next
        }
        logDf$succeeded[i] <- TRUE
        logDf$file[i] <- filename
        logDf$fail_message[i] <- ''
    }
    logDf
}

parseNcName <- function(x, what=c('start', 'end', 'expt')) {
    what <- match.arg(what)
    x <- basename(x)
    x <- gsub('\\.nc$', '', x)
    x <- strsplit(x, '_')[[1]]
    # prefix_?HYCOM_EXPT_START_END.nc
    len <- length(x)
    if(len < 4) {
        stop('NetCDF file name is in unexpected format, ',
             'expecting PREFIXHYCOM_EXPT_START_END.nc')
    }
    switch(what,
           'start' = {
               result <- x[[len-1]]
               result <- as.POSIXct(result, format='%Y-%m-%d', tz='UTC')
           },
           'end' = {
               result <- x[[len]]
               result <- as.POSIXct(result, format='%Y-%m-%d', tz='UTC')
           },
           'expt' = {
               result <- x[[len-2]]
           }
    )
    result
}

ekToStandardFormat <- function(data, tz='UTC', noTime=FALSE) {
    if(is.null(data) || nrow(data) == 0) {
        return(data)
    }
    if(is.character(data)) {
        if(!file.exists(data)) {
            stop('File ', data, ' does not exist.')
        }
        data <- read.csv(data, stringsAsFactors = FALSE)
    }
    # Look for Longitude columns
    if('mlon' %in% colnames(data)) {
        data <- rename(data, 'Longitude' = 'mlon')
    } else if('lon' %in% colnames(data)) {
        data <- rename(data, 'Longitude' = 'lon')
    } else if('lon180' %in% colnames(data)) {
        data <- rename(data, 'Longitude' = 'lon180')
    } else if('lon360' %in% colnames(data)) {
        data <- rename(data, 'Longitude' = 'lon360')
    }
    # Look for Latitude columns
    if('mlat' %in% colnames(data)) {
        data <- rename(data, 'Latitude' = 'mlat')
    } else if('lat' %in% colnames(data)) {
        data <- rename(data, 'Latitude' = 'lat')
    }
    if(isTRUE(noTime)) {
        if(!all(c('Longitude', 'Latitude') %in% colnames(data))) {
            stop('Could not find Latitude/Longitude columns, please rename')
        }
        return(data)
    }
    if(!'UTC' %in% colnames(data) &&
       all(c('year', 'month', 'day', 'mtime') %in% colnames(data))) {
        data$UTC <- paste0(data$year, '_',
                           data$month, '_',
                           data$day, '_')
        data$UTC <- as.POSIXct(data$UTC, format='%Y_%m_%d', tz=tz)
        data$UTC <- data$UTC + data$mtime * 3600
    }
    if(!all(c('UTC', 'Latitude', 'Longitude') %in% names(data))) {
        stop('"data" must have "UTC", "Longitude", and "Latitude"')
    }

    data$UTC <- parseToUTC(data$UTC, tz=tz)
    # data$UTC <- data$UTC + offset * 3600
    data
}

dataToLog <- function(data, mode=c('segment','full'), buffer, frequency=1) {
    if(is.null(data) || nrow(data) == 0) {
        return(data)
    }
    switch(match.arg(mode),
           'full' = {
               dataRange <- dataToRanges(data, buffer)
               days <- seq(from=floor_date(dataRange$UTC[1], unit='1day'),
                           to = floor_date(dataRange$UTC[2], unit='1day'),
                           by=24 * 3600 * frequency)
               logDf <- data.frame(day=days,
                                   minLat = min(dataRange$Latitude),
                                   maxLat = max(dataRange$Latitude),
                                   minLong = min(dataRange$Longitude),
                                   maxLong = max(dataRange$Longitude),
                                   attempted = FALSE,
                                   succeeded = FALSE,
                                   file = '',
                                   fail_message='')
           },
           'segment' = {
               logDf <- data %>%
                   mutate(day = floor_date(.data$UTC, unit='1day')) %>%
                   group_by(.data$day)
               if('cruiseNum' %in% colnames(data)) {
                   logDf <- group_by(logDf, .data$cruiseNum, .add=TRUE)
               }
               logDf <- logDf %>%
                   summarise(minLat=min(.data$Latitude),
                             maxLat=max(.data$Latitude),
                             minLong=min(.data$Longitude),
                             maxLong=max(.data$Longitude)) %>%
                   ungroup() %>%
                   mutate(attempted = FALSE,
                          succeeded = FALSE,
                          file = '',
                          fail_message = '')

           }
    )
    logDf
}

# add params from nc folder
checkLogFiles <- function(logData, folder, ncFiles=NULL) {
    if(is.null(ncFiles)) {
        ncFiles <- list.files(folder, full.names=TRUE, recursive=TRUE, pattern='nc$')
    }
    logNcExists <- file.exists(logData$file)
    noNcMatch <- character(0)
    multiMatch <- character(0)
    for(i in which(!logNcExists)) {
        # if it doesnt think it succeeded dont check
        if(is.na(logData$succeeded[i]) ||
           !logData$succeeded[i]) {
            next
        }
        tryFind <- grep(logData$file[i], ncFiles, value=TRUE)
        if(length(tryFind) == 0) {
            noNcMatch <- c(noNcMatch, logData$file[i])
            logData$file[i] <- NA
            next
        }
        if(length(tryFind) > 1) {
            multiMatch <- c(multiMatch, logData$file[i])
        }
        logData$file[i] <- tryFind[1]
    }
    if(length(noNcMatch) > 0) {
        warning(length(noNcMatch), ' NetCDF files in the log file were not found ',
                'in the folder (', paste0(basename(noNcMatch), collapse=', '), ')')
    }
    if(length(multiMatch) > 0) {
        warning(length(multiMatch), ' NetCDF files in the log file matched multiple ',
                'files in the folder (', paste0(basename(multiMatch), collapse=', '), ')')
    }
    logData
}

addEnvParams <- function(data, folder, tz='UTC', ekNames=TRUE) {
    ncFiles <- list.files(folder, full.names=TRUE, recursive=TRUE, pattern='nc$')
    logFiles <- list.files(folder, full.names=TRUE, recursive=TRUE, pattern='HYCOMLog.*csv$')
    logs <- bind_rows(lapply(logFiles, function(x) {
        oneLog <- read.csv(x, stringsAsFactors = FALSE)
        # oneLog$day <- as.POSIXct(oneLog$day, format='%Y-%m-%d', tz='UTC')
        oneLog$day <- parse_date_time(oneLog$day, orders=c('%Y-%m-%d', '%m/%d/%Y'), tz='UTC')
        naDay <- is.na(oneLog$day)
        if(sum(naDay) > 0) {
            warning(sum(naDay), ' row(s) in log file ', basename(x),
            ' were either NA or had "day" formats that could not be converted',
                    call. = FALSE)
            oneLog <- oneLog[!naDay, ]
        }
        oneLog
    }))

    logs <- checkLogFiles(logs, folder, ncFiles=ncFiles)
    if(is.character(data)) {
        if(!file.exists(data)) {
            stop('File ', data, ' does not exist.')
        }
        data <- read.csv(data, stringsAsFactors = FALSE)
    }
    oldCols <- colnames(data)
    nCols <- ncol(data)
    data <- ekToStandardFormat(data, tz=tz)
    # some temp columns to match times and return original order
    data$MATCHDAY <- floor_date(data$UTC, unit='1day')
    data$ORDERIX <- 1:nrow(data)
    on.exit({
        colnames(data[1:nCols]) <- oldCols
        data$MATCHDAY <- NULL
        data <- arrange(data, .data$ORDERIX)
        data$ORDERIX <- NULL
        return(data)
    })
    noDayMatch <- numeric()
    noDlMatch <- numeric()
    noCoordMatch <- numeric()
    data <- bind_rows(lapply(split(data, data$MATCHDAY), function(x) {
        # one day at a time for easier matching
        thisLog <- logs[logs$day == x$MATCHDAY[1], ]
        if(nrow(thisLog) == 0) {
            noDayMatch <<- c(noDayMatch, x$ORDERIX)
            return(x)
        }
        if(!any(thisLog$succeeded)) {
            noDlMatch <<- c(noDlMatch, x$ORDERIX)
            return(x)
        }
        x$MATCHEDIX <- rep(0, nrow(x))
        for(i in 1:nrow(thisLog)) {
            if(!thisLog$succeeded[i]) {
                next
            }
            thisLogMatch <- x$Longitude >= thisLog$minLong[i] &
                x$Longitude <= thisLog$maxLong[i] &
                x$Latitude >= thisLog$minLat[i] &
                x$Latitude <= thisLog$maxLat[i]
            x$MATCHEDIX[thisLogMatch] <- i
        }
        noCoordMatch <<- c(noCoordMatch, x$ORDERIX[x$MATCHEDIX == 0])
        x <- bind_rows(lapply(split(x, x$MATCHEDIX), function(y) {
            if(y$MATCHEDIX[1] == 0) {
                return(y)
            }
            rawEnv <- ncToData(y, nc=thisLog$file[y$MATCHEDIX[1]], buffer=c(.16, .16, 0), raw=TRUE, depth=c(0, 200), progress=FALSE)
            cbind(y, calcEKParams(rawEnv, ekNames=ekNames))
        }))
        x$MATCHEDIX <- NULL
        x
    }))
    if(length(noDlMatch) > 0) {
        warning(length(noDlMatch), ' rows in data matched days with failed downloads.')
    }
    if(length(noDayMatch) > 0) {
        warning(length(noDayMatch), ' rows in data matched no days in download logs.')
    }
    if(length(noCoordMatch) > 0) {
        warning(length(noCoordMatch), ' rows in data matched a day but did not match coordinate range in download logs.')
    }
    data
}

makeDailyCSV <- function(grid, folder, name, hour=NULL, hourTz='America/Los_Angeles',
                         progress=TRUE, retry=FALSE, ekNames=TRUE) {
    logData <- loadLog(folder)
    if(is.null(logData)) {
        stop('No download log file found in ', folder)
    }
    # keep track of these bc we rename to standard
    if('UTC' %in% colnames(grid)) {
        grid$UTC <- NULL
    }
    oldCols <- colnames(grid)
    nCols <- ncol(grid)
    grid <- ekToStandardFormat(grid, noTime=TRUE)
    logData <- checkLogFiles(logData, folder)
    logData <- logData[logData$succeeded & !is.na(logData$file), ]
    if(progress) {
        pb <- txtProgressBar(min=0, max=nrow(logData), style=3)
    }
    for(i in 1:nrow(logData)) {
        thisNc <- logData$file[i]
        thisGrid <- grid
        thisGrid$UTC <- logData$day[i] # MAY CHANGE? MEAN ACROSS?
        if(!is.null(hour)) {
            thisGrid$UTC <- thisGrid$UTC + hour * 3600
            thisGrid$UTC <- with_tz(force_tz(thisGrid$UTC, tzone=hourTz), tzone='UTC')
        }
        thisCsv <- paste0(name, '_', format(thisGrid$UTC[1], format='%Y-%m-%d_%H-%M-%S'), '.csv')
        thisCsv <- file.path(folder, thisCsv)
        if(retry || !file.exists(thisCsv)) {

            thisRaw <- ncToData(thisGrid, nc=thisNc, buffer=c(.16, .16, 0), raw=TRUE,
                                depth=c(0, 200), progress=FALSE)
            thisGrid$UTC <- NULL
            thisGrid <- cbind(grid, calcEKParams(thisRaw, ekNames=ekNames))
            colnames(thisGrid)[1:nCols] <- oldCols
            write.csv(thisGrid, file = thisCsv)
        }
        if(progress) {
            setTxtProgressBar(pb, value=i)
        }
    }
    TRUE
}

parseToUTC <- function(x, format=c('%m/%d/%Y %H:%M:%S', '%m-%d-%Y %H:%M:%S',
                                   '%Y/%m/%d %H:%M:%S', '%Y-%m-%d %H:%M:%S'), tz) {
    tryCatch({
        testTz <- parse_date_time('10-10-2020 12:00:05', orders = '%m/%d/%Y %H:%M:%S', tz=tz)
    },
    error = function(e) {
        msg <- e$message
        if(grepl('CCTZ: Unrecognized output timezone', msg)) {
            stop('Timezone not recognized, see function OlsonNames() for accepted options', call.=FALSE)
        }
    })
    if(is.Date(x)) {
        x <- as.POSIXct(x, tz=tz)
    }
    if(!inherits(x, 'POSIXct')) {
        origTz <- parse_date_time(x, orders=format, tz=tz, exact=TRUE, truncated=3)
        if(!inherits(origTz, 'POSIXct')) {
            stop('Unable to convert to POSIXct time.', call.=FALSE)
        }
    } else {
        origTz <- x
    }
    with_tz(origTz, tzone='UTC')
}

plotEnvComp <- function(x) {
    x$plotId <- 1:nrow(x)
    basicPlot <- ggplot(x, aes(x=plotId)) +
        geom_line(aes(y=sst.mean - sst_mean, col='sst.m')) +
        geom_line(aes(y=sst.SD - sst_sd, col='sst.s')) +
        geom_line(aes(y=sss.mean - sss_mean, col='sss.m')) +
        geom_line(aes(y=sss.SD - sss_sd, col='sss.s')) +
        geom_line(aes(y=u.mean - u_mean, col='u.m')) +
        geom_line(aes(y=u.SD - u_sd, col='u.s')) +
        geom_line(aes(y=v.mean - v_mean, col='v.m')) +
        geom_line(aes(y=v.SD - v_sd, col='v.s')) +
        geom_line(aes(y=ssh.mean - ssh_mean, col='ssh.m')) +
        geom_line(aes(y=ssh.SD - ssh_sd, col='ssh.s')) +
        ggtitle('Basic EnvData Comparison') +
        ylab('Difference (Absolute)')
    ildPlot <- ggplot(x, aes(x=plotId)) +
        geom_line(aes(y=ild.mean - ild_mean, col='ild.m')) +
        geom_line(aes(y=ild.SD - ild_sd, col='ild.s')) +
        ggtitle('ILD Comparison') +
        ylab('Difference (Absolute)')
    # sstmAvg <- median(abs(x$sst.mean), na.rm=TRUE)
    # sstsAvg <- median(abs(x$sst.SD), na.rm=TRUE)
    # sssmAvg <- median(abs(x$sss.mean), na.rm=TRUE)
    # ssssAvg <- median(abs(x$sss.SD), na.rm=TRUE)
    # umAvg <- median(abs(x$u.mean), na.rm=TRUE)
    # usAvg <- median(abs(x$u.SD), na.rm=TRUE)
    # vmAvg <- median(abs(x$v.mean), na.rm=TRUE)
    # vsAvg <- median(abs(x$v.SD), na.rm=TRUE)
    # sshmAvg <- median(abs(x$ssh.mean), na.rm=TRUE)
    # sshsAvg <- median(abs(x$ssh.SD), na.rm=TRUE)
    # ildmAvg <- median(abs(x$ild.mean), na.rm=TRUE)
    # ildsAvg <- median(abs(x$ild.SD), na.rm=TRUE)
    sstmAvg <- diff(range(x$sst.mean, na.rm=TRUE))
    sstsAvg <- diff(range(x$sst.SD, na.rm=TRUE))
    sssmAvg <- diff(range(x$sss.mean, na.rm=TRUE))
    ssssAvg <- diff(range(x$sss.SD, na.rm=TRUE))
    umAvg <- diff(range(x$u.mean, na.rm=TRUE))
    usAvg <- diff(range(x$u.SD, na.rm=TRUE))
    vmAvg <- diff(range(x$v.mean, na.rm=TRUE))
    vsAvg <- diff(range(x$v.SD, na.rm=TRUE))
    sshmAvg <- diff(range(x$ssh.mean, na.rm=TRUE))
    sshsAvg <- diff(range(x$ssh.SD, na.rm=TRUE))
    ildmAvg <- diff(range(x$ild.mean, na.rm=TRUE))
    ildsAvg <- diff(range(x$ild.SD, na.rm=TRUE))
    basicPlotPct <- ggplot(x, aes(x=plotId)) +
        geom_line(aes(y=(sst.mean - sst_mean)/sstmAvg, col='sst.m')) +
        geom_line(aes(y=(sst.SD - sst_sd)/sstsAvg, col='sst.s')) +
        geom_line(aes(y=(sss.mean - sss_mean)/sssmAvg, col='sss.m')) +
        geom_line(aes(y=(sss.SD - sss_sd)/ssssAvg, col='sss.s')) +
        geom_line(aes(y=(u.mean - u_mean)/umAvg, col='u.m')) +
        geom_line(aes(y=(u.SD - u_sd)/usAvg, col='u.s')) +
        geom_line(aes(y=(v.mean - v_mean)/vmAvg, col='v.m')) +
        geom_line(aes(y=(v.SD - v_sd)/vsAvg, col='v.s')) +
        # geom_line(aes(y=ild.mean - ild_mean, col='ildm')) +
        # geom_line(aes(y=ild.SD - ild_sd, col='ilds')) +
        geom_line(aes(y=(ssh.mean - ssh_mean)/sshmAvg, col='ssh.m')) +
        geom_line(aes(y=(ssh.SD - ssh_sd)/sshsAvg, col='ssh.s')) +
        ggtitle('Basic EnvData Comparison') +
        ylab('Difference (Percent)')
    ildPlotPct <- ggplot(x, aes(x=plotId)) +
        geom_line(aes(y=(ild.mean - ild_mean)/ildmAvg, col='ild.m')) +
        geom_line(aes(y=(ild.SD - ild_sd)/ildsAvg, col='ild.s')) +
        ggtitle('ILD Comparison') +
        ylab('Difference (Percent)')
    (basicPlot + ildPlot) /
        (basicPlotPct + ildPlotPct)
}
