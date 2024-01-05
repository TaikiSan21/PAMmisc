# hycomFunctions

library(lubridate)
library(dplyr)
library(PAMmisc)

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
calcEKParams <- function(x) {
    if(is.list(x) &&
       is.null(names(x))) {
        return(bind_rows(lapply(x, calcEKParams)))
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
    result
}
# INPUT
# data - a dataframe with UTC, Latitude, and Longitude
# folder - folder to store downloads for this project. .nc and log files will live here
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

downloadDailyHycom <- function(data, folder, log=NULL, buffer=c(0.16, 0.16, 0),
                               retry=FALSE, timeout=360, hyList=PAMmisc::hycomList, progress=TRUE) {
    if(!all(c('UTC', 'Latitude', 'Longitude') %in% names(data))) {
        stop('"data" must have "UTC", "Longitude", and "Latitude"')
    }
    if(is.null(log)) {
        log <- paste0(basename(folder), '_', 'HYCOMLog.csv')
        log <- file.path(folder, log)
    }
    if(!dir.exists(folder)) {
        dir.create(folder)
    }


    dataRange <- dataToRanges(data, buffer)
    days <- seq(from=floor_date(dataRange$UTC[1], unit='1day'),
                to = floor_date(dataRange$UTC[2], unit='1day'),
                by=24 * 3600)
    logDf <- data.frame(days=days,
                        attempted = FALSE,
                        succeeded = FALSE,
                        file = '',
                        fail_message='')
    if(file.exists(log)) {
        oldLog <- read.csv(log)
        oldLog$days <- as.POSIXct(oldLog$days, format='%Y-%m-%d', tz='UTC')
        logDf <- rbind(oldLog, logDf[!logDf$days %in% oldLog$days, ])
        logDf <- arrange(logDf, days)
    }
    if(retry) {
        toTry <- !logDf$succeeded
    } else {
        toTry <- !logDf$attempted
    }
    tried <- rep(FALSE, nrow(logDf))

    on.exit({
        nTried <- sum(tried)
        nSucceeded <- sum(logDf$succeeded[tried])
        nPlanned <- sum(toTry)
        write.csv(logDf, file = log, row.names = FALSE)
        cat('\nSucceeded on ', nSucceeded,
            'out of ', nTried, ' attempts ',
            '(out of ', nPlanned, ' planned attempts)', sep='')
    })
    if(!any(toTry)) {
        cat('No days to try to download.')
        if(isFALSE(retry)) {
            cat(' Did you mean to run with "retry=TRUE" ?')
        }
        return(logDf)
    }
    # hyList <- PAMmisc::hycomList
    if(progress) {
        pb <- txtProgressBar(min=0, max=sum(toTry), style=3)
        ix <- 0
    }
    for(i in which(toTry)) {
        logDf$attempted[i] <- TRUE
        tried[i] <- TRUE
        dlDf <- data.frame(Latitude = dataRange$Latitude,
                           Longitude = dataRange$Longitude,
                           UTC = c(logDf$days[i], logDf$days[i] + 24 * 3600))
        whichHy <- PAMmisc:::whichHycom(dlDf, hyList)[1]
        thisHy <- hyList$list[[whichHy]]
        thisHy <- varSelect(thisHy, select = c(T, T, T, T, T))
        filename <- paste0('HYCOM_',
                           format(logDf$days[i], format='%Y-%m-%d'),
                           '_',
                           format(logDf$days[i]+24*3600, format='%Y-%m-%d'),
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
