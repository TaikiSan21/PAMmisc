#' @title Add GPS to a Pamguard Database
#'
#' @description Add GPS data to an existing Pamguard database
#'
#' @param db database file to add gps data to
#' @param gps data.frame of gps data or a character of the file name to be read. If a
#'   data.frame or non-SPOT csv file, needs columns \code{UTC}, \code{Latitude}, and \code{Longitude}.
#'   If multiple separate tracks are present in the same dataset, this should be marked with
#'   a column labeled \code{Name}
#' @param source one of \code{SPOTcsv}, \code{SPOTgpx}, or \code{csv}. Describes the
#'   source of the GPS data, not needed if \code{gps} is a data.frame
#' @param format date format for converting to POSIXct, only needed for \code{source='csv'}.
#'   See \link{strptime}
#'
#' @return Adds to the database \code{db}, invisibly returns the \code{Name} of the GPS track
#'   if successful (\code{NA} if not named)
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#' \dontrun{
#' # not run because example files don't exist
#' myDb <- 'PamguardDatabase.sqlite3'
#' # adding from a .gpx file downloaded from SPOT
#' spotGpx <- 'SpotGPX.gpx'
#' addPgGps(myDb, spotGpx, source='SPOTgpx')
#' # adding from a csv file with a Y-M-D H:M date format
#' gpsCsv <- 'GPS.csv'
#' addPgGps(myDb, gpsCsv, source='csv', format='%Y-%m-%d %H:%M')
#' }
#'
#' @importFrom RSQLite dbConnect SQLite dbListTables dbReadTable dbDisconnect dbAppendTable dbSendQuery
#' @importFrom plotKML readGPX
#' @importFrom dplyr bind_rows
#' @importFrom lubridate parse_date_time
#'
#' @export
#'
addPgGps <- function(db, gps, source = c('SPOTcsv', 'SPOTgpx', 'csv'), format = '%m/%d/%Y %H:%M:%S') {
    source <- match.arg(source)
    if(!file.exists(db)) {
        stop('Could not find database file', db, call. = FALSE)
    }
    gps <- fmtGps(gps, source, format)
    con <- dbConnect(db, drv=SQLite())
    on.exit({

        dbDisconnect(con)
    })
    if(!('gpsData' %in% dbListTables(con))) {
        # dbCreateTable(con, 'gpsData', GPSDF) then dbAppendTable(con, 'gpsData', GPSDF)
        # is sort of an option if we convert UTC to character first
        tbl <- dbSendQuery(con,
                           "CREATE TABLE gpsData
            (Id INTEGER,
            UID INTEGER,
            UTC CHARACTER(50),
            UTCMilliseconds INTEGER,
            PCLocalTime CHARACTER(50),
            PCTime CHARACTER(50),
            ChannelBitmap INTEGER,
            SequenceBitmap INTEGER,
            GpsDate INTEGER,
            Latitude DOUBLE,
            Longitude DOUBLE,
            Speed DOUBLE,
            SpeedType CHARACTER(2),
            TrueHeading DOUBLE,
            MagneticHeading DOUBLE,
            MagneticVariation DOUBLE,
            GPSError INTEGER,
            DataStatus CHARACTER(3),
            PRIMARY KEY (Id))")
        on.exit(dbClearResult(tbl), add=TRUE, after=FALSE)
    }
    dbGps <- dbReadTable(con, 'gpsData')
    gpsAppend <- dbGps[FALSE, ]
    gpsAppend[1:nrow(gps), ] <- NA
    if(nrow(dbGps) == 0) {
        newIds <- 1:nrow(gps)
    } else {
        newIds <- 1:nrow(gps) + max(dbGps$Id)
    }
    timeChar <- as.character(gps$UTC)
    milliChar <- sprintf('%.3f', as.numeric(gps$UTC) - floor(as.numeric(gps$UTC)))
    milliChar <- gsub('^0', '', milliChar)
    timeChar <- paste0(timeChar, milliChar)
    gpsAppend$UTC <- gpsAppend$PCLocalTime <- gpsAppend$PCTime <- gpsAppend$GpsDate <- timeChar
    # do just UTC
    gpsAppend$UTCMilliseconds <- as.numeric(milliChar) * 1e3
    gpsAppend$Id <- gpsAppend$UID <- newIds
    gpsAppend$Latitude <- gps$Latitude
    gpsAppend$Longitude <- gps$Longitude
    dbAppendTable(con, 'gpsData', gpsAppend)
    invisible(unique(gps$Name))
}

#' @importFrom utils read.csv
#'
fmtGps <- function(x, source, format) {
    if(is.character(x)) {
        if(!file.exists(x)) {
            stop('Could not find GPS file', x, call. = FALSE)
        }
        # if file switch for differnt read types
        switch(source,
               'SPOTcsv' = {
                   format <- '%m/%d/%Y %H:%M:%S'
                   # sometimes have headers, sometimes not.
                   head <- read.csv(x, stringsAsFactors = FALSE, header=FALSE, nrows=1)
                   numericCol <- sapply(head, is.numeric)
                   result <- read.csv(x, header = !any(numericCol), stringsAsFactors = FALSE)
                   # check for row ID column and drop it
                   if(colnames(result)[1] == 'X') {
                       result <- result[-1]
                   }
                   if(is.na(parse_date_time(result[1, 1], orders=format,
                                            exact=TRUE, truncated=2, tz='UTC'))) {
                   # if(is.na(as.POSIXct(result[1, 1], tryFormats=format, tz='UTC'))) {
                       stop('File does not appear to be a SPOT csv. See note in ?addPgGps for non-SPOT csv files.')
                   }

                   numericCol <- which(sapply(result, is.numeric))
                   if(length(numericCol) != 2) {
                       stop('File does not appear to be a SPOT csv. See note in ?addPgGps for non-SPOT csv files.')
                   }
                   # sometimes a 2nd name column???
                   name <- result[[2]]
                   if(min(numericCol)-2 > 2) {
                       name <- paste0(name,'_', result[[3]])
                   }
                   result <- result[, unique(c(1, min(numericCol)-1, numericCol))]
                   colnames(result) <- c('UTC', 'Message', 'Latitude', 'Longitude')
                   result$Name <- name
                   result$UTC <- parse_date_time(result$UTC, orders=format,
                                                 tz='UTC', exact=TRUE, truncated=1)
                   # result$UTC <- as.POSIXct(result$UTC, tryFormats=format, tz='UTC')
               },
               'SPOTgpx' = {
                   gpx <- readGPX(x)
                   format <- '%Y-%m-%dT%H:%M:%SZ'
                   result <- do.call(rbind, lapply(gpx$tracks, function(x) {
                       tmp <- vector('list', length = length(x))
                       for(i in seq_along(x)) {
                           df <- x[[i]][, c('lon', 'lat', 'time')]
                           df$Name <- names(x)[i]
                           tmp[[i]] <- df
                       }
                       bind_rows(tmp)
                   }))
                   colnames(result) <- c('Longitude', 'Latitude', 'UTC', 'Name')
                   # result$UTC <- as.POSIXct(result$UTC, tz='UTC', format = format)
               },
               'csv' = {
                   result <- read.csv(x, stringsAsFactors = FALSE)
                   if(!all(c('UTC', 'Longitude', 'Latitude') %in% colnames(result))) {
                       stop('If uploading a non-SPOT csv file, must have columns UTC, Longitude, and Latitude.', call. = FALSE)
                   }
                   # result$UTC <- as.POSIXct(result$UTC, tz='UTC', format=format)
               },
               stop('Dont know how to process source ', source, call.=FALSE)
        )
    }
    # if df check formats and return
    if(is.data.frame(x)) {
        result <- x
    }
    # result should be df whether we read it in or was given
    if(!all(c('UTC', 'Longitude', 'Latitude') %in% colnames(result))) {
        stop('GPS data must have columns UTC, Longitude, and Latitude.', call. = FALSE)
    }
    if(is.character(result$UTC) ||
       is.factor(result$UTC)) {
        # result$UTC <- as.POSIXct(as.character(result$UTC), tz='UTC', format=format)
        result$UTC <- parse_date_time(as.character(result$UTC), orders=format,
                                      tz='UTC', exact=TRUE, truncated=2)
    }
    if(any(is.na(result$UTC))) {
        stop('Not able to properly convert UTC to POSIXct format, check format argument.', call. = FALSE)
    }
    if(!('Name' %in% colnames(result))) {
        result$Name <- NA
    }
    if(length(unique(result$Name)) > 1) {
        nameOpts <- unique(result$Name)
        nameChoice <- menu(choices = nameOpts, title=c('Found multiple named GPS tracks, pick one to add to the database.'))
        if(nameChoice == 0) {
            stop('Must choose a GPS track')
        }
        nameKeep <- nameOpts[nameChoice]
        for(i in seq_along(nameOpts[-nameChoice])) {
            nameOpts <- nameOpts[-nameChoice]
            nameChoice <- menu(choices = nameOpts,
                               title = c('Would you like to add another GPS track? Enter "0" to exit.'))
            if(nameChoice == 0) {
                break
            }
            nameKeep <- c(nameKeep, nameOpts[nameChoice])
        }
        result <- result[result$Name %in% nameKeep, ]
    }
    result
}
