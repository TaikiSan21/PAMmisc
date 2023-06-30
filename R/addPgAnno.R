#' @title Add Spectrogram Annotations to Pamguard Database
#'
#' @description Add new annotations to an existing Pamguard Spectrogram Annotations
#'   table
#'
#' @param db database file to add annotations to
#' @param anno annotations to add, must contain columns \code{UTC}, \code{Duration} (seconds),
#'   \code{f1} (min freq Hz), and \code{f2} (max freq Hz). Any other columns matching columns in the database
#'   will also be added
#' @param tableName name of the annotation table in the database
#' @param channel channel to display the annotations on
#' @param source annotation source. If \code{'manual'}, columns \code{UTC}, \code{DUration},
#'   \code{f1}, and \code{f2} must be present. Other options will attempt to automate
#'   conversion to these column names from specific output sources
#' @param format date format, default will try two variations of MDY HMS and YMD HMS
#' @param tz timezone of provided date
#'
#' @return Returns a dataframe of the rows added to the database
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#' \dontrun{
#' myDb <- 'PamguardDatabase.sqlite3'
#' myAnno <- data.frame(UTC = '2021/10/23 12:10:10', Duration = .563, f1=2300, f2=3600)
#' addPgAnno(myDb, myAnno, tableName='Spectrogram_Annotation', source='manual')
#' }
#'
#' @importFrom RSQLite dbConnect SQLite dbListTables dbReadTable dbDisconnect dbAppendTable
#' @importFrom dplyr bind_rows select setdiff rename
#' @importFrom utils read.table
#'
#' @export
#'
addPgAnno <- function(db, anno, tableName=NULL, channel=1,
                      source=c('manual', 'aplose', 'pammisc', 'annomate', 'raven'),
                      format = c('%m/%d/%Y %H:%M:%OS', '%m-%d-%Y %H:%M:%OS', '%Y/%m/%d %H:%M:%OS', '%Y-%m-%d %H:%M:%OS'),
                      tz='UTC') {
    source <- match.arg(source)
    if(!file.exists(db)) {
        stop('Could not find database file', db, call. = FALSE)
    }
    con <- dbConnect(db, drv=SQLite())
    on.exit({
        dbDisconnect(con)
    })
    tbls <- dbListTables(con)
    if(is.null(tableName)) {
        stop('Must specify Spectrogram Annotation database table name to append to')
    }
    tableName <- gsub(' ', '_', tableName)
    if(!tableName %in% tbls) {
        stop('Could not find table ', tableName, ' in database')
    }
    anno <- formatAnno(anno, source, format, tz)
    dbAnno <- dbReadTable(con, tableName)
    if(nrow(dbAnno) > 0) {
        # check for duplicates
        dbBox <- dbAnno[c('UTC', 'Duration', 'f1', 'f2')]
        annoBox <- anno[c('UTC', 'Duration', 'f1', 'f2')]
        # annoBox$UTC <- as.character(annoBox$UTC)
        dbBox$UTC <- as.POSIXct(dbBox$UTC, '%Y-%m-%d %H:%M:%OS', tz='UTC')
        isDupe <- duplicated(rbind(annoBox, dbBox), fromLast = TRUE)[1:nrow(anno)]
        if(all(isDupe)) {
            return(invisible(anno[FALSE,]))
        }
        anno <- anno[!isDupe, ]
    }

    annoAppend <- dbAnno[FALSE, ]
    annoAppend[1:nrow(anno), ] <- NA
    annoAppend$Note <- 'Entry created by PAMmisc'
    addCols <- colnames(anno)[colnames(anno) %in% colnames(dbAnno)]
    for(c in addCols) {
        annoAppend[[c]] <- anno[[c]]
    }
    if(nrow(dbAnno) == 0) {
        newIds <- 1:nrow(anno)
    } else {
        newIds <- 1:nrow(anno) + max(c(dbAnno$Id, dbAnno$UID), na.rm=TRUE)
    }
    timeChar <- format(anno$UTC, format='%Y-%m-%d %H:%M:%OS3')
    milliChar <- sprintf('%.3f', as.numeric(anno$UTC) - floor(as.numeric(anno$UTC)))
    milliChar <- gsub('^0', '', milliChar)
    # timeChar <- paste0(timeChar, milliChar)
    annoAppend$UTC <- annoAppend$PCLocalTime <- annoAppend$PCTime <- timeChar
    # do just UTC
    annoAppend$UTCMilliseconds <- as.numeric(milliChar) * 1e3
    annoAppend$Id <- annoAppend$UID <- newIds
    annoAppend$ChannelBitmap <- annoAppend$Sequence <- 2^(channel-1)
    dbAppendTable(con, tableName, annoAppend)
    annoAppend
}

formatAnno <- function(anno, source, format, tz) {
    anno <- switch(source,
                   'manual' = anno,
                   'aplose' = anno,
                   'raven' = fmtRaven(anno),
                   'pammisc' = {
                       rename(anno, UTC = 'start', f1='fmin', f2='fmax')
                   })
    needCols <- c('UTC', 'f1', 'f2', 'Duration')
    if(!all(needCols %in% colnames(anno))) {
        stop('"anno" must have columns "UTC", "Duration", "f1", and "f2"')
    }
    anno$UTC <- parseToUTC(anno$UTC, format, tz)
    anno
}

fmtRaven <- function(x) {
    if(!is.character(x) ||
       !file.exists(x)) {
        stop('Raven sources must be path to a Raven .txt file')
    }
    tbl <- read.table(x, header=TRUE, sep='\t')
    colnames(tbl) <- gsub('\\.', '', colnames(tbl))
    fileTime <- parseRavenPosix(x)
    tbl$DeltaTimes <- round(tbl$DeltaTimes, 3)
    tbl$BeginTimes <- round(tbl$BeginTimes, 3)
    out <- data.frame(UTC = fileTime + tbl[['BeginTimes']],
               Duration = tbl[['DeltaTimes']],
               f1 = tbl[['LowFreqHz']],
               f2 = tbl[['HighFreqHz']],
               Label = tbl[['Annotation']])
    distinct(out)
}

parseRavenPosix <- function(x) {
    format <- c('pamguard', 'pampal', 'soundtrap', 'sm3', 'icListens1', 'icListens2')
    # SUbbing raven table selection txt for wav because i already had code to work on .wav
    x <- gsub('Table\\.[0-9]*\\.selections\\.txt$', 'wav', x)
    for(f in format) {
        switch(
            f,
            'pamguard' = {
                date <- gsub('.*([0-9]{8}_[0-9]{6}_[0-9]{3})\\.wav$', '\\1', x)
                posix <- as.POSIXct(substr(date, 1, 15), tz = 'UTC', format = '%Y%m%d_%H%M%S')
                if(is.na(posix)) next
                millis <- as.numeric(substr(date, 17, 19)) / 1e3
                if(!is.na(posix)) {
                    break
                }
            },
            'pampal' = {
                date <- gsub('.*([0-9]{14}_[0-9]{3})\\.wav$', '\\1', x)
                posix <- as.POSIXct(substr(date, 1, 14), tz = 'UTC', format = '%Y%m%d%H%M%S')
                if(is.na(posix)) next
                millis <- as.numeric(substr(date, 16, 18)) / 1e3
                if(!is.na(posix)) {
                    break
                }
            },
            'soundtrap' = {
                date <- gsub('.*\\.([0-9]{12})\\.wav$', '\\1', x)
                posix <- as.POSIXct(date, format = '%y%m%d%H%M%S', tz='UTC')
                millis <- 0
                if(!is.na(posix)) {
                    break
                }
            },
            'sm3' = {
                date <- gsub('.*\\_([0-9]{8}_[0-9]{6})\\.wav$', '\\1', x)
                posix <- as.POSIXct(date, format = '%Y%m%d_%H%M%S', tz='UTC')
                millis <- 0
                if(!is.na(posix)) {
                    break
                }
            },
            'icListens1' = {
                date <- gsub('.*_([0-9]{8}-[0-9]{6})\\.wav$', '\\1', x)
                posix <- as.POSIXct(date, format = '%Y%m%d-%H%M%S', tz='UTC')
                millis <- 0
                if(!is.na(posix)) {
                    break
                }
            },
            'icListens2' = {
                date <- gsub('.*_([0-9]{6}-[0-9]{6})\\.wav$', '\\1', x)
                posix <- as.POSIXct(date, format = '%y%m%d-%H%M%S', tz='UTC')
                millis <- 0
                if(!is.na(posix)) {
                    break
                }
            }
        )
    }
    if(is.na(posix)) {
        stop('Could not parse wav file time from Raven table')
    }
    posix + millis
}