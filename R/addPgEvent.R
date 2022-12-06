#' @title Add Pamguard Event to Database
#'
#' @description Add a new event to an existing Pamguard database in the "OfflineEvents"
#'   table. If the specified \code{eventType} does not exist in the database, it will
#'   be added to the "Lookup" table.
#'
#' @param db database file to add an event to
#' @param UIDs vector of the UIDs of the individual detections to add to the event
#' @param binary binary file containing the detections from \code{UIDs}
#' @param eventType the name of the event type to add. If this is not already
#'   present in the database, it will be added to the "Lookup" table
#' @param comment (optional) a comment for the event
#' @param tableName (optional) specify the name of the Click Detector that generated the
#'   event table you want to add to. This only needs to be specified if you have
#'   more than one click detector, it defaults to the first "NAME_OfflineEvents"
#'   table in the database.
#' @param start (optional) start time of event. Mandatory if no detections are added
#' @param end (optional) end time of event. Mandatory if no detections are added
#' @param type type of event data to add, either \code{'click'} to add event data using
#'   the Click Detector module, or \code{'dg'} to add event data using the Detection
#'   Grouper module
#'
#' @return Adds to the database \code{db}, invisibly returns \code{TRUE} if successful
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#' \dontrun{
#' myDb <- 'PamguardDatabase.sqlite3'
#' myBinaries <- c('./Binaries/Bin1.pgdf', './Binaries/Bin2.pgdf')
#' addUIDs <- c(10000001, 10000002, 20000007, 20000008)
#' addPgEvent(db = myDb, UIDs = addUIDs, binary = myBinaries, eventType = 'MyNewEvent')
#' }
#' @importFrom RSQLite dbConnect SQLite dbListTables dbReadTable dbDisconnect dbAppendTable dbSendQuery dbClearResult
#' @importFrom PamBinaries loadPamguardBinaryFile convertPgDate pbToDf
#' @importFrom dplyr bind_rows select setdiff
#' @importFrom utils packageVersion
#'
#' @export
#'
addPgEvent <- function(db, UIDs=NULL, binary, eventType, comment = NA, 
                       tableName = NULL, start=NULL, end=NULL,type=c('click', 'dg')) {
    if(!file.exists(db)) {
        stop('Could not find database file', db, call. = FALSE)
    }
    type <- match.arg(type)
    con <- dbConnect(db, drv = SQLite())
    on.exit(dbDisconnect(con))
    tableList <- dbListTables(con)
    switch(type,
           'click' = {
               getName <- grep('OfflineClicks', tableList, value=TRUE)[1]
               tableName <- gsub('_OfflineClicks', '', getName)
               clickTableName <- paste0(tableName, '_OfflineClicks')
               eventTableName <- paste0(tableName, '_OfflineEvents')
               commentCol <- 'comment'
               lookupTopic <- 'OfflineRCEvents'
               endTimeCol <- 'EventEnd'
               nCol <- 'nClicks'
               if(!(clickTableName %in% tableList) ||
                  !(eventTableName %in% tableList)) {
                   stop('Could not find Click tables in database, check "tableName" or create ',
                        'Click Detector in Pamguard first.')
               }
           },
           'dg' = {
               getName <- grep('_Children', tableList, value=TRUE)[1]
               tableName <- gsub('_Children', '', getName)
               clickTableName <- paste0(tableName, '_Children')
               eventTableName <- tableName
               commentCol <- 'Text_Annotation'
               lookupTopic <- 'DGEventType'
               endTimeCol <- 'EndTime'
               nCol <- 'DataCount'
               if(!(clickTableName %in% tableList) ||
                  !(eventTableName %in% tableList)) {
                   stop('Could not find Detection Grouper tables in database, check "tableName" or create ',
                        'Detection Grouper in Pamguard first.')
               }
           }
    )
    
    
    # intiialise event and click tables to add
    eventData <- dbReadTable(con, eventTableName)
    eventAppend <- eventData[FALSE, ]
    eventAppend[1, ] <- NA
    if(nrow(eventData) == 0) {
        evId <- 1
        evUID <- 1
        evColor <- 0
    } else {
        evId <- max(eventData$Id, na.rm=TRUE) + 1
        evUID <- max(eventData$UID, na.rm=TRUE) + 1
        evColor <- (eventData$colour[nrow(eventData)] + 1) %% 13
        eventData$eventType <- str_trim(eventData$eventType)
    }
    clickData <- dbReadTable(con, clickTableName)
    chanCols <- c('ChannelBitmap', 'Channels')
    chanCols <- chanCols[chanCols %in% colnames(clickData)]
    if(!is.null(start)) {
        startMillis <- 0
    }
    addTime <- format(nowUTC(), format='%Y-%m-%d %H:%M:%S')
    #------------ Adding detection data----
    if(length(UIDs) == 0) {
        allAppend <- clickData[FALSE, ]
        addChan <- NA
        if(is.null(start) ||
           is.null(end)) {
            stop('If event has no detections must specify start/end time')
        }
    } else {
        UIDs <- sort(UIDs)
        UIDsToAdd <- UIDs
        allAppend <- vector('list', length = length(binary))
        names(allAppend) <- binary
        for(bin in binary) {
            if(length(UIDsToAdd) == 0) break
            binData <- loadPamguardBinaryFile(bin, skipLarge=TRUE, keepUIDs = UIDsToAdd)
            binDf <- pbToDf(binData)
            if(is.null(binDf) ||
               nrow(binDf) == 0) next
            UIDsToAdd <- UIDsToAdd[!(UIDsToAdd %in% binDf$UID)]
            binDf$millis <- binDf$millis - floor(binDf$date) * 1e3
            binDf$dbDate <- paste0(format(convertPgDate(binDf$date), format='%Y-%m-%d %H:%M:%S'), '.',
                                   binDf$millis)
            clickAppend <- clickData[FALSE, ]
            clickAppend[1:nrow(binDf), ] <- NA
            clickAppend$UTC <- binDf$dbDate
            clickAppend$UTCMilliseconds <- binDf$millis
            clickAppend$PCLocalTime <- clickAppend$UTC
            clickAppend$PCTime <- addTime
            clickAppend$UID <- binDf$UID
            clickAppend$parentID <- evId
            clickAppend$parentUID <- evUID
            if(type == 'click') {
                clickAppend$EventId <- evId
                clickAppend$ClickNo <- binDf$UID - floor(binDf$UID / 1e5) * 1e5 - 1 # java index start at 0
            }
            clickAppend$BinaryFile <- basename(bin)
            for(c in chanCols) {
                clickAppend[[c]] <- binDf$channelMap
            }
            clickAppend$LongDataName <- switch(
                binData$fileInfo$fileHeader$moduleType,
                'Click Detector' = paste0(binData$fileInfo$fileHeader$moduleName, ', ', binData$fileInfo$fileHeader$streamName),
                'WhistlesMoans' = paste0(binData$fileInfo$fileHeader$moduleName, c(', ', ' Contours'), collapse = ''),
                'GPL Detector' = paste0(binData$fileInfo$fileHeader$moduleName, c(', ', ' Detections'), collapse = '')
            )
            clickAppend$TEMPTIME <- binDf$date
            allAppend[[bin]] <- clickAppend
        }
        if(length(UIDsToAdd) > 0) {
            warning('Could not find UID(s) ', paste0(UIDsToAdd, collapse=', '),
                    ' in binary files.')
        }
        allAppend <- bind_rows(allAppend)
        if(is.null(start)) {
            start <- allAppend$UTC[which.min(allAppend$TEMPTIME)]
            startMillis <- allAppend$UTCMilliseconds[which.min(allAppend$TEMPTIME)]
        }
        if(is.null(end)) {
            end <- allAppend$UTC[which.max(allAppend$TEMPTIME)]
        }
        addChan <- unique(allAppend$ChannelBitmap)[1]
        allAppend['TEMPTIME'] <- NULL
    }
    # ----- working on event data ----
    start <- format(start, format='%Y-%m-%d %H:%M:%S')
    end <- format(end, format='%Y-%m-%d %H:%M:%S')
    chanCols <- c('ChannelBitmap', 'channels')
    chanCols <- chanCols[chanCols %in% colnames(eventAppend)]
    eventAppend$Id <- evId
    eventAppend$UID <- evUID
    
    eventAppend$UTC <- start
    eventAppend$PCLocalTime <- eventAppend$UTC
    eventAppend$PCTime <- addTime
    
    eventAppend[[endTimeCol]] <- end
    eventAppend$UTCMilliseconds <- startMillis
    
    eventAppend[[nCol]] <- nrow(allAppend)
    if(!'eventType' %in% colnames(eventData)) {
        addColQ <- paste0('ALTER TABLE ', eventTableName,
                          ' ADD COLUMN eventType CHAR(12)')
        colQ <- dbSendQuery(con, addColQ)
        dbClearResult(colQ)
    }
    eventAppend$eventType <- eventType
    for(c in chanCols) {
        eventAppend[[c]] <- addChan
    }
    if(is.na(comment)) {
        comment <- paste0('Created by PAMmisc v', packageVersion('PAMmisc'))
    }
    if(!commentCol %in% colnames(eventData)) {
        addColQ <- paste0('ALTER TABLE ', eventTableName,
                          ' ADD COLUMN ', commentCol, ' CHAR(80)')
        colQ <- dbSendQuery(con, addColQ)
        dbClearResult(colQ)
    }
    eventAppend[[commentCol]] <- comment
    if(type == 'click') {
        eventAppend$colour <- evColor
    }
    # clickData$Id <- NA
    # eventData$Id <- NA
    
    
    # Do append if there was no data or if data we want to add doesnt already exist
    if(nrow(eventData) == 0 ||
       nrow(setdiff(eventAppend[c('UTC', 'UTCMilliseconds', endTimeCol, 'eventType')],
                    eventData[c('UTC', 'UTCMilliseconds', endTimeCol, 'eventType')])) > 0) {
        dbAppendTable(con, clickTableName, allAppend)
        dbAppendTable(con, eventTableName, eventAppend)
    } else {
        return(FALSE)
    }
    
    # Add eventType to Lookup table if it isnt there, also create Lookup if not there
    if(!('Lookup' %in% tableList)) {
        tbl <- dbSendQuery(con,
                           "CREATE TABLE Lookup
            (Id INTEGER,
            Topic CHARACTER(50),
            DisplayOrder INTEGER,
            Code CHARACTER(12),
            ItemText CHARACTER(50),
            isSelectable INTEGER,
            FillColour CHARACTER(20),
            BorderColour CHARACTER(20),
            Symbol CHARACTER(2),
            PRIMARY KEY (Id))")
        dbClearResult(tbl)
    }
    lookup <- dbReadTable(con, 'Lookup')
    if(nrow(lookup) > 0 &&
       (eventType %in% str_trim(lookup$Code[str_trim(lookup$Topic) == lookupTopic]))) {
        return(invisible(TRUE))
    }
    # if(eventType %in% str_trim(lookup$Code)) {
    #     return(invisible(TRUE)) # dont need to add, just exit
    # }
    lookAppend <- lookup[FALSE, ]
    lookAppend[1, ] <- NA
    if(nrow(lookup) == 0) {
        lookAppend$Id <- 1
        lookAppend$DisplayOrder <- 10
    } else {
        lookAppend$Id <- max(lookup$Id, na.rm=TRUE) + 1
        lookAppend$DisplayOrder <- max(lookup$DisplayOrder, na.rm=TRUE) + 10
    }
    lookAppend$Topic <- lookupTopic
    lookAppend$Code <- eventType
    lookAppend$ItemText <- eventType
    lookAppend$isSelectable <- 1
    lookAppend$FillColour <- 'RGB(255,0,0)'
    lookAppend$BorderColour <- 'RGB(255,0,0)'
    lookAppend$Symbol <- '^'
    dbAppendTable(con, 'Lookup', lookAppend)
    invisible(TRUE)
}

createClickTables <- function(con) {
    # dbCreateTable(con, 'gpsData', GPSDF) then dbAppendTable(con, 'gpsData', GPSDF)
    # is sort of an option if we convert UTC to character first
    clickTbl <- dbSendQuery(con,
                            "CREATE TABLE Click_Detector_OfflineClicks
            (Id INTEGER,
            UID BIGINT,
            UTC TIMESTAMP,
            UTCMilliseconds INTEGER,
            PCLocalTime CHARACTER(50),
            PCTime CHARACTER(50),
            ChannelBitmap INTEGER,
            SequenceBitmap INTEGER,
            parentID INTEGER,
            parentUID INTEGER,
            LongDataName CHARACTER(80),
            BinaryFile CHARACTER(80),
            EventId INTEGER,
            ClickNo INTEGER,
            Amplitude DOUBLE,
            Channels INTEGER,
            PRIMARY KEY (Id))")
    dbClearResult(clickTbl)
    eventTbl <- dbSendQuery(con,
                            "CREATE TABLE Click_Detector_OfflineEvents
            (Id INTEGER,
            UID BIGINT,
            UTC TIMESTAMP,
            UTCMilliseconds INTEGER,
            PCLocalTime CHARACTER(50),
            PCTime CHARACTER(50),
            ChannelBitmap INTEGER,
            SequenceBitmap INTEGER,
            EventEnd TIMESTAMP,
            eventType CHAR(12),
            nClicks INTEGER,
            minNumber SMALLINT,
            bestNumber SMALLINT,
            maxNumber SMALLINT,
            colour SMALLINT,
            comment CHAR(80),
            channels INTEGER,
            TMModelName1 CHAR(30),
            TMLatitude1 DOUBLE,
            TMLongitude1 DOUBLE,
            BeamLatitude1 DOUBLE,
            BeamLongitude1 DOUBLE,
            BeamTime1 TIMESTAMP,
            TMSide1 INTEGER,
            TMChi21 DOUBLE,
            TMAIC1 DOUBLE,
            TMProbability1 DOUBLE,
            TMDegsFreedom1 INTEGER,
            TMPerpendicularDistance1 DOUBLE,
            TMPerpendicularDistanceError1 DOUBLE,
            TMDepth1 DOUBLE,
            TMDepthError1 DOUBLE,
            TMHydrophones1 INTEGER,
            TMError1 CHAR(128),
            TMComment1 CHAR(80),
            TMLatitude2 DOUBLE,
            TMLongitude2 DOUBLE,
            BeamLatitude2 DOUBLE,
            BeamLongitude2 DOUBLE,
            BeamTime2 TIMESTAMP,
            TMSide2 INTEGER,
            TMChi22 DOUBLE,
            TMAIC2 DOUBLE,
            TMProbability2 DOUBLE,
            TMDegsFreedom2 INTEGER,
            TMPerpendicularDistance2 DOUBLE,
            TMPerpendicularDistanceError2 DOUBLE,
            TMDepth2 DOUBLE,
            TMDepthError2 DOUBLE,
            TMHydrophones2 INTEGER,
            TMError2 CHAR(128),
            TMComment2 CHAR(80),
            PRIMARY KEY (Id))")
    on.exit(dbClearResult(eventTbl), add=TRUE, after=FALSE)
}

createUDFDropdown <- function(con) {
    UDFtbl <- dbSendQuery(con,
                          "CREATE TABLE UDF_Dropdown
            (Id INTEGER,
            Order INTEGER,
            Type CHAR(50),
            Title CHAR(50),
            PostTitle CHAR(50),
            DbTitle CHAR(50),
            Length INTEGER,
            Topic CHAR(50),
            NMEA_Module CHAR(50),
            NMEA_String CHAR(50),
            NMEA_Position INTEGER,
            Required BOOLEAN,
            AutoUpdate INTEGER,
            Autoclear BOOLEAN,
            ForceGps BOOELAN,
            Hint CHAR(100),
            ADC_Channel INTEGER,
            ADC_Gain DOUBLE,
            Analog_Multiply DOUBLE,
            Analog_Add DOUBLE,
            Plot BOOLEAN,
            Height INTEGER,
            Colour CHAR(20),
            MinValue DOUBLE,
            MaxValue DOUBLE,
            ReadOnly BOOLEAN,
            Send_Control_Name CHAR(50),
            Control_on_Subform CHAR(50),
            Get_Control_Data CHAR(50),
            Default CHAR(50),
            PRIMARY KEY (Id))")
    dbClearResult(UDFtbl)
    udf <- dbReadTable(con, 'UDF_Dropdown')
    appendUDF <- udf[FALSE,]
    appendUDF[1:2,] <- NA
    appendUDF$Id <- 1:2
    appendUDF$Order <- c(10, 20)
    appendUDF$Type <- c('ORDER', 'LOOKUP')
    appendUDF$Title[2] <- 'eventTypeDropdown'
    appendUDF$PostTitle[2] <- 'eventType'
    appendUDF$DbTitle[2] <- 'eventType'
    appendUDF$Length <- c(1, 12)
    appendUDF$Topic[2] <- 'DGEventType'
    appendUDF$Required <- c(0, 1)
    appendUDF$Autoclear <- c(0, 0)
    appendUDF$ForceGps <- c(0, 0)
    appendUDF$Plot <- c(0, 0)
    appendUDF$ReadOnly <- c(0, 0)
    dbAppendTable(con, 'UDF_Dropdown', appendUDF)
}
# Loop through all binary files until all UIDs are found. Warn if UIDs not found.
# Dont like update option - should be add only, not modify/delete
