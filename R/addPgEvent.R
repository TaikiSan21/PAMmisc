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
#' @importFrom PamBinaries loadPamguardBinaryFile convertPgDate
#' @importFrom dplyr bind_rows select
#'
#' @export
#'
addPgEvent <- function(db, UIDs, binary, eventType, comment = NA, tableName = NULL) {
    con <- dbConnect(db, drv = SQLite())
    on.exit(dbDisconnect(con))
    tableList <- dbListTables(con)
    if(is.null(tableName)) {
        getName <- grep('OfflineClicks', tableList, value=TRUE)[1]
        tableName <- gsub('_OfflineClicks', '', getName)
    }
    clickTableName <- paste0(tableName, '_OfflineClicks')
    eventTableName <- paste0(tableName, '_OfflineEvents')
    if(!(clickTableName %in% tableList) ||
       !(eventTableName %in% tableList)) {
        stop('Could not find Click tables in database, check tableName or create',
             'Click Detector in Pamguard first.')
    }
    # intiialise event and click tables to add
    eventData <- dbReadTable(con, eventTableName)
    eventAppend <- eventData[FALSE, ]
    eventAppend[1, ] <- NA
    if(nrow(eventData) == 0) {
        evId <- 1
        evUID <- 1
    } else {
        evId <- max(eventData$Id) + 1
        evUID <- max(eventData$UID) + 1
    }

    clickData <- dbReadTable(con, clickTableName)

    UIDs <- sort(UIDs)
    UIDsToAdd <- UIDs
    allAppend <- vector('list', length = length(binary))
    names(allAppend) <- binary
    for(bin in binary) {
        if(length(UIDsToAdd) == 0) break
        binData <- loadPamguardBinaryFile(bin, skipLarge=TRUE, keepUIDs = UIDsToAdd)
        binDf <- data.frame(binData)
        if(nrow(binDf) == 0) next
        UIDsToAdd <- UIDsToAdd[!(UIDsToAdd %in% binDf$UID)]
        binDf$millis <- binDf$millis - floor(binDf$date) * 1e3
        binDf$dbDate <- paste0(as.character(convertPgDate(binDf$date)), '.',
                               binDf$millis)
        clickAppend <- clickData[FALSE, ]
        clickAppend[1:nrow(binDf), ] <- NA
        clickAppend$ClickNo <- binDf$UID - floor(binDf$UID / 1e5) * 1e5 - 1 # java index start at 0
        clickAppend$UTC <- binDf$dbDate
        clickAppend$UTCMilliseconds <- binDf$millis
        clickAppend$UID <- binDf$UID
        clickAppend$parentID <- evId
        clickAppend$parentUID <- evUID
        clickAppend$EventId <- evId
        clickAppend$BinaryFile <- basename(bin)
        clickAppend$LongDataName <- switch(
            binData$fileInfo$fileHeader$moduleType,
            'Click Detector' = paste0(binData$fileInfo$fileHeader$moduleName, ', ', binData$fileInfo$fileHeader$streamName),
            'WhistlesMoans' = paste0(binData$fileInfo$fileHeader$moduleName, c(', ', ' Contours'), collapse = '')
        )
        clickAppend$TEMPTIME <- binDf$date
        allAppend[[bin]] <- clickAppend
    }
    if(length(UIDsToAdd) > 0) {
        warning('Could not find UID(s) ', paste0(UIDsToAdd, collapse=', '),
                ' in binary files.')
    }
    allAppend <- bind_rows(allAppend)
    eventAppend$Id <- evId
    eventAppend$UID <- evUID
    eventAppend$UTC <- allAppend$UTC[which.min(allAppend$TEMPTIME)]
    eventAppend$EventEnd <- allAppend$UTC[which.max(allAppend$TEMPTIME)]
    eventAppend$UTCMilliseconds <- allAppend$UTCMilliseconds[which.min(allAppend$TEMPTIME)]
    allAppend['TEMPTIME'] <- NULL
    eventAppend$nClicks <- nrow(allAppend)
    eventAppend$eventType <- eventType
    eventAppend$comment <- comment

    dbAppendTable(con, clickTableName, allAppend)
    dbAppendTable(con, eventTableName, eventAppend)

    # Add eventType to Lookup table if it isnt there, also create Lookup if not there
    if(!('Lookup' %in% tableList)) {
        dbSendQuery(con,
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
    }
    lookup <- dbReadTable(con, 'Lookup')
    if(eventType %in% gsub(' ', '', lookup$Code)) {
        return(invisible(TRUE)) # dont need to add, just exit
    }
    lookAppend <- lookup[FALSE, ]
    lookAppend[1, ] <- NA
    if(nrow(lookup) == 0) {
        lookAppend$Id <- 1
        lookAppend$DisplayOrder <- 10
    } else {
        lookAppend$Id <- max(lookup$Id) + 1
        lookAppend$DisplayOrder <- max(lookup$DisplayOrder) + 10
    }
    lookAppend$Topic <- 'OfflineRCEvents'
    lookAppend$Code <- eventType
    lookAppend$ItemText <- eventType
    lookAppend$isSelectable <- 1
    lookAppend$FillColour <- 'RGB(255,0,0)'
    lookAppend$BorderColour <- 'RGB(255,0,0)'
    lookAppend$Symbol <- '^'
    dbAppendTable(con, 'Lookup', lookAppend)
    invisible(TRUE)
}

# Loop through all binary files until all UIDs are found. Warn if UIDs not found.
# Dont like update option - should be add only, not modify/delete
