#' @title Update Detection UIDs
#'
#' @description Update the UIDs of detections in a Pamguard database.
#'   UIDs can become mismatched when re-running data, this will attempt
#'   to re-associate the new UIDs in binary files with detections in the
#'   database
#'
#' @param db database file to update UIDs
#' @param binaries folder of binary files to use for updating
#' @param verbose logical flag to show summary messages
#' @param progress logical flag to show progress bars
#'
#' @return Same database as \code{db}, but with an additional column
#'   "newUID" added to each detection table with updated UIDs if found.
#'   "newUID" will be -1 for any detections where no match was found
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#'
#' \dontrun{
#' # not run because sample data does not exist
#' db <- 'MismatchedUid.sqlite3'
#' bin <- './BinaryFolder'
#' updateUID(db, bin)
#' }
#'
#' @importFrom stringr str_trim
#' @importFrom RSQLite dbRemoveTable dbListFields
#' @importFrom dplyr distinct desc arrange
#'
#' @export
#'
updateUID <- function(db, binaries, verbose=TRUE, progress=TRUE) {
    binaries <- dirOrFile(binaries, pattern='pgdf')
    if(length(binaries) == 0) {
        stop('No binary files found, cannot update UIDs.')
    }

    if(length(db) > 1) {
        nAdded <- vector('list', length = length(db))
        names(nAdded) <- db
        for(i in seq_along(db)) {
            if(verbose) {
                cat('\nUpdating UIDs in database ', basename(db), sep='')
            }
            nAdded[[i]] <- updateUID(db[i], binaries, verbose, progress)
        }
        return(nAdded)
    }
    if(!file.exists(db)) {
        warning('Database ', db, ' does not exist.')
        return(0)
    }
    con <- dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    # binFiles <- list.files(binaries, recursive=TRUE, full.names=TRUE)

    detTables <- getEventTables(db)$detections
    nAdded <- vector('list', length = length(detTables))
    names(nAdded) <- detTables
    for(d in detTables) {
        thisTbl <- dbReadTable(con, d)
        if(nrow(thisTbl) == 0) next
        thisTbl$BinaryFile <- str_trim(thisTbl$BinaryFile)
        thisTbl$UTC <- as.POSIXct(as.character(thisTbl$UTC), format='%Y-%m-%d %H:%M:%OS', tz='UTC')
        if(progress) {
            cat('\nUpdating table "', d, '" ...\n', sep='')
            pb <- txtProgressBar(min = 0, max = length(unique(thisTbl$BinaryFile)), style=3)
        }
        newUids <- bind_rows(lapply(split(thisTbl, thisTbl$BinaryFile), function(b) {
            thisBin <- grep(b$BinaryFile[1], binaries, value=TRUE)
            if(length(thisBin) == 0) {
                return(NULL)
            }
            binUids <- lapply(thisBin, function(x) checkOneBin(b, x))
            goodCheck <- sapply(binUids, function(x) {
                isUid <- x$UID != -1
                sum(isUid)
            })
            hasMost <- min(which.max(goodCheck))
            result <- binUids[[hasMost]]
            for(i in seq_along(binUids)) {
                if(i == hasMost) next
                thisOne <- binUids[[i]]
                alreadyIn <- sapply(thisOne$Id, function(x) x %in% result$Id)
                isUid <- thisOne$UID > -1
                thisOne$Id <- thisOne$Id[!alreadyIn & isUid]
                thisOne$UID <- thisOne$UID[!alreadyIn & isUid]

                result <- bind_rows(result, thisOne)
            }
            if(progress) {
                setTxtProgressBar(pb, value = which(unique(thisTbl$BinaryFile) == basename(thisBin)))
            }
            result
        }))
        newUids <- arrange(distinct(newUids), desc(.data$UID))
        dupeId <- duplicated(newUids$Id)
        # if(any(newUids$UID[dupeId] > -1)) {
        #     #something something replicates
        #     1+1
        # }
        newUids <- newUids[!dupeId, ]
        nAdded[[d]] <- addNewUID(db, d, newUids)
        if(verbose) {
            cat('\nUpdated UIDs for ', nAdded[[d]], ' out of ',
                nrow(thisTbl), ' total detections.', sep='')
        }
    }
    # this has returned back the original detection table with a column "newUID" added
    # that is -1 if no match found, or the new UID if was match
    # need to go and update the actual database, but probably test this on some
    # studies youve already run first
    nAdded
}

# det is already df of table, binary is path to bin file
# see which det UIDs have a match in this binary file
checkOneBin <- function(det, binary) {
    if(length(binary) == 0) {
        return(NULL)
    }
    binData <- loadPamguardBinaryFile(binary, convertDate = FALSE, skipLarge=TRUE)$data

    binDf <- bind_rows(lapply(binData, function(x) {
        x[c('UID', 'date')]
    }))
    newUID <- sapply(det$UTC, function(x) {
        isMatch <- which((x - binDf$date) == 0)
        if(length(isMatch) == 0) {
            return(-1)
        }
        binDf$UID[min(isMatch)]
    })
    if(any(newUID == -1)) {
        # message('Binary file ', basename(binary), ' did not have matching times.')
    }
    return(list(Id = det$Id, UID = newUID))
    list(Id = det$Id, UID = binDf$UID)
}

# just helper get event table names cuz ppl can change bruh
getEventTables <- function(db) {
    con <- dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    tables <- dbListTables(con)
    # browser()
    result <- list(events = character(0),
                   detections = character(0))
    # click event first
    result$events <- c(result$events,
                       grep('OfflineEvents', tables, value=TRUE))
    result$detections <- c(result$detections,
                           grep('OfflineClicks', tables, value=TRUE))
    #DGL
    modules <- dbReadTable(con, 'PamguardModules')
    dgTables <- modules %>%
        mutate(Module_Name=str_trim(.data$Module_Name),
               Module_Type=str_trim(.data$Module_Type)) %>%
        filter(.data$Module_Name == 'Detection Group Localiser') %>%
        distinct(.data$Module_Type, .data$Module_Name)
    if(nrow(dgTables) > 0) {
        dgNames <- gsub(' ',  '_', dgTables$Module_Type)
        detTables <- sapply(dgNames, function(x) grep(x, tables, value=TRUE))
        result$events <- c(result$events, detTables[!grepl('Children', detTables)])
        result$detections <- c(result$detections, detTables[grepl('Children', detTables)])
    }
    result
}

# db is db path, table is table name, uids is df Id, UID (new)
# does the adding new column
addNewUID <- function(db, table, uids) {
    if(nrow(uids) == 0) {
        return(TRUE)
    }
    con <- dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    if('PAMmiscTEMP' %in% dbListTables(con)) {
        dropit <- dbRemoveTable(con, 'PAMmiscTEMP')
    }
    makeIt <- tbl <- dbSendQuery(con,
                                 "CREATE TABLE PAMmiscTEMP
            (Id INTEGER,
            UID INTEGER,
            PRIMARY KEY (Id))")
    on.exit(dbRemoveTable(con, 'PAMmiscTEMP'), add=TRUE, after=FALSE)
    dbClearResult(tbl)
    dbAppendTable(con, 'PAMmiscTEMP', uids)
    # add to existing tbl
    # browser()
    if(!('newUID' %in% dbListFields(con, table))) {
        addCol <- dbSendQuery(con,
                              paste0("ALTER TABLE ", table,
                                     " ADD newUID INTEGER"))
        dbClearResult(addCol)
    }

    addUid <- dbSendQuery(
        con,
        # paste0('UPDATE ', table, ' SET newUID = (SELECT UID FROM PAMmiscTEMP ',
        #        'WHERE PAMmiscTEMP.Id = ', table, '.Id)')
        # paste0('UPDATE ', table, ' SET ', table, '.newUID = PAMmiscTEMP.UID ',
        #        'FROM ', table, ' INNER JOIN PAMmiscTEMP ON ', table,'.Id = ',
        #        'PAMmiscTEMP.Id')
        paste0('UPDATE ', table, ' SET newUID = (SELECT UID FROM PAMmiscTEMP WHERE PAMmiscTEMP.Id = ', table, '.Id)',
               'WHERE Id IN (SELECT Id FROM PAMmiscTEMP)')
    )
    dbClearResult(addUid)
    # want ot return how mayn were updated
    newTable <- dbReadTable(con, table)
    isNew <- (newTable$newUID != -1) & # notnew if -1
        (newTable$UID != newTable$newUID) & # new if not same
        (newTable$Id %in% uids$Id) # new if we just added it
    sum(isNew)
}

dirOrFile <- function(x, pattern='pgdf') {
    unlist(lapply(x, function(f) {
        if(dir.exists(f)) {
            return(list.files(f, recursive = TRUE, pattern=pattern, full.names=TRUE))
        }
        if(file.exists(f)) {
            return(f)
        }
        character(0)
    }))
}
