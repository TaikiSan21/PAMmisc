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
                cat('\nUpdating UIDs in database ', basename(db[i]), sep='')
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
    # issue checkers
    noBinFound <- character(0)
    allBinEmpty <- character(0)
    for(d in detTables) {
        thisTbl <- dbReadTable(con, d)
        if(nrow(thisTbl) == 0) next
        thisTbl$BinaryFile <- str_trim(thisTbl$BinaryFile)
        thisTbl$UTC <- as.POSIXct(as.character(thisTbl$UTC), format='%Y-%m-%d %H:%M:%OS', tz='UTC')
        if(progress) {
            cat('\nUpdating table "', d, '" ...\n', sep='')
            pb <- txtProgressBar(min = 0, max = length(unique(thisTbl$BinaryFile)), style=3)
            val <- 1
        }
        newUids <- bind_rows(lapply(split(thisTbl, thisTbl$BinaryFile), function(b) {
            thisBin <- grep(b$BinaryFile[1], binaries, value=TRUE)
            if(length(thisBin) == 0) {
                if(progress) {
                    setTxtProgressBar(pb, value = val)
                    val <<- val + 1
                }
                noBinFound <<- c(noBinFound, b$BinaryFile[1])
                return(NULL)
            }
            binUids <- lapply(thisBin, function(x) checkOneBin(b, x))
            goodCheck <- sapply(binUids, function(x) {
                if(is.null(x)) {
                    return(0)
                }
                isUid <- x$UID != -1
                sum(isUid)
            })
            if(all(goodCheck == 0)) {
                allBinEmpty <<- c(allBinEmpty, b$BinaryFile[1])
                setTxtProgressBar(pb, value = val)
                val <<- val + 1
                return(NULL)
            }
            hasMost <- min(which.max(goodCheck))
            result <- binUids[[hasMost]]
            for(i in seq_along(binUids)) {
                if(i == hasMost ||
                   goodCheck[i] == 0) {
                    next
                }
                thisOne <- binUids[[i]]
                alreadyIn <- sapply(thisOne$Id, function(x) x %in% result$Id)
                isUid <- thisOne$UID > -1
                thisOne$Id <- thisOne$Id[!alreadyIn & isUid]
                thisOne$UID <- thisOne$UID[!alreadyIn & isUid]

                result <- bind_rows(result, thisOne)
            }
            if(progress) {
                setTxtProgressBar(pb, value = val)
                val <<- val + 1
            }
            result
        }))
        if(is.null(newUids) ||
           nrow(newUids) == 0) {
            nAdded[[d]] <- 0
        } else {
            newUids <- arrange(distinct(newUids), desc(.data$UID))
            dupeId <- duplicated(newUids$Id)
            # if(any(newUids$UID[dupeId] > -1)) {
            #     #something something replicates
            #     1+1
            # }
            newUids <- newUids[!dupeId, ]
            nAdded[[d]] <- addNewUID(db, d, newUids)
        }
        if(verbose) {
            cat('\nUpdated UIDs for ', nAdded[[d]], ' out of ',
                nrow(thisTbl), ' total detections.', sep='')
        }
    }
    # this has returned back the original detection table with a column "newUID" added
    # that is -1 if no match found, or the new UID if was match
    # need to go and update the actual database, but probably test this on some
    # studies youve already run first
    list(nAdded=nAdded,
         noMatchingBin = noBinFound,
         allBinEmpty = allBinEmpty)
}

# det is already df of table, binary is path to bin file
# see which det UIDs have a match in this binary file
checkOneBin <- function(det, binary) {
    if(length(binary) == 0) {
        return(NULL)
    }
    binData <- loadPamguardBinaryFile(binary, convertDate = FALSE, skipLarge=TRUE)$data
    if(length(binData) == 0) {
        return(NULL)
    }
    binDf <- bind_rows(lapply(binData, function(x) {
        x[c('UID', 'date')]
    }))
    if('ClickNo' %in% colnames(det)) {
        binDf <- binDf[det$ClickNo + 1, ]
        isMatch <- (det$UTC - binDf$date) == 0
        isMatch[is.na(isMatch)] <- FALSE
        newUID <- binDf$UID
        newUID[!isMatch] <- -1
    } else {
        newUID <- sapply(det$UTC, function(x) {
            isMatch <- which((x - binDf$date) == 0)
            if(length(isMatch) == 0) {
                return(-1)
            } else if(length(isMatch) > 1) {
                return(-1)
            }
            binDf$UID[isMatch]
        })
    }
    if(any(newUID == -1)) {
        # message('Binary file ', basename(binary), ' did not have matching times.')
    }
    list(Id = det$Id, UID = newUID)
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
    # modules <- dbReadTable(con, 'PamguardModules')
    # dgTables <- modules %>%
    #     mutate(Module_Name=str_trim(.data$Module_Name),
    #            Module_Type=str_trim(.data$Module_Type)) %>%
    #     filter(.data$Module_Name == 'Detection Group Localiser') %>%
    #     distinct(.data$Module_Type, .data$Module_Name)
    dgNames <- findModuleNames(con, 'Detection Group Localiser')
    if(length(dgNames) > 0) {
        # detTables <- sapply(dgNames, function(x) grep(x, tables, value=TRUE))
        result$events <- c(result$events, dgNames)
        result$detections <- c(result$detections, paste0(dgNames, '_Children'))
    }
    result
}

findModuleNames <- function(con, module='Detection Group Localiser') {
    tbls <- c('Pamguard_Settings', 'Pamguard_Settings_Last', 'Pamguard_Settings_Viewer', 'PamguardModules')
    typeCols <- c('unitType','unitType', 'unitType', 'Module_Name')
    nameCols <- c('unitName','unitName', 'unitName', 'Module_Type')
    result <- vector('list', length=length(tbls))
    for(i in seq_along(result)) {
        if(!tbls[i] %in% dbListTables(con)) next
        mods <- dbReadTable(con, tbls[i])
        if(nrow(mods) == 0) next
        dgTables <- mods[c(typeCols[i], nameCols[i])]
        names(dgTables) <- c('type', 'name')
        dgTables$type <- str_trim(dgTables$type)
        dgTables$name <- str_trim(dgTables$name)
        dgTables <- dgTables[dgTables$type == module, ]
        if(nrow(dgTables) == 0) next
        dgTables <- distinct(dgTables)
        result[[i]] <- dgTables
    }
    result <- bind_rows(result)
    if(is.null(result) ||
       nrow(result) == 0) {
        return(NULL)
    }
    result <- distinct(result)
    result$name <- gsub(' ', '_', dgTables$name)
    result$name[result$name %in% dbListTables(con)]
}

# db is db path, table is table name, uids is df Id, UID (new)
# does the adding new column
addNewUID <- function(db, table, uids) {
    if(nrow(uids) == 0) {
        return(0)
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
    if(!('newUID' %in% dbListFields(con, table))) {
        addCol <- dbSendQuery(con,
                              paste0("ALTER TABLE ", table,
                                     " ADD newUID INTEGER"))
        dbClearResult(addCol)
    }

    addUid <- dbSendQuery(
        con,
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
