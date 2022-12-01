library(dplyr)
library(xml2)
#--------------------------#
### Fixes on 2021-12-15 to account for possible re-name overlap resulting in a bunch of mssing files
# Changes in new version (2022-12-01):
# 1) Now changes all different file types in one batch
# 2) Changes file names twice - first to a temporary name,
#    then to the new name. This is to avoid problems where
#    there is overlap between your new & old file names - ex
#    ex if you have files exactly 7 hours apart, but want to
#    add 7 hours to the time. 
# 3) If offset=NULL it will look for log files first. If no log
#    files are found it will prompt you to enter an offset value.
#-------------------------#
getStTz <- function(x) {
    xml <- read_xml(x)
    tzNode <- xml_find_all(xml, '//WavFileHandler[@OffloaderTimeZone]')
    tzChar <- unique(xml_attr(tzNode, 'OffloaderTimeZone'))
    if(is.null(tzChar) || length(tzChar) == 0) {
        warning('Log file ', x, ' has no timezone')
        return(NA_real_)
    } else if(length(tzChar) > 1) {
        warning('Log file ', x, ' has more than one timezone')
        return(NA_real_)
    }
    tzOut <- switch(tzChar,
                    'Pacific Standard Time' = 7,
                    'Coordinated Universal Time' = 0,
                    tzChar
    )
    if(is.character(tzOut)) {
        warning('Dont yet have value mapped for time zone: "', tzOut, '"\nTell Taiki to add', call.=FALSE)
        return(NA_real_)
    }
    tzOut
}

prepTzFix <- function(x, offset=NULL, suffix=c('wav', 'sud', 'log.xml', 'accel.csv')) {
    suffix <- paste0('\\.',suffix, '$', collapse='|')
    suffix <- paste0('(', suffix, ')')
    # wavList <- list.files(x, full.names=FALSE, recursive = FALSE, pattern=paste0('\\.',suffix, '$', collapse='|'))
    wavList <- list.files(x, full.names=FALSE, recursive = FALSE, pattern=suffix)
    if(length(wavList) == 0) {
        stop('No files in this folder matching those suffixes')
    }
    tAdj <- data.frame(oldName=wavList)
    if(!is.na(processStWavNames(wavList[1], type = 'st', suffix=suffix))) {
        type <- 'st'
    } else if(!is.na(processStWavNames(wavList[1], type = 'sm3m', suffix=suffix))) {
        type <- 'sm3m'
    } else {
        stop('Could not parse wav file names for time information, send Taiki a file')
    }
    tAdj$oldTime <- processStWavNames(tAdj$oldName, type=type, suffix=suffix)
    if(is.null(offset)) {
        logList <- list.files(x, full.names=TRUE, recursive = FALSE, pattern = '\\.log\\.xml$')
        tAdj$offset <- sapply(basename(tAdj$oldName), function(w) {
            w <- gsub(paste0('\\.',suffix, '$'), '', w)
            match <- which(w %in% gsub('\\.log\\.xml$', '', basename(logList)))[1]
            if(length(match) == 0 ||
               is.na(match)) {
                return(NA)
            }
            getStTz(logList[match])
        })
    } else {
        tAdj$offset <- offset
    }
    naOff <- is.na(tAdj$offset)
    if(any(naOff)) {
        cat('Could not find matching logs for all files, enter a desired offset (hours):')
        userOff <- readline()
        userNum <- as.numeric(userOff)
        if(is.na(userNum)) {
            warning('"', userOff, '" is not a valid numeric value.')
        }
        tAdj$offset[naOff] <- userNum
        naOff <- is.na(tAdj$offset)
    }
    if(all(naOff)) {
        tAdj$newTime <- NA
        tAdj$newName <- NA
        warning('No files could be adjusted for new timezone')
        return(tAdj)
    }
    tAdj$newTime <- tAdj$oldTime + tAdj$offset * 3600
    tAdj$newName <- NA
    tAdj$tempName <- NA
    for(i in 1:nrow(tAdj)) {
        if(is.na(tAdj$offset[i])) next
        newTime <- processStWavNames(tAdj$newTime[i], type=type, suffix=suffix)
        oldTime <- processStWavNames(tAdj$oldTime[i], type=type, suffix=suffix)
        tAdj$newName[i] <- gsub(oldTime, newTime, tAdj$oldName[i])
        tAdj$tempName[i] <- gsub(oldTime, paste0('TEMP', i), tAdj$oldName[i])
        # oldPrefix <-  processStWavNames(tAdj$oldName[i], prefix=TRUE, type=type, suffix=suffix)
        # tAdj$tempName[i] <- paste0(oldPrefix, 'TEMP', i, '.', suffix)
        # tAdj$newName[i] <- paste0(oldPrefix, newTime, '.', suffix)
    }
    if(any(naOff)) {
        cat(sum(naOff), ' files out of ', length(naOff), ' could not be adjusted.\n', sep='')
    }
    tAdj
}

processStWavNames <- function(x, type=c('st', 'sm3m'), prefix=FALSE, suffix='wav') {
    type <- match.arg(type)
    switch(type,
           'st' = {
               format <- '%y%m%d%H%M%S'
               pattern <- paste0('(.*\\.)([0-9]{12})', suffix, '$')
           },
           'sm3m' = {
               format <- '%Y%m%d_%H%M%S'
               pattern <- paste0('(.*_)([0-9]{8}_[0-9]{6})', suffix, '$')
           }
    )
    if(isTRUE(prefix)) {
        return(gsub(pattern, '\\1', x))
    }
    if(is.character(x)) {
        return(as.POSIXct(gsub(pattern, '\\2', x), format=format, tz='UTC'))
    }
    if(inherits(x, 'POSIXct')) {
        return(as.character(x, format=format))
    }
    NULL
}

fixStTz <- function(dir, prep, reverse=FALSE, logname='STRenameLog') {
    if(isTRUE(reverse)) {
        revPrep <- rename(prep, oldName=newName, oldTime=newTime, newName=oldName, newTime=oldTime)
        revPrep <- revPrep[!is.na(revPrep$oldName), ]
        revPrep$offset <- revPrep$offset * -1
        return(fixStTz(dir,revPrep, FALSE, logname='STReverseLog'))
    }
    wavList <- list.files(dir, recursive=FALSE)
    if(!all(prep$oldName %in% wavList)) {
        warning('Not all wav files in prep file are in directory, run prep again')
        return(NULL)
    }
    prep$renamed <- FALSE
    on.exit(write.csv(prep, file=file.path(dir, paste0(logname, '.csv')), row.names = FALSE))
    cat('\nRenaming files to temporary name...\n')
    pb <- txtProgressBar(min=0, max=nrow(prep), style=3)
    for(i in 1:nrow(prep)) {
        if(any(is.na(prep[i, ]))) {
            setTxtProgressBar(pb, value=i)
            next
        }
        file.rename(from=file.path(dir, prep$oldName[i]),
                    to = file.path(dir, prep$tempName[i]))
        setTxtProgressBar(pb, value=i)
    }
    cat('\nRenaming files to new name...\n')
    pb <- txtProgressBar(min=0, max=nrow(prep), style=3)
    for(i in 1:nrow(prep)) {
        if(any(is.na(prep[i, ]))) {
            setTxtProgressBar(pb, value=i)
            next
        }
        file.rename(from=file.path(dir, prep$tempName[i]),
                    to = file.path(dir, prep$newName[i]))
        prep$renamed[i] <- TRUE
        setTxtProgressBar(pb, value=i)
    }
    cat('\nRenamed', sum(prep$renamed),
        'out of', nrow(prep),
        'files.\n')
    invisible(prep)
}
