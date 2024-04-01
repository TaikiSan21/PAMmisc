# TODO: Use PAMpal:::mapWavFolder to create "effort" column describing
#       how many minutes of each hour we were actually recording.
# TODO: Check Raven input on someones actual tables
# 2023-11-02: First version, basic binned hourly presence and data loading
# 2023-12-04: Adjusting to account for possible "End.time" or "end" columns instead
#             of only assuming detection at start time
# 2024-01-03: Adding better warning messages to catch time errors
# 2024-01-26: Effort wasnt proper for no-detection drifts
# 2024-01-28: Migrating to ADRIFT_Report repo and adding functions from PAMscapes

if(packageVersion('PAMmisc') < '1.11.9') {
    devtools::install_github('TaikiSan21/PAMmisc')
}
if(packageVersion('PAMscapes') < '0.5.5') {
    devtools::install_github('TaikiSan21/PAMscapes')
}
library(lubridate)
library(dplyr)
library(PAMmisc)
library(PAMpal)
library(readxl)
library(ggplot2)
library(patchwork)
library(RSQLite)

getDbEffort <- function(db, bin='hour') {
    con <- dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    depDet <- dbReadTable(con, 'deploymentData')
    depDet$DataStart <- as.POSIXct(depDet$DataStart, format='%Y-%m-%d %H:%M:%S', tz='UTC')
    depDet$DataEnd <- as.POSIXct(depDet$DataEnd, format='%Y-%m-%d %H:%M:%S', tz='UTC')
    hasNa <- is.na(depDet$DataStart) | is.na(depDet$DataEnd)
    depDet <- depDet[!hasNa, ]
    
    bind_rows(lapply(split(depDet, depDet$DriftName), function(x) {
        thisEffort <- timesToBins(c(x$DataStart, x$DataEnd), bin=bin)
        thisEffort$DeploymentSite <- x$DeploymentSite[1]
        thisEffort$DriftName <- x$DriftName[1]
        thisEffort
    }))
}

timesToBins <- function(x, bin='hour') {
    thisRange <- c(floor_date(min(x, na.rm=TRUE), unit=bin),
                   ceiling_date(max(x, na.rm=TRUE), unit=bin))
    dateSeq <- seq(from=thisRange[1], to=thisRange[2], by=bin)
    data.frame(UTC = dateSeq)
}

gpsToEffort <- function(gps, bin='hour') {
    gps <- filter(gps, recordingEffort)
    bind_rows(lapply(split(gps, gps$DriftName), function(x) {
        thisEffort <- timesToBins(x$UTC)
        thisEffort$DriftName <- x$DriftName[1]
        thisEffort$DeploymentSite <- x$DeploymentSite[1]
        thisEffort
    }))
}

# formats data into time bins based on "bin".
# Output has UTC, Lat/Long, species, call
# Last two columns are NA if no presence
formatBinnedPresence <- function(x, 
                                 effort=NULL,
                                 bin='hour', 
                                 gps,
                                 joinBy='DriftName',
                                 format=c('%m/%d/%Y %H:%M:%S', '%m-%d-%Y %H:%M:%S',
                                          '%Y/%m/%d %H:%M:%S', '%Y-%m-%d %H:%M:%S'), 
                                 tz='UTC',
                                 gpsThresh=3600*3) {
    
    # for future DriftName could be given as a "by" column if need to extend
    gps <- checkGps(gps, format=format, tz=tz)
    if(is.null(effort)) {
        effort <- gpsToEffort(gps, bin=bin)
    }
    x$UTC <- floor_date(x$UTC, unit=bin)
    if('DriftName' %in% colnames(x)) {
        x$DriftName <- toupper(x$DriftName)
    }
    if('end' %in% colnames(x) &&
       any(!is.na(x$end))) {
        x$end <- floor_date(x$end, unit=bin)
        x <- bind_rows(lapply(1:nrow(x), function(i) {
            if(is.na(x$end[i])) {
                return(x[i, ])
            }
            dates <- seq(from=x$UTC[i], to=x$end[i], by=bin)
            result <- as.list(x[i, ])
            result$UTC <- dates
            result
        }))
        x$end <- NULL
        x <- distinct(x)
    }
    
    # result <- vector('list', length=length(unique(x$DriftName)))
    # names(result) <- unique(x$DriftName)
    result <- vector('list', length=length(unique(effort[[joinBy]])))
    names(result) <- unique(effort[[joinBy]])
    for(i in seq_along(result)) {
        if(!joinBy %in% colnames(gps)) {
            thisGps <- gps
        } else {
            thisGps <- gps[gps[[joinBy]] == names(result)[i], ]
        }
        noGps <- is.null(thisGps) || nrow(thisGps) == 0
        # if(is.null(dateRange)) {
        #     if(noGps) {
        #         warning('No GPS matching drift ', names(result)[i],
        #                 ' provide "dateRange" manually or check "DriftName"')
        #         next
        #     }
        #     thisRange <- range(thisGps$UTC)
        # } else if(is.list(dateRange)) {
        #     if(!names(result)[i] %in% names(dateRange)) {
        #         warning('No GPS matching drift ', names(result)[i],
        #                 ' provide "dateRange" manually or check "DriftName"')
        #         next
        #     }
        #     thisRange <- dateRange[[names(result)[i]]]
        # } else {
        #     thisRange <- dateRange
        # }
        # thisRange <- parseToUTC(thisRange, format=format, tz=tz)
        # thisRange[1] <- floor_date(thisRange[1], unit=bin)
        # thisRange[2] <- ceiling_date(thisRange[2], unit=bin)
        thisResult <- effort[effort[[joinBy]] == names(result)[i], ]
        thisData <- x[x[[joinBy]] == names(result)[i], ]
        thisData[[joinBy]] <- NULL
        # dateSeq <- seq(from=thisRange[1], to=thisRange[2], by=bin)
        # thisResult <- data.frame(UTC = dateSeq) #, DriftName=names(result)[i])
        thisResult <- left_join(thisResult, thisData,
                                # thisResult <- left_join(thisResult, x,
                                # c('UTC', 'species', 'call')],
                                by='UTC')
        if(noGps) {
            warning('Could not find GPS for drift ', names(result)[i])
        } else {
            thisResult <- PAMpal::addGps(thisResult, thisGps, thresh=gpsThresh)
            if('DeploymentSite' %in% colnames(thisGps)) {
                thisResult$DeploymentSite <- thisGps$DeploymentSite[1]
            }
        }
        result[[i]] <- thisResult
    }
    #now utcs, lat/long, species, call at floor_date
    result <- distinct(bind_rows(result))
    years <- unique(year(result$UTC))
    result$year <- factor(year(result$UTC), levels=min(years):max(years))
    result
}

# Loads and formats detection data for use in above.
# Output has columns UTC, species, call, and DriftName
# call can be all NA if calltype is not logged
loadDetectionData <- function(x, 
                              source=c('csv', 'triton', 'df', 'raven', 'bm'), 
                              driftName=NULL,
                              driftPattern='([A-z]*_[0-9]{1,3})_.*',
                              format=c('%m/%d/%Y %H:%M:%S', '%m-%d-%Y %H:%M:%S',
                                       '%Y/%m/%d %H:%M:%S', '%Y-%m-%d %H:%M:%S'),
                              speciesCol='species',
                              typeCol=NULL,
                              tz='UTC',
                              sheet=c('Detections', 'AdhocDetections')) {
    if(length(x) > 1) {
        return(bind_rows(lapply(x, function(file) {
            loadDetectionData(file, source=source, driftName=driftName,
                              driftPattern=driftPattern, format=format,
                              speciesCol=speciesCol, typeCol=typeCol,
                              tz=tz, sheet=sheet)
        })))
    }
    switch(match.arg(source),
           'csv' = {
               if(is.null(driftName)) {
                   driftName <- gsub(driftPattern, '\\1', basename(x))
                   if(driftName == basename(x)) {
                       warning('Drift pattern could not parse file ', basename(x),
                               ', fix pattern or provide name directly to "driftName"')
                       return(NULL)
                   }
               }
               x <- read.csv(x, stringsAsFactors = FALSE)
               x$DriftName <- driftName
               return(loadDetectionData(x, source='df', driftName=NULL, format=format,
                                        speciesCol=speciesCol, typeCol=typeCol, tz=tz))
           },
           'triton' = {
               x <- loadTritonLog(x, driftPattern=driftPattern, driftName=driftName, tz=tz, sheet=sheet)
           },
           'df' = {
               if(!'species' %in% colnames(x)) {
                   if(!speciesCol %in% colnames(x)) {
                       warning('Must provide correct species ID column to "speciesCol"')
                       return(NULL)
                   }
                   x$species <- x[[speciesCol]]
                   x[[speciesCol]] <- NULL
               }
               if(!'call' %in% colnames(x)) {
                   if(is.null(typeCol) ||
                      !typeCol %in% colnames(x)) {
                       x$call <- NA
                   } else if(typeCol %in% colnames(x)) {
                       x$call <- x[[typeCol]]
                       x[[typeCol]] <- NULL
                   }
               }
               if(!'DriftName' %in% colnames(x)) {
                   if(is.null(driftName)) {
                       warning('Must provide "driftName" if no "DriftName" column present.')
                       return(NULL)
                   }
                   x$DriftName <- driftName
               }
               if(!'UTC' %in% colnames(x)) {
                   warning('Must have column "UTC"')
                   return(NULL)
               }
               x$UTC <- parseToUTC(x$UTC, format=format, tz=tz)
               if('duration' %in% colnames(x)) {
                   x$end <- x$UTC + x$end
               }
               if('end' %in% colnames(x)) {
                   x$end <- parseToUTC(x$end, format=format, tz=tz)
               }
           },
           'bm' = {
               if(grepl('xls$', x)) {
                   x <- read_xls(x)
               } else if(grepl('xlsx$', x)) {
                   x <- read_xlsx(x)
               }
               x <- rename(x,
                           'UTC' = 'Start_time',
                           'species' = 'Species_Code',
                           'call' = 'Call')
               x$end <- NA
               x$DriftName <- paste0(driftName, '_', formatNumber(x$Drift))
               x <- x[c('UTC', 'species', 'call', 'end', 'DriftName')]
           },
           'raven' = {
               #PAMmisc::formatAnno has fmtRaven
               # creates UTC, Duration, f1, f2, Label from
               # BeginTimes, DeltaTimes, LowFrq, HighFreq, Annotation
               x <- PAMmisc:::fmtRaven(x)
               x <- rename(x, species=Label)
               x$UTC <- parseToUTC(x$UTC, format=format, tz=tz)
               x$end <- x$UTC + x$Duration
               x$call <- NA
               # MISSING:::: DriftName stuff. Unsure logic of raven file names
               x$DriftName <- driftName
           }
    )
    if(is.null(x) ||
       nrow(x) == 0) {
        return(x)
    }
    naStarts <- is.na(x$UTC)
    if(any(naStarts)) {
        warning(sum(naStarts), ' times were not able to be processed in drift(s): ',
                paste0(unique(x$DriftName[naStarts]), collapse=', '))
    }
    naBounds <- is.na(x$end) | naStarts
    if(!all(naBounds)) {
        endBefore <- x$end[!naBounds] < x$UTC[!naBounds]
        if(any(endBefore)) {
            warning(sum(endBefore), ' end times were before start times in drift(s): ',
                    paste0(unique(x$DriftName[endBefore]), collapse=', '))
        }
    }
    x
}

formatNumber <- function(x) {
    outs <- as.character(x)
    outs[x < 10] <- paste0('0', outs[x < 10])
    outs[x < 100] <- paste0('0', outs[x < 100])
    outs
}

checkGps <- function(x,
                     format=c('%m/%d/%Y %H:%M:%S', '%m-%d-%Y %H:%M:%S',
                              '%Y/%m/%d %H:%M:%S', '%Y-%m-%d %H:%M:%S'),
                     tz='UTC') {
    if(is.character(x)) {
        if(!file.exists(x)) {
            warning('File ', x, ' does not exist')
            return(NULL)
        }
        x <- read.csv(x, stringsAsFactors = FALSE)
    }
    needCols <- c('UTC', 'Latitude', 'Longitude', 'DriftName')
    missCols <- !needCols %in% colnames(x)
    if(any(missCols)) {
        warning('GPS must have column(s) ', paste0(needCols[missCols], collapse=', '))
        return(NULL)
    }
    x$UTC <- parseToUTC(x$UTC, format=format, tz=tz)
    x
}

parseToUTC <- function(x, 
                       format=c('%m/%d/%Y %H:%M:%S', '%m-%d-%Y %H:%M:%S',
                                '%Y/%m/%d %H:%M:%S', '%Y-%m-%d %H:%M:%S'), 
                       tz,
                       excel=FALSE) {
    tryCatch({
        testTz <- parse_date_time('10-10-2020 12:00:05', orders = '%m/%d/%Y %H:%M:%S', tz=tz)
    },
    error = function(e) {
        msg <- e$message
        if(grepl('CCTZ: Unrecognized output timezone', msg)) {
            stop('Timezone not recognized, see function OlsonNames() for accepted options', call.=FALSE)
        }
    })
    if(all(is.na(x))) {
        return(x)
    }
    if(is.numeric(x) && isTRUE(excel)) {
        x <- as.POSIXct(x * 24 * 3600, origin = '1899-12-30', tz=tz)
    }
    if(!inherits(x, 'POSIXct')) {
        origTz <- parse_date_time(x, orders=format, tz=tz, exact=TRUE, truncated=3, quiet=TRUE)
        if(!inherits(origTz, 'POSIXct')) {
            stop('Unable to convert to POSIXct time.', call.=FALSE)
        }
    } else {
        origTz <- x
    }
    with_tz(origTz, tzone='UTC')
}

loadTritonLog <- function(x, 
                          driftPattern='([A-z]*_[0-9]{1,3})_.*',
                          driftName=NULL,
                          tz='UTC',
                          sheet=c('Detections', 'AdhocDetections')) {
    if(length(x) > 1) {
        return(
            bind_rows(lapply(x, function(f) {
                loadTritonLog(f, driftPattern, driftName, tz,
                              sheet)
            }))
        )
    }
    
    if(length(sheet) > 1) {
        return(bind_rows(lapply(sheet, function(s) {
            loadTritonLog(x, driftPattern, driftName, tz,
                          sheet=s)
        })))
    }
    if(is.null(driftName)) {
        driftName <- gsub(driftPattern, '\\1', basename(x))
        if(driftName == basename(x)) {
            warning('Drift pattern could not parse file ', basename(x),
                    ', fix pattern or provide name directly to "driftName"')
            return(NULL)
        }
    }
    isExcel <- FALSE
    if(grepl('csv$', x)) {
        x <- read.csv(x, stringsAsFactors = FALSE)
    } else if(grepl('xls$', x)) {
        isExcel <- TRUE
        x <- read_xls(x, sheet=sheet)
    } else if(grepl('xlsx$', x)) {
        isExcel <- TRUE
        x <- read_xlsx(x, sheet=sheet)
    }
    if(is.character(x) || is.null(x) || nrow(x) == 0) {
        return(NULL)
    }
    # isExcel <- FALSE
    nameDf <- data.frame(
        old = c('species.code', 'species code','start time', 'start.time', 'end time', 'end.time'),
        new = c('species', 'species', 'utc', 'utc', 'end', 'end')
    )
    colnames(x) <- tolower(colnames(x))
    for(i in 1:nrow(nameDf)) {
        hasThis <- colnames(x) == nameDf$old[i]
        if(!any(hasThis)) {
            next
        }
        colnames(x)[hasThis] <- nameDf$new[i]
    }
    # x <- x[c('Input.file', 'Event.Number', 'Species.Code', 'Call', 'UTC')]
    tritonCols <- c('utc', 'species', 'call', 'end')
    if(!all(tritonCols %in% colnames(x))) {
        warning('Not all expected columns found in file ', x,
                ' are you sure this is Triton output?')
        return(NULL)
    }
    
    
    x$DriftName <- driftName
    
    x <- x[c(tritonCols, 'DriftName')]
    colnames(x)[1] <- 'UTC'
    x$UTC <- parseToUTC(x$UTC, tz=tz, excel=isExcel)
    x$end <- parseToUTC(x$end, tz=tz, excel=isExcel)
    x
}

shortenOffs <- function(x, nMax=7) {
    result <- bind_rows(
        lapply(
            split(x, x$offGroup), function(g) {
                if(all(!g$off)) {
                    return(g)
                }
                isLast <- g$offGroup[1] == max(x$offGroup)
                if(isLast) {
                    # lastVal <- max(g$binDate) - 24 * 3600
                }
                ons <- g[!g$off, ]
                offs <- g[g$off, ]
                last <- min(nrow(offs), nMax)
                offs <- offs[c(1:last), ]
                if(isLast) {
                    # offs$binDate[nrow(offs)] <- lastVal
                }
                rbind(ons, offs)
            }
        )
    )
    years <- unique(year(result$binDate))
    result$months <- month(result$binDate)
    # if(length(years) == 1 &&
    #    any(result$months == 12)) {
    #     year(result$binDate[result$months == 12]) <- years - 1
    # }
    result <- arrange(result, binDate)
    result$plotX <- 1:nrow(result)
    # if(length(years) == 1 &&
    #    any(result$months == 12)) {
    #     year(result$binDate[result$months == 12]) <- years
    # }
    result$months <- NULL
    result
}

# labels which time bins have effort and how much
# "by" is effort column
markNumEffort <- function(x, 
                          by='DriftName',
                          bin='hour/day', 
                          keepCols=c('species', 'call')) {
    bin <- strsplit(bin, '/')[[1]]
    bin <- gsub('s$', '', bin)
    if(length(bin) == 1) {
        bin <- c(bin, bin)
    }
    x$UTC <- floor_date(x$UTC, unit=bin[1])
    x$binDate <- floor_date(x$UTC, unit=bin[2])
    # dateSeq <- seq(from=min(x$binDate), to=max(x$binDate), by=bin)
    dateSeq <- seq(from=floor_date(min(x$binDate), unit='year'),
                   to = ceiling_date(max(x$binDate), unit='year')-period(1, 'day'),
                   by=bin[2])
    effort <- x %>%
        select(all_of(c('UTC', 'binDate', by))) %>%
        distinct() %>%
        group_by(binDate) %>%
        summarise(nEffort=n()) %>%
        ungroup()
    missDates <- !dateSeq %in% effort$binDate
    effort <- bind_rows(effort, data.frame(binDate=dateSeq[missDates], nEffort=0))
    effort <- arrange(effort, binDate)
    effort$group <- FALSE
    effort$off <- effort$nEffort == 0
    effort$offGroup <- FALSE
    effort$offGroup[1] <- effort$off[1]
    effort$group[1] <- TRUE
    for(i in 2:nrow(effort)) {
        effort$group[i] <- effort$nEffort[i-1] == 0 & effort$nEffort[i] != 0
        effort$offGroup[i] <- isFALSE(effort$off[i-1]) & isTRUE(effort$off[i])
    }
    effort$offGroup <- cumsum(effort$offGroup)
    effort$nGroup <- cumsum(effort$group)
    effort$season <- markSeason(effort$binDate)
    
    # effort$year <- year(effort$binDate)
    x <- distinct(select(x, any_of(c('UTC', 'binDate','year',  by, keepCols))))
    x$season <- markSeason(x$binDate)
    if(!'year' %in% colnames(x)) {
        years <- unique(year(effort$binDate))
        x$year <- factor(year(x$binDate), levels=min(years):max(years))
    }
    effort$year <- factor(year(effort$binDate), levels=levels(x$year))
    # x$year <- year(x$binDate)
    list(dates=dateSeq, data=x, effort=effort)
}

markSeason <- function(x) {
    season <- c(rep('Winter', 2), rep('Upwelling', 4), rep('Post-Upwelling', 5), 'Winter')
    factor(season[month(x)], levels=c('Winter', 'Upwelling', 'Post-Upwelling'))
}

# adds points to square corners for line plot
# loc is where to put the new lines
formatEffortPlot <- function(x, loc=.5, buffer=.001) {
    higher <-which(c(FALSE,  x$nEffort[2:nrow(x)] > x$nEffort[1:(nrow(x)-1)]))
    lower <- which(c(FALSE, x$nEffort[2:nrow(x)] < x$nEffort[1:(nrow(x)-1)]))
    highDf <- x[higher, ]
    highDf$nEffort <- x$nEffort[higher]
    highDf$plotX <- highDf$plotX - loc
    highDf <- rbind(highDf, highDf)
    highDf$nEffort[1:length(higher)] <- x$nEffort[higher-1]
    highDf$plotX[1:length(higher)] <- highDf$plotX[1:length(higher)] - buffer
    
    lowDf <- x[lower, ]
    lowDf$nEffort <- x$nEffort[lower-1]
    lowDf$plotX <- lowDf$plotX - loc
    lowDf <- rbind(lowDf, lowDf)
    lowDf$nEffort[1:length(lower)] <- x$nEffort[lower]
    lowDf$plotX[1:length(lower)] <- lowDf$plotX[1:length(lower)] + buffer
    rbind(x, lowDf, highDf) %>%
        arrange(plotX)
}

plotYearlyPresence <- function(x, 
                               percent=TRUE, 
                               maxEff=NULL,
                               legend=c('show', 'blank', 'remove'),
                               botAxis=TRUE, 
                               by=NULL, 
                               leftLab=NULL,
                               title=NULL) {
    if(!is.null(by) && by %in% colnames(x)) {
        splitData <- split(x, x[[by]])
        legendIx <- floor(median(seq_along(splitData)))
        result <- vector('list', length=length(splitData))
        for(i in seq_along(result)) {
            result[[i]] <- plotYearlyPresence(splitData[[i]],
                                              percent=percent,
                                              maxEff=maxEff,
                                              # legend=ifelse(i==legendIx, 'show', 'blank'),
                                              legend='show',
                                              botAxis=i==length(result),
                                              by=NULL,
                                              leftLab = names(splitData)[i],
                                              title=NULL)
        }
        out <- wrap_plots(result) + plot_layout(ncol=1, guides = 'collect')
        if(!is.null(title)) {
            out <- out +
                plot_annotation(title=title, theme=theme(plot.title=element_text(hjust=.5)))
        }
        return(out)
    }
    if(is.data.frame(x)) {
        x <- markNumEffort(x, keepCols='species')
    }
    effort <- bind_rows(lapply(split(x$effort, x$effort$year), function(y) {
        if(nrow(y) <= 1) {
            # print(z)
            return(NULL)
        }
        shortenOffs(y, nMax=364)
    })
    )
    
    data <- x$data
    # browser()
    data <- left_join(data, effort[c('plotX', 'binDate', 'nEffort')],
                      by=join_by(binDate))
    labs <- list(ix = seq(from=min(effort$plotX), to=max(effort$plotX), length.out=5),
                 label = seq(from=min(effort$binDate), to=min(effort$binDate) + period(364, units='days'), length.out=5))
    
    labs$label <- format(labs$label, '%b-%d')
    effort <- formatEffortPlot(effort)
    if(is.null(maxEff)) {
        maxEff <- max(effort$nEffort)
    }
    effPlot <- ggplot(effort) +
        geom_path(data=effort, aes(x=plotX, y=nEffort, col=year, group=year)) +
        scale_color_manual(values=scales::hue_pal()(length(levels(data$year))),
                           limits=levels(data$year)) +
        theme_bw()
    # scale_y_continuous(breaks=(1:10)*24, name='Hours', limits=c(0, maxEff))
    data <- filter(data, !is.na(species))
    if(percent) {
        # browser()
        # THIS IST WORKING WHY. Mean effort is way higher than n()
        data <- data %>%
            group_by(plotX, year) %>%
            summarise(n=n(), pct=n()/mean(nEffort), eff=mean(nEffort), .groups='drop_last') %>%
            ungroup()
        binPlot <- ggplot() +
            geom_rect(data=data, aes(xmin=plotX-.45,xmax=plotX+.45, ymin=0, ymax=pct, fill=year))
        # geom_line(data=formatEffortPlot(effort), aes(x=plotX, y=nEffort / ymax *24, alpha=TRUE)) +
        # scale_y_continuous(expand=expansion(mult=c(0, 0.05)), limits=c(0, 1),
        #                    # sec.axis=sec_axis(trans = ~.*ymax/24, breaks=seq(from=0, to=ymax, by=24)),
        #                    breaks=c(0,.25, .5, .75, 1), name='Percent of Avail. Hours')
    } else {
        # making separate scale for each year's max effort
        blankData <- effort %>%
            group_by(year) %>%
            summarise(plotX=min(plotX), max=max(nEffort))
        binPlot <- ggplot() +
            geom_bar(data=data, aes(fill=year, x=plotX)) +
            geom_blank(data=blankData, aes(x=plotX, y=max))
        
    }
    binPlot <- binPlot + theme_bw()
    binPlot <- binPlot +
        facet_wrap(~year, ncol=1, drop = FALSE, scales='free_y') +
        # scale_x_continuous(breaks=labs$ix, labels=labs$label, limits=c(1, max(effort$plotX))) +
        # scale_x_continuous(limits=c(1, max(effort$plotX)), breaks=labs$ix, labels=NULL) +
        scale_fill_manual(values=scales::hue_pal()(length(levels(data$year))), limits=levels(data$year))
    if(isFALSE(botAxis)) {
        effPlot <- effPlot +
            scale_x_continuous(breaks=labs$ix, labels=NULL, name=NULL)
        binPlot <- binPlot +
            scale_x_continuous(limits=c(1, max(effort$plotX)), breaks=labs$ix, labels=NULL, name=NULL)
    } else {
        effPlot <- effPlot +
            scale_x_continuous(breaks=labs$ix, labels=labs$label, name=NULL)
        binPlot <- binPlot +
            scale_x_continuous(limits=c(1, max(effort$plotX)), breaks=labs$ix, labels=NULL, name=NULL)
    }
    switch(match.arg(legend),
           'remove' = {
               binPlot <- binPlot + theme(legend.position='none')
               effPlot <- effPlot + theme(legend.position='none')
           },
           'blank' = {
               binPlot <- binPlot +
                   theme(legend.key = element_rect(fill = "white"),
                         legend.text = element_text(color = "white"),
                         legend.title = element_text(color = "white")) +
                   guides(color = guide_legend(override.aes = list(color = NA)),
                          fill = guide_legend(override.aes = list(fill=NA)))
               effPlot <- effPlot +
                   theme(legend.key = element_rect(fill = "white"),
                         legend.text = element_text(color = "white"),
                         legend.title = element_text(color = "white")) +
                   guides(color = guide_legend(override.aes = list(color = NA)))
           },
           'show' = {
               effPlot <- effPlot +
                   theme(legend.key = element_rect(fill = "white"),
                         legend.text = element_text(color = "white"),
                         legend.title = element_text(color = "white")) +
                   guides(color = guide_legend(override.aes = list(color = NA)))
           }
    )
    # if(isFALSE(legend)) {
    #     binPlot <- binPlot + theme(legend.position='none')
    #     effPlot <- effPlot + theme(legend.position='none')
    # }
    # if(isFALSE(leftAxis)) {
    #     effPlot <- effPlot +
    #         scale_y_continuous(name=NULL, labels=NULL, limits=c(0, maxEff), breaks=(1:10)*24)
    #     binPlot <- binPlot +
    #         scale_y_continuous(name=NULL, labels=NULL, limits=c(0,1), breaks=c(0,.25, .5, .75, 1))
    # } else {
    effPlot <- effPlot +
        scale_y_continuous(breaks=(1:10)*24, name='Hours', limits=c(0, maxEff))
    if(isTRUE(percent)) {
        binPlot <- binPlot +
            scale_y_continuous(expand=expansion(mult=c(0, 0.05)), limits=c(0, 1),
                               # sec.axis=sec_axis(trans = ~.*ymax/24, breaks=seq(from=0, to=ymax, by=24)),
                               breaks=c(0,.25, .5, .75, 1), name='Percent of Avail. Hours')
    } else {
        ymax <- max(effort$nEffort)
        binPlot <- binPlot +
            scale_y_continuous(expand=expansion(mult=c(0, .05)), #limits=c(0, maxEff),
                               breaks=seq(from=0, to=ymax, by=24), name='Hours')
    }
    # }
    binPlot <- binPlot +
        theme(
            strip.background = element_blank(),
            strip.text.x = element_blank()
        )
    out <- binPlot/effPlot + plot_layout(heights=c(5,1), ncol=1)
    if(!is.null(leftLab)) {
        out <- wrap_elements(grid::textGrob(leftLab, rot=90)) + out +
            plot_layout(widths=c(1,40))
    }
    if(!is.null(title)) {
        out <- out +
            plot_annotation(title=title, theme=theme(plot.title=element_text(hjust=.5)))
    }
    out
}

plotRadialPresence <- function(x, bin=c('hour', 'month'), title=NULL) {
    bin <- match.arg(bin)
    switch(bin, 
           'hour' = {
               x$PLOTBIN <- (hour(x$UTC) - 7) %% 24
               lims <- c(-.5, 23.5)
               breaks <- seq(from=0, to=24, by=3)
               pStart <- -(.5/24)*2*pi
               xlab <- 'Hour'
           },
           'month' = {
               x$PLOTBIN <- month(x$UTC)
               lims <- c(.5, 12.5)
               breaks <- seq(from=1, to=12, by=1)
               pStart <- -(.5/12)*2*pi
               xlab <- 'Month'
           }
    )
    plotData <- group_by(x, PLOTBIN) %>% 
        summarise(nDetections = sum(!is.na(call)),
                  nTotal = n(), 
                  pctDetections = nDetections/nTotal)
    gPlot <- plotData %>% 
        ggplot() +
        geom_bar(aes(x=PLOTBIN, y=pctDetections, fill=pctDetections), stat='identity') +
        coord_polar(start=pStart) +
        scale_fill_viridis_c() +
        scale_x_continuous(breaks=breaks, limits=lims, expand=c(0, 0)) + 
        ggtitle(title) +
        labs(x=xlab)
    # list(plot=gPlot, data=plotData)
    gPlot
}
