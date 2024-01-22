# plotoff funcs
library(ggplot2)
library(dplyr)
library(lubridate)
library(patchwork)
library(readr)
library(PAMpal)

loadBmLogs <-  function(x, gps, thresh=3600*3) {
    x <- list.files(x, full.names=TRUE)
    lapply(x, function(f) {
        result <- read_csv(f, show_col_types = FALSE)[1:5]
        drift <- gsub('(ADRIFT_[0-9]{1,3})_.*', '\\1', basename(f))
        thisGps <- filter(gps, DriftName == drift)
        result$DriftName <- drift
        colnames(result) <- c('file', 'eventNumber', 'species', 'call', 'UTC', 'DriftName')
        result$UTC <- as.POSIXct(result$UTC, format='%m/%d/%Y %H:%M:%S', tz='UTC')
        result <- addGps(result, thisGps, thresh=thresh)
        result
    }) %>%
        bind_rows
}

prepBmData <- function(x, gps, thresh) {
    if(is.character(x)) {
        bmData <- loadBmLogs(x, gps, thresh)
    } else {
        bmData <- x
    }
    bmGps <- filter(gps, DriftName %in% unique(bmData$DriftName))
    result <- bmGps[c('Latitude', 'Longitude', 'UTC', 'DriftName')]
    result$species <- NA
    result$call <- NA
    result <- bind_rows(result, bmData[c('Latitude', 'Longitude', 'UTC', 'DriftName', 'species', 'call')])
    result <- arrange(result, UTC)
    result$year <- year(result$UTC)
    # season <- c(rep('Winter', 2), rep('Upwelling', 4), rep('Post-Upwelling', 5), 'Winter')
    result$season <- markSeason(result$UTC)
    result
}

markSeason <- function(x) {
    season <- c(rep('Winter', 2), rep('Upwelling', 4), rep('Post-Upwelling', 5), 'Winter')
    factor(season[month(x)], levels=c('Winter', 'Upwelling', 'Post-Upwelling'))
}

seasonLab <- function(x) {
    switch(x,
           'Winter' = {
               range <- c('2020-12-01 00:00:00', '2021-02-28 00:00:00')
           },
           'Upwelling' = {
               range <- c('2021-03-01 00:00:00', '2021-06-30 00:00:00')
           },
           'Post-Upwelling' = {
               range <- c('2021-07-01 00:00:00', '2021-11-30 00:00:00')
           }
    )
    range <- as.POSIXct(range, tz='UTC')
    lims <- seq(from=range[1], to=range[2], length.out=5)
    lims <- format(lims, '%b-%d')
    lims
}

# labels which time bins have effort and how much
# "by" is effort column
markNumEffort <- function(x, by='DriftName', bin='hour/day', keepCols=c('species', 'call')) {
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
    years <- unique(year(effort$binDate))
    effort$year <- factor(year(effort$binDate), levels=min(years):max(years))
    # effort$year <- year(effort$binDate)
    x <- distinct(x[c('UTC', 'binDate', by, keepCols)])
    x$season <- markSeason(x$binDate)
    x$year <- factor(year(x$binDate), levels=min(years):max(years))
    # x$year <- year(x$binDate)
    list(dates=dateSeq, data=x, effort=effort)
}

# cutt times with no effort down to a max of nMax points
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

makeEffLabs <- function(effort) {
    labs <- group_by(effort, nGroup) %>%
        summarise(ix = min(plotX),
                  label = min(binDate))
    if(isTRUE(effort$off[nrow(effort)])) {
        labs <- rbind(labs,
                      list(nGroup=max(effort$nGroup),
                           ix=max(effort$plotX),
                           label=max(effort$binDate)))
    }
    labs$label <- format(labs$label, format='%b-%d')
    labs
}

plotBarEffort <- function(x, title=NULL, bin='hour/day', fill='call', legend=TRUE, ymax=NULL, facet=NULL,
                          maxOff=7, percent=FALSE) {
    if(is.data.frame(x)) {
        x <- markNumEffort(x, bin=bin, keepCols = fill)
    }
    effort <- shortenOffs(x$effort, nMax=maxOff)
    if(is.null(ymax)) {
        ymax <- max(effort$nEffort, na.rm=TRUE)
    }
    data <- left_join(x$data, effort[c('plotX', 'binDate', 'nEffort')])
    # labs <- group_by(effort, nGroup) %>%
    #     summarise(ix = min(plotX),
    #               label = min(binDate))
    # if(isTRUE(effort$off[nrow(effort)])) {
    #     labs <- rbind(labs,
    #                   list(nGroup=max(effort$nGroup),
    #                        ix=max(effort$plotX),
    #                        label=max(effort$binDate)))
    # }
    # labs$label <- format(labs$label, format='%b-%d')
    labs <- makeEffLabs(effort)
    g <- ggplot()
    if(isTRUE(percent)) {
        # data <- left_join(x$data, effort[c('plotX', 'binDate', 'nEffort')])
        data <- filter(data, !is.na(.data[[fill]])) %>%
            group_by(plotX, .data[[fill]]) %>%
            summarise(n=n(), pct=n()/mean(nEffort), eff=mean(nEffort), ) %>%
            ungroup()
        g <- g +
            geom_rect(data=data, aes(xmin=plotX-.45,xmax=plotX+.45, ymin=0, ymax=pct*24, fill=.data[[fill]])) +
            # geom_line(data=formatEffortPlot(effort), aes(x=plotX, y=nEffort / ymax *24, alpha=TRUE)) +
            scale_y_continuous(expand=expansion(mult=c(0, 0.05)), limits=c(0, 1*24),
                               # sec.axis=sec_axis(trans = ~.*ymax/24, breaks=seq(from=0, to=ymax, by=24)),
                               breaks=c(0,6,12,18,24))
        g2 <- ggplot() +
            geom_line(data=formatEffortPlot(effort), aes(x=plotX, y=nEffort)) +
            scale_y_continuous(expand=expansion(mult=c(0, .05)), breaks=seq(from=0, to=ymax, by=24),
                               limits=c(0, ymax))

        # ymax <- 1

    } else {
        # data <- left_join(x$data, effort[c('plotX', 'binDate')])
        g <- g +
            geom_bar(data=filter(data, !is.na(.data[[fill]])),
                     aes(x=plotX, fill=.data[[fill]], group=.data[[fill]]), position=position_dodge(preserve='single')) +
            geom_line(data=formatEffortPlot(effort), aes(x=plotX, y=nEffort, alpha=TRUE))+
            scale_y_continuous(expand=expansion(mult=c(0, .05)), limits=c(0, ymax),
                               breaks=seq(from=0, to=ymax, by=24))
    }
    g <- g +
        geom_point(data=filter(effort, off), aes(x=plotX, y=nEffort + .02, alpha=factor(off, levels=c(T,F))), shape=4) +

        scale_alpha_manual(values=c(1,1), drop=F, labels=c('Off Effort', 'Num Buoys'), name='Effort',
                           guide=guide_legend(override.aes=list(shape=c(4, NA), linetype=c(0,1)))) +
        labs(y=paste0(stringr::str_to_title(bin), 's with detections'))

    # title <- paste0(title, ' (', bin, 's with detections)')
    if(isFALSE(legend)) {
        g <- g + theme(legend.position='none')
    }
    if(!is.null(facet)) {
        g <- g + facet_wrap(facet, ncol=1)
    }
    g <- g + ggtitle(title)
    if(isTRUE(percent)) {
        g2 <- g2 +
            scale_x_continuous(breaks=labs$ix, labels=labs$label) +
            theme(axis.text.x = element_text(angle=45, vjust=.5, face='bold', size=10)) +
            labs(x='Date')
        g <- g + labs(x=NULL) +
            scale_x_continuous(labels=NULL, breaks=NULL)
        g <- g / g2 + plot_layout(heights=c(5,1))
    } else {
        g <- g +
            scale_x_continuous(breaks=labs$ix, labels=labs$label) +
            theme(axis.text.x = element_text(angle=45, vjust=.5, face='bold', size=10)) +
            labs(x='Date')
    }
    g
}

plotComp <- function(x, by='year', bin='hour/day', fill='call', facet=NULL, maxOff=7, percent=TRUE) {
    x <- markNumEffort(x, bin=bin, keepCols=c(by, fill))
    # maxEff <- max(markNumEffort(x, bin=bin)$effort$nEffort)
    maxEff <- max(x$effort$nEffort)
    # years <- unique(year(x$UTC))
    # comp <- unique(x[[by]])
    if(is.factor(x$data[[by]])) {
        comp <- levels(x$data[[by]])
    } else {
        comp <- unique(x$data[[by]])
    }
    # comp <- ifelse(is.factor(x$data[[by]]), levels(x$data[[by]]),
    # unique(x$data[[by]]))
    result <- vector('list', length=length(comp))
    for(i in seq_along(result)) {
        thisData <- list(
            data = filter(x$data, .data[[by]] == comp[i]),
            effort = filter(x$effort, .data[[by]] == comp[i])
        )
        result[[i]] <- plotBarEffort(thisData,
                                     title=comp[i],
                                     legend=(i==length(result)),
                                     bin=bin,
                                     fill=fill,
                                     ymax=maxEff,
                                     facet=facet,
                                     maxOff=maxOff,
                                     percent=percent)
    }
    purrr::reduce(result, `|`)
}

tryComp <- function(x, percent=TRUE, botAxis=TRUE) {
    seasons <- c('Winter', 'Upwelling', 'Post-Upwelling')
    maxEff <- max(x$effort$nEffort)
    result <- vector('list', length(seasons))
    for(i in seq_along(seasons)) {
        result[[i]] <- tryPlot(x, plotSeason=seasons[i], percent=percent, maxEff=maxEff, legend=i==length(seasons),
                               leftAxis = i == 1, botAxis=botAxis)
    }
    purrr::reduce(result, `|`)
}

tryPlot <- function(x, percent=TRUE, maxEff=NULL, legend=TRUE,
                    leftAxis=TRUE, botAxis=TRUE) {
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
    data <- left_join(data, effort[c('plotX', 'binDate', 'nEffort')])
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
            summarise(n=n(), pct=n()/mean(nEffort), eff=mean(nEffort)) %>%
            ungroup()
        binPlot <- ggplot() +
            geom_rect(data=data, aes(xmin=plotX-.45,xmax=plotX+.45, ymin=0, ymax=pct, fill=year))
        # geom_line(data=formatEffortPlot(effort), aes(x=plotX, y=nEffort / ymax *24, alpha=TRUE)) +
        # scale_y_continuous(expand=expansion(mult=c(0, 0.05)), limits=c(0, 1),
        #                    # sec.axis=sec_axis(trans = ~.*ymax/24, breaks=seq(from=0, to=ymax, by=24)),
        #                    breaks=c(0,.25, .5, .75, 1), name='Percent of Avail. Hours')
    } else {
        binPlot <- ggplot(data) +
            geom_bar(aes(fill=year, x=plotX))

    }
    binPlot <- binPlot + theme_bw()
    binPlot <- binPlot +
        facet_wrap(~year, ncol=1, drop = FALSE) +
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
            scale_x_continuous(limits=c(1, max(effort$plotX)), breaks=labs$ix, labels=NULL)
    }

    if(isFALSE(legend)) {
        binPlot <- binPlot + theme(legend.position='none')
        effPlot <- effPlot + theme(legend.position='none')
    }
    if(isFALSE(leftAxis)) {
        effPlot <- effPlot +
            scale_y_continuous(name=NULL, labels=NULL, limits=c(0, maxEff), breaks=(1:10)*24)
        binPlot <- binPlot +
            scale_y_continuous(name=NULL, labels=NULL, limits=c(0,1), breaks=c(0,.25, .5, .75, 1))
    } else {
        effPlot <- effPlot +
            scale_y_continuous(breaks=(1:10)*24, name='Hours', limits=c(0, maxEff))
        binPlot <- binPlot +
            scale_y_continuous(expand=expansion(mult=c(0, 0.05)), limits=c(0, 1),
                               # sec.axis=sec_axis(trans = ~.*ymax/24, breaks=seq(from=0, to=ymax, by=24)),
                               breaks=c(0,.25, .5, .75, 1), name='Percent of Avail. Hours')
    }
    binPlot <- binPlot +
        theme(
            strip.background = element_blank(),
            strip.text.x = element_blank()
        )
    binPlot/effPlot + plot_layout(heights=c(5,1))
}
