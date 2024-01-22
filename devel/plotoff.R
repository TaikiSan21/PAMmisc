# PLOTOFF

bmDir <- '../Data/Plotoff/ADRIFT Blue Whale Logs/'
gps <- readRDS('../Data/Plotoff/ADRIFT_GPS.rds')
bmData <- loadBmLogs(bmDir, gps, 3600*3)
data <- prepBmData(bmData, gps)
# combine years but show on same monthly axis...
years <- unique(data$year)
data$yDiff <- data$year - min(data$year)
for(i in 1:nrow(data)) {
    data$UTC[i] <- data$UTC[i] - period(data$year[i] - min(data$year), units='year')
}
data$yearFac <- factor(data$year)
dataFake <- data
plotComp(data, by='season', fill='yearFac')

data %>%
    filter(year == 2022) %>%
    plotComp(by='season', fill='species')

data %>%
    # filter(drift == 'ADRIFT_001') %>%
    ggplot(aes(x=Longitude, y=Latitude)) +
    geom_path(aes(group=DriftName)) +
    geom_point(aes(color=call)) +
    scale_color_manual(values=scales::hue_pal()(3), na.value='transparent')





plotBarEffort(combGps, fill='species', bin='hour/day', percent=FALSE) /
    plotBarEffort(combGps,fill='species', bin='hour/day', percent=TRUE)
# 3 cols as season
# 3 rows as regions
# combine mutliple years into same plot
# give up on multiple species / calltype in same plot
g1 <- plotBarEffort(filter(combGps, year(UTC)==2021),fill='species', bin='hour/day', percent=TRUE)
g2 <- plotBarEffort(filter(combGps, year(UTC)==2022),fill='species', bin='hour/day', percent=TRUE)


## NOTES ##
# Season plot is fucked trying to fit into my old regime
# prev. T-axis was set strongly to full year
# Winter season crosses year line - fucked for Dec-01 to Feb-28 Limits
# Showing multiple years on one plot seems bad
# gonna have to re-fuck a lot of things to be manual not flexible
# I think facet could be okay for years, re-use facet fill colors for
# lower effort line?
# bottom plot could be diff lines for effort across years
prep <- markNumEffort(data, by='DriftName', bin='hour/day', keepCols='species')
effort <- prep$effort
effort <- bind_rows(lapply(split(effort, effort$year), function(x) {
    lapply(split(x, x$season), function(y) {
        # print(str(y))
        if(nrow(y) <= 1) {
            print(y)
            return(NULL)
        }
        shortenOffs(y, nMax=365)
    }) %>% bind_rows
}))

ggplot(effort, aes(x=plotX, y=nEffort, col=factor(year))) +
    geom_path(aes(group=factor(year))) +
    facet_wrap(~season)

plotEff <- function(effort) {

    # labs <- makeEffLabs(effort)
    labs <- list(ix = seq(from=min(effort$plotX), to=max(effort$plotX), length.out=5),
                 label = seq(from=min(effort$binDate), to=max(effort$binDate) - period(1, units='years'), length.out=5))
    labs$label <- format(labs$label, '%b-%d')
    effort <- formatEffortPlot(effort)
    g <- ggplot(effort) +
        geom_path(data=effort, aes(x=plotX, y=nEffort, col=factor(year), group=factor(year)))
    g <- g + scale_x_continuous(breaks=labs$ix, labels=labs$label)
    g
}

effPlot <- effort %>%
    filter(season=='Post-Upwelling') %>%
    plotEff

data <- prep$data
data$plotDate <- data$binDate
for(i in 1:nrow(data)) {
    diff <- data$year[i] - min(data$year)
    if(diff == 0) next
    data$plotDate[i] <- data$binDate[i] - period(diff, units='year')
}


tryPlot <- function(x, plotSeason='Post-Upwelling', percent=FALSE) {
    effort <- bind_rows(lapply(split(x$effort, x$effort$year), function(y) {
        lapply(split(y, y$season), function(z) {
            # print(str(y))
            if(nrow(z) <= 1) {
                # print(z)
                return(NULL)
            }
            shortenOffs(z, nMax=365)
        }) %>% bind_rows
    }))
    # effort <- filter(effort, .data$season==plotSeason)
    # data <- filter(x$data, .data$season == plotSeason)
    data <- x$data
    # browser()
    data <- left_join(data, effort[c('plotX', 'binDate', 'nEffort')])
    labs <- list(ix = seq(from=min(effort$plotX), to=max(effort$plotX), length.out=5),
                 label = seq(from=min(effort$binDate), to=max(effort$binDate) - period(1, units='years'), length.out=5))
    labs$label <- format(labs$label, '%b-%d')
    effort <- formatEffortPlot(effort)
    effPlot <- ggplot(effort) +
        geom_path(data=effort, aes(x=plotX, y=nEffort, col=factor(year), group=factor(year))) +
        scale_x_continuous(breaks=labs$ix, labels=labs$label) +
        scale_y_continuous(breaks=(1:10)*24, name='Hours')

    data <- filter(data, !is.na(species))
    if(percent) {
        # browser()
        # THIS IST WORKING WHY. Mean effort is way higher than n()
        data <- data %>%
            group_by(plotX, year) %>%
            summarise(n=n(), pct=n()/mean(nEffort), eff=mean(nEffort)) %>%
            ungroup()
        binPlot <- ggplot() +
            geom_rect(data=data, aes(xmin=plotX-.45,xmax=plotX+.45, ymin=0, ymax=pct, fill=year)) +
            # geom_line(data=formatEffortPlot(effort), aes(x=plotX, y=nEffort / ymax *24, alpha=TRUE)) +
            scale_y_continuous(expand=expansion(mult=c(0, 0.05)), limits=c(0, 1),
                               # sec.axis=sec_axis(trans = ~.*ymax/24, breaks=seq(from=0, to=ymax, by=24)),
                               breaks=c(0,.25, .5, .75, 1),name='Percent of Avail. Hours')+
            scale_x_continuous(breaks=labs$ix, labels=labs$label, limits=c(1, max(effort$plotX))) +
            facet_wrap(~year, ncol=1)
    } else {
        binPlot <- ggplot(data) +
            geom_bar(aes(fill=year, x=plotX)) +
            facet_wrap(~year, ncol=1, drop = FALSE) +
            scale_x_continuous(breaks=labs$ix, labels=labs$label, limits=c(1, max(effort$plotX)))
    }
    binPlot/effPlot + plot_layout(heights=c(5,1))
}

tryPlot(prep, percent=T, plotSeason='Upwelling')
