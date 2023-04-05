# soundscape import
bb <- readr::read_csv('../Data/Soundscape/ADRIFT_017_BB_mean_2min.csv')
names(bb)[1] <- 'UTC'
bb$UTC <- bb$UTC + 7 * 3600
gps <- readr::read_csv('../Data/Soundscape/ADRIFT_017_GPS.csv')
ol <- readr::read_csv('../Data/Soundscape/ADRIFT_017_OL_mean_2min.csv')
names(ol)[1] <- 'UTC'
ol$UTC <- ol$UTC + 7 * 3600
tol <- readr::read_csv('../Data/Soundscape/ADRIFT_017_TOL_mean_2min.csv')
names(tol)[1] <- 'UTC'
tol$UTC <- tol$UTC + 7 * 3600
psd <- readr::read_csv('../Data/Soundscape/ADRIFT_017_PSD_mean_2min.csv')



wtf <- readr::read_delim('~/../Downloads/ADRIFT_017_PSD_pct01_2min.csv', delim=',', skip=0, n_max=2, col_names=T)

# not sure which mets are relevant / how to store so much nonsense
# first test - download wind/etc data to gps track, compare this to noise levels
source('devel/GFSFunctions.R')

gps <- matchGFS(gps)
write.csv(gps, file='../Data/Soundscape/ADRIFT_017_GPS.csv', row.names = FALSE)
library(dplyr)

hm <- left_join(gps, bb, join_by(closest(UTC >= UTC)))

joinSoundscape <- function(x, ss) {
    if('yyyy-mm-ddTHH:MM:SSZ' %in% colnames(ss)) {
        ss <- rename(ss, 'sUTC' = 'yyyy-mm-ddTHH:MM:SSZ')
    }
    if('UTC' %in% colnames(ss)) {
        ss <- rename(ss, 'sUTC' = 'UTC')
    }
    xRange <- range(x$UTC)
    sRange <- range(ss$sUTC)
    x$isGreater <- x$UTC >= sRange[1]
    x <- bind_rows(lapply(split(x, x$isGreater), function(d) {
        if(d$isGreater[1]) {
            d <- left_join(d, ss, join_by(closest(UTC >= sUTC)))
        } else {
            d <- left_join(d, ss, join_by(closest(UTC <= sUTC)))
        }
        d
    }))
    x$sUTC <- NULL
    x$isGreater <- NULL
    x
}

hm <- joinSoundscape(gps, bb)
hm <- joinSoundscape(hm, ol)
hm <- joinSoundscape(hm, tol)
hm$windMag <- sqrt(hm$windU^2 + hm$windV^2)
hm <- tidyr::gather(hm, 'Measure', 'Level', `BB_100-24000`:`TOL_20000`)
hm$UTC <- hm$UTC + 7 * 3600

library(ggplot2)
ggplot(hm, aes(x=windMag, y=Level, col=Measure)) +
    geom_line()

ggplot(hm, aes(x=precRate, y=Level, col=Measure)) +
    geom_line()

dbPlot <- ggplot(hm, aes(x=UTC, y=Level, col=Measure)) +
    geom_line() +
    xlim(range(hm$UTC))

library(patchwork)
windPlot <- ggplot(hm, aes(x=UTC, y=windMag)) +
    geom_line()

dbPlot/windPlot
hm$precRate <- hm$precRate * 3600 # converting from kg/m2/s to kg/m2/h == 1L per m2 per hr
coeff <- mean(hm$precRate) / mean(hm$windMag)

envPlot <- ggplot(hm, aes(x=UTC + 3 * 3600)) +
    geom_line(aes(y=windMag), col='darkgray') +
    geom_line(aes(y=precRate / coeff), col='blue') +
    scale_y_continuous(
        name = 'Wind Speed (m/s)',
        sec.axis = sec_axis(~.*coeff, name='Precipitation Rate (kg/m2/hr)')
    ) +
    theme(
        axis.title.y = element_text(color = 'darkgray', size=13),
        axis.title.y.right = element_text(color = 'blue', size=13)
    ) +
    xlim(range(hm$UTC))
dbPlot / envPlot
# not really sure what to do with PSDrange

ggplot(gps, aes(x=Longitude, y=Latitude, col=UTC)) +
    geom_point()
