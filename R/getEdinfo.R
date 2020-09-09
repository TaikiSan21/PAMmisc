#' @title Browse a List of Curated Environmental Datasets
#'
#' @description This function gets the list of environmental datasets provided
#'   as a recommended starting point for various measures
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @return a list of edinfo list objects
#'
#' @examples
#'
#' ediList <- getEdinfo()
#' ediList[[1]]
#' ediList[['jplMURSST41']]
#'
#' @export
#'
getEdinfo <- function() {
    hycomList <- list(
         'HYCOM_GLBu19.1' = list(base = 'http://ncss.hycom.org/thredds/ncss/',
                                 dataset = 'GLBu0.08/expt_19.1',
                                 fileType = 'netcdf',
                                 vars = c('surf_el', 'salinity', 'water_temp', 'water_u', 'water_v'),
                                 is180 = TRUE,
                                 limits = list(
                                     Longitude = c(-180, 179.92),
                                     Latitude = c(-80, 80),
                                     UTC = as.POSIXct(c('1995-08-01 00:00:00', '2012-12-31 00:00:00'), tz='UTC')
                                 ),
                                 spacing = list(
                                     Longitude = .08,
                                     Latitude = .08,
                                     UTC = 86400
                                 ),
                                 stride = 1,
                                 source = 'hycom'))
    for(i in seq_along(hycomList)) {
        class(hycomList[[i]]) <- c('edinfo', 'list')
    }
    c(hycomList, PAMmisc::erddapList)
    # hycomlist
}

#' @export
#'
print.edinfo <- function(x, ...) {
    cat('Dataset id ', x$dataset, ' has ', length(x$vars), ' variables:\n    ',
        paste0(x$vars, collapse=', '), '\n  With valid coordinate limts:\n    ',
        rngPrinter(x$limits), '\n  And average coordinate spacing:\n    ',
        rngPrinter(x$spacing), sep='')
}

# update edinfo objects, mostly relevant for -Present time ranges
updateEdinfo <- function() {
    baseURLs <- c('https://upwell.pfeg.noaa.gov/erddap/')
    datasets <- list(id = c('jplMURSST41mday', 'jplMURSST41', 'jplMURSST41clim', 'erdMH1pp8day', 'erdMBchla8day', 'erdSrtm30plusSeafloorGradient'),
                     baseIx = c(1, 1, 1, 1, 1, 1))
    erddapList <- vector('list', length = length(datasets$id))
    for(i in seq_along(datasets$id)) {
        erddapList[[i]] <- erddapToEdinfo(datasets$id[i], baseURLs[datasets$baseIx[i]], chooseVars = FALSE)
    }
    names(erddapList) <- datasets$id
    save(erddapList, file = './data/erddapList.RData')
}

rngPrinter <- function(x) {
    allRng <- vector('character', length = length(x))
    for(n in seq_along(allRng)) {
        rngVal <- x[[n]]
        if(names(x)[n] == 'UTC' &&
           is.numeric(rngVal)) {
            if(max(rngVal) > 3000) {
                rngVal <- round(rngVal / 24 / 3600, 3) # converting time spacing to days instead of seconds
            }
            rngVal <- paste0(rngVal, ' (days)')
        }
        if(is.numeric(rngVal)) {
            rngVal <- round(rngVal, 3)
        }
        allRng[n] <- paste0(names(x)[n], ': ',
                            paste0(as.character(rngVal), collapse = ' to '))
    }
    paste0(allRng, collapse = ', ')
}
