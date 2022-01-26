#' @title Create an edinfo Object from an ERDDAP Dataset Id
#'
#' @description Creates an edinfo object that can be used to create a URL for
#'   downloading environmental data using \link{edinfoToURL}
#'
#' @param dataset an ERDDAP or HYCOM dataset id, or the result from \link[rerddap]{info}
#' @param baseurl the base URL of an ERDDAP/HYCOM server
#' @param chooseVars logical flag whether or not to select which variables you want now
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @return an edinfo list object that can be used to download environmental data
#'
#' @examples
#' \dontrun{
#' # examples not run because they require internet connection
#' sstEdi <- erddapToEdinfo('jplMURSST41')
#' # dataset from a diferent erddap server
#' sshEdi <- erddapToEdinfo('hawaii_soest_2ee3_0bfa_a8d6',
#'                           baseurl = 'http://apdrc.soest.hawaii.edu/erddap/')
#' # THese work the same - erddap function will pass to hycom if appears to be hycom dataset
#' hycomEdi <- hycomToEdinfo('GLBy0.08/expt_93.0')
#' hycomEdi <- erddapToEdinfo('GLBy0.08/expt_93.0')
#' }
#'
#' @importFrom rerddap info
#' @importFrom xml2 xml_contents xml_attrs
#' @export
#'
erddapToEdinfo <- function(dataset, baseurl='https://upwell.pfeg.noaa.gov/erddap/', chooseVars = TRUE) {
    if(is.character(dataset)) {
        if(grepl('^GLB.{4,6}/expt', dataset)) {
            return(hycomToEdinfo(dataset=dataset, chooseVars=chooseVars))
        }
        dataset <- info(dataset, url = baseurl)
    }
    if(!inherits(dataset, 'info')) {
        stop(dataset, ' must be a valid ERDDAP dataset id or result from rerddap::info')
    }
    data <- dataset$alldata
    names(data) <- standardCoordNames(names(data))
    if('Depth' %in% names(data) &&
       data$Depth[1,1] == 'variable') {
        names(data)[names(data) == 'Depth'] <- data$Depth[1, 2]
    }
    base <- dataset$base_url
    # base <- gsub('/$', '', base)
    # base <- paste0(base, '/griddap/')
    result <- list(base = base)
    result$dataset <- attr(dataset, 'datasetid')
    result$fileType <- 'nc'
    result$vars <- names(data)[!(names(data) %in% c('UTC', 'Longitude', 'Latitude', 'Depth', 'NC_GLOBAL'))]
    getRangeParse <- function(dim) {
        char <- dim[dim$attribute_name == 'actual_range', 'value']
        char <- paste0('c(', char, ')')
        val <- eval(parse(text = char))

        # i think first row is this header thing, has info
        hdr <- dim[1, 'value']
        hdr <- gsub(' ', '', hdr)
        hdr <- strsplit(hdr, ',')[[1]]
        hasAvg <- sapply(hdr, function(x) grepl('averageSpacing', x))
        if(any(hasAvg) &&
           dim[dim$attribute_name == 'ioos_category', 'value'] != 'Time') {
            spacing <- as.numeric(gsub('averageSpacing=', '', hdr[hasAvg]))
        } else {
            hdr <- hdr[sapply(hdr, function(x) grepl('nValues', x))]
            nVals <- as.numeric(gsub('nValues=', '', hdr))
            if(nVals == 1) {
                spacing <- NA
            } else if(dim[dim$attribute_name == 'ioos_category', 'value'] == 'Time') {
                val <- ncTimeToPosix(val, dim[dim$attribute_name == 'units', 'value'])
                spacing <- as.double(diff(val), units='secs')/(nVals-1)
            } else {
                spacing <- diff(val)/(nVals-1)
            }
        }
        list(range=val, spacing=spacing)
    }
    longInfo <- getRangeParse(data$Longitude)
    latInfo <- getRangeParse(data$Latitude)
    result$limits <- list(
        Longitude = longInfo$range,
        Latitude = latInfo$range
    )
    result$spacing <- list(
        Longitude = longInfo$spacing,
        Latitude = latInfo$spacing
    )
    if('UTC' %in% names(data)) {
        timeInfo <- getRangeParse(data$UTC)
        result$limits$UTC <- timeInfo$range
        result$spacing$UTC <- timeInfo$spacing
    }
    if('Depth' %in% names(data)) {
        depthInfo <- getRangeParse(data$Depth)
        result$limits$Depth <- depthInfo$range
        result$spacing$Depth <- depthInfo$spacing
    }
    result$is180 <- dataIs180(result$limits$Longitude)
    result$source <- 'erddap'
    if(chooseVars) {
        result <- varSelect(result)
    }
    class(result) <- c('edinfo', 'list')
    result
}

#' @export
#' @rdname erddapToEdinfo
#'
hycomToEdinfo <- function(dataset='GLBy0.08/expt_93.0',
                          baseurl = 'https://ncss.hycom.org/thredds/ncss/',
                          chooseVars=TRUE) {

    xmlUrl <- paste0(baseurl, 'grid/', dataset, '/dataset.xml')
    xml <- read_xml(xmlUrl)
    axis <- xml_find_all(xml, 'axis')
    axText <- xml_find_all(axis, 'values') %>%  xml_contents() %>% xml_text() %>% strsplit(' ')
    coordNames <- xml_attr(axis, 'name')
    if(length(axText) != length(axis)) {
        time <- xml_find_all(axis, 'values[@start]') %>% xml_attrs()
        time <- time[[1]]
        if(!all(c('start', 'increment', 'npts') %in% names(time))) {
            warning('Cant parse XML for dataset ', dataset)
            return(NULL)
        }
        time <- sapply(time, as.numeric)
        axText[[length(axText)+1]] <- seq(from=time['start'], by=time['increment'], length.out=time['npts'])
        whereTime <- which(coordNames == 'time')
        if(whereTime != length(coordNames)) {
            reorg <- 1:(length(coordNames)-1)
            reorg <- c(reorg[0:(whereTime-1)], length(coordNames), reorg[whereTime:(length(coordNames)-1)])
            axText <- axText[reorg]
        }
    }

    names(axText) <- standardCoordNames(coordNames)
    axText <- lapply(axText, as.numeric)
    axText$UTC <- axText$UTC * 3600
    limits <- lapply(axText, range)
    limits$UTC <- as.POSIXct(limits$UTC, tz='UTC', origin="2000-01-01 00:00:00" )

    spacing <- vector('list', length(axText))
    names(spacing) <- names(axText)
    for(i in names(spacing)) {
        spacing[[i]] <-
            if(i == 'UTC') {
                tbl <- table(diff(axText$UTC))
                as.numeric(names(tbl)[which.max(tbl)[1]])
            } else if(i %in% c('Latitude', 'Longitude')) {
                round(diff(range(axText[[i]])) / (length(axText[[i]]) - 1), 2)
            } else {
                diff(range(axText[[i]])) / (length(axText[[i]]) - 1)
            }
    }
    varNames <- xml_find_all(xml, 'gridSet/grid') %>% xml_attr('name')
    result <- list(base = baseurl,
                   dataset = dataset,
                   fileType = 'netcdf4',
                   vars = varNames[varNames %in% c('surf_el', 'salinity', 'water_temp', 'water_u', 'water_v')],
                   limits = limits,
                   spacing = spacing,
                   stride = 1,
                   source='hycom'
    )
    result$is180 <- dataIs180(result$limits$Longitude)
    if(chooseVars) {
        result <- varSelect(result)
    }
    class(result) <- c('edinfo', 'list')
    result
}

# only for 1 x
whichHycom <- function(x, hycom) {
    if(is.data.frame(x)) {
        return(sapply(x$UTC, function(u) {
            whichHycom(u, hycom)
        }))
    }
    if(inherits(hycom, 'hycomList')) {
        hycom <- hycom$list
    }
    possHy <- which(sapply(hycom, function(h) {
        if(!is.null(h$isCurrent) &&
           isTRUE(h$isCurrent)) {
            h$limits$UTC[2] <- nowUTC()
        }
        (x >= h$limits$UTC[1]) &&
            (x <= h$limits$UTC[2])
    }))
    if(length(possHy) == 0) {
        return(NA)
    }
    unname(possHy[length(possHy)])
}
