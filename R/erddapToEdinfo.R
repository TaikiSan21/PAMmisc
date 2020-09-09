#' @title Create an edinfo Object from an ERDDAP Dataset Id
#'
#' @description Creates an edinfo object that can be used to create a URL for
#'   downloading environmental data using \link{edinfoToURL}
#'
#' @param dataset an ERDDAP dataset id, or the result from \link[rerddap]{info}
#' @param baseurl the base URL of an ERDDAP server
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
#' }
#'
#' @importFrom rerddap info
#' @export
#'
erddapToEdinfo <- function(dataset, baseurl='https://upwell.pfeg.noaa.gov/erddap/', chooseVars = TRUE) {
    if(is.character(dataset)) {
        dataset <- info(dataset, url = baseurl)
    }
    if(!inherits(dataset, 'info')) {
        stop(dataset, ' must be a valid ERDDAP dataset id or result from rerddap::info')
    }
    data <- dataset$alldata
    names(data) <- standardCoordNames(names(data))
    result <- list(base = paste0(dataset$base_url, 'griddap/'))
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
