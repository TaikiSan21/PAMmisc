#' @title Utility for Selecting Variables to Download
#'
#' @description Loops through the available variables in an edinfo object and asks
#'   whether or not each should be downloaded, then stores the result for
#'   passing on to \link{formatURL}
#'
#' @param edinfo a datalist, either from \link{getEdinfo} or created by
#'   \link{erddapToEdinfo}
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom utils menu
#' @export
#'
varSelect <- function(edinfo) {
    selected <- rep(FALSE, length(edinfo$vars))
    for(i in seq_along(edinfo$vars)) {
        keepVar <- menu(title = paste0('Do you wish to download variable ', edinfo$vars[i], '?'),
                        choices = c('Yes', 'No'))
        if(keepVar == 1) {
            selected[i] <- TRUE
        }
    }
    edinfo$varSelect <- selected
    edinfo
}
