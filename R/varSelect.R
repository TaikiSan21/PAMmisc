#' @title Utility for Selecting Variables to Download
#'
#' @description Loops through the available variables in an edinfo object and asks
#'   whether or not each should be downloaded, then stores the result for
#'   passing on to \link{formatURL}
#'
#' @param edinfo a datalist, either from \link{getEdinfo} or created by
#'   \link{erddapToEdinfo}
#' @param select (optional) logical vector of which variables to select.
#'   If left as default \code{NULL}, user will be prompted to select which
#'   variables to keep. If not \code{NULL}, can either be a single \code{TRUE}
#'   to select all variables, or a logical vector of length equal to the number
#'   of variables in \code{edinfo}
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @return the same object as \code{edinfo} with an updated \code{varSelect} field
#'
#' @examples
#'
#' sstEdi <- getEdinfo()[['jplMURSST41']]
#' \dontrun{
#' # interactively select
#' sstEdi <- varSelect(sstEdi)
#' }
#' # select all variables
#' sstEdi <- varSelect(sstEdi, TRUE)
#' # select the first two of four
#' sstEdi <- varSelect(sstEdi, c(TRUE, TRUE, FALSE, FALSE))
#'
#' @importFrom utils menu
#' @export
#'
varSelect <- function(edinfo, select=NULL) {
    if(length(select) == 1) {
        select <- rep(select, length(edinfo$vars))
    }
    if(is.null(select)) {
        select <- rep(FALSE, length(edinfo$vars))
        for(i in seq_along(edinfo$vars)) {
            keepVar <- menu(title = paste0('Do you wish to download variable ', edinfo$vars[i], '?'),
                            choices = c('Yes', 'No'))
            if(keepVar == 1) {
                select[i] <- TRUE
            }
        }
    }
    if(length(select) != length(edinfo$vars)) {
        stop('Length of select does not match number of variables.')
    }
    if(!any(select)) {
        warning('No variables selected.')
    }
    edinfo$varSelect <- select
    edinfo
}
