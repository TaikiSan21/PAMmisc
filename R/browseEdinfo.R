#' @title Browse a List of Environmental Datasets
#'
#' @description This function browses the list of selected environmental datasets
#'   that are recommended as a starting point, and prompts the user to select one to use,
#'   returning an edinfo object. Also allows user to filter by variable name, matching
#'   will be attempted using regex
#'
#' @param var the name or partial name of a variable to filter the available datasets by
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @return Returns an \code{edinfo} class object that can be used to get environmental
#'   data with other functions
#'
#' @examples
#'
#' \dontrun{
#' # browse the full list (interactive)
#' edi <- browseEdinfo()
#'
#' # search for sst datasets (interactive)
#' edi <- browseEdinfo(var='sst')
#' }
#'
#' @export
#'
browseEdinfo <- function(var=NULL) {
    allList <- getEdinfo()
    if(!is.null(var)) {
        hasVar <- sapply(allList, function(x) any(grepl(var, x$vars)))
        if(sum(hasVar) == 0) {
            stop('No datasets had variables matching ', var)
        }
        allList <- allList[hasVar]
    }
    cat(length(allList), ' available datasets:', sep='')
    for(i in seq_along(allList)) {
        cat('\n[', i, ']\n', sep='')
        print(allList[[i]])
    }
    ediChoice <- menu(title = 'Select a dataset:',
                      choices = seq_along(allList))
    if(ediChoice == 0) {
        cat('No selection made.')
        return(invisible(NULL))
    }
    varSelect(allList[[ediChoice]])
}
