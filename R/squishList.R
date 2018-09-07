#' @title Compress a List by Name
#'
#' @description Attempts to compress a list by combining elements with
#'   the same name, searching recursively if there are lists in your
#'   list
#'
#' @details items with the same name are assumed to have the same structure
#'   and will be combined. Dataframes will be combined with bind_rows, vectors
#'   just be collapsed into one vector, lists will be combined recursively with
#'   another call to \code{squishList}
#'
#' @param myList a list with named elements to be compressed
#'
#' @return a list with one element for every unique name in the original list
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#'
#' myList <- list(a=1:3, b=letters[1:4], a=5:6, b=letters[4:10])
#' squishList(myList)
#'
#' myList <- list(a=1:3, b=data.frame(x=1:3, y=4:6), b=data.frame(x=10:14, y=1:5))
#' squishList(myList)
#'
#' myList <- list(a=list(c=1:2, d=2), b=letters[1:3], a=list(c=4:5, d=6:9))
#' squishList(myList)
#'
#' @importFrom dplyr bind_rows
#' @export
#'
squishList <- function(myList) {
    myNames <- unique(names(myList))
    if(is.null(myNames)) return(myList)
    result <- vector('list', length=length(myNames))
    names(result) <- myNames
    for(n in myNames) {
        whichThisName <- which(names(myList)==n)
        thisNameData <- myList[whichThisName]
        thisClasses <- sapply(thisNameData, class)
        # This is a mess, but oh well.
        result[[n]] <- if(length(whichThisName)==1) {
            thisNameData[[1]]
        } else if('list' %in% thisClasses) {
            thisNameData <- unlist(thisNameData, recursive = FALSE)
            names(thisNameData) <- gsub(paste0(n, '\\.'), '', names(thisNameData))
            squishList(thisNameData)
        } else if(all(thisClasses=='data.frame')) {
            bind_rows(thisNameData)
        } else if(all(thisClasses=='NULL')) {
            next
        } else {
            # thisNameData[[1]]
            unlist(thisNameData, use.names = FALSE)
        }
    }
    result
}
