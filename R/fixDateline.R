#' @title Fix Longitude Data Across the Dateline for Plotting
#'
#' @description Returns a dataframe identical to the input after checking
#'    if the \code{Longitude} column crosses the dateline. If it does,
#'    check whether the center is positive or negative and set all values
#'    to the same sign.
#'
#' @param data data frame with a \code{Longitude} column to adjust for
#'    cross-dateline plotting
#' @param lonName name of the Longitude column
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
fixDateline <- function(data, lonName) {
    boundLong <- range(data[[lonName]])
    if(boundLong[2]-boundLong[1] > 180) {
        if(sum(boundLong) < 0) {
            data[[lonName]] <- (data[[lonName]] %% 360)
        } else {
            data[[lonName]] <- (data[[lonName]] %% 360) - 360
        }
    }
    data
}
