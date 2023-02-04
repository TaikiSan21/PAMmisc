#' @title getFigshareInfo
#' 
#' @description downloads filename and recording URL information from a Figshare
#'   article. Requires a users API token from their figshare account
#'   
#' @param token Personal API token from users Figshare account
#' @param id Figshare article ID to download information for
#' 
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#' 
#' @return dataframe with columns \code{filename} and \code{recording_url}
#' 
#' @importFrom rjson fromJSON
#' @importFrom httr GET
#' @importFrom dplyr bind_rows
#' 
#' @export
#' 
getFigshareInfo <- function(token, id) {
    base <- "http://api.figshare.com/v2/"
    method <- paste("articles", id, sep = "/")
    url <- paste0(base, method)
    hdr <- add_headers(Authorization = paste0('token ', token))
    figdl <- GET(url=url, config=hdr)
    outs <- fromJSON(rawToChar(figdl$content))
    outs <- bind_rows(outs$files)[c('name', 'download_url')]
    names(outs) <- c('filename', 'recording_url')
    outs
}
