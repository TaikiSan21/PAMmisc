#' @title Read Pamguard Spectrogram Annotation Table
#'
#' @description Reads the Spectrogram Annotation table from a PAMGuard database
#'   and applies some minor formatting
#'
#' @param db database file to read data from
#'
#' @return a dataframe containing spectrogram annotation data
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#' \dontrun{
#' myDb <- 'PamguardDatabase.sqlite3'
#' specAnno <- readSpecAnno(db)
#' }
#' @importFrom RSQLite dbConnect SQLite dbListTables dbReadTable dbDisconnect
#'
#' @export
#'
readSpecAnno <- function(db) {
    con <- dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    if(!'Spectrogram_Annotation' %in% dbListTables(con)) {
        warning('No Spectrogram_Annotation table found in db ', db)
        return(NULL)
    }
    sa <- dbReadTable(con, 'Spectrogram_Annotation')
    sa$UTC <- as.POSIXct(as.character(sa$UTC), format='%Y-%m-%d %H:%M:%OS', tz='UTC')
    sa$id <- as.character(sa$Id)
    sa$start <- sa$UTC
    sa$end <- sa$start + sa$Duration
    sa$fmin <- sa$f1
    sa$fmax <- sa$f2
    dropCols <- c('UTC', 'UTCMilliseconds', 'UID', 'PCLocalTime', 'PCTime', 'ChannelBitmap',
                  'SequenceBitmap', 'Sequence', 'f1', 'f2', 'Id')
    keepCols <- !(colnames(sa) %in%
                      c('UTC', 'UTCMilliseconds', 'UID', 'PCLocalTime', 'PCTime', 'ChannelBitmap',
                        'SequenceBitmap', 'Sequence', 'f1', 'f2', 'Id'))
    sa <- sa[, keepCols, drop=FALSE]
    isChar <- which(sapply(sa, function(x) inherits(x, 'character')))
    for(c in isChar) {
        sa[[c]] <- str_trim(sa[[c]])
    }
    sa$db <- db
    # doing some reorderin since we renamed and made new stuff
    baseCols <- c('id', 'start', 'end', 'fmin', 'fmax', 'Note', 'Label', 'Duration', 'snr', 'RMS', 'ZeroPeak', 'PeakPeak', 'SEL')
    extraCols <- colnames(sa)[!colnames(sa) %in% baseCols]
    sa[c(baseCols, extraCols)]
}
