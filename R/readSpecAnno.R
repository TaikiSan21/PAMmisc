#' @title Read Pamguard Spectrogram Annotation Table
#'
#' @description Reads the Spectrogram Annotation table from a PAMGuard database
#'   and applies some minor formatting
#'
#' @param db database file to read data from
#' @param table name of the Spectrogram Annotation table to read
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
readSpecAnno <- function(db, table='Spectrogram_Annotation') {
    con <- dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    if(!table %in% dbListTables(con)) {
        warning('No table named ',table, ' found in database ', db)
        return(NULL)
    }
    sa <- dbReadTable(con, table)
    sa$UTC <- as.POSIXct(format(sa$UTC, format='%Y-%m-%d %H:%M:%OS3'), format='%Y-%m-%d %H:%M:%OS', tz='UTC')
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
        sa[[c]] <- gsub('^\\s+|\\s+$', '', sa[[c]])
    }
    sa$db <- db
    sa$tableName <- table
    # doing some reorderin since we renamed and made new stuff
    baseCols <- c('id', 'start', 'end', 'fmin', 'fmax', 'Note', 'Label', 'Duration', 'snr', 'RMS', 'ZeroPeak', 'PeakPeak', 'SEL')
    extraCols <- colnames(sa)[!colnames(sa) %in% baseCols]
    sa[c(baseCols, extraCols)]
}
