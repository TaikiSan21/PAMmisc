context('Test environmental data functions.')

test_that('Test variable selection on edi object', {
    edi <- getEdinfo()[['jplMURSST41']]

    ediSelectAll <- varSelect(edi, select=TRUE)
    ediSelectAll2 <- varSelect(edi, select=rep(TRUE,4))

    expect_is(ediSelectAll, 'edinfo')
    expect_is(edi, 'edinfo')
    expect_error(varSelect(edi, select=c(TRUE, FALSE)))
    expect_warning(varSelect(edi, select=FALSE))
    expect_identical(ediSelectAll, ediSelectAll2)
})

test_that('Making data range for URL requests', {
    gps <- data.frame(Latitude = c(31.9, 32, 32.1, 32.2, 32.2),
                      Longitude = c(-117, -117.1, -117.2, -117.3, -117.4),
                      UTC = as.POSIXct(c('2005-01-01 00:00:00', '2005-01-01 00:00:10',
                                         '2005-01-01 00:00:20', '2005-01-01 00:00:30',
                                         '2005-01-01 00:00:40'), tz='UTC'))
    range <- dataToRanges(gps)
    rangeBuffer <- dataToRanges(gps, buffer=c(.01, .01, 86400))
    expect_equal(range$Latitude[1],
                 min(gps$Latitude))
    expect_equal(range$Longitude[2],
                 max(gps$Longitude))
    expect_equal(rangeBuffer$Longitude[1],
                 min(gps$Longitude)-.01)
    expect_equal(rangeBuffer$UTC[1],
                 as.POSIXct('2004-12-31 00:00:00', tz='UTC'))
    expect_error(dataToRanges(gps[-1]), 'Must have columns')
})

test_that('Matching .nc data to a dataframe', {
    gps <- data.frame(Latitude = c(31.95, 32, 32.05),
                      Longitude = c(-117, -117.1, -117.1),
                      UTC = as.POSIXct(c('2005-01-01 00:00:00', '2005-01-01 00:00:10',
                                         '2005-01-01 00:00:20'), tz='UTC'))
    nc <- system.file('extdata', 'sst.nc', package='PAMmisc')
    matched <- ncToData(gps, nc, progress = FALSE)
    expect_identical(gps, matched[, 1:ncol(gps)])
    expect_true(all(paste0('analysed_sst_', c('mean')) %in% colnames(matched)))
    expect_identical(matched, matchEnvData(gps, nc, progress=FALSE))
    # all within buffer
    raw <- ncToData(gps, nc, raw=TRUE, buffer=c(.01, .01, 86400), progress = FALSE)
    expect_is(raw, 'list')
    expect_equal(length(raw), nrow(gps))
    # new functions
    meanPlusOne <- function(x) mean(x, na.rm=TRUE) + 1
    plusOne <- ncToData(gps, nc, FUN = c(mean, meanPlusOne), progress=FALSE)
    expect_true(all(paste0('analysed_sst_', c('mean', 'meanPlusOne')) %in% colnames(plusOne)))
    expect_identical(plusOne$analysed_sst_mean + 1, plusOne$analysed_sst_meanPlusOne)
    expect_identical(plusOne, matchEnvData(gps, nc, FUN=c(mean, meanPlusOne), progress=FALSE))
    # test warn if seems out of data bounds
    gpsOOB <- data.frame(Latitude = 33, Longitude = -116, UTC = as.POSIXct('2005-01-01 00:00:00', tz='UTC'))
    expect_warning(ncToData(gpsOOB, nc, progress=FALSE))
})
