context('Testing straightPath')

test_that('Test on gps with and without heading', {
    db <- system.file( 'extdata', 'PgDb.sqlite3', package='PAMmisc')
    con <- dbConnect(db, drv=SQLite())
    gps <- dbReadTable(con, 'gpsData')
    gps$UTC <- as.POSIXct(gps$UTC, format='%Y-%m-%d %H:%M:%S', tz='UTC')
    straight <- straightPath(gps, nSmall=5, nLarge=15)

    expect_is(straight, 'data.frame')
    expect_is(straight$straight, 'logical')
    # nLarge - 1 NAs are expected because of rolling average length
    expect_equal(sum(is.na(straight$straight)), 15-1)
    # should be a turn in there
    expect_true(any(!straight$straight))

    bigThresh <- straightPath(gps, nSmall=20, nLarge=30, thresh = 60)
    # any with FALSE, NA only is NA so if NA means found no turns
    expect_equal(any(!bigThresh$straight), NA)
    dbDisconnect(con)
})
