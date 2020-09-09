context('Pamguard database adding functions')

test_that('Test gps adding', {
    db <- system.file( 'extdata', 'PgDb.sqlite3', package='PAMmisc')
    tmpFile <- tempfile(fileext = '.csv')
    # putting 999 Lat so its obviously fake data
    gps <- data.frame(Latitude = c(999, 999, 999, 999, 999),
                      Longitude = c(-117, -117.1, -117.2, -117.3, -117.4),
                      UTC = as.POSIXct(c('2005-01-01 00:00:00', '2005-01-01 00:00:10',
                              '2005-01-01 00:00:20', '2005-01-01 00:00:30',
                              '2005-01-01 00:00:40'), tz='UTC'))

    write.csv(gps, tmpFile)
    addPgGps(db, gps, source='csv', format='%Y-%m-%d %H:%M:%S')
    file.remove(tmpFile)
    con <- dbConnect(db, drv=SQLite())
    fromDb <- dbReadTable(con, 'gpsData')
    # test db has 200 rows of GPS, 201 on should be our new stuff
    fromDb <- fromDb[201:nrow(fromDb), ]
    fromDb$UTC <- as.POSIXct(fromDb$UTC, format = '%Y-%m-%d %H:%M:%S', tz='UTC')
    expect_identical(gps$Latitude, fromDb$Latitude)
    expect_identical(gps$Longitude, fromDb$Longitude)
    expect_identical(gps$UTC, fromDb$UTC)
    # clean up test rows
    del <- dbSendQuery(con,
                       'DELETE FROM gpsData WHERE Id > 200'
    )
    RSQLite::dbClearResult(del)
    dbDisconnect(con)
})

test_that('Test event adding', {
    db <- system.file( 'extdata', 'PgDb.sqlite3', package='PAMmisc')
    bin <- system.file('extdata', 'Click.pgdf', package='PAMmisc')
    hm <- loadPamguardBinaryFile(bin)
    uids <- c(4000001, 4000002, 4000004)
    addPgEvent(db = db, UIDs = uids, binary = bin, eventType = 'MyNewEvent')
    con <- dbConnect(db, drv=SQLite())
    ev <- dbReadTable(con, 'Click_Detector_OfflineEvents')
    click <- dbReadTable(con, 'Click_Detector_OfflineClicks')
    lookup <- dbReadTable(con, 'Lookup')
    # test matching data got added
    expect_true('MyNewEvent' %in% lookup$Code)
    expect_equal(click$UID, uids)
    expect_equal(ev$UID, unique(click$parentUID))
    expect_equal(ev$nClicks, nrow(click))
    expect_equal(ev$eventType, 'MyNewEvent')
    expect_equal(ev$EventEnd, click$UTC[3])
    # clean up test rows
    del <- dbSendQuery(con, 'DELETE FROM Click_Detector_OfflineEvents')
    RSQLite::dbClearResult(del)
    del <- dbSendQuery(con, 'DELETE FROM Click_Detector_OfflineClicks')
    RSQLite::dbClearResult(del)
    del <- dbSendQuery(con, "DELETE FROM Lookup WHERE Code = 'MyNewEvent'")
    RSQLite::dbClearResult(del)
    dbDisconnect(con)
})
