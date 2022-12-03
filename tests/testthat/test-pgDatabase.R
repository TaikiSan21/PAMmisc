context('Pamguard database adding functions')

test_that('Test gps adding', {
    db <- system.file( 'extdata', 'PgDb.sqlite3', package='PAMmisc')
    td <- tempdir()
    file.copy(from = db, to = td)
    tmpDb <- file.path(td, basename(db))
    expect_true(file.exists(tmpDb))
    tmpFile <- tempfile(fileext = '.csv')
    # putting 999 Lat so its obviously fake data
    gps <- data.frame(Latitude = c(999, 999, 999, 999, 999),
                      Longitude = c(-117, -117.1, -117.2, -117.3, -117.4),
                      UTC = as.POSIXct(c('2005-01-01 00:00:00', '2005-01-01 00:00:10',
                              '2005-01-01 00:00:20', '2005-01-01 00:00:30',
                              '2005-01-01 00:00:40'), tz='UTC'))

    write.csv(gps, tmpFile)
    addPgGps(tmpDb, tmpFile, source='csv', format='%Y-%m-%d %H:%M:%S')
    addPgGps(tmpDb, tmpFile, source='csv', format='%Y-%m-%d %H:%M:%S', tz='Etc/GMT-1')
    expect_error(addPgGps(tmpDb, tmpFile, source='csv', format='%Y-%m-%d %H:%M:%S', tz='DNE'),
                 'Timezone not recognized')
    unlink(tmpFile)
    # test adding gpx
    gpxFile <- system.file('extdata', 'GPX.gpx', package='PAMmisc')
    gpxData <- readGPXTrack(gpxFile)
    addPgGps(tmpDb, gpxFile, source='SPOTgpx')
    con <- dbConnect(tmpDb, drv=SQLite())
    fromDb <- dbReadTable(con, 'gpsData')
    # test db has 200 rows of GPS, 201 on should be our new stuff
    fromDb <- fromDb[201:nrow(fromDb), ]
    fromDb$UTC <- as.POSIXct(fromDb$UTC, format = '%Y-%m-%d %H:%M:%S', tz='UTC')
    # check CSV stuff
    expect_identical(gps$Latitude, fromDb$Latitude[1:5])
    expect_identical(fromDb$UTC[1:5], fromDb$UTC[6:10] + 60*60)
    expect_identical(gps$Longitude, fromDb$Longitude[1:5])
    expect_identical(gps$UTC, fromDb$UTC[1:5])
    # check GPX stuff
    expect_identical(gpxData$UTC, fromDb$UTC[11:nrow(fromDb)])
    expect_identical(gpxData$Latitude, fromDb$Latitude[11:nrow(fromDb)])
    expect_error(addPgGps(db='DNE'))

    # clean up test rows
    del <- dbSendQuery(con,
                       'DELETE FROM gpsData WHERE Id > 200'
    )
    dbClearResult(del)
    dbDisconnect(con)
    unlink(tmpDb)
    expect_true(!file.exists(tmpDb))
    expect_true(!file.exists(tmpFile))
})

test_that('Test event adding', {
    db <- system.file( 'extdata', 'PgDb.sqlite3', package='PAMmisc')
    td <- tempdir()
    file.copy(from = db, to = td)
    tmpDb <- file.path(td, basename(db))
    expect_true(file.exists(tmpDb))
    bin <- system.file('extdata', 'Click.pgdf', package='PAMmisc')
    hm <- loadPamguardBinaryFile(bin)
    uids <- c(4000001, 4000002, 4000004)
    addPgEvent(db = tmpDb, UIDs = uids, binary = bin, eventType = 'MyNewEvent', type='click')
    addPgEvent(db = tmpDb, UIDs = uids, binary = bin, eventType = 'MyNewEvent', type='click')
    now <- format(nowUTC(), '%Y-%m-%d %H:%M:%S')
    addPgEvent(db = tmpDb, eventType='BlankEvent', type='click', start=now, end=now)
    con <- dbConnect(tmpDb, drv=SQLite())
    ev <- dbReadTable(con, 'Click_Detector_OfflineEvents')
    expect_equal(nrow(ev), 2)
    click <- dbReadTable(con, 'Click_Detector_OfflineClicks')
    lookup <- dbReadTable(con, 'Lookup')
    # test matching data got added
    expect_true(all(c('MyNewEvent', 'BlankEvent') %in% lookup$Code))
    expect_equal(click$UID, uids)
    expect_equal(ev$UID[1], unique(click$parentUID))
    expect_equal(ev$nClicks, c(nrow(click), 0))
    expect_equal(ev$eventType, c('MyNewEvent', 'BlankEvent'))
    expect_equal(ev$EventEnd, c(click$UTC[3], now))
    expect_error(addPgEvent(db='DNE'))
    # clean up test rows
    del <- dbSendQuery(con, 'DELETE FROM Click_Detector_OfflineEvents')
    dbClearResult(del)
    del <- dbSendQuery(con, 'DELETE FROM Click_Detector_OfflineClicks')
    dbClearResult(del)
    del <- dbSendQuery(con, "DELETE FROM Lookup WHERE Code = 'MyNewEvent'")
    dbClearResult(del)
    dbDisconnect(con)
    # Add DG style
    addPgEvent(db = tmpDb, UIDs = uids, binary = bin, eventType = 'MyNewEventDG', type='dg', comment='DG')
    addPgEvent(db = tmpDb, UIDs = uids, binary = bin, eventType = 'MyNewEventDG', type='dg')
    addPgEvent(db = tmpDb, eventType = 'BlankDG', type='dg', start=now, end=now)

    con <- dbConnect(tmpDb, drv=SQLite())
    ev <- dbReadTable(con, 'Detection_Grouper')
    expect_equal(nrow(ev), 2)
    click <- dbReadTable(con, 'Detection_Grouper_Children')
    lookup <- dbReadTable(con, 'Lookup')
    # test matching data got added
    expect_true('MyNewEventDG' %in% lookup$Code)
    expect_equal(click$UID, uids)
    expect_equal(ev$UID[1], unique(click$parentUID))
    expect_equal(ev$DataCount, c(nrow(click), 0))
    expect_equal(ev$eventType, c('MyNewEventDG', 'BlankDG'))
    expect_equal(ev$EndTime, c(click$UTC[3], now))
    expect_equal(ev$Text_Annotation, c('DG', paste0('Created by PAMmisc v', packageVersion('PAMmisc'))))
    # clean up test rows
    del <- dbSendQuery(con, 'DELETE FROM Detection_Grouper')
    dbClearResult(del)
    del <- dbSendQuery(con, 'DELETE FROM Detection_Grouper_Children')
    dbClearResult(del)
    del <- dbSendQuery(con, "DELETE FROM Lookup WHERE Code = 'MyNewEventDG'")
    dbClearResult(del)
    dbDisconnect(con)
    unlink(tmpDb)
    expect_true(!file.exists(tmpDb))
})

test_that('Test update UID', {
    db <- system.file('extdata', 'ExBadUID.sqlite3', package='PAMmisc')
    td <- tempdir()
    file.copy(from = db, to = td)
    tmpDb <- file.path(td, basename(db))
    expect_true(file.exists(tmpDb))
    goodUIDs <- c(8000003, 529000024, 529000026, 386000022, 529000027)
    bin <- system.file('extdata', 'Binaries', package='PAMmisc')
    nUpd <- updateUID(tmpDb, bin, verbose=FALSE, progress=FALSE)
    expect_equal(nUpd$nAdded[[1]], 5)
    con <- dbConnect(tmpDb, drv=SQLite())
    det <- dbReadTable(con, 'Click_Detector_OfflineClicks')
    expect_equal(det$newUID[1:5], goodUIDs)
    expect_equal(det$newUID[6:26], det$UID[6:26])
    dbDisconnect(con)
    unlink(tmpDb)
    expect_true(!file.exists(tmpDb))
})

test_that('Test spectrogram annotation', {
    db <- system.file('extdata', 'PgDb.sqlite3', package='PAMmisc')
    td <- tempdir()
    file.copy(from = db, to = td)
    tmpDb <- file.path(td, basename(db))
    expect_true(file.exists(tmpDb))
    readAnno <- readSpecAnno(tmpDb, 'Spectrogram_Annotation')
    expect_equal(nrow(readAnno), 3)
    addAnno <- addPgAnno(tmpDb, readAnno, tableName='Spectrogram_Annotation', source='pammisc')
    expect_equal(nrow(addAnno), 0)
    newAnno <- data.frame(UTC='2020/10/23 12:11:10', Duration=.5, f1=2300, f2=3600)
    addAnno <- addPgAnno(tmpDb, newAnno, tableName='Spectrogram_Annotation', source='manual')
    expect_equal(nrow(addAnno), 1)
    addAnno <- addPgAnno(tmpDb, newAnno, tableName='Spectrogram_Annotation', source='manual')
    expect_equal(nrow(addAnno), 0)
    unlink(tmpDb)
    expect_true(!file.exists(tmpDb))
})
