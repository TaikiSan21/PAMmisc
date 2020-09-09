context('Peak trough test')

test_that('Peak trough gets right values', {
    click3 <- createClickWave(signalLength = .01, clickLength = 1e5, clicksPerSecond = 9,
                              frequency = 3e3, sampleRate = 20e3, gainFactor = .8)
    click5 <- createClickWave(signalLength = .01, clickLength = 1e5, clicksPerSecond = 9,
                              frequency = 5e3, sampleRate = 20e3, gainFactor = .8)

    together <- click3 + click5
    sp <- seewave::spec(together, plot=FALSE, norm = FALSE, correction = 'amplitude')
    sp[, 2] <- 20*log10(sp[, 2])
    pt <- peakTrough(sp, freqBounds = c(.5, 3))
    expect_equal(unname(pt$peak), 3)
    expect_equal(unname(pt$peak2), 5)
    expect_equal(unname(pt$trough), 4)
    expect_equal(unname(pt$peakToPeak2), 2)
    # check warning if units appear to be hz not khz
    spHz <- sp
    spHz[, 1] <- spHz[, 1] * 1e3
    expect_warning(peakTrough(spHz))
    ptHz <- suppressWarnings(peakTrough(spHz, freqBounds = c(.5, 3)))
    expect_identical(pt, ptHz)
    # search range restricted
    pt <- peakTrough(sp, freqBounds = c(3, 10))
    expect_equal(unname(pt$peak2), 0)
})
