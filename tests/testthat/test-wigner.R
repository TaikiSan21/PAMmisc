context('Wigner-ville transform test')

test_that('Test proper output', {
    clickWave <- createClickWave(signalLength = .05, clickLength = 1000, clicksPerSecond = 200,
                                 frequency = 3e3, sampleRate = 10e3)
    wt <- wignerTransform(clickWave@left, n = 1000, sr = 10e3, plot=FALSE)

    # n time steps is next power of 2 from length of clip 500 -> 512
    expect_equal(length(wt$t), .05 * 10e3)
    # should be n
    expect_equal(length(wt$f), 1000)
    # and these are dims of tfr
    expect_identical(dim(wt$tfr), c(1000L, 500L))
    # no NAs
    expect_true(!any(is.na(wt$tfr)))
})
