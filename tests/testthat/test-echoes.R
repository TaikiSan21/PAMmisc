context('Test echo functions')

test_that('Test echo timing', {
    cps <- 200
    clickSpacing <- 1/cps
    clickWave <- createClickWave(.03, 
                                 clickLength = 100, 
                                 clicksPerSecond = cps, 
                                 frequency = 10e3, 
                                 sampleRate = 42e3)
    echoes <- findEchoTimes(clickWave, plot=FALSE)
    expect_equal(round(echoes$time, 3), c(1:3) * clickSpacing)
    # check same result on numeric vec
    wavNumeric <- clickWave@left / 2^(clickWave@bit - 1)
    echoNum <- findEchoTimes(wavNumeric, sr=clickWave@samp.rate, plot=FALSE)
    expect_equal(echoNum, echoes)
    
    echoes <- findEchoTimes(clickWave, plot=FALSE, minTime=clickSpacing * 1.5)
    expect_equal(round(echoes$time[1], 3), clickSpacing * 2)
    echoes <- findEchoTimes(clickWave, plot=FALSE, maxTime=clickSpacing * 2.5)
    expect_true(!clickSpacing*3 %in% round(echoes$time, 3))
    # test to make sure bad filter values dont error
    expect_warning(findEchoTimes(clickWave, plot=FALSE, filter=c(0, 50e3)),
                   'Filter values')
    echoes <- findEchoTimes(clickWave, plot=FALSE, filter=c(20e3))
    echoNum <- findEchoTimes(wavNumeric, sr=clickWave@samp.rate, plot=FALSE)
})
