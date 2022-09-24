#' @title Perform QA/QC on Soundtrap Files
#'
#' @description Gathers data from Soundtrap XML log files to perform QA/QC on
#'   a set of recordings.
#'
#' @param dir directory containing Soundtrap XML logs, wav files, and SUD files.
#'   Can either be a single directory containing folders with all files (will
#'   search recursively), or a vector of three directories containing the SUD files,
#'   wav files, and XML files (in that order - alphabetical S-W-X)
#' @param outDir if provided, output plots and data will be written to this folder
#' @param xlim date limit for plots
#' @param label label to be used for plots and names of exported files
#' @param plot logical flag to create output plots
#'
#' @return list of dataframes with summary data for \code{$xmlInfo}, \code{$sudInfo},
#'   and \code{$wavInfo}
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#'
#' \dontrun{
#' # not run
#' stDir <- './Data/SoundtrapFiles/'
#' stData <- soundtrapQAQC(stDir, plot=TRUE)
#' # save data
#' stData <- soundtrapQAQC(stDir, outDir='./Data/SoundtrapFiles/QAQC', plot=TRUE)
#' # or provide separate folders of data
#' stDirs <- c('./Data/SoundtrapFiles/SUDFiles',
#'             './Data/SoundtrapFiles/WavFiles',
#'             './Data/SoundtrapFiles/XMLFiles')
#' stData <- soundtrapQAQC(stDirs, plot=TRUE)
#' }
#'
#' @importFrom xml2 read_xml xml_find_all xml_contents
#' @importFrom graphics axis.POSIXct mtext par
#' @importFrom grDevices dev.off png
#' @importFrom dplyr bind_rows
#'
#' @export
#'
soundtrapQAQC <- function(dir, outDir=NULL, xlim=NULL, label=NULL, plot=TRUE) {
    if(!is.null(outDir) &&
       !dir.exists(outDir)) {
        dir.create(outDir)
    }
    if(length(dir) == 1) {
        allFiles <- list.files(dir, recursive=TRUE, full.names=TRUE, pattern='sud$|wav$|xml$')
        wavFiles <- allFiles[grepl('wav$', allFiles)]
        xmlFiles <- allFiles[grepl('xml$', allFiles)]
        sudFiles <- allFiles[grepl('sud$', allFiles)]
    } else if(length(dir) == 3) {
        sudFiles <- list.files(dir[1], recursive=TRUE, full.names=TRUE, pattern='sud$')
        wavFiles <- list.files(dir[2], recursive=TRUE, full.names=TRUE, pattern='wav$')
        xmlFiles <- list.files(dir[3], recursive=TRUE, full.names=TRUE, pattern='xml$')
    } else {
        stop('"dir" must be length 1 or 3.')
    }
    xmlInfo <- bind_rows(lapply(xmlFiles, function(x) {
        result <- doOneQAQC(x)
        if(is.null(result)) {
            return(NULL)
        }
        result$xmlName <- basename(x)
        result
    }))
    # xmlInfo$xmlName <- basename(xmlFiles)
    sudInfo <- data.frame(sudName=basename(sudFiles),
                          sudSize = sapply(sudFiles, file.size))
    wavInfo <- data.frame(wavName=basename(wavFiles),
                          wavSize = sapply(wavFiles, file.size))
    outs <- list(xmlInfo=xmlInfo, sudInfo=sudInfo, wavInfo=wavInfo)
    if(plot) {
        doQAQCPlot(outs, outDir=outDir, xlim=xlim, label=label)
    }
    if(!is.null(outDir)) {
        saveRDS(outs, file=file.path(outDir, paste0(label, '_Data.rds')))
    }
    outs
}

doQAQCPlot <- function(x, outDir=NULL, xlim=NULL, label=NULL) {
    # Can only plot these if num files is equal
    xmlInfo <- x$xmlInfo
    sudInfo <- x$sudInfo
    wavInfo <- x$wavInfo

    if(is.null(xlim)) {
        xlim <- range(xmlInfo$startUTC)
    }
    # Battery/ Temperature plot
    ## set up some fake test data
    ## add extra space to right margin of plot within frame
    if(!is.null(outDir)) {
        png(filename=file.path(outDir, paste0(label, '_TV.png')), width=6, height=4, units='in', res=300)
    }
    op <- par(mar=c(5, 4, 4, 6) + 0.1)
    on.exit(par(op))

    ## Plot first set of data and draw its axis
    plot(x=xmlInfo$startUTC, y=xmlInfo$batt,
         xaxt='n', yaxt='n', xlab="", ylab="", type="l",col="darkblue")
    mtext("External Battery, V",side=2, col='darkblue', line=3)
    axis(2, ylim=xmlInfo$temp,las=1, col='darkblue', col.axis='darkblue')
    title(label)
    par(new=TRUE)

    plot(x=xmlInfo$startUTC, y=xmlInfo$temp,
         xlab="", ylab="", xaxt='n', yaxt='n', type='l', col='darkorange')

    mtext("Internal Temperature, C",side=4,line=3.5, col='darkorange')
    axis(4, ylim=xmlInfo$temp,las=1, col='darkorange', col.axis='darkorange')
    axis.POSIXct(1, x=xlim, format='%b-%d')

    par(op)
    on.exit()

    if(!is.null(outDir)) {
        dev.off()
        png(filename=file.path(outDir, paste0(label, '_GAPS.png')), width=6, height=4, res=300, units='in')
    }
    # Time gap plot
    gap <- xmlInfo$startUTC[2:nrow(xmlInfo)] - xmlInfo$endUTC[1:(nrow(xmlInfo)-1)]
    plot(x=xmlInfo$startUTC[1:(nrow(xmlInfo)-1)], y=gap/3600,
         ylab='Hours', xaxt='n', xlab='', col='darkblue')
    axis.POSIXct(1, x=xlim, format='%b-%d')
    title('Time gap between files')

    # Mulitpanel Plot
    doSud <- nrow(xmlInfo) == nrow(sudInfo)
    doWav <- nrow(xmlInfo) == nrow(wavInfo)
    nPlots <- 6 + doSud + doWav
    if(!is.null(outDir)) {
        dev.off()
        png(filename=file.path(outDir, paste0(label, '_QC.png')), width=12, height=3*(ceiling(nPlots/2)), res=300, units='in')
        on.exit(dev.off())
    }
    op <- par(mfrow=c(ceiling(nPlots/2), 2), mar=c(2.1, 4.1, 2.1, 2.1))
    on.exit(par(op))
    # 1 sud size plot
    if(doSud) {
        plot(x=xmlInfo$startUTC, y=sudInfo$sudSize/(1024^3),
             ylab='Gb', type='l', col='darkblue', xlim=xlim, xaxt='n', xlab='')
        title('Size of compressed files')
    }
    # 2 wav size plot
    if(doWav) {
        plot(x=xmlInfo$startUTC, y=wavInfo$wavSize/(1024^3),
             ylab='Gb', type='l', col='darkblue', xlim=xlim, xaxt='n', xlab='')
        title('Size of wav files')
    }
    # 3 period plot
    plot(x=xmlInfo$startUTC, y=xmlInfo$period,
         ylab='Hours', type='l', col='darkblue', xlim=xlim, xaxt='n', xlab='')
    title('Sampling Time Period')
    # 4 count/s plot
    plot(x=xmlInfo$startUTC, y=xmlInfo$sampleCount/(xmlInfo$period*1e-6),
         ylab='Samples/sec', type='l', col='darkblue', xlim=xlim, xaxt='n', xlab='')
    title('SampleCount / SampleTimePeriod')
    # 5 count plot
    plot(x=xmlInfo$startUTC, y=xmlInfo$sampleCount,
         ylab='Samples', type='l', col='darkblue', xlim=xlim, xaxt='n', xlab='')
    title('Number of samples per file')
    # 6 Time diff plot
    dt <- as.numeric(difftime(xmlInfo$endUTC, xmlInfo$startUTC, units='secs'))
    plot(x=xmlInfo$startUTC, y=xmlInfo$period*1e-6 - dt,
         ylab='Seconds', type='l', col='darkblue', xlim=xlim, xaxt='n', ylim=c(-1, 1), xlab='')
    title('SampTimePeriod - (DateStop - DateStart)')
    # 7 samp Gap plot
    par(mar=c(5.1, 4.1, 3.1, 2.1))
    plot(x=xmlInfo$startUTC, y=xmlInfo$gap*1e-6,
         ylab='Seconds', type='l', col='darkblue', xlim=xlim, xaxt='n', xlab='')
    axis.POSIXct(1, x=xlim, format='%b-%d')
    title('Cumulative Sampling Gap')
    # 8 file gap plot

    plot(x=xmlInfo$startUTC[1:(nrow(xmlInfo)-1)], y=gap,
         ylab='Seconds', type='l', col='darkblue', xlim=xlim, xaxt='n', xlab='')
    axis.POSIXct(1, x=xlim, format='%b-%d')
    title('Gaps between files')
    if(!is.null(outDir)) {
        dev.off()
    }
}

doOneQAQC <- function(xml) {
    if(is.character(xml)) {
        tryXml <- try(read_xml(xml))
        if(inherits(tryXml, 'try-error')) {
            warning('Unable to read file ', xml)
            return(NULL)
        }
        xml <- tryXml
    }
    if(!inherits(xml, 'xml_document')) {
        return(NULL)
    }
    result <- list()
    startNode <- xml_find_all(xml, '//@SamplingStartTimeUTC')
    if(length(startNode) > 0) {
        result$startUTC <- stToPosix(as.character(xml_contents(startNode)))
    } else {
        result$startUTC <- NA
    }
    endNode <- xml_find_all(xml, '//@SamplingStopTimeUTC')
    if(length(endNode) > 0) {
        result$endUTC <- stToPosix(as.character(xml_contents(endNode)))
    } else {
        result$endUTC <- NA
    }
    battNode <- xml_find_all(xml, '//EX_BATT')
    if(length(battNode) > 0) {
        result$batt <- as.numeric(gsub(' ', '', as.character(xml_contents(battNode)))) * .001
    } else {
        result$batt <- NA
    }
    tempNode <- xml_find_all(xml, '//TEMPERATURE')
    if(length(tempNode) > 0) {
        result$temp <- as.numeric(gsub(' ', '', as.character(xml_contents(tempNode)))) * .01
    } else {
        result$temp <- NA
    }
    gapNode <- xml_find_all(xml, '//@CumulativeSamplingGap')
    if(length(gapNode) > 0) {
        result$gap <- as.numeric(gsub('\\s*us$', '', as.character(xml_contents(gapNode))))
    } else {
        result$gap <- NA
    }
    periodNode <- xml_find_all(xml, '//@SamplingTimePeriod')
    if(length(periodNode) > 0) {
        result$period <- as.numeric(gsub('\\s*us$', '', as.character(xml_contents(periodNode))))
    } else {
        result$period <- NA
    }
    sampleNode <- xml_find_all(xml, '//@SampleCount')
    if(length(sampleNode) > 0) {
        result$sampleCount <- as.numeric(as.character(xml_contents(sampleNode)))
    } else {
        result$sampleCount <- NA
    }
    result
}

stToPosix <- function(x) {
    parse_date_time(x, orders=c('%Y-%m-%dT%H:%M:%S', '%m/%d/%Y %I:%M:%S %p'), tz='UTC', exact=TRUE)
}