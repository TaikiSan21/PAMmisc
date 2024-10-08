% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/soundtrapQAQC.R
\name{soundtrapQAQC}
\alias{soundtrapQAQC}
\alias{processSoundtrapLogs}
\title{Perform QA/QC on Soundtrap Files}
\usage{
soundtrapQAQC(
  dir,
  outDir = NULL,
  xlim = NULL,
  label = NULL,
  voltSelect = c("internal", "external"),
  plot = TRUE
)

processSoundtrapLogs(dir, voltSelect = c("internal", "external"))
}
\arguments{
\item{dir}{directory containing Soundtrap XML logs, wav files, and SUD files.
Can either be a single directory containing folders with all files (will
search recursively), or a vector of three directories containing the SUD files,
wav files, and XML files (in that order - alphabetical S-W-X)}

\item{outDir}{if provided, output plots and data will be written to this folder}

\item{xlim}{date limit for plots}

\item{label}{label to be used for plots and names of exported files}

\item{voltSelect}{one of "internal" or "external" to select which battery voltage to use}

\item{plot}{logical flag to create output plots}
}
\value{
list of dataframes with summary data for \code{$xmlInfo}, \code{$sudInfo},
  and \code{$wavInfo}
}
\description{
Gathers data from Soundtrap XML log files to perform QA/QC on
  a set of recordings.
}
\examples{

\dontrun{
# not run
stDir <- './Data/SoundtrapFiles/'
stData <- soundtrapQAQC(stDir, plot=TRUE)
# save data
stData <- soundtrapQAQC(stDir, outDir='./Data/SoundtrapFiles/QAQC', plot=TRUE)
# or provide separate folders of data
stDirs <- c('./Data/SoundtrapFiles/SUDFiles',
            './Data/SoundtrapFiles/WavFiles',
            './Data/SoundtrapFiles/XMLFiles')
stData <- soundtrapQAQC(stDirs, plot=TRUE)
}

}
\author{
Taiki Sakai \email{taiki.sakai@noaa.gov}
}
