% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fastReadWave.R
\name{fastReadWave}
\alias{fastReadWave}
\title{Load Wave File}
\usage{
fastReadWave(where, from = 0, to = NA_real_, header = FALSE, toWaveMC = FALSE)
}
\arguments{
\item{where}{file to load data from}

\item{from}{starting point to load data from (seconds)}

\item{to}{end point to read data to (seconds), \code{NA} to read til end}

\item{header}{logical flag to read only header information}

\item{toWaveMC}{logical flag to return a \link[tuneR]{WaveMC} object}
}
\value{
returns an object of the class \code{audioSample} as loaded from
  the WAVE file, or if \code{header=TRUE} a named list with the sample.rate,
  num channels, bit rate, and sample length. \code{audioSample} objects
  store the wav data as a matrix wth one row for every channel and
  attributes "rate" and "bits"
}
\description{
loads data from a WAVE file
}
\details{
WAVE is a RIFF (Resource Interchange File Format) widely used for
  storage of uncompressed audio data. It is often identified by the
  extension .WAV on DOS-legacy systems (such as Windows). Although
  WAVE files may contain compressed data, the above functions only
  support plain, uncompressed PCM data.

  This function was originally written Simon Urbanek, all credit for the bulk
  of the C programming goes to him. Adapted by Taiki Sakai to add additional
  features and fix some bugs. See additional license comments in file.c
}
\author{
Taiki Sakai \email{taiki.sakai@noaa.gov}
}
