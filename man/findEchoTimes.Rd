% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findEchoTimes.R
\name{findEchoTimes}
\alias{findEchoTimes}
\title{Find Estimated Echo Times}
\usage{
findEchoTimes(
  wav,
  sr = NULL,
  filter = NULL,
  clipLen = 0.03,
  peakMin = 0.01,
  minTime = 0.001,
  maxTime = NULL,
  channel = NULL,
  n = 3,
  plot = TRUE,
  plotText = NULL
)
}
\arguments{
\item{wav}{waveform to find echoes in. Can be a numeric vector, 
\link[tuneR]{Wave}, or \link[tuneR]{WaveMC} class object}

\item{sr}{sample rate of the waveform, if \code{wav} is a \code{Wave}
or \code{WaveMC} object it will use the \code{samp.rate} slot}

\item{filter}{filter to apply to \code{wav}, a vector of two numbers
specifying the lower and upper bounds of the filter in Hz. A first
value of \code{0} means no highpass filter is applied, a second value
greater than \code{sr/2} means no lowpass filter is applied.}

\item{clipLen}{length of clip (seconds) to analyse for echoes, 
measured from start of \code{wav}}

\item{peakMin}{minimum magnitude of autocorrelation value to be 
considered a possible peak}

\item{minTime}{minimum allowed echo time (seconds), this should be large
enough to avoid correlating the original pulse with itself}

\item{maxTime}{maximum allowed echo time (seconds)}

\item{channel}{if \code{wav} has multiple channels, channel to use}

\item{n}{the number of potential echoes to return, times with the 
\code{n} highest autocorrelation magnitude will be returned}

\item{plot}{logical flag to create plot, will create a two-panel plot
of the waveform (top) and the autocorrelation (bottom). Points of 
the selected candidate echo times are also drawn}

\item{plotText}{optional text to plot on the upper waveform plot}
}
\value{
a list with elements \code{mag}, \code{time} and \code{wav}
}
\description{
Finds the estimated times of echoes in a waveform clip. This 
  function was developed to estimate the time of a surface reflected echo
  of echolocation clicks of deep diving marine mammals. The times of echoes
  are estimated by finding peaks in the autocorrelation of a signal.
}
\author{
Taiki Sakai \email{taiki.sakai@noaa.gov}
}
