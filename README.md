<!-- badges: start -->
[![R-CMD-check](https://github.com/TaikiSan21/PAMmisc/workflows/R-CMD-check/badge.svg)](https://github.com/TaikiSan21/PAMmisc/actions)
<!-- badges: end -->

# PAMmisc

PAMmisc is a collection of random utility functions that might be useful
for anyone working in passive acoustics. If you have any useful tools you'd like
to add, please contact Taiki Sakai: <taiki.sakai@noaa.gov>.

### Installation

Install the latest version from GitHub:

```r
# make sure you have Rtools installed
if(!require('devtools')) install.packages('devtools')
# install from GitHub
devtools::install_github('TaikiSan21/PAMmisc')

```

### Functions

* `writeClickWave` - Create synthetic delphinid click wav files

* `writeAMWave` - Create synthetic amplitude modulated wav files

* `decimateWavFiles` - Decimate .wav files to a new sample rate and write them
to a new folder. Works on either a single .wav file or an entire folder. Currently
only works with single channel data, if multi-channel wav files are fed in it will
only process and output the first channel.

* `squishList` - A utility for compressing a list that has elements with the same
name. See examples.

* `peakTrough` - Find multiple peaks and the troughs / valleys between those peaks
in the spectrum of a click. Also has a plotting feature so that you can easily
see the values being chosen as peaks / troughs in the spectrum and adjust the 
search parameters. See help file for algorithm details.

* `wignerTransform` - Compute the Wigner-Ville transform of a signal, and optionally
plot the result. This will zero-pad the input signal to the next power of two,
and the result is an NxN matrix where N is the zero-padded length. Returns a list
with the NxN matrix, as well as the values for the time and frequency axes.

* `addPgEvent` - Add a new Event to a Pamguard database. Provide the database to
add the event to, the UIDs of the individual detections for this event, the binary
files that these detections are found in, and an "eventType" name for the event.
Existing events cannot be added to, so all detections for an event must be added
at the same time. This function should only add to an existing database, it will not
modify or remove existing entries in the database. 

* `addPgGps` - Add a gpsData table to a Pamguard database. Gps source can either be
a dataframe, SPOT csv file, or SPOT gpx file, or csv file with UTC, Longitude, and
Latitude

* Environmental data fetching functions to be described later

### TODO

Rework `decimateWavFiles` to work with multi-channel data.
