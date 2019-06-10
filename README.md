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

* `getFittedMap` - Get nicely fitted maps to plot your data. Either downloads
from Google Maps or uses coastline shape files in offline mode. **NOTE** this 
now requires configuring a Google Maps API key!!!

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

### Versions

**1.2.1** - minor change in error handling for `peakTrough`
- `writeClickWave` can handle vectors for CPS and frequency

**1.2.0** - `writeAMWave` function added to create synthetic amplitude modulated waves

**1.1.0** - `addPgEvent` function added to add new events to an existing Pamguard database by
providing a vector of UIDs

**1.0.4** - fixed typo that broke `wignerTransform`

**1.0.3** - minor change to output of `wignerTransform`, resizes back to length of
original signal

**1.0.2** - bug fixed in `decimateWavFiles` when trying to write a folder of files

**1.0.1** - `wignerTransform` added

### TODO

Rework `decimateWavFiles` to work with multi-channel data.

Finish fixing `getFittedMap` Can we check
for 403 forbidden error?? More color or other options for offline? Apparently
`rnaturalearth` is a package. More flexibility in column names. I can probably redo
this using `sf` stuff a lot nicer, I had to do some janky business to get coordinates
to look like not-a-shit-show

