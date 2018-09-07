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

* getFittedMap - Get nicely fitted maps to plot your data. Either downloads
from Google Maps or uses coastline shape files in offline mode.

* writeClickWave - Create synthetic delphinid click wav files

* decimateWavFiles - Decimate .wav files to a new sample rate and write them
to a new folder. Works on either a single .wav file or an entire folder.

* squishList - A utility for compressing a list that has elements with the same
name. See examples.

* peakTrough - Find multiple peaks and the troughs / valleys between those peaks
in the spectrum of a click. Also has a plotting feature so that you can easily
see the values being chosen as peaks / troughs in the spectrum and adjust the 
search parameters. See help file for algorithm details.
