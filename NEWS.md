# PAMmisc 1.12.7

* Small bug fix for `fastReadWave` on incomplete wav files to no longer
crash just return what it got

# PAMmisc 1.12.6

* Small bug fix for `addPgEvent` not creating millisecond characters
properly (character(0) text bug)

# PAMmisc 1.12.5

* `pwelch` accidentally using too many windows, now correctly stops at the last
window that extends past the length of input

# PAMmisc 1.12.4

* `fastReadWave` with `header=TRUE` was not properly closing file connections

* `matchEnvData` and `ncToData` were not returning failed files properly
when `raw=TRUE`

* Adding `window` option to `pwelch`

* Allowing `downloadEnv`, `matchEnvData`, `erddapToEdinfo`, and `ncToData` to
work with certain stationary Tabledap datasets (previously only griddap)

# PAMmisc 1.12.3

* Adding `nc='HYCOM'` option to make it easier to specify that you want
to (try) to get to HYCOM data from `matchEnvData`

# PAMmisc 1.12.2

* `matchEnvData` and `ncToData` were really slow in some cases, up
to 30x faster now in some cases

* Merging in `fastReadWave` C-based wav file reading function. Must faster
than current alternatives, based on the not-always-working `audio::load.wave`
but with improvements

* Added `pwelch` apparently

# PAMmisc 1.12.1

* Fixing `wignerTransform` bug introduced in 1.12.0

# PAMmisc 1.12.0

* adding `findEchoTimes` function and tests for DCLDE workshop

# PAMmisc 1.11.11

* Make a wish! Dropping `hoardr` dependency because defaults go against CRAN
wishes on MacOS

# PAMmisc 1.11.10

* Bug fix where `erddapToEdInfo` was crashing instead of working through
the server list

# PAMmisc 1.11.9

* Minor adjustments to various NC functions for HYCOM project

# PAMmisc 1.11.8

* Added required column error messages for `matchEnvData` functinos

# PAMmisc 1.11.7

* `alpha` wasn't working for `plotPresGrid` single color plots

# PAMmisc 1.11.6

* Safer fail case in `peakTrough`

# PAMmisc 1.11.5

* Allowing for smaller HYCOM download with modification to `formatURL`

* Adding another NetCDF time format check

# PAMmisc 1.11.4

* Dealing with NA coordinate values for enviro functions

# PAMmisc 1.11.3

* Fixing `readSpecAnno` if no rows in anno table

* `plotPresBar` and `plotPresGrid` added `plotTz` option to control timezone
of output

# PAMmisc 1.11.2

* Added `by` argument to `plotPresGrid` that will either overlay colors
with an alpha value or split graphs with `facet_wrap`

* Removed `data.table` dependency and changed `viridisLite` to `scales`

# PAMmisc 1.11.1

* Changed `plotPresGrid` and `plotPresBar` arguments to be more intuitive.
Both now just use `bin` for the binning, and `plotPresBar` can be specified
as `hours/day` instead of having to remember which was which. 

* Updates to `ncToData` to allow more flexible time specification. Should
work with a larger variety of netcdf files with more arbitrary time specs

# PAMmisc 1.11.0

* Added `getFigshareInfo` function

* Dropped `stringr` dependency (was only using `str_trim` before)

# PAMmisc 1.10.6

* `matchEnvData` can now set timeout length with `timeout` arg

* `createSSP` has new args `progress` and `...` to pass to `matchEnvData`

# PAMmisc 1.10.5

* Bug fix so that environmental data functions work properly with NA values 
(they are just not matched, previously would error)

* Change to `varSelect`, `ncToData`, and `matchEnvData` to be able to use
variable names in `var` argument to select without interaction

# PAMmisc 1.10.4

* Changing `downloadEnv` timeout time to 120s. Not sure why it was set to 1200
previously, either type or misunderstanding of units?

# PAMmisc 1.10.3

* Updating `addPgEvent` to work with events that have 0 detections, and also
allow for manual setting of start/end times (useful for creating validation
events)

# PAMmisc 1.10.2

* Adding some type checks for `plotPresBar` and `plotPresGrid` functions

# PAMmisc 1.10.1

* Adding `trainSplitPermute` function

# PAMmisc 1.10.0

* Adding `plotPresBar` and `plotPresGrid` plotting functions

# PAMmisc 1.9.6

* Updating `soundtrapQAQC` to have option for internal/external battery voltage

# PAMmisc 1.9.5

* Fixing as.character(POSIX) errors for r-devel update

# PAMmisc 1.9.4

* Bug fix for bad XML files in `soundtrapQAQC`

# PAMmisc 1.9.3

* Pull request adding a new time format for `ncToData`

* Adding logic to drop variables with no dimensions in `ncToData` and also
deal with variables that have non-standard dimensions (default behavior is to read
all - if you need to do something in particular with it use the `raw` option)

# PAMmisc 1.9.2

* Fixed a bug where error message in `matchEnvData` could cause R to crash (oh the irony)

* `ncToData` was not behaving well if columns were not `Latitude` and `Longitude`

* `ncToData` also bug if some vars had depth and others did not

* LEV is lev is Depth

# PAMmisc 1.9.1

* Adding `soundtrapQAQC` function

# PAMmisc 1.9.0

* Added `addPgAnno` function to add spectrogram annotations to Pamguard databases

* `addPgEvent` now has a type argument to specify adding Click Detector events vs
Detection Grouper events

# PAMmisc 1.8.3

* `readSpecAnno` adjusted to allow for different table names

* Added option `unique` to `squishList` to try and simplify results to only unique values

# PAMmisc 1.8.2

* Added `readSpecAnno` function to read Spectrogram_Annotation tables
from PAMGuard databases

# PAMmisc 1.8.1

* Updated `decimateWavFiles` to work with wav files with bit rates different
than 16 (bugs in `seewave` functions do not pass along bit rate properly)

* Bug fix in `matchEnvData` for datasets that had no `UTC` axis

* Changed defaults in `matchEnvData` and `ncToData` to only calculate `mean`, not
`median` and `sd`

* `matchEnvData` and `ncToData` have a `depth` parameter you can use to specify
matching depth values to use. This makes it possibly to specify a range of depths
to summarise over for a single coordinate (previously only single depth or all)

# PAMmisc 1.8.0

* Reworking of enviro functions to make HYCOM datasets easier to use. Will now
automatically figure out which ones you need based on dates and partition your
data for you

* Enviro functions reworked for temporary download to be much faster - no longer
does a separate download for every row, will check to estimate download size first 
and break in to ~20MB chunks at once

* Added `createSSP` function to create soundspeed profiles

# PAMmisc 1.7.1

* Minor changes to smooth out usability of `addPgGps`

# PAMmisc 1.7.0

* Added `raytrace` function based on MATLAB code by Val Schmidt

* Updating `matchEnvData` and associated functions to work with provided
Depth values instead of only being able to return all Depth values if a 
Z dimension is present

* `squishList` works with matrices

* Added `readGPXTrack` to remove dependency on `plotKML::readGPX`

# PAMmisc 1.6.12

* `wignerTransform` plotting now uses option `useRaster = TRUE`

# PAMmisc 1.6.11

* `addPgEvent` fixed issue with repeated Lookup codes and added colours

# PAMmisc 1.6.10

* `addPgEvent` fixed potential issue with reading empty binaries

# PAMmisc 1.6.9

* `squishList` no longer breaks on `data.table` objects

# PAMmisc 1.6.8

* `peakTrough` ignores first and last frequencies for possible peaks

* `addPgEvent` and `addPgGps` added `na.rm=TRUE` to Id checking logic to avoid issues with
NULL id values in current database

* Improvements for `addPgEvent` check for duplicate data

# PAMmisc 1.6.7

* `wignerTransform` fixed output size to actually be n x t

* `updateUID` progress bar minor bug fix

# PAMmisc 1.6.6

* `wignerTransform` works with Wave class input

* `wignerTransform` scaled output by length

* `updateUID` bug checking for SR, should crash less with missing data

* tests for UID adding plus test data for bad UIDs added

# PAMmisc 1.6.5

* Bug fixes for `updateUID`. Will check for "ClickNo" column first if that is more accurate,
and will not update a UID if it is matching based on time and there is more than one match

* Changed database testing to work on copied file in tmpdir for CRAN checks

# PAMmisc 1.6.4

* Added `updateUID` function to try and realign UID mismatches in Pamguard databases
between event detections and their corresponding binary files

# PAMmisc 1.6.3

* Changed `addPgGps` to use `parse_date_time` for date conversions to allow
for truncated date formats to be properly parsed because thanks Excel for
rounding those dates didn't need to know there were 0 seconds anyway

* `matchEnvData` not propagating `progress` argument properly

# PAMmisc 1.6.2

* Checks in database adding function to make sure file exists

* `writeAMWave` example resets par() settings to original

# PAMmisc 1.6.1

* Added a `NEWS.md` file to track changes to the package.

* Fixing `addPgGps` for spot CSVs

* Tons of documentation in prep of CRAN

* Added files in inst/extdata for testing

* Adding lots of unit testing

# PAMmisc 1.6.0 

* Whoa, environmental data functions might work fine for crossing the dateline now.

* `getFittedMap` also removed because `ggmap` package has been orphaned.

# PAMmisc 1.5.0 

*Added `addPgGps` to add GPS data to a Pamguard database

# PAMmisc 1.4.1 

* Internal changes to make `matchEnvData` a generic method so can write methods
for non-dataframe sources easier

# PAMmisc 1.4.0 

* Added functions for downloading environmental data and matching it to your
data. Replaced older verison of `formatURL` from v 1.3.0

* New exported functions are `edinfoToURL`, `downloadEnv`, `erddapToEdinfo`, `varSelect`, `getEdinfo`,
`ncToData`, `matchEnvData`, `browseEdinfo`. Updated tutorial to follow later.

# PAMmisc 1.3.1 

* bug fix where `straightPath` was not properly averaging angles. Changed to
polar coordinate style averaging, will now handle angles near the 0-360 border properly

# PAMmisc 1.3.0 

* added `formatURL` functions for making ERDDAP downloading URLs automatically

# PAMmisc 1.2.1 

* minor change in error handling for `peakTrough`

* `writeClickWave` can handle vectors for CPS and frequency

# PAMmisc 1.2.0 

* `writeAMWave` function added to create synthetic amplitude modulated waves

# PAMmisc 1.1.0 

* `addPgEvent` function added to add new events to an existing Pamguard database by
providing a vector of UIDs

# PAMmisc 1.0.4 

* fixed typo that broke `wignerTransform`

# PAMmisc 1.0.3 

* minor change to output of `wignerTransform`, resizes back to length of
original signal

# PAMmisc 1.0.2 

* bug fixed in `decimateWavFiles` when trying to write a folder of files

# PAMmisc 1.0.1 

* `wignerTransform` added
