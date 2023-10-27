source('soundtrapRenamer.R')
#------------------------------#
# Changes in new version (12/1/2022):
# 1) Now changes all different file types in one batch
# 2) Changes file names twice - first to a temporary name,
#    then to the new name. This is to avoid problems where
#    there is overlap between your new & old file names - ex
#    ex if you have files exactly 7 hours apart, but want to
#    add 7 hours to the time.
# 3) If offset=NULL it will look for log files first. If no log
#    files are found it will prompt you to enter an offset value.
#-------------------------------#

# change this to whatever file endings you want to change
suffixes <- c('wav', 'sud', 'log.xml', 'accel.csv')
# Change this to whatever folder these files are in
dir <- '../Data/Renamer/orig/'

prep <- prepTzFix(dir, offset=NULL, suffix=suffixes)
# this will create a STRenameLog.csv that is a copy of this "prep" dataframe
# that also has an additional "renamed" TRUE/FALSE column to mark which files
# it renamed (will be all FALSE if data already in UTC)
fixStTz(dir, prep)

# if for some reason you decide that the correction it applied was in error,
# you can load that and run again with reverse=TRUE to revert to the original
# state. This will create a STReverseLog.csv log
# revPrep <- read.csv(file.path(dir, 'STRenameLog.csv'), stringsAsFactors = FALSE)
# fixStTz(dir, revPrep, reverse=TRUE)
