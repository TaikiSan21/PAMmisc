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
fixStTz(dir, prep)
