source('soundtrapRenamer.R')
# change this to whatever file endings you want to change
suffixes <- c('wav', 'sud', 'log.xml', 'accel.csv')
# Change this to whatever folder these files are in
dir <- '../Data/Renamer/'

# this will run through process for all file types and create separate log files
# I think you said +7 hours, so offset=7 should work
for(s in suffixes) {
    prep <- prepTzFix(dir, offset=7, suffix=s)
    fixStTz(dir, prep, suffix=s)
}

# TODO
# adjust the "dont break shit" section to check
# if there are any overlap first, then check iff all(not average)
# are same direction.
# if they aren't, then figure out which aren't and mark something as
# NA so the rename doesnt run, we can either have these run the code
# again (so name conflict shouldnt happen maybe?) or use temporary
# names and rename twice