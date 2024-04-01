# swapping 53.X datasets for GLBu
library(PAMmisc)
hy191 <- hycomToEdinfo('GLBu0.08/expt_19.1', chooseVars = F)
hy909 <- hycomToEdinfo('GLBu0.08/expt_90.9', chooseVars = F)
hy91 <- hycomToEdinfo('GLBu0.08/expt_91.0', chooseVars = F)
hy911 <- hycomToEdinfo('GLBu0.08/expt_91.1', chooseVars = F)

hyOrig <- PAMmisc::hycomList
hyOrig$list

hyNew <- hyOrig
hyNew$list <- hyNew$list[-(2:22)]
hyNew$list[['GLBu0.08/expt_19.1']] <- hy191
hyNew$list[['GLBu0.08/expt_90.9']] <- hy909
hyNew$list[['GLBu0.08/expt_91.0']] <- hy91
hyNew$list[['GLBu0.08/expt_91.1']] <- hy911
View(hyNew)
hyNew$list <- hyNew$list[c(1, 9:12, 2:8)]
View(hyNew)




segFile <- '~/../Downloads/NWTT_segs_1991_2018_forHYCOM_TEST2.csv'
segData <- read.csv(segFile, stringsAsFactors = FALSE)
# Folder to store the downloaded segment NetCDF files
segFolder <- 'ProjectName_Segment'
# Like the full grid, this will work throuugh downloading all the data you need
# the "tz" argument is used if the times in your dataset are not in UTC
segLog <- downloadHYCOM(data=segData, folder=segFolder, mode='segment', tz='America/Los_Angeles', hyList=hyNew)
# Once the downloads are all finished, use this to match the downloaded data to 
# your segment dataframe. Then you can save this as a CSV and you're all set!
segData <- addEnvParams(segData, folder=segFolder, tz='America/Los_Angeles')