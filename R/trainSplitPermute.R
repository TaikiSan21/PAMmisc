#' @title trainSplitPermute
#' 
#' @description Find a desired train/val/test split of a dataset through random
#'   permutation. Uses a variable in your dataset to randomly split by (for example,
#'   could be the location of different sites, or different months of data), then
#'   tries to find the split that most closesly matches your desired distribution
#'   of data for a set of labels. It can often be difficult to find a good split
#'   if the distribution of your labels is not consistent across sites, so this
#'   function tries a bunch of random splits then uses a score to find the best
#'   one.
#' 
#' @param x a dataframe of data you want to find splits for
#' @param probs a vector of 3 values that sum to one defining what percentage
#'   of data should be in your training, validation, and test sets (respectively)
#' @param n number of random samples to try. If your labels are fairly evenly distributed
#'   this can be smaller, but needs to be larger for more uneven distributions
#' @param splitBy name of column containing the variable you want to split by
#' @param label name of the column containing your dataset labels
#' @param countCol the names of any additional columns in your dataset defining
#'   the quantities you want to count (see example for why this is useful)
#' @param minCount minimum count for each split category, usually safe to leave 
#'   this as the default of 1 for all splits
#' @param top the number of results to return. Usually you want to use just the
#'   best scoring result, but this can occasionally result in splits that are 
#'   distributed in an undesirable way by random chance (eg maybe all sites in 
#'   your validation data are unintentionally clustered together)
#' @param seed random seed to set for reproducibility 
#'
#' @return  a list of the \code{top} results. Each
#'   individual result contains \code{$splitMap} containing the random split marked 
#'   as integer 1, 2, 3 corresponding to train, val, test and \code{$splitVec} a vector 
#'   marking each row of \code{x} with its category. These two results are named by
#'   the levels of \code{splitBy}. \code{$distribution} a table of the distribution of \code{label}
#'   in the split, and \code{$score} the split score (lower is closer to desired 
#'   \code{probs})
#' 
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#' 
#' @examples 
#' 
#' # making some dummy data
#' df <- data.frame(
#'     species = sample(letters[1:5], prob=c(.4, .2, .1, .1, .2), 1e3, replace=TRUE),
#'     site = sample(LETTERS[1:12], 1e3, replace=TRUE),
#'     event = 1:1e3
#' )
#' # try a split with n=3
#' split <- trainSplitPermute(df, probs=c(.7, .15, .15), n=3, label='species', splitBy='site')
#' # assign the best split as the split cateogry
#' df$cat <- split[[1]]$splitVec
#' # distribution is not close to our desired .7, .15, .15 split because n is too low
#' round(table(df$species, df$cat) /
#'     matrix(rep(table(df$species), 3), nrow=5), 2)
#'
#' # rerun with higher n to get closer to desired distribution
#' split <- trainSplitPermute(df, probs=c(.7, .15, .15), n=1e3, label='species', splitBy='site')
#' df$cat <- split[[1]]$splitVec
#' round(table(df$species, df$cat) /
#'     matrix(rep(table(df$species), 3), nrow=5), 2)
#'     
#' # adding a new site that has significantly more detections than others
#' addSite <- data.frame(
#'     species = sample(letters[1:5], 500, replace=TRUE),
#'     site = rep(LETTERS[13], 500),
#'     event = 1001:1500)
#' df$cat <- NULL
#' df <- rbind(df, addSite)
#' 
#' # now just splitting by site does not result in a balanced split for our number of species
#' # it splits the sites to approx .7, .15, .15 but this does not result in balanced species
#' split <- trainSplitPermute(df, probs=c(.7, .15, .15), n=1e3, label='species', splitBy='site')
#' df$cat <- split[[1]]$splitVec
#' round(table(df$species, df$cat) /
#'     matrix(rep(table(df$species), 3), nrow=5), 2)
#'
#' # adding 'event' as a countCol fixes this
#' split <- trainSplitPermute(df, probs=c(.7, .15, .15), n=1e3, label='species', 
#'     splitBy='site', countCol='event')
#' df$cat <- split[[1]]$splitVec
#' round(table(df$species, df$cat) /
#'     matrix(rep(table(df$species), 3), nrow=5), 2)
#'
#' 
#' @importFrom dplyr select all_of distinct
#' 
#' @export
#'   
trainSplitPermute <- function(x, probs=c(.7, .15, .15), n=1e3, 
                              splitBy='drift', label='species',
                              countCol=NULL, minCount=c(1,1,1), 
                              top=3, seed=112188) {
    set.seed(seed)
    selCols <- c(countCol, label, splitBy)
    # make subset of x to do split and check against original dets
    x <- select(x, all_of(selCols))
    origSplit <- x[[splitBy]]
    x <- distinct(x)
    x[[label]] <- factor(x[[label]], levels=unique(x[[label]]))
    splitLevels <- unique(x[[splitBy]])
    result <- vector('list', length=n)
    scores <- vector('numeric', length=n)
    pb <- txtProgressBar(min=0, max=n, style=3)
    for(i in 1:n) {
        samps <- sample(1:3, size=length(splitLevels), prob=probs, replace=TRUE)
        x$splitCat <- NA
        x$splitCat[x[[splitBy]] %in% splitLevels[samps == 1]] <- 1
        x$splitCat[x[[splitBy]] %in% splitLevels[samps == 2]] <- 2
        x$splitCat[x[[splitBy]] %in% splitLevels[samps == 3]] <- 3
        x$splitCat <- factor(x$splitCat, levels=1:3)
        result[[i]]$splitMap <- samps
        result[[i]]$distribution <- table(x[[label]], x$splitCat)
        result[[i]]$score <- splitScore(result[[i]]$distribution, probs, minCount)
        scores[i] <- result[[i]]$score
        setTxtProgressBar(pb, i)
    }
    bestIx <- sort(scores, index.return=TRUE)$ix[1:top]
    result <- result[bestIx]
    for(i in seq_along(result)) {
        names(result[[i]]$splitMap) <- splitLevels
        result[[i]]$splitVec <- result[[i]]$splitMap[origSplit]
    }
    # list(levels=splitLevels, splits=result)
    result
}
# check splits against minimum values
isGoodSplit <- function(x, minCount=c(1, 1, 1)) {
    min(x[,1]) > minCount[1] &
        min(x[,2]) > minCount[2] &
        min(x[, 3]) > minCount[3]
}
# assign score to each split. Used 'prob' metric - compares distribution
# of events to desired distribution and squares difference.
splitScore <- function(x, probs=c(.7, .15, .15), minCount=c(1, 1, 1)) {
    # dont enforce a min for prob 0 categories - lets us do t/v only
    isZero <- probs == 0
    minCount[isZero] <- 0
    if(!isGoodSplit(x, minCount)) {
        return(Inf)
    }
    out <- apply(x, 1, FUN=function(s) {
        sum((s / sum(s) - probs)^2)
    })
    sum(out)
}

# writeTrTeCsv <- function(x, dir='.', name='PASCAL_BW',
#                          cols=c('species', 'drift', 'station', 'file', 'wigMin','wigMax',
#                                 'dBPP', 'snr', 'Latitude','Longitude', 'ici', 'survey', 'sm3m')) {
#     x <- select(x, all_of(c('trainCat', cols)))
#     x$file <- gsub('../../CV4Ecology/', '', x$file)
#     lapply(split(x, x$trainCat), function(s) {
#         suff <- switch(as.character(s$trainCat[1]),
#                        '1' = 'Train',
#                        '2' = 'Val',
#                        '3' = 'Test'
#         )
#         fname <- file.path(dir, paste0(name, '_', suff, '.csv'))
#         write.csv(s[cols], file=fname, row.names=FALSE)
#         fname
#     })
# }