## add dependencies 

library(ggplot2)
library(plyr)
library(stringr)
library(reshape2)

## Import initial and second experiment

fbImport <- function(fpath) {
  df <- read.csv(
    file = fpath,
    header = TRUE,
    as.is = TRUE
  )
  coln <- names(df)
  # remove survey data for now
  df <- df[, grep('fb\\.|user\\.', coln)]
  # 'Incomplete isn't a different treatment, just a marker of missingness
  test.grp <- df[, 'user.group']
  # Add column for missingness if we want it
  df[, 'user.missing'] <- ifelse(
    grepl('incomplete', test.grp, ignore.case = TRUE),
    TRUE,
    FALSE
  )
  # Rename to make plotting (and everything else) simpler
  df[, 'user.group'] <- ifelse(
    grepl('test', test.grp, ignore.case = TRUE),
    'Test',
    'Control'
  )

  return(df)
}
# Your path here may be different. If so, either change this or
# move the data into the folder containing the import script
first <- fbImport(file.path(getwd(), 'data', 'fb-initial', 'results.csv'))
second <- fbImport(file.path(getwd(), 'data', 'fb-second', 'results_update.csv'))


toLong <- function(df, observables = c("female", "male", "unknown")) {
  result <- list()
  coln <- names(df)
  
  # extract the stage of the experiment, embedded in the names as
  # somethingfb.date (or any one of the observables above)
  snapshot.stages <- unique(str_sub(str_extract(coln, '^.*fb\\.'), end = -2))
  snapshot.stages <- snapshot.stages[!is.na(snapshot.stages)]
  # we want the actual column names here
  invariants <- coln[grep('user\\.', coln)]
  
  # poor (wo)man's reshape. Stack rows as they vary by stage and
  # data collected
  for (i in observables) {
    # we drop date from observables because it isn't super useful in
    # 'long' format and stack won't work with date values (boo!)
    matches <- match(paste0(snapshot.stages, '.', i), coln)
    result[[i]] <- stack(df, matches)[, "values"]
  }
  
  # for everything that is invariant to collection time, duplicate
  for (i in invariants) {
    result[[i]] <- rep(df[, i], length(snapshot.stages))
  }
  
  # Order this factor so we can graph it properly
  result[["Stage"]] <- factor(
    x = rep(snapshot.stages, each = nrow(df)),
    levels = unique(snapshot.stages),
    ordered = TRUE
  )
  return(as.data.frame(result))
}  

first.long <- toLong(first)
second.long <- toLong(second)

### Import population data

jr.interact <- read.csv(file.path(getwd(), 'data', 'population', 'journo_data.csv'), na.strings = c("NA", "#N/A", "<NA>", ""), as.is = TRUE)

jr.factors <- c('screen_name', 'gender', 'orgs')
jr.obs <- c("follow.female", "follow.male", "follow.unknown", 
            "follow.total",
            "interactions.female", "interactions.male", "interactions.unknown", 
            "interactions.total")

prune <- function(df) {
  collect <- lapply(df, function(x) {
    return(which(!grepl('^[0-9]*$', x))) 
  })
  return(unique(unlist(collect)))
}

jr.interact <- jr.interact[-prune(jr.interact[, 'follow.female']), ]
jr.interact[, 'gender'] <- str_trim(jr.interact[, 'gender'])
jr.interact[, jr.obs] <- lapply(jr.interact[, jr.obs], as.numeric)

dupMulti <- function(df = jr.interact, sep = ';', col = 'orgs') {
  dups <- df[str_detect(df[, col], sep), ]
  nonDups <- df[!str_detect(df[, col], sep), ]
  splitVal <- str_split(dups[, col], sep)
  r.df <- dups
  dups[, col] <- sapply(splitVal, `[[`, 1)
  r.df[, col] <- sapply(splitVal, `[[`, 2)
  return(rbind(nonDups, dups, r.df))
}

jr.interact <- dupMulti()
