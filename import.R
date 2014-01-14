## Import initial and second experiment

importFB <- function(path) {
  df <- read.csv(
    path,
    header = FALSE,
    as.is = TRUE
  )
  return(preClean(df))
}

first <- read.csv(
  file.path(getwd(), 'data', 'fb-initial', 'results.csv'),
  header = TRUE,
  as.is = TRUE
)
second <- read.csv(
  file.path(getwd(), 'data', 'fb-second', 'results.csv'),
  header = TRUE,
  as.is = TRUE
)

toLong <- function(df) {
  result <- list()
  coln <- names(df)
  observables <- c("date", "female", "male", "unknown")
  
  snapshot.stages <- str_sub(str_extract(coln, '^.*fb\\.'), end = -2)
  snapshot.stages <- snapshot.stages[!is.na(snapshot.stages)]
  invariants <- coln[grep('user\\.', coln)]
  for (i in observables) {
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
head(toLong(first))
