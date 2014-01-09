control <- read.csv(
  file.path("data", "fb-initial", "control.csv")
)
  
treatment <- read.csv(
  file.path("data", "fb-initial", "treatment.csv")
)
  

preClean <- function(df) {
  # Removes columns which are all NA
  # sometimes a side effect of importing from xls
  x <- df[, colSums(is.na(df)) < nrow(df)]
  # converts all columns to numeric 
  return(lapply(x, as.numeric))
}

# Combine into one data frame
subjects <- rbind(
  data.frame(preClean(control), Type = "Control"),
  data.frame(preClean(treatment), Type = "Treatment")
)

# Convert the type to a factor (this is mostly a convenience for us)
subjects[, "Type"] <- as.factor(subjects[, "Type"])

raw.names <- c(
  # reorder the df a bit to group things together 
  "friends_initial", "friends_login", "friends_last",
  "female_initial", "female_login", "female_last",
  "male_initial", "male_login", "male_last",
  "unknown_initial", "unknown_login", "unknown_last",
  "time_to_login", "time_to_last",
  "Type"
)

subjects <- subjects[, raw.names]

# standardize names 
names(subjects) <- c(
  "friends_first", "friends_login", "friends_last",
  "female_first", "female_login", "female_last",
  "male_first", "male_login", "male_last",
  "unknown_first", "unknown_login", "unknown_last",
  "time_to_login", "time_to_last",
  "Type"
)


# Converts the data to 'long' format, where it may be easier to graph
toLong <- function(df) {
  result <- list()
  observables <- c("friends", "female", "male", "unknown")
  times <- c("First", "Login", "Last")
  
  invariants <- c("time_to_login", "time_to_last", "Type")
  # For each observable, collect only the values
  for (i in observables) {
    # cheap and easy matching
    matches <- grepl(paste0('^', i), names(df))
    result[[i]] <- stack(df, matches)[, "values"]
  }
  # for everything that is invariant to collection time, duplicate
  for (i in invariants) {
    result[[i]] <- rep(df[, i], length(times))
  }
  # Order this factor so we can graph it properly
  result[["Time"]] <- factor(
    x = rep(times, each = nrow(df)),
    levels = c("First", "Login", "Last"),
    ordered = TRUE
  )
  # We've attached each element to a list as we iterated, return a data frame
  return(as.data.frame(result))
}

subjects.long <- toLong(subjects)

subjects.long[, "Female.prop"] <- with(subjects.long, female / friends)


