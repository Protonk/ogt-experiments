### Timing and difference analysis

## We can use both subjects and subjects.long here
## but we'll likely use the 'wide' dataset


# Analysis of variance will have problems if the group sizes are too different
# so select a subset of the treatment group to compare to the control
balanced <- function() {
  df <- fb.init
  # Difference in proportion of female friends from start to end of experiment 
  df[, "fromFirst"] <- with(df, female_last / friends_last - female_first / friends_first)
  ct <- df[df[, "Type"] == "Control", ]
  tr <- df[df[, "Type"] == "Treatment", ]
  tr <- tr[sample(1:nrow(tr), nrow(ct)), ]
  g <- lm(fromFirst ~ Type, rbind(ct, tr))
  return(g)
}

summary(balanced())

balancedSecond <- function(type) {
  df <- fb.second

  df[, 'propFirst'] <- with(
    df,
    ffb.female / (ffb.male + ffb.female + ffb.unknown)
  )
  df[, 'propLast'] <- with(
    df,
    efb.female / (efb.male + efb.female + efb.unknown)
  )
  ct <- df[df[, "Type"] == "Control", ]
  tr <- df[df[, "Type"] == "Treatment", ]
  tr <- tr[sample(1:nrow(tr), nrow(ct)), ]
  if (type == 'prop') {
    g <- lm(propLast - propFirst ~ Type, rbind(ct, tr))
  } else {
    g <- lm(efb.female - ffb.female ~ Type + I(ffb.male + ffb.female + ffb.unknown), rbind(ct, tr))
  }
  
  return(g)
}


# Tiny bootstrap

second.boot <- function(type = 'prop') {
  res <- replicate(200, balancedSecond(type)$coefficients)
  # we resample the treatment group, not the control
  data.frame(
    # Add in a factor to make plotting a bit easier with ggplot
    Type = 'Treatment',
    Difference = res[2, ] - res[1, 1]
  )
}
balanced.out <- second.boot()


# instead of comparing just the difference, compare the eventual proportion
# to the initial proportion
withInit <- lm(female_last / friends_last ~ Type + I(female_first / friends_first), data = subjects)

