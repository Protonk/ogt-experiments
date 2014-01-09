### Timing and difference analysis

## We can use both subjects and subjects.long here
## but we'll likely use the 'wide' dataset

# Difference in proportion of female friends from start to end of experiment 
subjects[, "fromFirst"] <- with(subjects, female_last / friends_last - female_first / friends_first)
# Analysis of variance will have problems if the group sizes are too different
# so select a subset of the treatment group to compare to the control
balanced <- function(df) {
  ct <- df[df[, "Type"] == "Control", ]
  tr <- df[df[, "Type"] == "Treatment", ]
  tr <- tr[sample(1:nrow(tr), nrow(ct)), ]
  g <- lm(fromFirst ~ Type, rbind(ct, tr))
  return(g)
}

summary(balanced(subjects))


# instead of comparing just the difference, compare the eventual proportion
# to the initial proportion
withInit <- lm(female_last / friends_last ~ Type + I(female_first / friends_first), data = subjects)

