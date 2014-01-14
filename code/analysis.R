### Timing and difference analysis

## We can use both subjects and subjects.long here
## but we'll likely use the 'wide' dataset


# Difference in female friends from start to end of experiment
diffStagger <- function(df, select, long = FALSE) {
  # use a list to collect values
  res <- list()
  # first values can be computed outside the loop
  names.first <- str_c('ffb', c('male', 'female', 'unknown'), sep = '.')
  tot.init <- rowSums(df[, names.first], na.rm = TRUE)
  for (i in select) {
    # absolute and proportional differences
    abs.name <- str_c(i, 'ff_abs', sep = '.')
    prop.name <- str_c(i, 'ff_prop', sep = '.')
    names.i <- str_c(i, c('male', 'female', 'unknown'), sep = '.')
    tot.i <- rowSums(df[, names.i], na.rm = TRUE)
    # average both initital and 'current' total friends
    prop.mean <- rowMeans(cbind(tot.init, tot.i), na.rm = TRUE)
    # cache 'current' female friends
    current <- df[, str_c(i, '.female')]
    # compute average and absolute differences from start of experiment
    res[[prop.name]] <- (current - df[, 'ffb.female']) / prop.mean
    res[[abs.name]] <- current - df[, 'ffb.female']
  }
  res <- as.data.frame(res)
  if (long) {
    s.prop <- stack(res, select = names(res)[grep('prop', names(res))])
    s.abs <- stack(res, select = names(res)[grep('abs', names(res))])
    out <- data.frame(
      user.id = df[, 'user.id'],
      Stage = sub('\\..*$', '', s.abs[, 'ind']),
      prop_diff = s.prop[, 'values'],
      abs_diff = s.abs[, 'values'],
      init_tot = tot.init
    )
    return(out)
  } else {
    # Add initital total so it's easier to get later
    res[, 'init_tot'] <- tot.init
    return(res)
  }
}

second <- cbind(second, diffStagger(second, c('lfb', 'X1fb', 'X2fb')))
first <- cbind(first, diffStagger(first, c('lfb', 'X1fb', 'X3fb')))

# A little trickier to merge to the 'long' format because diffs don't exist
# for the first snapshot

second.long <- rbind(
  data.frame(
    second.long[second.long[, 'Stage'] == 'ffb', ],
    abs_diff = NA,
    prop_diff = NA,
    init_tot = second[, 'init_tot']
  ),
  merge(
    second.long,
    diffStagger(second, c('lfb', 'X1fb', 'X2fb'), long = TRUE),
    all.y = TRUE, by.x = c('user.id', 'Stage')
  )
)
first.long <- rbind(
  data.frame(
    first.long[first.long[, 'Stage'] == 'ffb', ],
    abs_diff = NA,
    prop_diff = NA,
    init_tot = first[, 'init_tot']
  ),
  merge(
    first.long,
    diffStagger(first, c('lfb', 'X1fb', 'X3fb'), long = TRUE),
    all.y = TRUE, by.x = c('user.id', 'Stage')
  )
)

first.long[, 'Stage'] <- factor(
  first.long[, 'Stage'],
  labels = c('Initial', 'Login', 'End', 'Followup'),
  ordered = TRUE
)
second.long[, 'Stage'] <- factor(
  second.long[, 'Stage'],
  labels = c('Initial', 'Login', 'End', 'Followup'),
  ordered = TRUE
)

