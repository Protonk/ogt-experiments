## "Long" format charts
## these won't work for 'first' and 'second'
## instead use fbHist(second.long)

### Histogram
### works for both experiments, though the different groups are confounding
fbHist <- function(df) {
  # drop initital for diffs
  df <- df[df[, 'Stage'] != 'Initial', ]
  p <- ggplot(data = df) +
    geom_histogram(aes(x = abs_diff, fill = Stage), alpha = 0.6) +
    geom_vline(xintercept = 0, colour = 'red', linetype = 'dashed') +
    guides(fill = FALSE) +
    xlab('Absolute change in Female Friends') + ylab('') +
    facet_grid(Stage ~ user.group)
  return(p)
}

### Density estimator

fbDens <- function(df) {
  # drop initital for diffs
  df <- df[df[, 'Stage'] != 'Initial', ]
  p <- ggplot(data = df) +
    geom_density(aes(x = abs_diff, fill = Stage), alpha = 0.6) +
    geom_vline(xintercept = 0, colour = 'red', linetype = 'dashed') +
    guides(fill = FALSE) +
    xlab('Absolute change in Female Friends') + ylab('') +
    facet_grid(Stage ~ user.group)
  return(p)
}

### Yay, boxplots

fbBox <- function(df) {
  # drop initital for diffs
  df <- df[df[, 'Stage'] != 'Initial', ]
  p <- ggplot(data = df) +
    geom_boxplot(aes(x = Stage, y = abs_diff, fill = user.group)) +
    ylab('Absolute Change in Female Friends') + xlab('Stage') +
    scale_fill_discrete(name = 'Group')
  return(p)
}

### Plot proportion against absolute change
### Not super helpful, but it can offer some indication as to
### how heavily very active users impacted the outcome
# as indicated by friends at the outset
absVsInit <- function(df) {
  p <- ggplot(data = na.omit(df), aes(
    x = init_tot,
    y = abs_diff,
    colour = user.group
  )) +
    geom_point() + geom_smooth(method = 'lm') +
    facet_grid(Stage ~ .) +
    ylim(-100, 150) + xlim(0, 4000) +
    xlab('Initial Total Friends') +
    ylab('Absolute Change in Female Friends')
  return(p)
}

## 'Wide' format charts

# actions only works for the second experiment
fbActions <- function() {
  df <- second
  apu <- quantile(df[, 'user.total_actions'])
  df[, 'Actions Per User'] <- cut(
    df[, 'user.total_actions'],
    breaks = unname(apu),
    labels = names(apu)[-1]
  )
  # Drop NA values introduced by cut()
  p <- ggplot(data = na.omit(df)) +
    # don't split by control/treatment as we actions
    # for control group don't vary (it's part of treatment)
    geom_boxplot(aes(x = `Actions Per User`, y = X2fb.ff_abs)) +
    ylab('Change in Female Friends') +
    xlab('Percentile of User Actions') +
    ggtitle('Users interacting more heavily with FollowBias show greater change')
  return(p)
}
