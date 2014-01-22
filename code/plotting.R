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
    ylab('Absolute Change in Female Friends') + xlab('') +
    ggtitle('Net Change in Female Friends over the Experiment')
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

### plots for models

df.plot <- subset(model.time, Parameters %in% c('Control', 'Treatment'))
df.plot[, 'Parameters'] <- as.character(df.plot[, 'Parameters'])
modelFx <- function() {
  p <- ggplot(data = df.plot, aes(x = as.numeric(Stage))) +
    geom_line(aes(y = Estimate, colour = Parameters, group = Parameters), size = 1.5) +
    geom_ribbon(aes(ymax = Upper, ymin = Lower, group = Parameters, fill = Parameters), alpha = 0.3) +
    xlab('Stages') + ylab('Point Estimate') +
    scale_x_continuous(breaks = 1:4, labels = c('Login', 'End', 'First Followup', 'Second Followup')) +
    ggtitle('Estimated Effect of Treatment Corrected for Initial Total Friends')
  return(p)
}
modelFx()

## Plots for organizations

fbOrgs <- function() {
  org.chart <- ggplot(data = gen.long, aes(x = Organization, fill = Gender, y = Staff)) +
    geom_bar(
      stat = 'identity',
      position = 'dodge'
    ) + 
    xlab('') + ylab('') +
    scale_fill_manual(values = c('#66aa66', '#ddaa44')) +
    guides(
      fill = guide_legend(
        title = NULL,
        label.theme = element_text(angle = 0, size = 9)
      )
    ) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.65, size = 7.5),
      axis.text.y = element_text(angle = 0, size = 9),
      legend.justification = c(1, 1),
      legend.position = c(1, 1),
      legend.key.size = unit(0.4, "cm"),
      legend.background = element_blank()
    )
  
  return(org.chart)  
}

## Saving charts (may get noisy for dropbox)
ggsave(filename = 'plots/staff_final.pdf', plot = org.chart, width = 3.31, height = 4.4, units = 'in')

