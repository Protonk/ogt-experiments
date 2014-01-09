library(ggplot2)

## "Long" format data

box.p <- ggplot(data = subjects.long) +
  geom_boxplot(aes(x = Type, y = Female.prop, fill = Type)) +
  facet_wrap(~ Time) +
  ylab('Proportion of Female Friends') + xlab('') +
  ggtitle('Control and Treatment Groups, Before and After Experiment')

## Descriptive charts

prop.p <- ggplot(data = subjects.long) +
  geom_density(aes(x = Female.prop, fill = Type), alpha = 0.4) +
  xlab('Proportion of Female Friends') + ylab('')
count.p <- ggplot(data = subjects.long) +
  geom_histogram(aes(x = friends, fill = Type), alpha = 0.4, binwidth = 100) +
  xlab('Total Number of Friends')
### Another look at the count
points.p <- ggplot(data = subjects.long) +
  geom_point(aes(x = friends, y = Female.prop, colour = Type)) +
  xlab('Total Number of Friends') + ylab('Proportion of Female Friends')
