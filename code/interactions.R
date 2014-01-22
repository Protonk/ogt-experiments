
library(reshape2)
jr.interact <- read.csv("~/dev/R/ogt-experiments/data/population/journo_data.csv", na.strings = c("NA", "#N/A", "<NA>", ""), as.is = TRUE)

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


byOrgs <- function(x) { 
  propF <- x[, 'follow.female'] / x[, 'follow.total']
  propI <- x[, 'interactions.female'] / x[, 'interactions.total']
  return(
    data.frame(
      f.interact = sum(x[, 'interactions.female'], na.rm = TRUE),
      f.follow = sum(x[, 'follow.female'], na.rm = TRUE),
      p.follow = mean(propF, na.rm = TRUE),
      p.interact = mean(propI, na.rm = TRUE),
      Men = sum(ifelse(x[, 'gender'] == 'Male', 1, 0), na.rm = TRUE),
      Women = sum(ifelse(x[, 'gender'] == 'Female', 1, 0), na.rm = TRUE)
    )
  )
}

org.sum <- ddply(jr.interact, 'orgs', byOrgs)



ggplot(
    data = subset(jr.interact, gender %in% c('Male', 'Female')), aes(
    x = follow.female/follow.total,
    y = interactions.female/interactions.total,
    colour = gender)
  ) +
  geom_point() + geom_abline(
    intercept = 0,
    slope = 1,
    colour = 'black',
    linetype = 'dashed',
    alpha = 0.8
  ) +
  xlim(0, 0.5) + ylim(0, 0.5) +
  geom_smooth(alpha = 0.3) +
  ggtitle('Propotional Interactions and Follows by Gender') +
  xlab('Female Accounts Followed') +
  ylab('Interactions with Female Accounts')

org.sum <- org.sum[with(org.sum, order(f.interact, decreasing = TRUE)), ]
org.sum[, 'orgs'] <- factor(
  org.sum[, 'orgs'],
  levels = as.character(org.sum[with(org.sum, order(f.interact, decreasing = TRUE)), 'orgs']),
  labels = pretty.labs,
  ordered = TRUE
)

gen.long <- melt(
  org.sum[with(org.sum, order(Men + Women, decreasing = TRUE))[1:8], c('orgs', 'Men', 'Women')],
  id.vars = 'orgs',
  variable.name = 'Gender',
  value.name = 'Staff'
)
names(gen.long)[1] <- 'Organization'

ggplot(data = gen.long) +
  geom_bar(
    aes(x = Organization, fill = Gender, y = Staff),
    stat = 'identity',
    position = 'dodge'
  ) +
  xlab('') + ylab('Staff on Twitter') +
  theme(axis.text.x  = element_text(angle = 45, vjust = 0.3)) +
  scale_fill_manual(values = c('#ddaa44', '#66aa66'))




