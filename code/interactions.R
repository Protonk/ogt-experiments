
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

org.order <- with(org.sum, order(Men + Women, decreasing = TRUE))

gen.long <- melt(
  org.sum[org.order[1:8], c('orgs', 'Men', 'Women')],
  id.vars = 'orgs',
  variable.name = 'Gender',
  value.name = 'Staff'
)
names(gen.long)[1] <- 'Organization'

org.tags <- unique(as.character(gen.long[, 'Organization']))
org.pretty <- c("New York\nTimes",
                "Bloomberg",
                "Telegraph\nNews", 
                "NPR",
                "Huffington\nPost",
                "Boston\nGlobe", 
                "Chicago\nTribune",
                "Washington\nPost"
               )

gen.long[, 'Organization'] <- factor(
  x = gen.long[, 'Organization'],
  levels = org.sum[org.order[1:8], 'orgs'],
  labels = org.pretty,
  ordered = TRUE
)
