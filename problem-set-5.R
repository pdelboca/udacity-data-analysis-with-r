setwd("~/Repos//data-analysis-with-r")
library(ggplot2)

# Create a histogram of diamond prices.
# Facet the histogram by diamond color
# and use cut to color the histogram bars.

# The plot should look something like this.
# http://i.imgur.com/b5xyrOu.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the histogram using
# scale_fill_brewer(type = 'qual')

head(diamonds)
summary(diamonds)

ggplot(diamonds, aes(x=price)) + 
  geom_histogram(aes(fill = cut), binwidth = 500) + 
  facet_wrap(~color) +
  scale_fill_brewer(type = 'qual')
  
# Create a scatterplot of diamond price vs.
# table and color the points by the cut of
# the diamond.

# The plot should look something like this.
# http://i.imgur.com/rQF9jQr.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the scatterplot using
# scale_color_brewer(type = 'qual')

ggplot(diamonds, aes(x = table, y = price)) +
  geom_point(aes(color = cut)) + 
  scale_x_continuous(breaks = seq(43,95,2), labels = seq(43,95,2)) +
  scale_color_brewer(type = 'qual')

# Create a scatterplot of diamond price vs.
# volume (x * y * z) and color the points by
# the clarity of diamonds. Use scale on the y-axis
# to take the log10 of price. You should also
# omit the top 1% of diamond volumes from the plot.

# Note: Volume is a very rough approximation of
# a diamond's actual volume.

# The plot should look something like this.
# http://i.imgur.com/excUpea.jpg

# Note: In the link, a color palette of type
# 'div' was used to color the scatterplot using
# scale_color_brewer(type = 'div')

diamonds$volume <- diamonds$x * diamonds$y + diamonds$z

ggplot(data = subset(diamonds, volume < quantile(volume,probs = 0.99)), 
       aes(x=volume,y=price)) +
  geom_point(aes(color = clarity)) +
  scale_y_log10() +
  scale_color_brewer(type = 'div')

# Many interesting variables are derived from two or more others.
# For example, we might wonder how much of a person's network on
# a service like Facebook the user actively initiated. Two users
# with the same degree (or number of friends) might be very
# different if one initiated most of those connections on the
# service, while the other initiated very few. So it could be
# useful to consider this proportion of existing friendships that
# the user initiated. This might be a good predictor of how active
# a user is compared with their peers, or other traits, such as
# personality (i.e., is this person an extrovert?).

# Your task is to create a new variable called 'prop_initiated'
# in the Pseudo-Facebook data set. The variable should contain
# the proportion of friendships that the user initiated.

pf <- read.csv("./data//pseudo_facebook.tsv", sep="\t")
pf$prop_initiated <- pf$friendships_initiated / pf$friend_count

pf$prop_initiated <- ifelse(pf$friend_count >0,
                            pf$friendships_initiated / pf$friend_count,
                            0)

# Create a line graph of the proportion of
# friendships initiated ('prop_initiated') vs.
# tenure and color the line segment by
# year_joined.bucket.

# Recall, we created year_joined.bucket in Lesson 5
# by first creating year_joined from the variable tenure.
# Then, we used the cut function on year_joined to create
# four bins or cohorts of users.

# (2004, 2009]
# (2009, 2011]
# (2011, 2012]
# (2012, 2014]

# The plot should look something like this.
# http://i.imgur.com/vNjPtDh.jpg
# OR this
# http://i.imgur.com/IBN1ufQ.jpg

pf$year_joined <- floor(2014 - pf$tenure/365)
pf$year_joined.bucket <- cut(pf$year_joined,
                             breaks=c(2004,2009,2011,2012,2014))

ggplot(subset(pf, tenure >=1),
       aes(x=tenure, y = prop_initiated)) +
  geom_line(stat="summary", fun.y=mean, aes(color = year_joined.bucket)) +
  geom_smooth()

x <- subset(pf, year_joined.bucket == "(2012,2014]")
mean(x$prop_initiated,na.rm = TRUE)

# Create a scatter plot of the price/carat ratio
# of diamonds. The variable x should be
# assigned to cut. The points should be colored
# by diamond color, and the plot should be
# faceted by clarity.

# The plot should look something like this.
# http://i.imgur.com/YzbWkHT.jpg.

# Note: In the link, a color palette of type
# 'div' was used to color the histogram using
# scale_color_brewer(type = 'div')

ggplot(diamonds, aes(x = cut, y = price/carat)) +
  geom_jitter(aes(color= color)) + 
  facet_wrap(~ clarity) +
  scale_color_brewer(type = 'div')
