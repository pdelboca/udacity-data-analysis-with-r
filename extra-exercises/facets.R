# Excersise from: http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
library(reshape2)
library(ggplot2)

## Problem:
# You want to do split up your data by one or more variables and plot the 
# subsets of data together.

# Initial Exploratory commands

?tips
names(tips)
dim(tips)
str(tips)
summary(tips)

# Day and Time has non-intuitive levels...
levels(tips$day) <- c("Thur","Fri","Sat","Sun")
levels(tips$time) <- c("Lunch","Dinner")

# This is a scatterplot of the tip percentage by total bill size.
sp <- ggplot(tips, aes(x=total_bill, y=tip/total_bill)) + geom_point(shape=1)
sp

# Divide by levels of "sex", in the vertical direction
sp + facet_grid(sex ~ .)

# Divide by levels of "sex", in the horizontal direction
sp + facet_grid(. ~ sex)

# Divide with "sex" vertical, "day" horizontal
sp + facet_grid(sex ~ day)

# Divide by day, going horizontally and wrapping with 2 columns
sp + facet_wrap( ~ day, ncol=2)

###############################################################################
###############################################################################

## PERSONAL STUDY ABOUT THE DATA: ggplot and EDA
# First lets do some explorative quick plots :)
?qplot
qplot(tip, data=tips)
qplot(sex, data=tips)
qplot(time, data=tips)
qplot(size, data=tips)
qplot(day, data=tips, fill=time)
qplot(total_bill, data=tips)

# Now using some facets...
qplot(tip, data=tips, facets = sex ~ .)
qplot(tip, data=tips, facets = day ~ .)

# Now using ggplot
ggplot(tips, aes(tip)) + geom_histogram()
ggplot(tips, aes(day, fill=time)) + geom_histogram() + theme_bw()
ggplot(tips, aes(tip, fill=time)) + geom_histogram() + facet_grid(day ~ .) + theme_bw()

# Who gives more dollars in tips, male or female?
sum(subset(tips, sex=="Male",select=tip)) # 485.07
sum(subset(tips,sex=="Female",select=tip)) # 246.51

ggplot(tips, aes(sex, tip)) + geom_bar(stat="identity")
ggplot(tips, aes(sex, tip)) + stat_summary(fun.y = sum, geom = "bar")

# Which gender, in average, leaves more money?
ggplot(tips, aes(sex, tip)) + stat_summary(fun.y = mean, geom = "bar")

# Who gives more tips, smokers or no-smokers?
sum(subset(tips, smoker=="Yes", select=tip)) # 279.81
sum(subset(tips, smoker=="No", select=tip)) # 451.77

ggplot(tips, aes(smoker, tip)) + stat_summary(fun.y = sum, geom="bar")


# When do the people leave more tips? Dinner or Lunch?
sum(subset(tips, time=="Dinner", select=tip))
sum(subset(tips, time=="Lunch", select=tip))

ggplot(tips, aes(time, tip)) + stat_summary(fun.y = sum, geom = "bar")

# Which days the people gives more money in tips?
ggplot(tips, aes(day,tip)) + stat_summary(fun.y = sum, geom = "bar")