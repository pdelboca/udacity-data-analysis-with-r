library(ggplot2)
head(diamonds)
str(diamonds)
summary(diamonds)
ggplot(data = diamonds, aes(x=price, y = x)) + geom_point()
ggplot(data = diamonds, aes(x = x, y = price)) + geom_point()

cor.test(diamonds$x, diamonds$price)
cor.test(diamonds$y, diamonds$price)
cor.test(diamonds$z, diamonds$price)

ggplot(data = diamonds, aes(x = depth, y = price)) + geom_point()
# Change the code to make the transparency of the
# points to be 1/100 of what they are now and mark
# the x-axis every 2 units. See the instructor notes
# for two hints.
ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha = 1/100) + 
  scale_x_continuous(breaks =  seq(43,79,2), labels =  seq(43,79,2))
cor.test(diamonds$price, diamonds$depth)

# Create a scatterplot of price vs carat
# and omit the top 1% of price and carat
# values.
summary(diamonds$price)
summary(diamonds$carat)

top_price <- quantile(diamonds$price,probs = 0.99)
top_carat <- quantile(diamonds$carat, probs = 0.99)
data <- subset(diamonds, diamonds$price < top_price & diamonds$carat < top_carat)

ggplot(data = data,
       aes(x = carat, y = price)) + geom_point()

# Create a scatterplot of price vs. volume (x * y * z).
# This is a very rough approximation for a diamond's volume.

diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(diamonds, aes(x = volume, y = price)) + geom_point()

vol.cor.data <- subset(diamonds, volume != 0 & volume < 800 )
cor.test(vol.cor.data$volume, vol.cor.data$price)

# Subset the data to exclude diamonds with a volume
# greater than or equal to 800. Also, exclude diamonds
# with a volume of 0. Adjust the transparency of the
# points and add a linear model to the plot. (See the
# Instructor Notes or look up the documentation of
# geom_smooth() for more details about smoothers.)

ggplot(vol.cor.data, aes(x = volume, y = price)) +
  geom_point(alpha = 1/10) +
  geom_smooth(method = "lm")

# Name the data frame diamondsByClarity

# The data frame should contain the following
# variables in this order.

#       (1) mean_price
#       (2) median_price
#       (3) min_price
#       (4) max_price
#       (5) n
library(dplyr)
str(diamonds)
diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarise(mean_price = mean(price),
            median_price = median(as.numeric(price)),
            min_price = min(price,na.rm = TRUE),
            max_price = max(price,na.rm = TRUE),
            n = n()) %>%
  arrange(clarity)

# We've created summary data frames with the mean price
# by clarity and color. You can run the code in R to
# verify what data is in the variables diamonds_mp_by_clarity
# and diamonds_mp_by_color.

# Your task is to write additional code to create two bar plots
# on one output image using the grid.arrange() function from the package
# gridExtra.

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

library(gridExtra)
p1 <- ggplot(diamonds_mp_by_clarity, aes(x=clarity,y=mean_price)) +
  geom_bar(stat = "identity")
p2 <- ggplot(diamonds_mp_by_color, aes(x=color,y=mean_price)) +
  geom_bar(stat = "identity")

grid.arrange(p1,p2,ncol = 1)
