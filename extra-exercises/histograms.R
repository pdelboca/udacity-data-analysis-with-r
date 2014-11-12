# Histograms excercise based on webpage:
# http://flowingdata.com/2014/02/27/how-to-read-histograms-and-use-them-in-r/

setwd('~/Repos/players-analysis-with-r/extra-exercises')

# Load and Tidy the dataset
players <- read.csv('NBA-Census-10.14.2013.csv',stringsAsFactors=FALSE)
names(players) <- gsub("\\.\\.",".",names(players))
names(players) <- gsub("\\.\\.",".",names(players))
names(players) <- gsub("$.","",names(players))
names(players) <- tolower(names(players))
names(players)

# Plot "Heights of Golden State Warriors"
warriors <- subset(players, team=="Warriors")
warriors.o <- warriors[order(warriors$ht.in.),]
barplot(warriors.o$ht.in., 
        names.arg = warriors.o$name,
        horiz = TRUE,
        border = NA,
        las = 1, # Horizontal players names
        main="Heights of Golden State Warriors")

# Plot Average height of players, for each position.
avg_heights <- aggregate(ht.in. ~ pos, data = players, mean)
avg_heights.o <- avg_heights[order(avg_heights$ht.in.,decreasing = FALSE), ]
barplot(avg_heights$ht.in.,
        names.arg = avg_heights$pos,
        border = NA,
        las = 1)

# Some messy code to create an histogram
height_ranges <- range(players$ht.in.)
counts <- rep(0,20)
y <- c()
for(i in 1:length(players[,1])) {
    countsIndex <- players$ht.in.[i] - height_ranges[1] +1
    counts[countsIndex] <- counts[countsIndex] + 1
    y <- c(y,counts[countsIndex])
}
plot(players$ht.in., y,
     type ="n",
     main = "Players heights",
     xlabs = "inches",
     ylab = "count")
points(players$ht.in., y,
       pch = 21,
       col = NA,
       bg = "#999999")

barplot(counts, 
        names.arg=69:88, 
        main="Player heights", 
        xlab="inches", 
        ylab="count", 
        border=NA, 
        las=1)

# Now ploting some histograms
par(mfrow = c(1,3), mar = c(3,3,3,3))
hist(players$ht.in. , main="NBA Player Heights", xlab="inches", 
     breaks=seq(65, 90, 1))
hist(players$ht.in. , main="NBA Player Heights", xlab="inches", 
     breaks=seq(65, 90, 2))
hist(players$ht.in. , main="NBA Player Heights", xlab="inches", 
     breaks=seq(65,90, 5))

# The height distribution for each position
par(mfrow = c(2,3), las = 1, mar = c(5,5,4,1) )

positions <- unique(players$pos)
for (i in 1:length(positions)){
    curr_players <- subset(players, pos == positions[i])
    
    hist(curr_players$ht.in.,
         main = positions[i],
         breaks = 65:90,
         xlab = "inches",
         border = "#ffffff",
         col = "#999999",
         lwd = 0.4)
}
