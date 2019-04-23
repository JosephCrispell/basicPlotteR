# plotteR
## Author: Joseph Crispell
## Repository created: 23-04-19
## Licence: GPL-3
An R package containing all the functions I have developed to make plotting with base R a little easier

Package can be directly installed into R using:
```
install.packages("devtools")
devtools::install_github("JosephCrispell/plotteR")
library(plotteR)
```

## Package contents
`plotteR` is a collection of tools designed for particular tasks. As I develop different tools that help with plotting in R, I'll add them into this general package. These are the current tools available in `plotteR`:
- `addTextLabels` for add non-overlapping labels onto existing R plot
- `spreadPoints` for adding points onto boxplot(s) that are deterministically spread out to minimise overlap
- `plotMultipleHistograms` for plotting multiple histograms on the same plot

## `addTextLabels`
```
# Create some random points
n <- 50
testLabels <- c("short", "mediummm", "looooonnnnnnngggggg", "0090292002", "9", "A Different label")
coords <- data.frame(X=runif(n), Y=runif(n, min=0, max=100), Name=sample(testLabels, size=n, replace=TRUE),
                     stringsAsFactors = FALSE)

# Plot them without labels
plot(x=coords$X, y=coords$Y, pch=19, bty="n", xaxt="n", yaxt="n", col="red", xlab="X", ylab="Y")

# With potentially overlapping labels
plot(x=coords$X, y=coords$Y, pch=19, bty="n", xaxt="n", yaxt="n", col="red", xlab="X", ylab="Y")
text(coords$X, coords$Y, labels=coords$Name, xpd=TRUE)

# Plot them with non-overlapping labels
plot(x=coords$X, y=coords$Y, pch=19, bty="n", xaxt="n", yaxt="n", col="red", xlab="X", ylab="Y")
addTextLabels(coords$X, coords$Y, coords$Name, cex=1, col.label="black")

# Plot them with non-overlapping labels
plot(x=coords$X, y=coords$Y, pch=19, bty="n", xaxt="n", yaxt="n", col="red", xlab="X", ylab="Y")
addTextLabels(coords$X, coords$Y, coords$Name, cex=1, col.background=rgb(0,0,0, 0.75), col.label="white")
```

![](ExampleImages/addTextLabels.gif)

## `spreadPoints`
Adding points to single boxplot:
```
# Set the seed
set.seed(254534)

# Generate some example points - drawn from exponential distribution
values <- rexp(n=50, rate=2)
 
# Plot a boxplot
boxplot(values, xlab="",  ylab="", frame=FALSE, las=1, pch=19, outcol=rgb(1,0,0, 0.5),
        horizontal=FALSE)
        
# Plot the points spread along the X axis
spreadPoints(values, position=1)
```

![](ExampleImages/spreadPoints_1.png)

Adding points to multiple boxplots:
```
# Set the seed
set.seed(254534)

# Generate some example points - drawn from normal distribution and randomly assign them to categories
randomSamples <- data.frame(Values = rnorm(500), Category = sample(c('A', 'B', 'C', 'D', 'E'), size=500, replace=TRUE))
 
# Plot a boxplot of the samples from the normal distribution versus there categories - multiple boxplots
boxplot(Values ~ Category, data = randomSamples, lwd = 2)
 
# Plot the points for each category spread along the X axis
spreadPointsMultiple(data=randomSamples, responseColumn="Values", categoriesColumn="Category")
```

![](ExampleImages/spreadPoints_2.png)

## `plotMultipleHistograms`
```
# Set the seed
set.seed(254534)

# Create two random samples from a normal distribution
distributions <- list(rnorm(500, mean=5, sd=0.5), 
                      rnorm(500, mean=8, sd=5), 
                      rnorm(500, mean=20, sd=2))

# Plot overlapping histograms
plotMultipleHistograms(distributions, nBins=20, colours=c(rgb(1,0,0, 0.5), rgb(0,0,1, 0.5), rgb(0,1,0, 0.5)), las=1)
```

![](ExampleImages/plotMultipleHistograms.png)
