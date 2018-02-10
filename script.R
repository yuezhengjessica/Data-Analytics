# Based on Python Data Visualizations

library(ggplot2)
library(readr)

getwd()
# Next, we'll load the Iris flower dataset, which is in the "../input/" directory
iris = read_csv("Iris.csv")

# Let's see what's in the iris data
head(iris)

# Let's see how many examples we have of each species
table(iris$Species)

# Make a scatterplot of Iris features
p <- ggplot(iris, aes(x=SepalLengthCm, y=SepalWidthCm))
p + geom_point()

# We could make a jointplot showing bivariate scatterplots and univariate histograms 
# in the same figure but that would require some extra work which I won't do here :)
# Instead, we'll show them separately
ggplot(iris, aes(x=SepalWidthCm)) + geom_histogram()
ggplot(iris, aes(x=SepalLengthCm)) + geom_histogram()

# One piece of information missing in the plots above is what species each plant is
# We simply add `color=Species` to aes
p <- ggplot(iris, aes(x=SepalLengthCm, y=SepalWidthCm, color=Species)) # only update
p + geom_point()

# We can look at an individual feature through a boxplot
g <- ggplot(iris, aes(x=Species,y=PetalLengthCm,color=Species))
(gb <- g + geom_boxplot())

# One way we can extend this plot is adding a layer of individual points on top of it
# 
# We'll use geom_jitter() so that all the points don't fall in single vertical lines
# above the species
#
gb + geom_jitter(color="grey")

# A violin plot combines the benefits of the previous two plots and simplifies them
# Denser regions of the data are fatter, and sparser thiner in a violin plot
gb + geom_violin()

# A final seaborn plot useful for looking at univariate relations is the kdeplot,
# which creates and visualizes a kernel density estimate of the underlying feature
ggplot(iris,aes(PetalLengthCm,color=Species)) + geom_density()

# Another useful seaborn plot is the pairplot, which shows the bivariate relation
# between each pair of features
# 
# From the pairplot, we'll see that the Iris-setosa species is separataed from the other
# two across all feature combinations
pairs(iris[1:4], lower.panel=panel.smooth, pch=20, cex=0.5, col=as.numeric(iris$Species))

# The diagonal elements in a pairplot show the histogram by default
# We can update these elements to show other things, such as a kde
# Using GGaly's ggpairs, we get a very informative plot
library(GGally)
ggpairs(iris, mapping = aes(color=Species), columns=1:4)

# Now that we've covered seaborn, let's go back to some of the ones we can make with Pandas
# We can quickly make a boxplot with Pandas on each feature split out by species
# Now we make a boxplot on each feature split out by species
library(reshape2)
iris_melted <- melt(iris,id.var="Species") # melting by 'Species'
ggplot(iris_melted, aes(x=variable,y=value)) + geom_boxplot(aes(fill=Species))

# Note. I have no idea what Andrew curves are for but here's how to show them :)
# "One cool more sophisticated technique pandas has available is called Andrews Curves
# Andrews Curves involve using attributes of samples as coefficients for Fourier series
# and then plotting these"
#library(andrews)
#andrews(iris, type=1, clr="Species", ymax=3)

# Another multivariate visualization technique pandas has is parallel_coordinates
# Parallel coordinates plots each feature on a separate column & then draws lines
# connecting the features for each data sample
library(MASS)
parcoord(iris[1:4], col=as.numeric(iris$Species))

# A final multivariate visualization technique pandas has is radviz
# Which puts each feature as a point on a 2D plane, and then simulates
# having each sample attached to those points through a spring weighted
# by the relative value for that feature
#from pandas.tools.plotting import radviz
#radviz(iris.drop("Id", axis=1), "Species")
#TODO 

# # Wrapping Up
# 
# I hope you enjoyed this quick introduction to some of the quick, simple data visualizations you can create with R.

# I encourage you to run through these examples yourself, tweaking them and seeing what happens. From there, you can try applying these methods to a new dataset and incorprating them into your own workflow!