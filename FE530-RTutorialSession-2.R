################# R Tutorial for FE530 ###########################
# Main Goals: 
# 1. To provide the installation toolset for R
# 2. To learn how to install and use a package (quantmod, TTR)
# 3. To introduce basic concepts using R: Data Containers, Basic Operations
# 4. To run a simple example downloading data and plotting the time series

##### Useful Resources #####
# 1. Simple R - Using R for Introductory Statistics by John Verzani
# Link: http://cran.r-project.org/doc/contrib/Verzani-SimpleR.pdf
# 2. Using R for Data Analysis and Graphics: Introduction, Code and Commentary by JH Maindonald
# Link: http://cran.r-project.org/doc/contrib/usingR.pdf
# 3. Introduction to R
# Link: http://cran.r-project.org/doc/contrib/Lam-IntroductionToR_LHL.pdf
# 4. Interactive R Class Tutorial
# Link: http://tryr.codeschool.com/

##### 1. R Introduction
# Link: http://cran.r-project.org/bin/windows/base/
# Click Download
# Run Installer
# Open R
# (Optional: Download RStudio - http://www.rstudio.com/products/rstudio/)

##### 2. To learn how to install and use a package
# To install packages automatically
install.packages("quantmod")
install.packageS("TTR")
# To load packages
library(quantmod)
library(TTR)
# To inquire about the details of a package
?quantmod
?TTR

##### 3. To introduce basic concepts using R
# Basic Computation
10+8
10*1000
print("Hello World")
# Integral
integrand <- function(x) 1 / ((x+1) * sqrt(x))
integrate(integrand, lower = 0, upper = Inf)
# Variable Assignment
y <- 1
y <- y + 1
x <- y * 20
z <- "Welcome to FE670" 
y <- "a"
(n <- sqrt(100))

### Data Containers: vector, matrix, data.frame, list
### A. Vector: 1-dimensional array
firstVector  <- c(1:6)
secondVector <- c("a", "b", "hello")
thirdVector  <- c("a", 2, 23)
firstVector
thirdVector
# Concatenation (to place additional items in existing vectors)
newVector <- c(firstVector, 7, 8, 9)
newVector
# Operation on Vector
example1 <- newVector[4] #extract the 4th element of a vector
example2 <- newVector[ c(5, 8)] #extract the 5th and the 8th elements
example1
example2
# Mathematical Operations
x  <- c(1, 5, 10, 15, 20,200)
y <- x[4] + x[6] #index of the vector
y
x2 <- 2 * x #vectorized (no need for loop, operation is performed on all elements at once)
x3 <- x ^ 2 
x4 <- x / x2
x5 <- round(x * (x / x2) ^ 3.5 + sqrt(x4), 3)
x6 <- round(c(c(x2[2:4], x3[1:2]), x5[4]), 2)

### B. Matrix: 2-dimensional vector, hold data of similar type
myMatrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
myMatrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE)
ans <- myMatrix[1, 3]
# Operation on Matrix
newMatrix1 <- myMatrix * myMatrix
newMatrix2 <- sqrt(myMatrix)
mat1 <- matrix(rnorm(1000), nrow = 100)
mat2 <- mat1[1:25, ] ^ 2
round(mat1[1:5, 2:6], 3)
head(round(mat2,0), 9)[, 1:7]

### C. Data Frame: a hybrid 2-dimensional container that can include both numeric, character, and factor types.
df <- data.frame(price  = c(89.2, 23.2, 21.2), symbol = c("MOT", "AAPL", "IBM"), action = c("Buy", "Sell", "Buy"))
df2 <- data.frame(col1 = c(6, 8, 10), col2 = c(6,8,10,12))
df3 <- data.frame(col1 = c(1, 2, 3, 4), col2 = c(1, 2, 3, 4))
# Operation on Data Frame
price1 <- df[1, 1]
symbols <- df$symbol
class(symbols) #symbol as factor
# Factors are a convenient data-type that can assist in the categorization and analysis of data.
# Disable the Factor conversion from character into factor
df3 <-data.frame(price  = c(89.2, 23.2, 21.2), symbol = c("MOT", "AAPL", "IBM"), action = c("Buy", "Sell", "Buy"), stringsAsFactors = FALSE)
class(df3$symbol) #symbol as character now

### D. List: the most general container
myList <- list(a = c(1, 2, 3, 4, 5), b = matrix(1:10, nrow = 2, ncol = 5), c = data.frame(price = c(89.3, 98.2, 21.2), stock = c("MOT", "IBM", "CSCO")))
myList
# Operation on Lists
firstComp <- myList[[1]]
class(firstComp)
secondComp <- myList[["b"]]
class(secondComp)
partOfList <- myList[ c(1, 3)]
class(partOfList)
length(myList) #number of component in the list

### E. Useful Functions
#1.  Greate 1000 normally distributed random numbers of mean 0 and standard dev 1
x     <- rnorm(1000, mean = 0, sd = 1)
#2.  Find the length of the vector x.
xL    <- length(x)
#3.  Compute the mean and standard deviation of those numbers
xM    <- mean(x)
xS    <- sd(x)
#4.  Find the sum of all the numbers in x vector x.
xSum  <- sum(x)
#5.  Do a cumulative sum of the values in x
xCSum <- cumsum(x)
#6.  Look at the first 3 elements a vector
head(xCSum, 3)
#7.  Look at the last 3 elements of a vector
tail(xCSum, 3)
#8.  Look at summary statistics
summary(x)
#9.  Sort x from smallest to largest and from largest to smallest.
xSIn  <- sort(x)
xSDec <- sort(x, decreasing = TRUE)
#10. Compute the median value of the vector
xMed  <- median(x)
#11. Compute the range of a variable
xR    <- range(x)
#12. Compute the difference between elements
y     <- c(100.1, 100.2, 100, 99, 99.9)
yDiff <- diff(y)
#13. Create a sequence from 1 to 10
s     <- 1:10
#14. A sequence from 1 to 10 in steps of 0.1
z     <- seq(1, 10, 0.1)

### G: Basic Plotting
#Create a vector of numbers x and plot it
x <- c(1, 2, 3.2, 4, 3, 2.1, 9, 19)
plot(x)
plot(x, type = "l")

# Plotting random number
plot(rnorm(1000), col = "red", main = "Random Returns", xlab = "Time", ylab = "Returns")
grid()
abline(v = 400, lwd = 2, col = "blue")
abline(h = 2, lwd = 3, col = "orange")

# Create a 2-row/2-column format
par(mfrow = c(2, 2)) #dividing your screen into 2x2 size
plot(rnorm(100), col = "green", main = "1st Group")
plot(rnorm(100), col = "red", main = "2nd Group", type = "l")
plot(rnorm(100), col = "gold", main = "3rd Group", type = "s")
abline(v = 50, col = "blue", lwd = 4)
plot(rnorm(100) , col = "gray", main = "4th Group")
abline(a = -1, b = 0.1, lwd = 2, col = "purple")
#Reset the plotting window
par(mfrow = c(1, 1)
    
###### 4. To run a simple example downloading data and plotting the time series
# Using quantmod package
# Reference: cran.r-project.org/web/packages/quantmod/quantmod.pdf
symbol <- "AAPL"
# first chart
data <- getSymbols(symbol)
barChart(AAPL) 
# second chart
chartSeries(AAPL) 
addMACD() 
addBBands() 

# Getting stock data
data <- getYahooData("AAPL", 19990404, 20140807)
data <- data[,c("High","Low","Open","Close")]
tail(data,5)