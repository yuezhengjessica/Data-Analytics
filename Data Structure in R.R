# AGENDA:
#     1. Data Structures in R
#         vector
#         matrix
#         list
#     2. Loops
#         for
#         if-else
#         while


# ================================== Baptism =======================================


Hello World
# oops

"Hello World"
print("Hello World")

# calculator
2 + 2

# ================================== 1. Data Structure =======================================
# ================================== Atomic Classes ==============================================


# R has five atomic classes
# "logical"
# "integer"
# "numeric"
# "complex"
# "character"

a = 50
a <- 50
typeof(a)
mode(a)

# Explicit Coersion between types

x <- -3:3
x
typeof(x)
# [1] "integer"
is.vector(x)
# [1] TRUE
is.numeric(x)
# [1] TRUE
is.character(x)
# [1] FALSE

as.numeric(x)
# [1] -3 -2 -1  0  1  2  3
as.logical(x)
# [1]  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE
as.character(x)
# [1] "-3" "-2" "-1" "0"  "1"  "2"  "3" 

# ================================== 1.1 Vector ==============================================
# Vector: consists of variables in the SAME type.

# 1. Three common ways to create vectors
# c()
# seq()
# rep()

x <- c(4, 5, 6)        # numeric
x <- c(TRUE, FALSE)    # logical
x <- c(T, F)           # logical
x <- c('a', 'b', 'c')  # character
x <- 1:100             # numeric
x <- c(1+0i, 3+5i)     # complex

# FYI: single quote and double quote: 
?Quotes


?seq
x <- seq(10, 20, by = 2) 
x
# [1] 10 12 14 16 18 20
x[3]
# [1] 14
x[0]


?rep

y <- rep(x = c(1, 2, 3), 2) 
y <- rep(x = c(1, 2, 3), times=2) 
yy <- rep(x = c(1, 2, 3), each=2) 
yy

y
# [1] 1 2 3 1 2 3
x
y
z <- c(x, y)
z
# [1] 10 12 14 16 18 20  1  2  3  1  2  3



# ======================================= 1.2 Matrix =======================================

# All elements in matrix have the same type
# Because matrices are actually vectors with two indices

x <- 1:9
x

matrix(x, nrow = 3, ncol = 3)
matrix(x, nrow = 3, ncol = 2)
matrix(x, nrow = 3, ncol = 4)

# byrow
matrix(x, nrow = 3, ncol = 3, byrow=T)


# rbind and cbind
# combine vectors to matrices
x <- 1:3
y <- 4:6

m1 <- rbind(x, y)
m1

m2<- cbind(x, y)
m2

matrix(1:9, nrow = 3, byrow = T)
m <- matrix(1:9, nrow = 3, byrow = T)
m

# subsetting
m[2, 2]


# transpose and inverse
# transpose
t(m)
# Don't know how to inverse a matrix?
# GOOGLE!
solve(m)    
# of course we cannot, m is singular
m[1, 1] = 10
m[2, 2] = 8
m
solve(m)


# ======================================= 1.3 List =======================================

rm(list = ls()) # clear variables


# different type of varialbes in one objects
l <- list("John", 12345, "Male")
l

# list with name
l <- list(name = "John", ID = 12345, gender = "Male")
l

l$name
l$ID

l[[1]]


unlist(l)


l <- list(name = c("John", "Mike"), ID = 12345, gender = "Male")
l

# ======================================= 1.4 summary  =======================================

# create variables
x <- 1:10                               # vector
y <- c('a', 'b', 'c')                   # vector
m <- matrix(x, nrow = 2, byrow = T)     # matrix
l <- list(numbers = x, chars = y)       # list

# subsetting
x[3]            # subsetting of a vector
m[2, 2]         # subsetting of a matrix
l[[1]]          # subsetting of a list
l[["numbers"]]
l$numbers


l[[1]][3]       # nested subsetting
l[[c(1, 3)]]



# data.frame
# factor

# =================================== 2 Control Structure ===================================
# =================================== 2.1 For loops =========================================

for (...) # some condition
{
    # statements
}

for (i in 1:5)
{
    print(i)    # every iteration i increments.
}

x <- 1:5
for(i in x)
{
    print(i)    # same thing
}

# example: calculate 1 + 2 + 3 + ... + 100 = 5050



result <- 0
for (i in 1:100)
{
    result = result + i
}
result




# nested for loops
m1 <- matrix(1:12, nrow = 3, byrow = T)
m2 <- matrix(NA, nrow = 3, ncol = 4)        # NA matrix
m1
m2
for (i in 1:3)      # i is row index
{
    for (j in 1:4)  # j is col index
    {
        m2[i, j] <- m1[i, j] * 10
    }
}
m2


# =================================== 2.2 if statement =========================================
x <- T
if (x)
{
    "x is TRUE"
}



x <- F
if (x)
{
    "x is TRUE"
} else {
    "x is FALSE"
}


x <- 1:10
if (length(x) > 15)
{
    print("x is a long array")
} else {
    print("x is short array")
}


# try to figure out the function ifelse()
# similar to x ? y : z in C/C++





# Exercise 1: for loop
# Multiples of 3 or 5

# If we list all the natural numbers below 10 
# that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
# The sum of these multiples is 23.
# Find the sum of all the multiples of 3 or 5 below 1000.

# result 234168


sum <- 0
for(i in 1:1000){
    if(i %% 3 == 0 | i %% 5 == 0){
        sum = sum + i
    }
}
sum



# =================================== 2.3 While loop =========================================


# while loops
while(...)  # condition
{
    # statement
}

result <- 0
while(TRUE)     # infinite loop
{
    print(result)
    result = result + 1
}


result <- 0
while(result < 1000)
{
    print(result)
    result = result + 1
}


result <- 0
sum <- 0
while (result <= 100)
{
    if (sum > 3000)     # break loop using if statement
    {
      break
    }    
    sum = sum + result
    print(sum)
    result = result + 1
}



# Exercise 2: Fibonacci:
# 0, 1, 1, 2, 3, 5, 8, 13.......
a <- 0
b <- 1
MAX = 1000
while (TRUE)
{
    print (a)
    if (a > MAX)
        break
    a <- b
    b <- a + b
}


a <- 0
b <- 1
MAX = 1000
while (TRUE)
{
    print (a)
    if (a > MAX)
        break
    tmp <- a
    a <- b
    b <- tmp + b
}
