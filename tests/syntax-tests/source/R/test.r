# take input from the user
num = as.integer(readline(prompt="Enter a number: "))
factorial = 1
# check is the number is negative, positive or zero
if(num < 0) {
print("Sorry, factorial does not exist for negative numbers")
} else if(num == 0) {
print("The factorial of 0 is 1")
} else {
for(i in 1:num) {
factorial = factorial * i
}
print(paste("The factorial of", num ,"is",factorial))
}

x <- 0
if (x < 0) {
print("Negative number")
} else if (x > 0) {
print("Positive number")
} else
print("Zero")

x <- 1:5
for (val in x) {
if (val == 3){
next
}
print(val)
}

x <- 1
repeat {
print(x)
x = x+1
if (x == 6){
break
}
}

`%divisible%` <- function(x,y)
{
if (x%%y ==0) return (TRUE)
else          return (FALSE)
}

switch("length", "color" = "red", "shape" = "square", "length" = 5)
[1] 5

recursive.factorial <- function(x) {
if (x == 0)    return (1)
else           return (x * recursive.factorial(x-1))
}

pow <- function(x, y) {
# function to print x raised to the power y
result <- x^y
print(paste(x,"raised to the power", y, "is", result))
}

A <- read.table("x.data", sep=",",
                col.names=c("year", "my1", "my2"))
nrow(A)                                 # Count the rows in A

summary(A$year)                        

A$newcol <- A$my1 + A$my2               # Makes a new
newvar <- A$my1 - A$my2                 # Makes a 
A$my1 <- NULL                           # Removes 
str(A)
summary(A)
library(Hmisc)          
contents(A)
describe(A)

set.seed(102)                           # This yields a good illustration.
x <- sample(1:3, 15, replace=TRUE)
education <- factor(x, labels=c("None", "School", "College"))
x <- sample(1:2, 15, replace=TRUE)
gender <- factor(x, labels=c("Male", "Female"))
age <- runif(15, min=20,max=60)

D <- data.frame(age, gender, education)
rm(x,age,gender,education)
print(D)

# Table about education
table(D$education)

# Table about education and gender --
table(D$gender, D$education)
# Joint distribution of education and gender --
table(D$gender, D$education)/nrow(D)

# Add in the marginal distributions also
addmargins(table(D$gender, D$education))
addmargins(table(D$gender, D$education))/nrow(D)

# Generate a good LaTeX table out of it --
library(xtable)
xtable(addmargins(table(D$gender, D$education))/nrow(D),
       digits=c(0,2,2,2,2))  

by(D$age, D$gender, mean)
by(D$age, D$gender, sd)
by(D$age, D$gender, summary)

a <- matrix(by(D$age, list(D$gender, D$education), mean), nrow=2)
rownames(a) <- levels(D$gender)
colnames(a) <- levels(D$education)
print(a)
print(xtable(a))

dat <- read.csv(file = "files/dataset-2013-01.csv", header = TRUE)
interim_object <- data.frame(rep(1:100, 10),
                             rep(101:200, 10),
                             rep(201:300, 10))
object.size(interim_object) 
rm("interim_object") 
ls() 
rm(list = ls())

vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)
array1 <- array(c(vector1,vector2),dim = c(3,3,2))
vector3 <- c(9,1,0)
vector4 <- c(6,0,11,3,14,1,2,6,9)
array2 <- array(c(vector1,vector2),dim = c(3,3,2))
matrix1 <- array1[,,2]
matrix2 <- array2[,,2]
result <- matrix1+matrix2
print(result)

column.names <- c("COL1","COL2","COL3")
row.names <- c("ROW1","ROW2","ROW3")
matrix.names <- c("Matrix1","Matrix2")
result <- array(c(vector1,vector2),dim = c(3,3,2),dimnames = list(row.names,
   column.names, matrix.names))
print(result[3,,2])
print(result[1,3,1])
print(result[,,2])

# Load the package required to read JSON files.
library("rjson")
result <- fromJSON(file = "input.json")
print(result)

x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)
relation <- lm(y~x)
print(relation)

relation <- lm(y~x)
png(file = "linearregression.png")
plot(y,x,col = "blue",main = "Height & Weight Regression",
abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "Weight in Kg",ylab = "Height in cm")
dev.off()

data <- c("East","West","East","North","North","East","West","West","West","East","North")
print(data)
print(is.factor(data))
factor_data <- factor(data)
print(factor_data)
print(is.factor(factor_data))

v <- c(7,12,28,3,41)

# Give the chart file a name.
png(file = "line_chart_label_colored.jpg")
plot(v,type = "o", col = "red", xlab = "Month", ylab = "Rain fall", main = "Rain fall chart")
