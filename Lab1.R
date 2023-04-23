#Lab 1 
# Exercise 1: 
aa <- 1 
bb <- 3
cc <- 4 

aa <- bb/cc 
cc <- aa 
bb <- cc+aa 
bb

# Exercise 1's bb value should be 1.5 
#Exercise 2 ----
vector1 <-c(1,2,3,4) 
vector2 <- c(5,6,7) 
vector1 - vector2 

#exercise 2 answer: the output is element by element 
#subtraction, but the additional element of vector 2 
#returns a -1 (negative number) to signify invalid element subtraction

#Exercise 3 ----
vec1 <- c(seq(0, 100, by = 4))

#Exercise 4 ----
sum(vec1)

#Exercise 5 ----
length(vec1)

#Exercise 6 ----
vec2 <- rep("happy", 15)

#Exercise 7
sub1 <- rep("5", 100) 
sub2 <- rep("6", 34)
vec3 <- c(sub1, sub2)
vec3

#Exercise 8 
x <- seq(from = -3, to = 3, by = 0.5)
y <- sin(x) 
plot (x,y)

#Exercise 9 
plot(x,y,type = "b")
# plot different figures on a grid
# par(mfrow = c(num_rows, num_cols))

#Exercise 10
par(mfrow = c(2,1))
x <- seq(from = -10, to = 10, by = 1)
y1 <- x^4 
y2 <- x^3 + x
plot(x, y1, type = "b", col = "green", xlab = "X", ylab = "Y", main = "X vs. Y1") 
plot(x, y2, type = "o", col = "red", xlab = "X", ylab = "Y", main = "X vs. Y2")

#where two plots are together
plot(x, y1, type = "b", col = "green", xlab = "X", ylab = "Y", main = "X vs. Y1 and Y2") 
lines(x, y2, type = "o", col = "red")
legend(x = "right", legend = c("Y1","Y2"), # add a legend to distinguish
      col = c("green", "red"), lty = 1)

