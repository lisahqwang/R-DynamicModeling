data <- seq(1,20)
matrix(data, nrow = 4, ncol = 5)

prey <- c(1,4,7,3)
predator <- c(4,6,3,3)
rbind(prey, predator) #each row is either the prey or predator 
cbind(prey, predator) #each column is either the prey or predator

myMatrix <- matrix(seq(1,25), nrow = 5, ncol = 5)
myMatrix

#Exercise 1------
prey<- c(1,4,7,3)
predator <- c(4,6,3,3)
dataMatrix <- rbind(prey, predator) 
dataMatrix[which(dataMatrix <4)]

# outputs: 1 3 3 3 because it goes through every elememt in the matrix 
# of elements that are less than 4, which are 4 elements

#Exercise 2----
row1 <- c(4,6,9,8)
row2 <- c(5,5,2,2)
row3 <- c(12, 0.4, 0.1, 7) 
row4 <- c(8.1, 0, 0.3, 0)
L <- rbind(row1, row2, row3, row4)
L

#Exercise 3----
#Generate 2-by-2 matrices with random values between 0 and 1 
#R <- matrix(data = runif(4), nrow = 2)
#S <- matrix(data = runif(4), nrow = 2)
#R+S #add them together

R <- matrix(data = runif(4), nrow = 2) 
S <- matrix(data = runif(4), nrow = 2) 
R; S
R*S 
R %*% S
R^2 
R%^%2

#Exercise 4----
L <- dim(diag(4))
L <- diag(0, 4, 4) # guess what?
L[1,2] <- 1
L[1,3] <- 4
L[1,4] <- 2
L[2,1] <- 0.2 
L[3,2] <- 0.4
L[4,3] <- 0.3
L

#Exercise 5 -----
# x_bar(t+1) = L*x_bar(t)
#timesteps <- 10 
#popOutput <- matrix(data = NA, nrow = 4, ncol = timesteps +1)
#popOutput[,1] <- c(30, 50, 0, 10) 
#popOutput

L <- matrix(data = c(1, 3, 0.3, 0), nrow = 2)
timesteps <- 10
popOutput <- matrix(data = NA, nrow = 2, ncol = timesteps + 1)
popOutput[,1] <- c(1,2)
popOutput[,2] <- L%*%popOutput[,1]

for( i in seq(1,10)){
  popOutput[,i+1] <- L %*% popOutput[,i]
}
popOutput

plot(seq(0,10), popOutput[1,seq(1,11)], type = "o", xlab = 'Time', ylab = 'Population Size', col = "red", main = 'Population vs. Time')
lines(seq(0,10), popOutput[2,seq(1,11)], type = "o", xlab = 'Time', ylab = 'Population Size', col = "blue", main = 'Population vs. Time')
legend(x = "topright", legend = c("Init = 1", "Init = 2"), col = c("red", "blue"), lty = 1)


#Exercise 6-----
data <- c(0.5, 0.4, 1.4, 0)
L <- matrix(data, ncol = 2, nrow = 2)
L

#Exercise 7----
#done! 

#Exercise 8---
timesteps <- 20 
popOutput1 <- matrix(data = NA, nrow = 2, ncol = timesteps + 1)
popOutput1[,1] <- c(10,35)
popOutput1[,2] <- L%*%popOutput1[,1]

for( i in seq(1,20)){
  popOutput1[,i+1] <- L %*% popOutput1[,i]
}

plot(seq(0,20), popOutput1[1,seq(1,21)], type = "o", xlab = 'Time', ylab = 'Population Size', col = "red", main = 'Population vs. Time')
lines(seq(0,20), popOutput1[2,seq(1,21)], type = "o", xlab = 'Time', ylab = 'Population Size', col = "blue", main = 'Population vs. Time')
lines(seq(0, 20), colSums(popOutput1), type = "o", xlab = 'Time', ylab = 'Population Size', col = "purple", main = 'Population vs. Time')
legend(x = "topright", legend = c("Juvenile", "Adult", "Total"), col = c("red", "blue", "purple"), lty = 1)

# This fishery is growing at a linear rate after 5 years, and the juvenile fish are growing at a faster
# rate than the adult fish population. 

#Exercise 9 ----
# create 2 matrices, shift time, divide matrices, plot ratio
Jmatrix <- popOutput1[1,]
Jmatrix_adjusted <- Jmatrix[-length(Jmatrix)]
Jmatrixshifted <- Jmatrix[2:21]
popOutput1
Jmatrixshifted
Jmatrix_adjusted
ratio <- Jmatrixshifted / Jmatrix_adjusted
ratio

Amatrix <- popOutput1[2,]
Amatrix_adjusted <- Amatrix[-length(Amatrix)]
Amatrixshifted <- Amatrix[2:21]
Amatrixshifted 
Amatrix_adjusted
ratio1 <- Amatrixshifted / Amatrix_adjusted
ratio1

total <- colSums(popOutput1)
total_adjusted <- total[-length(total)]
totalshifted <- total[2:21]
totalshifted 
total_adjusted
ratio2 <- totalshifted / total_adjusted
ratio2

plot(seq(1,20), ratio, col = "red", type = "o", xlab = 'time in years', ylab = 'N_t+1 / N_t', main = "Ratios")
lines(seq(1,20), ratio1, col = "blue", type = "o", xlab = 'time in years', ylab = 'N_t+1 / N_t', main = "Ratios")
lines(seq(1,20), ratio2, col = "purple", type = "o", xlab = 'time in years', ylab = 'N_t+1 / N_t', main = "Ratios")
legend(x = "topright", legend = c("Juvenile_ratio", "Adult_ratio", "Total_ratio"), col = c("red", "blue", "purple"), lty = 1)
