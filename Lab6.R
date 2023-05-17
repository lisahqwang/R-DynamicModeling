#Lab 6 - Lisa Wang 
LL <- matrix(c(0.5, 1.4, 0.4, 0), nrow = 2, byrow = T)
out <- eigen(LL)
out #outputs values and vectors 

out$values 
out$vectors 
out$values[2]
out$vectors[,2] # the second column is the second eigenvector 

#Exercise 1-----------
LM <- matrix(c(0.5, 1.4, 0, 0.4, 0, 0, 0, 0.8, 0), nrow = 3, byrow = T)
out <- eigen(LM)
out$values
out$vectors

#Exercise 2------------
Largest_eigenvalue <- max(abs(out$values))
Largest_eigenvalue

Corresponding_eigenvector <- out$vectors[,which.max(abs(out$values))]
Corresponding_eigenvector

#eigenvectors should be normalized to sum to 1, to get stable age distribution

out <- eigen(LM) 
v1 <- out$vectors[,which.max(abs(out$values))]
v1 / sum(v1) #normalized

#Exercise 3--------------

out$vectors[,1]/sum(out$vectors[,1])
out$vectors[,2]/sum(out$vectors[,2])
out$vectors[,3]/sum(out$vectors[,3])

#the expected stable age distribution for this population, W_1 is 
# 0.5947333, 0.2289667, 0.1763000 
# we should expect the majority being in the first age class, about 60% 
# otherwise, an even split betweem stage 2 and 3 

#Exercise 4-------
n0 <- matrix(c(10,20,30), ncol = 1) #initial population vector 
L <- matrix(c(0.5, 1.4, 0, 0.4, 0, 0, 0, 0.8, 0), nrow = 3, byrow = T)

pop <- matrix(NA, ncol = 20, nrow = 3) 
pop[,1] <- n0 

#loop over time steps of interest
for(ii in 2:20){
  pop[,ii] <- L %*% pop[, ii -1]
}

plot(seq(1,20), pop[1,], ylim= (c(0,55)), type = "o", xlab = 'Time', ylab = 'Population Size', col = "red", main = 'Population vs. Time')
lines(seq(1,20), pop[2,], type = "o", xlab = 'Time', ylab = 'Population Size', col = "blue", main = 'Population vs. Time')
lines(seq(1,20), pop[3,], type = "o", xlab = 'Time', ylab = 'Population Size', col = "purple", main = 'Population vs. Time')
legend(x = "topleft", legend = c("age class 1", "age class 2", "age class 3"), col = c("red", "blue", "purple"), lty = 1)

#Exercise 5----
totalPop <- colSums(pop) #calculates total poplution sizes
juvProp <- pop[1,]/totalPop #calculates proportion of juveniles 
twoProp <- pop[2,]/totalPop
threeProp <- pop[3,]/totalPop
plot(seq(1,20), juvProp, ylim = (c(0, 1)),type = "o", xlab = 'Time', ylab = 'Proportion to total', col = "red", main = 'Prop to total vs. Time')
lines(seq(1,20), twoProp, type = "o", xlab = 'Time', ylab = 'Proportion to total', col = "blue", main = 'Prop to total vs. Time')
lines(seq(1,20), threeProp, type = "o", xlab = 'Time', ylab = 'Proportion to total', col = "purple", main = 'Prop to total vs. Time')
legend(x = "topleft", legend = c("age class 1", "age class 2", "age class 3"), col = c("red", "blue", "purple"), lty = 1)

abline(h = 0.5947333, lty = "dashed")
abline(h = 0.2289667, lty = "dashed")
abline(h = 0.1763000, lty = "dashed")

#The long term dynamics of this system match the predicted the stable age distribution. 
#the values converge to 0.6, 0.22, and 0.17 around time 8. 