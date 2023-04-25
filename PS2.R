#1a. 
x <- seq(15, 75, by = 5)

#1b.
x[3] <- x[3]^2
x[6] <- x[6]^2

#1c.
x[-7]

#1d. 
min(which(x > 42))

#1e. 
c(x[1:5], x[9:13])
c(x[-6:-8])
x[-c(6,7,8)]

#1f. 
c(-sort(-x[which(x> 45)]))

#Question 2a ----------------------------------

r <- 0.5 #setting r to 0.5
K <- 100 # carrying capacity to 50 
popVector <- rep(0, 11) # 10 iteration
popVector[1] <- 10 #default starting with 10 individuals

for(year in seq(1, 10)){
  popVector[year+1] = popVector[year]*exp(r*(1- (popVector[year]/K)))
}
# this is the function

plot(popVector, type = "o",xlab = "time in years", ylab = "Population size")
# plotting it 

#2b. stable equilibrium at n* = 0 -------------------------
r <- -0.3
K <- 100 
popVector <- rep(0, 11) 
popVector1 <- rep(0, 11)
popVector[1] <- 5 

for(year in seq(1, 10)){
  popVector[year+1] = popVector[year]*exp(r*(1- (popVector[year]/K)))
}

plot(popVector, type = "o",xlab = "time in years", ylab = "Population size", main = "n* = 0, Pop vs. time")
popVector1[1] <- 7 #trying different initial population 

for(year in seq(1, 10)){
  popVector1[year+1] = popVector1[year]*exp(r*(1- (popVector1[year]/K)))
}

lines(popVector1, type = "o", col = "red")
legend(x = "bottomright", legend = c("N0 = 5", "N0 = 7"), col = c("black", "red"), lty = 1)


#response: the value of r that seems to give stable equilibrium at n* = 0 
#is any negative rate, so that population tends to 0. 

#2c. stable equilibrium at n* = K----------------------------

r <- 0.5
K <- 100 
popVector <- rep(0, 11) 
popVector[1] <- 10 

for(year in seq(1, 10)){
  popVector[year+1] = popVector[year]*exp(r*(1- (popVector[year]/K)))
}

plot(popVector, type = "o",xlab = "time in years", ylab = "Population size", main = "n* = K, Pop vs. time")

popVector[1] <- 20 #trying different initial population
for(year in seq(1, 10)){
  popVector[year+1] = popVector[year]*exp(r*(1- (popVector[year]/K)))
}
lines(popVector, type = "o", col = "green")
legend(x = "bottomright", legend = c("N0 = 10", "N0 = 20"), col = c("black", "green"), lty = 1)

#response: the value of r that seems to give stable equilibrium at n* = K 
# is 0.5, any value greater than 1.3 is going to give oscillations

#2d. stable equilibrium, decaying oscillations -------------------------

r <- 1.6
K <- 100 
popVector <- rep(0, 21) 
popVector[1] <- 15

for(year in seq(1, 20)){
  popVector[year+1] = popVector[year]*exp(r*(1- (popVector[year]/K)))
}

plot(popVector, type = "o",xlab = "time in years", ylab = "Population size", main = "n* = K, Pop vs. time")

popVector[1] <- 25 #trying different initial population
for(year in seq(1, 20)){
  popVector[year+1] = popVector[year]*exp(r*(1- (popVector[year]/K)))
}
lines(popVector, type = "o", col = "purple")
legend(x = "bottomright", legend = c("N0 = 15", "N0 = 25"), col = c("black", "purple"), lty = 1)

#response: the value of r that seems to give stable equilibrium and decaying oscillations
# is 1.8, converges eventually 

#2e. stable equilibrium, 2 or 4 cycle ----------------------------------

r <- 2.2
K <- 100 
popVector <- rep(0, 41) 
popVector[1] <- 15

for(year in seq(1, 40)){
  popVector[year+1] = popVector[year]*exp(r*(1- (popVector[year]/K)))
}

plot(popVector, type = "o",xlab = "time in years", ylab = "Population size", main = "n* = K, Pop vs. time")

popVector[1] <- 25 #trying different initial population
for(year in seq(1, 40)){
  popVector[year+1] = popVector[year]*exp(r*(1- (popVector[year]/K)))
}
lines(popVector, type = "o", col = "brown")
legend(x = "bottomright", legend = c("N0 = 15", "N0 = 25"), col = c("black", "brown"), lty = 1)

#response: the value of r that seems to give stable equilibrium and regular oscillation
# is 2.2

#2f. Chaos. Demonstrate the ‘extreme sensitivity to initial conditions’ that
#is a hallmark of chaos, by showing how the dynamics of nt can diverge sharply for two
#values of n0 that are very close together.

r <- 3
K <- 100 
popVector <- rep(0, 41) 
popVector[1] <- 10

for(year in seq(1, 40)){
  popVector[year+1] = popVector[year]*exp(r*(1- (popVector[year]/K)))
}

plot(popVector, type = "o",xlab = "time in years", ylab = "Population size", main = "n* = K, Pop vs. time")

popVector[1] <- 13 #trying different initial population
for(year in seq(1, 40)){
  popVector[year+1] = popVector[year]*exp(r*(1- (popVector[year]/K)))
}
lines(popVector, type = "o", col = "brown")
legend(x = "bottomright", legend = c("N0 = 5", "N0 = 7"), col = c("black", "brown"), lty = 1)

#response: the graphs at the end diverges to black line being 50 and the brown line 
#being up to 240. This is demonstrating chaos or extreme sensitivity to initial conditions 
#because we know initial population 5 and 7 are very close to each other
#however, these diffeqs diverges by oscillation

#Question 3: 
install.packages("deSolve")
library("deSolve")

init <- c(S = 9999, I = 1, C = 0, R = 0)
times <- seq(0, 30, 1/52) # here I put 1/52 because weeks 
parms <- c(beta = 0.8, epsilon = 0.03, gamma = 0.25, 
           alpha = 1/52, p = 0.1, sigma = 1/260, theta = 1/260,
           lambda = 1/52)

EpiODE <- function(tt, init, parms){
  dSdt = (-(parms["beta"]*init["S"])*(init["I"] + parms["epsilon"]*init["C"]))/(S + R + I + C)
  + (parms["theta"]*init["S"]) - init["S"]*parms["sigma"]
  
  dIdt = ((parms["beta"]*init["S"])*(init["I"] + parms["epsilon"]*init["C"]))/(S + R + I + C)
  - (parms["gamma"]*init["I"]) - (parms["sigma"]*init["I"]) - (parms["lambda"]*init["I"])
  
  dCdt = (parms["p"]*parms["gamma"]*init["I"]) - (parms["alpha"]*init["C"]) - (parms["sigma"]*init["C"])
  
  dRdt = (1-parms["p"])*parms["gamma"]*init["S"] + parms["alpha"]*init["C"] - init["R"]*parms["sigma"]
  return(list(c(dSdt, dIdt, dCdt, dRdt)))
}

EpiOutput <- lsode(init, times, EpiODE, parms)
head(EpiOutput)

plot(EpiOutput[,1], EpiOutput[,2], type = "o", xlab = "time, in years", ylab = "popSize", main = "PopSize vs. Time")
lines(EpiOutput[,1], EpiOutput[,3], type = "o", xlab = "time", ylab = "popSize", col = "red")
lines(EpiOutput[,1], EpiOutput[,4], type = "o", xlab = "time", ylab = "popSize", col = "green")
lines(EpiOutput[,1], EpiOutput[,5], type = "o", xlab = "time", ylab = "popSize", col = "blue")

legend(x = "bottomright", legend = c("S", "I", "C", "R"), col = c("black", "red", "green", "blue"), lty = 1)
# Now we set birth and death rates to 0 

init <- c(S = 9999, I = 1, C = 0, R = 0)
times <- seq(0, 30, 1/52) # here I put 1/52 because weeks 
parms <- c(beta = 0.8, epsilon = 0.03, gamma = 0.25, 
           alpha = 1/52, p = 0.1, sigma = 0, theta = 0,
           lambda = 0)

EpiODE1 <- function(tt, init, parms){
  dSdt = (-(parms["beta"]*init["S"])*(init["I"] + parms["epsilon"]*init["C"]))/(S + R + I + C)
  + (parms["theta"]*init["S"]) - init["S"]*parms["sigma"]
  
  dIdt = ((parms["beta"]*init["S"])*(init["I"] + parms["epsilon"]*init["C"]))/(S + R + I + C)
  - (parms["gamma"]*init["I"]) - (parms["sigma"]*init["I"]) - (parms["lambda"]*init["I"])
  
  dCdt = (parms["p"]*parms["gamma"]*init["I"]) - (parms["alpha"]*init["C"]) - (parms["sigma"]*init["C"])
  
  dRdt = (1-parms["p"])*parms["gamma"]*init["S"] + parms["alpha"]*init["C"] - init["R"]*parms["sigma"]
  return(list(c(dSdt, dIdt, dCdt, dRdt)))
}

EpiOutput <- lsode(init, times, EpiODE1, parms)
head(EpiOutput)

plot(EpiOutput[,1], EpiOutput[,2], type = "l", xlab = "time, in years", ylab = "popSize", main = "PopSize vs. Time")
lines(EpiOutput[,1], EpiOutput[,3], type = "l", xlab = "time", ylab = "popSize", col = "red")
lines(EpiOutput[,1], EpiOutput[,4], type = "l", xlab = "time", ylab = "popSize", col = "green")
lines(EpiOutput[,1], EpiOutput[,5], type = "l", xlab = "time", ylab = "popSize", col = "blue")



#Response: the biological significance is that the difference is so miniscule that
#we can not noticeably see 6 week's change (because 52*5 = 260) in the SIR model. 
#So without the demographic turnover, we would see similar graphs. 

#part g)--------------------------------------------



init <- c(S = 9999, I = 1, C = 0, R = 0, D =0)
times <- seq(0, 30, 1/52) # here I put 1/52 because weeks 
parms <- c(beta = 0.8, epsilon = 0.03, gamma = 0.25, 
           alpha = 1/52, p = 0.1, sigma = 1/260, theta = 1/260,
           lambda = 1/52)

EpiODE1 <- function(tt, init, parms){
  dSdt = (-(parms["beta"]*init["S"])*(init["I"] + parms["epsilon"]*init["C"]))/(S + R + I + C)
  + (parms["theta"]*init["S"]) - init["S"]*parms["sigma"]
  
  dIdt = ((parms["beta"]*init["S"])*(init["I"] + parms["epsilon"]*init["C"]))/(S + R + I + C)
  - (parms["gamma"]*init["I"]) - (parms["sigma"]*init["I"]) - (parms["lambda"]*init["I"])
  
  dCdt = (parms["p"]*parms["gamma"]*init["I"]) - (parms["alpha"]*init["C"]) - (parms["sigma"]*init["C"])
  
  dRdt = (1-parms["p"])*parms["gamma"]*init["S"] + parms["alpha"]*init["C"] - init["R"]*parms["sigma"]
  
  dDdt = parms["lambda"]*init["I"]
  
  return(list(c(dSdt, dIdt, dCdt, dRdt, dDdt)))
}

EpiOutput <- lsode(init, times, EpiODE1, parms)
head(EpiOutput)

plot(EpiOutput[,1], EpiOutput[,2], type = "l", xlab = "time, in years", ylab = "popSize", main = "PopSize vs. Time")
lines(EpiOutput[,1], EpiOutput[,3], type = "l", xlab = "time", ylab = "popSize", col = "red")
lines(EpiOutput[,1], EpiOutput[,4], type = "l", xlab = "time", ylab = "popSize", col = "green")
lines(EpiOutput[,1], EpiOutput[,5], type = "l", xlab = "time", ylab = "popSize", col = "blue")
lines(EpiOutput[,1], EpiOutput[,6], type = "l", xlab = "time", ylab = "popSize", col = "brown")


#Question 4-----------------
#a. I decided to translate this question into an ODE model

init <- c(P1 = 500, P2 = 500, E = 1000)
times <- seq(0, 50, 0.5) 
parms <- c(beta1 = 0.5, beta2 = 0.6, beta3 = 0.4, alpha1 = 0.4, 
           alpha2 = 0.25, alpha3 = 0.3, r1 = 0.2, r2 = 0.8, s1 = 0.3)

predODE <- function(tt, init, parms){
  
  dP1dt = parms["beta1"]*init["P1"] - parms["alpha1"]*init["P1"] + init["E"]*parms["r1"] - parms["s1"]*init["P1"]
  dP2dt = parms["s1"]*init["P1"] + parms["beta2"]*init["P2"] + parms["r2"]*init["E"] - parms["alpha2"]*init["P2"]
  dEdt = parms["beta3"]*init["E"] - parms["alpha3"]*init["E"] - parms["r1"]*init["E"] - parms["r2"]*init["E"]
  
  return(list(c(dP1dt, dP2dt, dEdt)))
}

predOutput <- lsode(init, times, predODE, parms)
head(predOutput)

plot(predOutput[,1], predOutput[,2], type = "o", xlab = "time, in years", ylab = "popSize", main = "PopSize vs. Time")
lines(predOutput[,1], predOutput[,3], type = "o", xlab = "time", ylab = "popSize", col = "red")
lines(predOutput[,1], predOutput[,4], type = "o", xlab = "time", ylab = "popSize", col = "green")

##here alpha's are death rates, beta's are birth rates, r is predation rate, s is the conversion of per predator killed to species
## baby generated coefficient. 
## what I am seeing is that usually the stronger predator's population will take off
## essentially, the prey and predator1 will diminish to 0, as shown in this graph. 
