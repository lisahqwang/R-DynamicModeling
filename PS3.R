#PS3
#1d------------------
x <- seq(0, 100, by = 0.1)
plot(x, y = (0.0002 + 0.01*exp(-x/10)), type = "l", xlab="rainfall in mm", ylab="b1", 
        col="blue", lwd=1, main="Plot of B1 vs. rain fall in mm")
legend(x = "topright", legend = c("rainfall"), col = c("blue"), lty = 1)

#1e-------------------------
#Write a function that computes b1 for any given amount of rainfall
Calcb1 <- function(rain){
  b1 <- 0.0002 + 0.01*exp(-rain/10)
  return(b1)
}
  
Calcb1(34) # testing 1 2 
# >0.0005337327

#Computes K for any values of b0, b1, and d0 
Carrying <- function(b0, b1, d0){
  K <- (b0 - b0*d0 - d0)/(b1-(b1*d0))
  return(K)
}

#Calculate K for 10 mm of rain, 60 mm of rain, and 100 mm of rain

K <- Carrying(b0 = 0.6, b1 = Calcb1(10), d0 = 0.125)
print("The K for 10mm of rain is 117.85")
K <- Carrying(b0 = 0.6, b1 = Calcb1(60), d0 = 0.125)
print("The K for 60mm of rain is 2033.667")
K <- Carrying(b0 = 0.6, b1 = Calcb1(100), d0 = 0.125)
print("The K for 100mm of rain is 2280.537")

rain1 <- seq(0, 200, by = 1)
plot(rain1, y = Carrying(b0 = 0.6, b1 = Calcb1(rain1), d0 = 0.125), 
     type = "l", xlab="rainfall (mm)", ylab="K", 
     col="red", lwd=1, main="Graph of K vs. rainfall")

#Question 1f--------------

calcRainfall <- function(year){
  rain <- 100 - 2*(year)
  return(rain)
}

Calcb1 <- function(rain){
  b1 <- 0.0002 + 0.01*exp(-rain/10)
  return(b1)
}

Carrying <- function(b0, b1, d0){
  K <- (b0 - b0*d0 - d0)/(b1-(b1*d0))
  return(K)
}

#Above are the same functions as previous questions 
r_d <- 0.4
popVector <- rep(0, 81) #I decided that 80 would be capturing the whole dynamic
popVector[1] <- 500


for(year in seq(1, 80)){
  popVector[year+1] = popVector[year] + 
    (r_d*(popVector[year])*(1- (popVector[year]/ Carrying(b0 = 0.6, b1 = Calcb1(calcRainfall((year))), d0 = 0.125) )))
}
plot(popVector, type = "o", xlab="time (yrs)", ylab="N", 
     col="black", lwd=1, main="Graph of N vs. time (yrs)" )

#1g-----------

Calcb1 <- function(rain){
  b1 <- 0.0002 + 0.01*exp(-rain/10)
  return(b1)
}

Carrying <- function(b0, b1, d0){
  K <- (b0 - b0*d0 - d0)/(b1-(b1*d0))
  return(K)
}

rain2 <- rnorm(n = 1, mean = 60, sd = 10)
r_d <- 0.4
popVector <- rep(0, 51) #changed to 50 iterations 
popVector[1] <- 500


for(year in seq(1, 50)){
  popVector[year+1] = popVector[year] + 
    (r_d*(popVector[year])*(1- (popVector[year]/ Carrying(b0 = 0.6, b1 = Calcb1(rain2), d0 = 0.125) )))
}
plot(popVector, type = "o", ylim = c(0,3000), xlab="time (yrs)", ylab="N", 
     col="brown", lwd=1, main="Graph of N vs. time (yrs)" )


rain2 <- rnorm(n = 1, mean = 60, sd = 10)
r_d <- 0.4
popVector <- rep(0, 51) #changed to 50 iterations 
popVector[1] <- 500


for(year in seq(1, 50)){
  popVector[year+1] = popVector[year] + 
    (r_d*(popVector[year])*(1- (popVector[year]/ Carrying(b0 = 0.6, b1 = Calcb1(rain2), d0 = 0.125) )))
}
lines(popVector, type = "o", xlab="time (yrs)", ylab="N", 
      col="red", lwd=1, main="Graph of N vs. time (yrs)")


rain2 <- rnorm(n = 1, mean = 60, sd = 10)
r_d <- 0.4
popVector <- rep(0, 51) #changed to 50 iterations 
popVector[1] <- 500


for(year in seq(1, 50)){
  popVector[year+1] = popVector[year] + 
    (r_d*(popVector[year])*(1- (popVector[year]/ Carrying(b0 = 0.6, b1 = Calcb1(rain2), d0 = 0.125) )))
}
lines(popVector, type = "o", xlab="time (yrs)", ylab="N", 
      col="blue", lwd=1, main="Graph of N vs. time (yrs)")


rain2 <- rnorm(n = 1, mean = 60, sd = 10)
r_d <- 0.4
popVector <- rep(0, 51) #changed to 50 iterations 
popVector[1] <- 500


for(year in seq(1, 50)){
  popVector[year+1] = popVector[year] + 
    (r_d*(popVector[year])*(1- (popVector[year]/ Carrying(b0 = 0.6, b1 = Calcb1(rain2), d0 = 0.125) )))
}
lines(popVector, type = "o", xlab="time (yrs)", ylab="N", 
      col="yellow", lwd=1, main="Graph of N vs. time (yrs)")


rain2 <- rnorm(n = 1, mean = 60, sd = 10)
r_d <- 0.4
popVector <- rep(0, 51) #changed to 50 iterations 
popVector[1] <- 500


for(year in seq(1, 50)){
  popVector[year+1] = popVector[year] + 
    (r_d*(popVector[year])*(1- (popVector[year]/ Carrying(b0 = 0.6, b1 = Calcb1(rain2), d0 = 0.125) )))
}
lines(popVector, type = "o", xlab="time (yrs)", ylab="N", 
      col="gray", lwd=1, main="Graph of N vs. time (yrs)")

legend(x = "bottomright", legend = c("run1", "run2", "run3", "run4", "run5"), col = c("brown", 
      "red", "blue", "yellow", "grey"), lty = 1)

#------------------------revised 1g
pop <- rep(0, 51)
r_d <- 0.4
pop[1]<- 500 

for(year in seq_along(pop)){
  pop[year +1] <- pop[year] + 
    ((r_d*pop[year]) * (1- (pop[year] / Carrying(b0 = 0.6, b1 = Calcb1
                                                 (rnorm(1, 60, 10)), d0 = 0.125))))
}

plot(pop, xlab = "Time in years", ylab = "Population size", col = "brown", type = "l")


for(year in seq_along(pop)){
  pop[year +1] <- pop[year] + 
    ((r_d*pop[year]) * (1- (pop[year] / Carrying(b0 = 0.6, b1 = Calcb1
                                                 (rnorm(1, 60, 10)), d0 = 0.125))))
}

lines(pop, xlab = "Time in years", ylab = "Population size", col = "red", type = "l")


for(year in seq_along(pop)){
  pop[year +1] <- pop[year] +
    ((r_d*pop[year]) * (1- (pop[year] / Carrying(b0 = 0.6, b1 = Calcb1
                                                 (rnorm(1, 60, 10)), d0 = 0.125))))
}

lines(pop, xlab = "Time in years", ylab = "Population size", col = "blue", type = "l")

for(year in seq_along(pop)){
  pop[year +1] <- pop[year] + 
    ((r_d*pop[year]) * (1- (pop[year] / Carrying(b0 = 0.6, b1 = Calcb1
                                                 (rnorm(1, 60, 10)), d0 = 0.125))))
}

lines(pop, xlab = "Time in years", ylab = "Population size", col = "yellow", type = "l")

for(year in seq_along(pop)){
  pop[year +1] <- pop[year] + 
    ((r_d*pop[year]) * (1- (pop[year] / Carrying(b0 = 0.6, b1 = Calcb1
                                                 (rnorm(1, 60, 10)), d0 = 0.125))))
}

lines(pop, xlab = "Time in years", ylab = "Population size", col = "grey", type = "l")

legend(x = "bottomright", legend = c("run1", "run2", "run3", "run4", "run5"), 
       col = c("brown", 
                                                                                    
                 "red", "blue", "yellow", "grey"), lty = 1)

#1h------------------
#writing a batch script to repeat stochastic run many times 


StochasticRun <- function(numRuns){
  counter <- 0
  meancounter <- 0 
  while (counter < numRuns) {
  rain2 <- rnorm(n = 1, mean = 60, sd = 10)
  
  
  r_d <- 0.4
  popVector <- rep(0, 51) #changed to 50 iterations 
  popVector[1] <- 500
  
  for(year in seq(1, 50)){
    popVector[year+1] = popVector[year] + 
      (r_d*(popVector[year])*(1- (popVector[year]/ Carrying(b0 = 0.6, b1 = Calcb1(rain2), d0 = 0.125) )))
    }
  
  if(counter == 0){
    plot(popVector, type = "o", ylim = c(0,3000), xlab="time (yrs)", ylab="N", 
         col="black", lwd=1, main="Graph of N vs. time (yrs)" )
  }
  else{
    lines(popVector, type = "o", xlab="time (yrs)", ylab="N", 
          col="blue", lwd=1, main="Graph of N vs. time (yrs)" )
  }
  
  counter = counter + 1
  meancounter = meancounter + popVector[30]
  }
  return(meancounter/numRuns)
}

StochasticRun(20)# so that's 20 of them 

#Calculate the carrying capacity that corresponds 
#to the mean level of rainfall, and add it
#to the plot as a red line (use the abline command) 

#Response: the mean level of rainfall is 60mm

value <- Carrying(b0 = 0.6, b1 = Calcb1(60), d0 = 0.125)
abline(h = value, col = "red")
value


#calculate the mean population size across stochastic runs 
#response: at the very end I included the line return(meancounter/numRuns)
#this line would account for the number of runs and the average N for all runs.

abline(h = StochasticRun(20), col = "orange")

#Does the mean population size match your calculated carrying capacity?
#Why or why not?
#response: the mean population size does not match my calculated carrying 
#capacity because the deterministic value is a predicted mean, whereas the
#stochastic model has varying rainfall, and is subject to change. One is
#higher and one is lower, and they shouldn't match!

#----------------------------revised 
plot(0, type = "n", xlim= c(0, length(pop)), ylim = c(0, 2200),
     xlab = "time in years", ylab = "population size")

stochastic <- function(init){
  pop <- rep(0, 50)
  pop[1] <- init

Calcb1 <- function(rain){
  b1 <- 0.0002 + 0.01*exp(-rain/10)
  return(b1)
}

Carrying <- function(b0, b1, d0){
  K <- (b0 - b0*d0 - d0)/(b1-(b1*d0))
  return(K)
}

for(year in seq_along(pop)){
  pop[year +1] <- pop[year] +
    ((r_d*pop[year]) * (1- (pop[year] / Carrying(b0 = 0.6, b1 = Calcb1
                                                 (rnorm(1, 60, 10)), d0 = 0.125))))
}

return (lines(pop, xlab = "Time in years", ylab = "Population size", type = "l"))
}

for (i in seq(1,20)){
  stochastic(500)
  abline(h = mean(pop), col = "blue")
}

abline(h = Carrying(0.6, b1 = Calcb1(mean(rep(rnorm(1,60, 10), 50))), 0.125), col = "red")

print(mean(pop)) #for this particular run, 1779
print(Carrying(0.6, b1 = Calcb1(mean(rep(rnorm(1,60, 10), 50))), 0.125)) 
#for this particular run, 2092


#Question 2d -------------------------
p <- seq(0, 1, by = 0.001)
Pfunc <- function(m){
  result <- (m*p*(1-p))
}

efunc <- function(e){
  result1 <- e*p
}

plot(p, Pfunc(3), type ="l", col = "blue", xlab="p", ylab="dp/dt", main = "dpdt vs. p")
lines(p, efunc(2), type ="l", col = "red", xlab="p", ylab="dp/dt", main = "dpdt vs. ")
legend(x = "bottomright", legend = c("mp(1-p)", "ep"), col = c("blue", "red"), lty = 1)

Pfunction <- function(m, e){
  result2 <- (m*p*(1-p)) - e*p
}
plot(p, Pfunction(3,2), type ="l", col = "brown", xlab="p", ylab="dp/dt", main = "dpdt vs. p")
abline(h = 0, col="blue")
legend(x = "bottomleft", legend = c("dp/dt", "y = 0"), col = c("brown", "blue"), lty = 1)
# for visualization purposes, show intersection 


# Question 2e
install.packages("deSolve")
library("deSolve")
init <- c(p = 0.1)
p <- seq(0, 20)
parms <- c(m = 3, e = 2)
PODE <- function(p, init, parms){
  derivs <- (parms["m"]*init["p"]*(1-init["p"])) - (parms["e"]*init["p"])
  return(list(derivs))
}
POutput <- lsoda(init, p, PODE, parms)
head(POutput)
plot(POutput[,1], POutput[,2], xlim = c(0, 20), ylim = c(0, 1), col = "black", type ="l",
     xlab="time", ylab="P", main = "P vs. time")


init <- c(p = 0.15)
p <- seq(0, 20)
parms <- c(m = 3, e = 2)
PODE <- function(p, init, parms){
  derivs <- (parms["m"]*init["p"]*(1-init["p"])) - (parms["e"]*init["p"])
  return(list(derivs))
}
POutput <- lsoda(init, p, PODE, parms)
lines(POutput[,1], POutput[,2], col = "red", type ="l", xlab="time", ylab="P", 
      main = "P vs. time")


init <- c(p = 0.3)
p <- seq(0, 20)
parms <- c(m = 3, e = 2)
PODE <- function(p, init, parms){
  derivs <- (parms["m"]*init["p"]*(1-init["p"])) - (parms["e"]*init["p"])
  return(list(derivs))
}
POutput <- lsoda(init, p, PODE, parms)
lines(POutput[,1], POutput[,2], col = "orange", type ="l", xlab="time", ylab="P", 
      main = "P vs. time")



init <- c(p = 0.45)
p <- seq(0, 20)
parms <- c(m = 3, e = 2)
PODE <- function(p, init, parms){
  derivs <- (parms["m"]*init["p"]*(1-init["p"])) - (parms["e"]*init["p"])
  return(list(derivs))
}
POutput <- lsoda(init, p, PODE, parms)
lines(POutput[,1], POutput[,2], col = "yellow", type ="l", xlab="time", 
      ylab="P", main = "P vs. time")


init <- c(p = 0.55)
p <- seq(0, 20)
parms <- c(m = 3, e = 2)
PODE <- function(p, init, parms){
  derivs <- (parms["m"]*init["p"]*(1-init["p"])) - (parms["e"]*init["p"])
  return(list(derivs))
}
POutput <- lsoda(init, p, PODE, parms)
lines(POutput[,1], POutput[,2], col = "green", type ="l", xlab="time", 
      ylab="P", main = "P vs. time")


init <- c(p = 0.7)
p <- seq(0, 20)
parms <- c(m = 3, e = 2)
PODE <- function(p, init, parms){
  derivs <- (parms["m"]*init["p"]*(1-init["p"])) - (parms["e"]*init["p"])
  return(list(derivs))
}
POutput <- lsoda(init, p, PODE, parms)
lines(POutput[,1], POutput[,2], col = "blue", type ="l", xlab="time",
      ylab="P", main = "P vs. time")


init <- c(p = 0.9)
p <- seq(0, 20)
parms <- c(m = 3, e = 2)
PODE <- function(p, init, parms){
  derivs <- (parms["m"]*init["p"]*(1-init["p"])) - (parms["e"]*init["p"])
  return(list(derivs))
}
POutput <- lsoda(init, p, PODE, parms)
lines(POutput[,1], POutput[,2], col = "purple", type ="l", xlab="time", 
      ylab="P", main = "P vs. time")



init <- c(p = 1)
p <- seq(0, 20)
parms <- c(m = 3, e = 2)
PODE <- function(p, init, parms){
  derivs <- (parms["m"]*init["p"]*(1-init["p"])) - (parms["e"]*init["p"])
  return(list(derivs))
}
POutput <- lsoda(init, p, PODE, parms)
lines(POutput[,1], POutput[,2], col = "brown", type ="l", xlab="time", 
      ylab="P", main = "P vs. time")

legend(x = "topright", legend = c("p = 0.1", "p = 0.15", "p = 0.3", 
                                  "p = 0.45", "p = 0.55", "p = 0.7", "p = 0.9", "p = 1"),
       col = c("black", "red", "orange", "yellow", "green", "blue", 
               "purple", "brown"), lty = 1)

head(POutput[15,]) #By running head(POutput[15,]), I estimated that
#the curve reached equilibrium point around timestamp 15, and it give us
#the correct value of 0.33335 as predicted by calculating P* = 1-e/m


# 2f. 
p <- seq(0, 1, by = 0.001)
Pfunc <- function(m){
  result <- (m*p*(1-p))
}

efunc <- function(e){
  result1 <- e*p
}

plot(p, Pfunc(4), type ="l", col = "blue", xlab="p", ylab="dp/dt", main = "dpdt vs. p")
lines(p, efunc(4), type ="l", col = "red", xlab="p", ylab="dp/dt", main = "dpdt vs. ")
legend(x = "bottomright", legend = c("mp(1-p)", "ep"), col = c("blue", "red"), lty = 1)

Pfunction <- function(m, e){
  result2 <- (m*p*(1-p)) - e*p
}
plot(p, Pfunction(4,4), type ="l", col = "brown", xlab="p", ylab="dp/dt", main = "dpdt vs. p")
abline(h = 0, col="blue")
legend(x = "bottomleft", legend = c("dp/dt", "y = 0"), col = c("brown", "blue"), lty = 1)

init <- c(p = 0.3) 
p <- seq(0, 10, 0.01)
parms <- c(m = 4, e = 4)
PODE <- function(p, init, parms){
  derivs <- (parms["m"]*init["p"]*(1-init["p"])) - (parms["e"]*init["p"])
  return(list(derivs))
}
POutput <- lsode(init, p, PODE, parms)
head(POutput)
plot(POutput[,1], POutput[,2], xlim = c(0, 10), ylim = c(0, 1), col = "black", type ="l",
     xlab="time", ylab="P", main = "P vs. time")

legend(x = "right", legend = c("m = 4, e = 4"), col = c("black"), lty = 1)

