#Lab3 
## Exercise 1 
quad <- function(a,b,c) 
{
  xPlus <- (-b + sqrt(b^2 - 4*a*c))/(2*a) 
  xMinus <- (-b - sqrt(b^2 - 4*a*c))/(2*a)
  return(c(xPlus, xMinus))
}

#2x^2 + 15x + 5 = 0 ; a = 2, b = 15, c = 5 
quad(2, 15, 5)
#-x^2 + 6x + 3 = 0 ; a = -1, b = 6, c = 3
quad(-1, 6, 3)
#x^2 + 7x + 10 = 0 ; a = 1, b = 7, c = 10 
quad(1, 7, 10)
#10x^2 + 10x = 0 ; a = 10, b = 10, c = 0 
quad(10, 10, 0)

## Exercise 2 
Ftemp <- function(celsius){
  return(1.8*celsius + 32)
}

Ftemp(26)

## Exercise 3 
fact <- function(num){
  product <- 1 
  if (num == 0)
  {
    return(1)
  }
  for(num1 in seq(from = 0, to = num, by = 1)){
    product = product*num1
  }
  return(product)
}

fact(7)
fact(0)

## Exercise 4 
install.packages("deSolve")
library("deSolve")

#lsoda(initial, t, mymodel, parameters)
#lsoda(times = t, func = mymodel, parms =parameters, y = initial)

## Exercise 5 
init <- c(N = 10)
times <- seq(0, 10, 0.1)
parms <- c(R = 0.6)

expGrowthODE <- function(tt, init, parms){
  derivs <- parms["R"]*init["N"]
  return(list(derivs))
}

expOutput <- lsode(init, times, expGrowthODE, parms)
head(expOutput)

plot(expOutput[,1], expOutput[,2], type ="l", xlab="time", ylab="popsize")

#---- 
init <- c(N = 10)
times <- seq(0, 20, 0.1)
parms <- c(R = 0.8, K = 100)

LogGrowthODE <- function(tt, init, parms){
  derivs <- (parms["R"]*init["N"])*(1 - (init["N"]/parms["K"]))
  return(list(derivs))
}

LogOutput <- lsode(init, times, LogGrowthODE, parms)
head(LogOutput)

plot(LogOutput[,1], LogOutput[,2], xlim = c(0, 20), ylim = c(0, 150), type = "l", xlab = "time", ylab = "popSize", main = "PopSize vs. Time")
par(mar = c(5,4,4,2) + 0.1)

# Exercise 6 -- 
# plotting multiple functions on the same plot 
# plot(LogOutput[,1], LogOutput[,2], type = "l", xlab = "time", ylab = "popSize", main = "PopSize vs. Time")
init <- c(N = 1)
LogOutput <- lsode(init, times, LogGrowthODE, parms)
lines(LogOutput[,1], LogOutput[,2], type = 'l', col = "red")

init <- c(N = 25)
LogOutput <- lsode(init, times, LogGrowthODE, parms)
lines(LogOutput[,1], LogOutput[,2], type = 'l', col = "purple")

init <- c(N = 50)
LogOutput <- lsode(init, times, LogGrowthODE, parms)
lines(LogOutput[,1], LogOutput[,2], type = 'l', col = "blue")

init <- c(N = 75)
LogOutput <- lsode(init, times, LogGrowthODE, parms)
lines(LogOutput[,1], LogOutput[,2], type = 'l', col = "brown")

init <- c(N = 100)
LogOutput <- lsode(init, times, LogGrowthODE, parms)
lines(LogOutput[,1], LogOutput[,2], type = 'l', col = "grey")

init <- c(N = 125)
LogOutput <- lsode(init, times, LogGrowthODE, parms)
lines(LogOutput[,1], LogOutput[,2], type = 'l', col = "black")

init <- c(N = 150)
LogOutput <- lsode(init, times, LogGrowthODE, parms)
lines(LogOutput[,1], LogOutput[,2], type = 'l', col = "green")

# division ----- Bonus question 

IC <- c(N1 = 8, N2 = 50)
times <- seq(0, 40, by = 0.1)
parms <- c(r = 0.8, K = 100, alpha = 0.5, v = 0.4)

twopatchODE <- function(tt, init, parms){
  dN1 <- parms["r"]*init["N1"]*(1 - init["N1"]/parms["K"]) + parms["alpha"]*(init["N2"] - init["N1"])
  
  dN2 <- parms["r"]*init["N2"]*(1 - init["N2"]/parms["K"]) + parms["alpha"]*(init["N1"] -init["N2"]) - parms["v"]*init["N2"]
  
  return(list(c(dN1, dN2)))

}

output <- lsoda(IC, times, twopatchODE, parms)
head(output)

plot(output[,1], output[,2], type = "l", xlab = "t", ylab = "N", ylim = c(0,150), col = "blue")
par(mar = c(5,4,4,2) + 0.1)
lines(output[,1], output[,3], col = "red")
abline(h = 100, lty = 2) #dashed horizontal line at carrying capacity 
legend(x = "topright", legend = c("N1", "N2"), col = c("blue", "red"), lty = 1)


# The second differential equation needs to be added another -vN2 term
# The steady state of the first population is estimated to be around 85, whereas the steady state of 
# the second population is around 65. They are different because there is an effect of constant effort 
# harvesting 

