# lab 4
runif(10)
runif(10, min = 10, max = 20)
unifSamples <-runif(10, min = 10, max = 20)
hist(unifSamples, breaks = c(seq(10, 20, 1)))

unifSamples <- runif(100000, min = 10, max = 20) 
hist(unifSamples, border = "blue4", col = "blue1", breaks = c(seq(10, 20,1)))

rnorm(1)
rnorm(10, mean = 1, sd = 4)
hist(rnorm(10000, mean = 1, sd =4))

normSamples <- rnorm(100000, mean = 1, sd =4)
mean(normSamples)

sd(normSamples)

#Exercise 1---------------------
help(rbinom)
rbinom(n = 10, size =5, prob =0.4)

#Exercise 2---------------------
hist(rbinom(n = 10000, size = 5, prob = 0.4))
#note: rbinom, rpois, rexp 

#Exercise 3-------------------
r_d <- 0.5 
K <- 50 
unifSamples <- runif(40)


popVector <- rep(0,41) 
popVector[1] <- 10 #initial number of sheep 
for (year in seq(1,40)){
  if(unifSamples[year] < 0.4){
    r_d <- 0.5
  }
  if(unifSamples[year] >0.4){
    r_d <- -0.5
  }
  popVector[year + 1] <- popVector[year] + r_d*popVector[year]*(1-popVector[year]/K)
}
plot(seq(0,40), popVector, type = "l", xlab = "time", ylab = "pop Size", main = "Pop Size vs. time")
par(mar = c(5,4,4,2) + 0.1)

#Exercise 4 -------------------
# response: the plots are different, they never reached carrying capacity. Almost always 
# the graphs reached 0. 

#Exercise 5--------------
# to-do list: unif within for loop, exception for when unifSample 
# is less than 0.05

r_d <- 0.5 
K <- 50 

popVector <- rep(0,41)
popVector[1] <- 10 
for (year in seq(1,40)){
  unifSample <- runif(40)
  if(unifSample[year] < 0.05){
    popVector[year+1] <- popVector[year]/2
  }
  else{
    popVector[year +1] <- popVector[year] + r_d*popVector[year] * (1-popVector[year]/K)
  }
}
plot(seq(0,40), popVector, type = "l", xlab = "time", ylab = "pop size", main = "Pop size vs. Time")

#Exercise 6-------------------
# After running it for a few times, the graphs changed because 
# we were running different trials of random distributions of 0 to 1
# so the model also deducts its population at random intervals. The 
# population almost always reaches carrying capacity, I have not yet 
# ran a trial where the population crashes to 0. 

# Exercise 7--------------
install.packages("deSolve")
library("deSolve")

#lsoda(initial, t, mymodel, parameters)
#lsoda(times = t, func = mymodel, parms =parameters, y = initial)

IC <- c(S = 999, I = 1, R = 0)
N <- 1000
times <- seq(0, 30, 1)  #Create a sequence of times 
pars <- c(beta = 0.8, gamma = 0.1, mu= 0.01)

SIRODE <- function(t, vars, pars) {
  with(as.list(c(vars, pars)), { 
    #This is a function that allows you to refer to the named
    #parameters inside the vector pars directly by their name, without having to index pars.
    dSdt <- mu*N - ((beta*S*I)/N) - (mu*S)
    dIdt <- (beta*S*I)/N - mu*I - gamma*I
    dRdt <- gamma*I - mu*R
    return (list(c(dSdt, dIdt,dRdt)))
  })
}
output <- lsoda(IC, times, SIRODE, pars, rtol = 1e-5, atol = 1e-5)
head(output)
#Then make the plot 

plot(output[,1], output[,2], type = "l", xlab = "time", col = "red",ylab = "popSize", main = "PopSize vs. Time")
lines(output[,1], output[,3], type = "l", xlab = "time", col = "blue", ylab = "popSize", main = "PopSize vs. Time")
lines(output[,1], output[,4], type = "l", xlab = "time", col = "purple", ylab = "popSize", main = "PopSize vs. Time")

legend(x = "topright", legend = c("S", "I", "R"), col = c("red", "blue", "purple"), lty = 1)

# Exercise 8 ------------------------

IC <- c(S = 999, I = 1, R = 0)
N <- 1000
times <- seq(0, 30, 1)  #Create a sequence of times 
pars <- c(beta = (-0.8*t/30) + 0.8, gamma = 0.1, mu= 0.01)

SIRODE <- function(t, vars, pars) {
  with(as.list(c(vars, pars)), { 
    #This is a function that allows you to refer to the named
    #parameters inside the vector pars directly by their name, without having to index pars.
    dSdt <- mu*N - ((((-0.8*t/30) + 0.8)*S*I)/N) - (mu*S)
    dIdt <- (((-0.8*t/30) + 0.8)*S*I)/N - mu*I - gamma*I
    dRdt <- gamma*I - mu*R
    return (list(c(dSdt, dIdt,dRdt)))
  })
}
output <- lsoda(IC, times, SIRODE, pars, rtol = 1e-6, atol = 1e-6)
head(output)
#Then make the plot 

plot(output[,1], output[,2], type = "l", ylim = c(0,1000), xlab = "time", col = "red",ylab = "popSize", main = "PopSize vs. Time")
lines(output[,1], output[,3], type = "l", xlab = "time", col = "blue", ylab = "popSize", main = "PopSize vs. Time")
lines(output[,1], output[,4], type = "l", xlab = "time", col = "purple", ylab = "popSize", main = "PopSize vs. Time")

legend(x = "topright", legend = c("S", "I", "R"), col = c("red", "blue", "purple"), lty = 1)

