#Lab 8 
#5/26/2023

#Exercise 1 
#part 1____________________________________
#plotting the histogram 
Poisson1 <- rpois(1000, lambda = 1)
hist(Poisson1,xlab = "Weight",col = "yellow",border = "blue")


simulate_sheep <- function(N0, surv_prob, birth_rate, timesteps){
  popVe <- numeric(timesteps +1) #population vector to update
  popVe[1] <- N0 #initial condition
  
  #loop over time steps and update your population size
  for(i in seq(1,timesteps)){
    popVe[i+1] <- surv_prob*popVe[i] + birth_rate*surv_prob*popVe[i]
    #(s* n_t) + b* (s * n_t) because only those that survive can reproduce
  }
  #return your population vector 
  return(popVe)
}


#part 2____________________________________
time <- seq(1,31)
result <- simulate_sheep(N0 = 100, surv_prob = 0.5, birth_rate = rpois(1, lambda = 1), timesteps = 30)
result
plot(time, result, col = 'red', type = 'l', xlab = 'Time', ylab = 'Population size')
#it works 

#part 3_____________________________________
for(i in 1:8){
  if (i == 1){
  time <- seq(1,31)
  result <- simulate_sheep(N0 = 100, surv_prob = 0.5, birth_rate = rpois(1, lambda = 1), timesteps = 30)
  plot(time, result, col = 'red', type = 'l', xlab = 'Time', ylab = 'Population size')
  i <- i + 1 
  }
  else{
    time <- seq(1,31)
    result <- simulate_sheep(N0 = 100, surv_prob = 0.5, birth_rate = rpois(1, lambda = 1), timesteps = 30)
    lines(time, result, col = 'blue', type = 'l', xlab = 'Time', ylab = 'Population size')
    i <- i + 1 
  }
}

#part 4______________________________________

#I will write a function such that it takes in the number of times that we want to run the simulatin 

runFunction <- function(numTimes, initial_pop){
  counter <- 0
  for(i in 1:numTimes){
    if (i == 1){
      time <- seq(1,31)
      result <- simulate_sheep(N0 = initial_pop, surv_prob = 0.5, birth_rate = rpois(1, lambda = 1), timesteps = 30)
      plot(time, result, col = 'red', type = 'l', xlab = 'Time', ylab = 'Population size')
      i <- i + 1 
      
      if(result[31] < 1){
        counter <- counter + 1
      }
    }
    else{
      time <- seq(1,31)
      result <- simulate_sheep(N0 = 100, surv_prob = 0.5, birth_rate = rpois(1, lambda = 1), timesteps = 30)
      lines(time, result, col = 'blue', type = 'l', xlab = 'Time', ylab = 'Population size')
      i <- i + 1 
      
      if(result[31] < 1){
        counter <- counter + 1
      }
    }
  }
  return(counter) 
}

runFunction(8, initial_pop = 100) #this function returns the number of times the population went extinct


#part 5_________________________________________
runFunction(numTimes = 500, initial_pop = 100)
#The percentage of runs that went extinct was about 172/500, 196/500, or 34.4 and 39.2 percent.
#The max that I got was 196, so I think this range of 34.4-39.2 percent captures the phenonmenon well.
#Comparing with my graphs, they look correspondingly accurate because there are a darkened line on 0. 

#part 6___________________________________________

list1 <- list(c(0)) 
list2 <- list(c(0))
for(i in 1:50){
  initial_pop <- sample(1:100, 1)
  ext_rate <- runFunction(numTimes = 500, initial_pop)/500
  list1[i] <- c(initial_pop)
  list2[i] <- c(ext_rate)
}

print(list1)
print(list2)

#part 7________________________________________________

list1 <- list(c(0)) 
list2 <- list(c(0))
for(i in 1:50){
  initial_pop <- sample(1:100, 1)
  ext_rate <- runFunction(numTimes = 500, initial_pop)/500
  list1[i] <- c(initial_pop)
  list2[i] <- c(ext_rate)
}

plot(list1, list2, xlab = 'Initial Population Size', ylab = 'Percentage of extinction')

#The scatterplot showed no-correlation, and that the initial population size had not a clear
#indication of the percentage for extinction. The system behaves in the manner such that the 
#population is equally prone to extinction and takeoff regardless of initial population size. 
#This is expected behavior because population dynamics are not subject to ICs. 

#Exercise 2_______________________________________________________
#SIR model 
beta <- 1/N
gamma <- 0.5
N <- 1000
SVector <- rep(0, 31) 
IVector <- rep(0, 31)
RVector <- rep(0, 31)
SVector[1] <- 999
IVector[1] <- 1
RVector[1] <- 0

for(t in seq(1, 30)){
  p_inf <- 1 - exp(-beta*IVector[t])
  p_rec <- 1- exp(-gamma)
  SVector[t + 1] <- SVector[t] - p_inf*SVector[t]
  IVector[t + 1] <- IVector[t] + p_inf*SVector[t] - p_rec*IVector[t]
  RVector[t + 1] <- RVector[t] + p_rec*IVector[t]
}

plot(seq(1, 31), SVector, ylim = c(0, 1000), type = 'l', col = 'red',xlab = "time", ylab = "Population size", main = 'SIR model')
lines(seq(1,31), IVector, type = 'l', col = 'blue')
lines(seq(1,31), RVector, type = 'l', col = 'purple')
legend(x = "topleft", legend = c("Susceptible", "Infected", "Recovered"), col = c("red", "blue", "purple"), lty = 1)

#Exercise 3__________________________________________

#We are going to initialize a vector that is going to store the 1000 numbers of infected individuals count 
IVector1 <- rep(0,1001)

RunExercise3 <- function(run_number, gammaValue){
  
  for(i in 1:run_number){
    beta <- 1/1000
    gamma <- gammaValue
    N <- 1000
    SVector <- rep(0, 31) 
    IVector <- rep(0, 31)
    SVector[1] <- 999
    IVector[1] <- 1

      for(t in seq(1, 30)){
        p_inf <- 1 - exp(-beta*IVector[t])
        p_rec <- 1- exp(-gamma)
        SVector[t + 1] <- SVector[t] - p_inf*SVector[t]
        IVector[t + 1] <- IVector[t] + p_inf*SVector[t] - p_rec*IVector[t]
        RVector[t + 1] <- RVector[t] + p_rec*IVector[t]
      }
    print(IVector[31])
  }

}

RunExercise3(1000, gammaValue = 0.5)
RunExercise3(1000, gammaValue = 0.9)

