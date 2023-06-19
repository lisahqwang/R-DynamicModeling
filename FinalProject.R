#Install deSolve package for lsoda
install.packages("deSolve")
library(deSolve)

##################### BASE MODEL ###########################

#Initial conditions and parameters
IC <- c(S_n = 10000, S_p = 10000, I_n = 5, I_a = 5, A = 5, Death = 0)
times <- seq(0,20)
params <- c(alpha = 0.75, 
            beta = 0.25,
            gamma = 0.0001,
            delta = 0.11,
            epsilon = 0.056,
            rho = 0.4,
            tau = 0.2371,
            pi = 0.1,
            omega = 0.1,
            mu = 0.01097,
            Lambda_2 = 1,
            Lambda_1 = 1)

#Creating a function for the model
ODE <- function(tt, IC, params){
  with(as.list(c(params, IC)), {
    dS_n <- (S_p*alpha-S_n*beta*Lambda_1-S_n*gamma*I_n-S_n*mu)
    dS_p <- (S_n*beta*Lambda_1-S_p*alpha-S_p*gamma*delta*I_a-S_p*mu)
    dI_n <- (S_n*gamma*I_n+I_a*epsilon-I_n*rho*Lambda_2-I_n*tau-I_n*mu)
    dI_a <- (S_p*gamma*delta*I_a+I_n*rho*Lambda_2-I_a*epsilon-I_a*tau*pi-I_a*mu)
    dA <- (I_n*tau+I_a*tau*pi-A*(omega+mu))
    dDeath <- A*(omega)
    return(list(c(dS_n, dS_p, dI_n, dI_a, dA, dDeath)))
  })
}

#Putting results into a table
output <- lsoda(IC, times, ODE, params)
head(output) 
sum(output[, 6], output[,7]) #gives total deaths and cases at the end of 20 years

#Plot results
plot(output[,1], output[,2],  type = "l", xlab = "t (years)", ylab = "N",
     ylim = c(0, 20000), col = "blue")
title(main = "Base Model (Lambda1 = 1, Lamda2 = 1)")
lines(output[,1], output[,3], col = "red")
lines(output[,1], output[,4], col = "pink")
lines(output[,1], output[,5], col = "purple")
lines(output[,1], output[,6], col = "seagreen4")
lines(output[,1], output[,7], col = "grey")
legend(x = "topright", legend = c("S_n","S_p", "I_n","I_a","A", "deaths"),
       col = c("blue", "red","pink", "purple", "seagreen4", "grey"), lty = 1)
text(15, 8000, "Total AIDS-Related Deaths and Cases: 57222.09")


################## MAXIMUM ACCESSIBILITY ###########################

#Initial conditions and parameters
IC <- c(S_n = 10000, S_p = 10000, I_n = 5, I_a = 5, A = 5, Death = 0) 
times <- seq(0,20)
params <- c(alpha = 0.75, 
            beta = 0.25,
            gamma = 0.0001,
            delta = 0.11,
            epsilon = 0.056,
            rho = 0.4,
            tau = 0.2371,
            pi = 0.1,
            omega = 0.337,
            mu = 0.01097,
            Lambda_2 = 3.72,
            Lambda_1 = 1.3671)

#Model Function
ODE <- function(tt, IC, params){
  with(as.list(c(params, IC)), {
    dS_n <- (S_p*alpha-S_n*beta*Lambda_1-S_n*gamma*I_n-S_n*mu)
    dS_p <- (S_n*beta*Lambda_1-S_p*alpha-S_p*gamma*delta*I_a-S_p*mu)
    dI_n <- (S_n*gamma*I_n+I_a*epsilon-I_n*rho*Lambda_2-I_n*tau-I_n*mu)
    dI_a <- (S_p*gamma*delta*I_a+I_n*rho*Lambda_2-I_a*epsilon-I_a*tau*pi-I_a*mu)
    dA <- (I_n*tau+I_a*tau*pi-A*(omega+mu))
    dDeath <- A*(omega)
    return(list(c(dS_n, dS_p, dI_n, dI_a, dA, dDeath)))
  })
}

#Putting results into a table
output <- lsoda(IC, times, ODE, params)
head(output) 
sum(output[, 6], output[,7]) #gives total deaths and cases at the end of 21 years

#Plot results
plot(output[,1], output[,2],  type = "l", xlab = "t (years)", ylab = "N",
     ylim = c(0, 20000), col = "blue")
title(main = "Maximum Accessibility (Lambda1 = 1.3671, Lambda2 = 3.72)")
lines(output[,1], output[,3], col = "red")
lines(output[,1], output[,4], col = "pink")
lines(output[,1], output[,5], col = "purple")
lines(output[,1], output[,6], col = "seagreen4")
lines(output[,1], output[,7], col = "grey")
legend(x = "topright", legend = c("S_n","S_p", "I_n","I_a","A", "deaths"),
       col = c("blue", "red","pink", "purple", "seagreen4", "grey"), lty = 1)
text(13, 7000, "Total AIDS-Related Deaths and Cases = 450.7687")

#Zoomed in results of the maximum accessibility analysis
#Plot results
plot(output[,1], output[,2],  type = "l", xlab = "t (years)", ylab = "N",
     ylim = c(0, 200), col = "blue")
title(main = "Maximum Accessibility Zoom")
lines(output[,1], output[,3], col = "red")
lines(output[,1], output[,4], col = "pink")
lines(output[,1], output[,5], col = "purple")
lines(output[,1], output[,6], col = "seagreen4")
lines(output[,1], output[,7], col = "grey")
legend(x = "topright", legend = c("S_n","S_p", "I_n","I_a","A", "deaths"),
       col = c("blue", "red","pink", "purple", "seagreen4", "grey"), lty = 1)
text(16, 45, "Total AIDS-Related Deaths and Cases = 450.7687")


########## NO PREP BUT MAXIMUM ART ##########

#Initial conditions and parameters
IC <- c(S_n = 20000, S_p = 0, I_n = 5, I_a = 5, A = 5, Death = 0) #Place what would be the S_p population into S_n since they are not getting PrEP tx
times <- seq(0,20)
params <- c(alpha = 0.75, 
            beta = 0.25,
            gamma = 0.0001,
            delta = 0.11,
            epsilon = 0.056,
            rho = 0.4,
            tau = 0.2371,
            pi = 0.1,
            omega = 0.1,
            mu = 0.01097,
            Lambda_2 = 1.3671,
            Lambda_1 = 0)

#Model Function
ODE <- function(tt, IC, params){
  with(as.list(c(params, IC)), {
    dS_n <- (S_p*alpha-S_n*beta*Lambda_1-S_n*gamma*I_n-S_n*mu)
    dS_p <- (S_n*beta*Lambda_1-S_p*alpha-S_p*gamma*delta*I_a-S_p*mu)
    dI_n <- (S_n*gamma*I_n+I_a*epsilon-I_n*rho*Lambda_2-I_n*tau-I_n*mu)
    dI_a <- (S_p*gamma*delta*I_a+I_n*rho*Lambda_2-I_a*epsilon-I_a*tau*pi-I_a*mu)
    dA <- (I_n*tau+I_a*tau*pi-A*(omega+mu))
    dDeath <- A*(omega)
    return(list(c(dS_n, dS_p, dI_n, dI_a, dA, dDeath)))
  })
}

#Results Table
output <- lsoda(IC, times, ODE, params)
head(output) 
sum(output[, 6], output[,7]) #gives total deaths and cases at the end of 21 years

#Plot results
plot(output[,1], output[,2],  type = "l", xlab = "t (years)", ylab = "N",
     ylim = c(0, 20000), col = "blue")
title(main = "Maximumm ART, No PrEP (Lambda1 = 0, Lambda2 = 1.3671)")
lines(output[,1], output[,3], col = "red")
lines(output[,1], output[,4], col = "pink")
lines(output[,1], output[,5], col = "purple")
lines(output[,1], output[,6], col = "seagreen4")
lines(output[,1], output[,7], col = "grey")
legend(x = "topright", legend = c("S_n","S_p", "I_n","I_a","A", "deaths"),
       col = c("blue", "red","pink", "purple", "seagreen4", "grey"), lty = 1)
text(14, 5000, "Total AIDS-Related Deaths and Cases = 86064.71")



############## NO ART BUT MAXIMUM PREP ##########
IC <- c(S_n = 10000, S_p = 10000, I_n = 5, I_a = 0, A = 5, Death = 0) 
times <- seq(0,20)
params <- c(alpha = 0.75, 
            beta = 0.25,
            gamma = 0.0001,
            delta = 0.11,
            epsilon = 0.056,
            rho = 0.4,
            tau = 0.2371,
            pi = 0.1,
            omega = 0.1,
            mu = 0.01097,
            Lambda_2 = 0,
            Lambda_1 = 3.72)

#Model Function
ODE <- function(tt, IC, params){
  with(as.list(c(params, IC)), {
    dS_n <- (S_p*alpha-S_n*beta*Lambda_1-S_n*gamma*I_n-S_n*mu)
    dS_p <- (S_n*beta*Lambda_1-S_p*alpha-S_p*gamma*delta*I_a-S_p*mu)
    dI_n <- (S_n*gamma*I_n+I_a*epsilon-I_n*rho*Lambda_2-I_n*tau-I_n*mu)
    dI_a <- (S_p*gamma*delta*I_a+I_n*rho*Lambda_2-I_a*epsilon-I_a*tau*pi-I_a*mu)
    dA <- (I_n*tau+I_a*tau*pi-A*(omega+mu))
    dDeath <- A*(omega)
    return(list(c(dS_n, dS_p, dI_n, dI_a, dA, dDeath)))
  })
}

#Results Table
output <- lsoda(IC, times, ODE, params)
head(output) 
sum(output[, 6], output[,7]) #gives total deaths and cases at the end of 21 years

#Plot results
plot(output[,1], output[,2],  type = "l", xlab = "t (years)", ylab = "N",
     ylim = c(0, 20000), col = "blue")
title(main = "Maximum PrEP, No ART (Lambda1= 3.72, Lambda2 = 0)")
lines(output[,1], output[,3], col = "red")
lines(output[,1], output[,4], col = "pink")
lines(output[,1], output[,5], col = "purple")
lines(output[,1], output[,6], col = "seagreen4")
lines(output[,1], output[,7], col = "grey")
legend(x = "topright", legend = c("S_n","S_p", "I_n","I_a","A", "deaths"),
       col = c("blue", "red","pink", "purple", "seagreen4", "grey"), lty = 1)
text(16, 9000, "Total AIDS-Related Deaths and Cases = 55511.88")

######################## SENSITIVITY ANALYSIS ############################

########### PrEP vs. ART allocation scaled to be in percentages
linear <- function(allocation_to_ART){ 
  y <- allocation_to_ART*(-4/1.47) + 372 
  return(y)
}

#plotting PrEP vs. ART graph
plot(seq(0, 150, by = 1), linear(seq(0, 150, by = 1)), type = "l", xlab = "Percent allocation to ART(%)", ylab = "Percent allocation to PrEP (%)", col = "blue", main = "Graph of Resource allocation Relationship")
points(100,100)

#CLEAR FIELD#
#rescale to proportions of Lambda for the rest of the analysis

#PrEP vs. ART allocation as a linear function
linear <- function(allocation_to_ART){ 
  y <- allocation_to_ART*(-4/1.47) + 3.72 
  return(y)
}

#plotting PrEP vs. ART allocation graph
plot(seq(0, 1.5, by = 0.1), linear(seq(0, 1.5, by = 0.1)), type = "l", xlab = "Percent allocation to ART(%)", ylab = "Percent allocation to PrEP (%)", col = "blue", main = "Graph of Resource allocation Relationship")
points(1,1)

#Set up values and an empty vector for Lambda_2 vs Deaths and Cases
Lambda_2_vals <- seq(0, 1.5, 0.1) 
empty_array <- rep(NA, 16)

#Create a for loop for Lambda_2 values
for(i in Lambda_2_vals){
  L1 <- linear(i)
  
  IC <- c(S_n = 10000, S_p = 10000, I_n = 5, I_a = 5, A = 5, Death = 0)
  times <- seq(0,20)
  params <- c(alpha = 0.75, 
              beta = 0.25,
              gamma = 0.0001,
              delta = 0.11,
              epsilon = 0.056,
              rho = 0.4,
              tau = 0.2371,
              pi = 0.1,
              omega = 0.1,
              mu = 0.01097,
              Lambda_2 = i,
              Lambda_1 = L1)
  ODE <- function(tt, IC, params){
    with(as.list(c(params, IC)), {
      dS_n <- (S_p*alpha-S_n*beta*Lambda_1-S_n*gamma*I_n-S_n*mu)
      dS_p <- (S_n*beta*Lambda_1-S_p*alpha-S_p*gamma*delta*I_a-S_p*mu)
      dI_n <- (S_n*gamma*I_n+I_a*epsilon-I_n*rho*Lambda_2-I_n*tau-I_n*mu)
      dI_a <- (S_p*gamma*delta*I_a+I_n*rho*Lambda_2-I_a*epsilon-I_a*tau*pi-I_a*mu)
      dA <- (I_n*tau+I_a*tau*pi-A*(omega+mu))
      dDeath <- A*(omega)
      return(list(c(dS_n, dS_p, dI_n, dI_a, dA, dDeath)))
    })
  }
  
  output <- lsoda(IC, times, ODE, params)
  
  #sum of deaths from AIDS and AIDS cases
  temp <- sum(output[, 6]) + sum(output[, 7])
  
  #place new values into the empty vector
  empty_array[i*10  + 1] <- temp
}

#Find the smallest number of deaths and cases
min(empty_array)

#place values of Lambda_2 and AIDS-related deaths and cases into a matrix to see the coordinates 
cbind(seq(0,1.5,0.1), empty_array) #Find Lambda_2 value correlating to the minimum number of cases and deaths

#Plot the relationship of Lambda_2 and AIDS-related deaths and cases
plot(seq(0, 1.5, by = 0.1), empty_array, ylim = c(0, 85000), xlab = "Value of Lambda_2 (ART resource allocation)", ylab = "Total AIDS Cases and Deaths", type = 'l', main = "Sensitivity Analysis")
points(0.5, 47869.83, col = "red") #Add point at the minimum value of deaths and cases
arrows(0.5, 40000, x1 = 0.5, y1 = 47000, length = 0.1, col = "red") #add arrow and text to label the coordinate
text(0.65, 38000, "minimum = (0.5,47869.83)")

################## OPTIMAL MODEL ###########################

#Initial conditions and parameters
IC <- c(S_n = 10000, S_p = 10000, I_n = 5, I_a = 5, A = 5, Death = 0) 
times <- seq(0,20)
params <- c(alpha = 0.75, 
            beta = 0.25,
            gamma = 0.0001,
            delta = 0.11,
            epsilon = 0.056,
            rho = 0.4,
            tau = 0.2371,
            pi = 0.1,
            omega = 0.1,
            mu = 0.01097,
            Lambda_2 = 0.5, #Value given in sensitivity analysis
            Lambda_1 = 2.359) #Plug in Lambda_2 value into linear function to get value

#Model Function
ODE <- function(tt, IC, params){
  with(as.list(c(params, IC)), {
    dS_n <- (S_p*alpha-S_n*beta*Lambda_1-S_n*gamma*I_n-S_n*mu)
    dS_p <- (S_n*beta*Lambda_1-S_p*alpha-S_p*gamma*delta*I_a-S_p*mu)
    dI_n <- (S_n*gamma*I_n+I_a*epsilon-I_n*rho*Lambda_2-I_n*tau-I_n*mu)
    dI_a <- (S_p*gamma*delta*I_a+I_n*rho*Lambda_2-I_a*epsilon-I_a*tau*pi-I_a*mu)
    dA <- (I_n*tau+I_a*tau*pi-A*(omega+mu))
    dDeath <- A*(omega)
    return(list(c(dS_n, dS_p, dI_n, dI_a, dA, dDeath)))
  })
}

#Results Table
output <- lsoda(IC, times, ODE, params)
head(output) 
sum(output[, 6], output[,7]) #gives total deaths and cases at the end of 21 years

#Plot results
plot(output[,1], output[,2],  type = "l", xlab = "t (years)", ylab = "N",
     ylim = c(0, 20000), col = "blue")
title(main = "Minimum AIDS-Related Deaths and Cases (Lambda_1 = 2.359 and Lambda_2 = 0.5)")
lines(output[,1], output[,3], col = "red")
lines(output[,1], output[,4], col = "pink")
lines(output[,1], output[,5], col = "purple")
lines(output[,1], output[,6], col = "seagreen4")
lines(output[,1], output[,7], col = "grey")
legend(x = "topright", legend = c("S_n","S_p", "I_n","I_a","A", "deaths"),
       col = c("blue", "red","pink", "purple", "seagreen4", "grey"), lty = 1)
text(16, 7000, "Total AIDS-Related Deaths and Cases = 47883.59") #may be slightly off due to precision (steps in the sensitivity analysis sequence)