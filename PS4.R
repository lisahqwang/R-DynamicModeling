#PS 4 
#1a---------------------------------
A <- matrix(c(0, 0.5, 20, 0.3, 0, 0, 0, 0.5, 0.9), nr = 3, byrow = TRUE)
A
N0 <- matrix(c(100, 250, 50), ncol = 1)
N1 <- A %*% N0 
N1

years <- 6
N.projections <- matrix(0, nrow = nrow(A), ncol = years + 1) 
N.projections[,1] <- N0 
#now we perform the iteration with the for loop
for (i in 1:years){
  N.projections[, i + 1] <- A %*% N.projections[,i]
}

matplot(0:years, t(N.projections), type = "l", lty = 1:3, col = 1, ylab = "Stage Abundance", xlab = "Year", main = "Lisa Wang")
legend("topleft", legend = c("Seeds", "Small Adult", "Large Adult"), lty = 1:3, col = 1, bty = "n")


#Annual growth rate 
N.totals <- apply(N.projections, 2, sum)
Rs <- N.totals[-1]/N.totals[-(years +1)]
plot(0:(years - 1), Rs, type = "b", xlab = "Year", ylab = "R", main = "Lisa Wang")

eigs.A <- eigen(A) 
eigs.A 

here <- eigs.A$values
here1 <- eigs.A$vectors 

#Finding lambda 
dom.pos <- which.max(eigs.A[["values"]])
L1 <- Re(eigs.A[["values"]][dom.pos])
L1 


#Power iteration method of eigenanalysis 
t <- 20 
Nt <- N0/sum(N0)

R.t <- numeric(t) 
for (i in 1:t) R.t[i] <- {
  Nt1 <- A %*% Nt 
  R <- sum(Nt1)/sum(Nt) 
  Nt <- Nt1/sum(Nt1)
  R
}
par(mar = c(5, 4, 3, 2))
plot(1:t, R.t, type = "b", main = "Lisa Wang")
points(t, L1, pch = 19, cex = 1.5)


#1b----------------------------
w <- Re(eigs.A[["vectors"]][,dom.pos])
ssd <- w/sum(w)
round(ssd, 3) 

#calculating reproductive value
M <- eigen(t(A))
v <- Re(M$vectors[, which.max(Re(M$values))])
RV <- v/v[1]
RV 

#Calculating sensitivity
vw.s <- v %*% t(w) 
(S <- vw.s/as.numeric(v %*% w))

#Response: 

#we are interested in the demographic parameters that have the biggest influence on lambda, the long term 
#growth rate of the population. Sensitivity describes the absolute response of lambda to absolute changes 
#L_ij, if the rest of matrix are held constant. S_ij = d(lambda)/d(L_ij) . S31 is the highest sensitivity 
#because the lambda increases the most if the plant can undergo survivorship 
#directly to reproductive stage, because the greatest proportion of population is in seedling stage 
#and adult age exhibits the largest reproductive rate. However, this is not possible because of 
#no skipping stage 2. 

#The plant does not exhibit this transition, and it means the magnitude of 
#yield if all seedlings are fully surviving the second stage, and they become 
#able to reproduce in adult stage. 

#This is still biologically useful information, but we would prefer to use elasticity instead. 

#1c-----------------------
elas <- (A/L1) * S 
round(elas, 3) 

#Response: 

#The question is asking whether reducing survivorship of stage 3 by 20% 
#or reducing fecundity of stage 3 by 20% is going to have a bigger effect 
#on the long term population growth rate lambda. 

#Based on the elasticity matrix, we know 
#0.246 = elasticity of fecundity of stage 3 plants 
#0.238 = elasticity of survivorship of stage 3 plants on self-survivorship, remaining in the system 
#from these results, we can deduce that reducing the fecundity will decrease lambda more since 0.246 > 0.238
#we would consider the proportion change in a transition element. 

#1d------------------------
B <- matrix(c(0, 0.5, 20, 0.3, 0, 0, 0, 0.1, 0.9), nr = 3, byrow = TRUE)
B
N0 <- matrix(c(100, 250, 50), ncol = 1)
N1 <- B %*% N0 
N1

#the transition parameter is reduced to 0.1, after 80% reduction 

#1e---------------------------------
#making plots of the projected population sizes and annual growth rate

years <- 6
N.projections <- matrix(0, nrow = nrow(B), ncol = years + 1) 
N.projections[,1] <- N0 
#now we perform the iteration with the for loop
for (i in 1:years){
  N.projections[, i + 1] <- B %*% N.projections[,i]
}

matplot(0:years, t(N.projections), type = "l", lty = 1:3, col = 1, ylab = "Stage Abundance", xlab = "Year", main = "New plot")
legend("topleft", legend = c("Seeds", "Small Adult", "Large Adult"), lty = 1:3, col = 1, bty = "n")


N.totals <- apply(N.projections, 2, sum)
Rs <- N.totals[-1]/N.totals[-(years +1)]
plot(0:(years - 1), Rs, type = "b", xlab = "Year", ylab = "R", main = "New plot of Annual Growth rate")



#1f----------------------------------
#calculating the long term population growth rate, lambda

eigs.B <- eigen(B) 
eigs.B

here <- eigs.B$values
here1 <- eigs.B$vectors 
dom.pos <- which.max(eigs.B[["values"]])
L1 <- Re(eigs.B[["values"]][dom.pos])
L1 

#lambda = 1.294
stable_stage_distribution <- eigs.B$vectors[,which.max(abs(eigs.B$values))]
stable_stage_distribution
#the corresponding vector is  0.97255374+0i 0.22551915+0i 0.05727425+0i

#1g-----------------------------------
#proportion of individuals for new projection matrix 

B <- matrix(c(0, 0.5, 20, 0.3, 0, 0, 0, 0.1, 0.9), nr = 3, byrow = TRUE)
N0 <- matrix(c(100, 250, 50), ncol = 1) #initial population vector

pop <- matrix(NA, ncol = 30, nrow = 3) 
pop[,1] <- N0 

#loop over time steps of interest
for(ii in 2:30){
  pop[,ii] <- B %*% pop[, ii -1]
}

plot(seq(1,30), pop[1,], type = "o", xlab = 'Time', ylab = 'Population Size', col = "red", main = 'Population vs. Time')
lines(seq(1,30), pop[2,], type = "o", xlab = 'Time', ylab = 'Population Size', col = "blue", main = 'Population vs. Time')
lines(seq(1,30), pop[3,], type = "o", xlab = 'Time', ylab = 'Population Size', col = "purple", main = 'Population vs. Time')
legend(x = "topleft", legend = c("age class 1", "age class 2", "age class 3"), col = c("red", "blue", "purple"), lty = 1)

totalPop <- colSums(pop) #calculates total population sizes
totalPop
juvProp <- pop[1,]/totalPop #calculates proportion of juveniles 
twoProp <- pop[2,]/totalPop
threeProp <- pop[3,]/totalPop
plot(seq(1,30), juvProp, ylim = c(0,1), type = "o", xlab = 'Time', ylab = 'Proportion to total', col = "red", main = 'Prop to total vs. Time')
lines(seq(1,30), twoProp, type = "o", xlab = 'Time', ylab = 'Proportion to total', col = "blue", main = 'Prop to total vs. Time')
lines(seq(1,30), threeProp, type = "o", xlab = 'Time', ylab = 'Proportion to total', col = "purple", main = 'Prop to total vs. Time')
legend(x = "topright", legend = c("age class 1", "age class 2", "age class 3"), col = c("red", "blue", "purple"), lty = 1)


eigs.B$vectors[,1]/sum(eigs.B$vectors[,1])
#prints 0.775, 0.179, 0.045 

abline(h = 0.77472892, lty = "dashed")
abline(h = 0.17964685, lty = "dashed")
abline(h = 0.04562423, lty = "dashed")

#We have shown, over 30 iterations, that the proportion of age groups approaches stability 

#1h-------------------------
#calculating sensitivity and elasticity 
w <- Re(eigs.B[["vectors"]][,dom.pos])
ssd <- w/sum(w)
round(ssd, 3) 

#calculating reproductive value
M <- eigen(t(B))
v <- Re(M$vectors[, which.max(Re(M$values))])
RV <- v/v[1]
RV 

#Calculating sensitivity matrix 
vw.s <- v %*% t(w) 
(S <- vw.s/as.numeric(v %*% w))

#Calculating elasticity matrix 

elas <- (A/L1) * S 
round(elas, 3) 

#The proportional change I would make to the mugwort 
#population would be introducing some external agent that 
#reduces the survivorship from year 1 to year 2. Specifically, because
#the elascity of s_12 = 0.912, highest of all elements
#and the sensitivity of s_12 = 2.35, highest of all elements. I would 
#reduce by 100%. 

#1i------------------
#recalculating lambda, except projection matrix changed from 
C <- matrix(c(0, 0.5, 20, 0.3, 0, 0, 0, 0.1, 0.45), nr = 3, byrow = TRUE)
C

eigs.C <- eigen(C) 
eigs.C

eigenvalues <- eigs.C$values
eigenvectors <- eigs.C$vectors 
dom.pos <- which.max(eigs.C[["values"]])
L1 <- Re(eigs.C[["values"]][dom.pos])
L1 

#lambda = 1.0626
stable_stage_distribution1 <- eigs.B$vectors[,which.max(abs(eigs.B$values))]
stable_stage_distribution1
#This is still a growing population in long term speculation, because
#it is larger than 1

D <- matrix(c(0, 0.5, 20, 0.3, 0, 0, 0, 0.1, 0.293), nr = 3, byrow = TRUE)
D

eigs.D <- eigen(D) 
eigs.D

eigenvalues <- eigs.D$values
eigenvectors <- eigs.D$vectors 
dom.pos <- which.max(eigs.D[["values"]])
L1 <- Re(eigs.D[["values"]][dom.pos])
L1 

#lambda = 0.999
stable_stage_distribution1 <- eigs.D$vectors[,which.max(abs(eigs.D$values))]
stable_stage_distribution1
#This lambda value is now lower than 1, so it looks like a over 63% reduction
# of adult survivorship would result in decreasing population of mugworts. 

#2a-------------------
#I am choosing the SIR model, with Initial Conditions, parameters, and can return 
#a vector of values as a function of time 
install.packages("deSolve")
library("deSolve")

IC <- c(S = 999, I = 1, R = 0)
N <- 1000
times <- seq(0, 30, 1)  #Create a sequence of times 
pars <- c(beta = 0.5, gamma = 0.1, mu= 0.01)

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
output <- lsoda(IC, times, SIRODE, pars, rtol = 1e-6, atol = 1e-6)
head(output)
#Then make the plot 

plot(output[,1], output[,2], type = "l", ylim = c(0,1000), xlab = "time", col = "red",ylab = "popSize", main = "PopSize vs. Time")
lines(output[,1], output[,3], type = "l", xlab = "time", col = "blue", ylab = "popSize", main = "PopSize vs. Time")
lines(output[,1], output[,4], type = "l", xlab = "time", col = "purple", ylab = "popSize", main = "PopSize vs. Time")

legend(x = "topright", legend = c("S", "I", "R"), col = c("red", "blue", "purple"), lty = 1) 

#2b-----------------------------
#variation of key parameter: beta 

IC <- c(S = 999, I = 1, R = 0)
N <- 1000
times <- seq(0, 50, 1)  #Create a sequence of times 
pars <- c(beta = 0.5, gamma = 0.1, mu= 0.01)

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
output <- lsoda(IC, times, SIRODE, pars, rtol = 1e-6, atol = 1e-6)
head(output)
#Then make the plot 

plot(output[,1], output[,2], type = "l", ylim = c(0,1000), xlab = "time", col = "red",ylab = "popSize", main = "PopSize vs. Time")
lines(output[,1], output[,3], type = "l", xlab = "time", col = "blue", ylab = "popSize", main = "PopSize vs. Time")
lines(output[,1], output[,4], type = "l", xlab = "time", col = "purple", ylab = "popSize", main = "PopSize vs. Time")

legend(x = "topright", legend = c("S", "I", "R"), col = c("red", "blue", "purple"), lty = 1) 

install.packages("plotly")
library(plotly)

batch <- function(trials){
  pars <- c(beta = trials*0.1, gamma = 0.1, mu= 0.01)
  output <- lsoda(IC, times, SIRODE, pars, rtol = 1e-6, atol = 1e-6)
  plot(output[,1], output[,2], type = "l", ylim = c(0,1000), xlab = "time", col = "red",ylab = "popSize", main = trials*0.1)
  while(trials > 0){
    pars <- c(beta = trials*0.1, gamma = 0.1, mu= 0.01)
    output <- lsoda(IC, times, SIRODE, pars, rtol = 1e-6, atol = 1e-6)
    
  lines(output[,1], output[,2], type = "l", ylim = c(0,1000), xlab = "time", col = "red",ylab = "popSize", main = trials*0.1)
  lines(output[,1], output[,3], type = "l", xlab = "time", col = "blue", ylab = "popSize")
  lines(output[,1], output[,4], type = "l", xlab = "time", col = "purple", ylab = "popSize")
  legend(x = "topright", legend = c("S", "I", "R"), col = c("red", "blue", "purple"), lty = 1) 
  trials = trials - 1
  }
}

batch(7) #running the batch function 

#Commentary on biological significance: It looks like that the effects of the SIR model 
#simply propagates out. Beta is the disease transmission rate, and the higher the beta value
#the faster the drop of S curve and sharper the peak for Infected. Therefore, we 
#notice the height of the I peak go higher and higher. 

#2c----------------------------
#changing the initial conditions 

initial <- function(trials){
  IC <- c(S = 999 - trials*30, I = 1 + trials*30, R = 0)
  N <- 1000
  times <- seq(0, 30, 1)  #Create a sequence of times 
  pars <- c(beta = 0.5, gamma = 0.1, mu= 0.01)
  
  output <- lsoda(IC, times, SIRODE, pars, rtol = 1e-6, atol = 1e-6)
  
  plot(output[,1], output[,2], type = "l", ylim = c(0,1000), xlab = "time", col = "red",ylab = "popSize", main = "PopSize vs. Time")
  
  while(trials > 0){
  IC <- c(S = 999 - trials*30, I = 1 + trials*30, R = 0)
  output <- lsoda(IC, times, SIRODE, pars, rtol = 1e-6, atol = 1e-6)
  lines(output[,1], output[,2], type = "l", ylim = c(0,1000), xlab = "time", col = "red",ylab = "popSize", main = "PopSize vs. Time")
  lines(output[,1], output[,3], type = "l", xlab = "time", col = "blue", ylab = "popSize", main = "PopSize vs. Time")
  lines(output[,1], output[,4], type = "l", xlab = "time", col = "purple", ylab = "popSize", main = "PopSize vs. Time")
  legend(x = "topright", legend = c("S", "I", "R"), col = c("red", "blue", "purple"), lty = 1)
  trials = trials -1
  }
}

initial(4)

#Commentary on biological signficance: whereas the shape of the curves were actually different
#for changing beta, changing the initial conditions seems to have shifted the curves
#incrementally to the right more. As we decrease the number of susceptibles and increasing the
# count for infected, there seem to be similar shifting behavior on recovered individuals. 


