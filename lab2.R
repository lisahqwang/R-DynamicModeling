# Template code 
#my_vector <- seq(1,10, by = 2) 
#my_vector 

#my_vector[c(-5, -2)] #removes the fifth and the second element 

#R <- 1.5 
#n <- 10 

#for(year in seq(1,10)){
#  n <- R*n
#}
#print(n)

#Exercise 1 --- 
sheepData <- seq(0,100, by = 5) 
newSheepData <-sheepData[c(1,3,5)]
newSheepData[-2]

indices <- seq(0, 20, by = 2) 
sheepData[indices]

## Answer to Exercise 1 & 2: 
## sheepData has been initialized to 20 numbers, on increments of 5
## newSheepData gets initialized to 0, 10, and 20
## then newSheepData gets its second item removed, so it outputs 0 and 20

## We can confirm that indices has 0, 2, ... , 20 by increments of 2
## sheepData accesses the indices until it reaches the
## last element of itself, which is 95 

#Exercise 3 --- 
product <- 1 
for(num in seq(from = 1, to = 15, by = 1)){
  product = product*num
  print(product)
}


#Exercise 4 ---
sum <- 0 
for(num in seq(from = 1, to = 10, by = 1)){
  sum = num + sum 
  print(sum)
}

#Exercise 5 ---
grade <- sample.int(100, 1)
if (grade > 50){
  print("A")
} else{
  print("B")
}

#Exercise 6 ---
r_d <- 0.5 
K <- 50 
popVector <- rep(0, 11) 
popVector[1] <- 10 

for(year in seq(1, 10)){
  popVector[year+1] = popVector[year] + (r_d*(popVector[year])*(1- (popVector[year]/K)))
}
plot(popVector, type = "o")

par("mar")
par(mar=c(1,1,1,1))
