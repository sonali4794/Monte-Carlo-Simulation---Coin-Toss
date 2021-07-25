library(mosaic)
library(dplyr)

#Define the the p and N values calculated using the mathematical formula
p=0.5
N=5
set.seed(1200)
#Define the two possible outcomes, i.e H for heads and T for tails
cointoss <- c("H","T")
cointoss

#Perform a Monte Carlo Simulation 100 times to execute the activity of tossing the coin N times and calculating the no of heads. 
simulation = do(100)*{
  flip_result <- sample(cointoss, size=N, replace = TRUE)
  flip_result
  no_of_heads <- sum(flip_result == "H")
  no_of_heads
  p1 = no_of_heads/N
  p1
}

#Verify the results of the simulation
head(simulation, 100)

#compare simulated and theoretical mean and SD values
mean_theoritical = p
sd_theoritical = (p*(1-p)/N)^0.5

mean_simulation = mean(simulation$result)
sd_simulation = sd(simulation$result)

delta_mean = abs(mean_simulation-mean_theoritical)
delta_SD = abs(sd_simulation - sd_theoritical)

#generate a table summarising
df <- data.frame(Theoretical = c(mean_theoritical,sd_theoritical),
                 Simulated = c(mean_simulation,sd_simulation),
                 Difference = c(delta_mean,delta_SD))
rownames(df) <- c("Mean","SD")
df


#Repeat the sam process for other values of N.

trials <- c(5,10,25,50,100)
trials
i = 1

#Define empty arrays to store values of theoretical and simualted mean and SD. 
mean_theoritical1 = rep(0,5)
sd_theoritical1 = rep(0,5)
mean_simulation1 = rep(0,5)
sd_simulation1 = rep(0,5)

#open for loop to simulate 100 times for each of the 5 values of different trials
for (n in trials) {
  simulation1 = do(100)*{
    flip_result <- sample(cointoss, size=N, replace = TRUE)
    flip_result
    no_of_heads <- sum(flip_result == "H")
    p1 = no_of_heads/n
    p1
  }
  head(simulation1,100)
  
  mean_theoritical1[i] = p
  sd_theoritical1[i] = (p*(1-p)/n)^0.5
  mean_simulation1[i] = mean(simulation1$result)
  sd_simulation1[i] = sd(simulation1$result)
  i=i+1
}

#see the final result 
mean_theoritical1
sd_theoritical1
mean_simulation1
sd_simulation1

#plot grpahs comparing the two curves of SD
plot(trials, sd_theoritical1, type="l", main="Theoretical vs Simulated SD",xlab="No of coin flips", ylab="SD", col="red")
par(new=TRUE)
plot(trials, sd_simulation1, type ="l", main="Theoretical vs Simulated SD",xlab="No of coin flips", ylab="SD", col="blue" )



