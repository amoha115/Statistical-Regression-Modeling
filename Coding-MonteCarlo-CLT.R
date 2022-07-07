# needed package to use the triangle distribution
# UNCOMMENT THE LINE BELOW IF YOU NEED TO GET THE TRIANGLE LIBRARY
#  install.packages('triangle')

library(triangle)

#######################################################
# Monte Carlo
#######################################################

# plot a histogram random variable with a triangle distribution
hist( rtriangle(1000, a = 10000, b = 30000, c = 25000))

# create a data frame which hold the random inputs
df <- data.frame(cost = rtriangle(1000, a = 10000, b = 35000, c = 25000),
                 revenue =  rexp(1000, 0.0001))

# create an empty data frame to store simulation data 
simulationData <- data.frame( cost = double(), revenue = double(), profit = double())

# run the simulation 500 times
for(i in 1:500){
    
    # draw a random sample for the cost input
    cost <- sample(df$cost , 1 )              
    
    # draw a random sample for the revenue input
    revenue <- sample(df$revenue , 1 )
    
    # run a realization of the  profit model 
    # round the result to 2 digit places
    profit <- round( revenue - cost, digits = 2)
    
    # store the results with inputs 
    simulationData <- rbind(simulationData, data.frame(cost, revenue, profit))
}

par(mfrow=c(3, 1)) 

# make histogram horizontal axis not use scientific 
options(scipen = 999)

# plot histograms 
hist(simulationData$cost)
hist(simulationData$revenue)
hist(simulationData$profit)


par(mfrow=c(1, 1)) 

# get the empirical CDF from the simulation
empiricalCDF <- ecdf(simulationData$profit)

# plot the empirical CDF
plot(empiricalCDF)


quantile(simulationData$profit, probs = c(seq(0,1,by = 0.05)))

px <- data.frame (value = quantile(simulationData$profit, probs = c(seq(0,1,by = 0.05))))
summary(px)

sprintf("Interval of Interest: [ %.2f , %.2f ]",  px$value[2], px$value[20])
sprintf("Min: %.2f    Center: %.2f   Max: %.2f", px$value[1], px$value[11], px$value[21])


#######################################################
# Using CLT with Monte Carlo
#######################################################

storage <- matrix(ncol = 50, nrow = 500)

for(i in 1:50){
  simulationData <- data.frame( cost = double(), revenue = double(), profit = double())
    for(j in 1:500){
      cost <- sample(df$cost , 1 )
      revenue <- sample(df$revenue , 1 )
      profit <- round( revenue - cost, digits = 2)
      simulationData <- rbind(simulationData, data.frame(cost, revenue, profit))
    }
  storage[ , i] <- simulationData$profit
}

storage <- data.frame(storage)

cltData <- data.frame( mean = colMeans(storage))

par(mfrow=c(1, 1)) 

hist(cltData$mean)


# The R function shapiro.test() can be used to perform the Shapiro-Wilk test 
# From the output, the p-value > 0.05 implying that the distribution of the data 
# are not significantly different from normal distribution. 
# If this is the case we can assume  normality.

shapiro.test(cltData$mean)


# build the 90% confidence interval 
n <- nrow(cltData)
cltMean <- mean(cltData$mean)
cltSD <- sd(cltData$mean)

margin <- qt(0.90, df = n-1 )* cltSD/sqrt(n)

lowerBound <- cltMean - margin
higherBound <- cltMean + margin

sprintf("Interval of Interest: [ %.2f , %.2f ]",  lowerBound, higherBound)





