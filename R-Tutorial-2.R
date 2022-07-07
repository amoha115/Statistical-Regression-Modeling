##########################################################
# R-Tutorial-2
# Linear Regression
#########################################################

# using R's build in data set cars
simpleCarData <-cars


# display the begining of the data
head(simpleCarData)


#####################################
# Looking at the data visually 
#####################################

# Scatter Plot
scatter.smooth(x=simpleCarData$speed, y=simpleCarData$dist,
               main="Dist ~ Speed",
               xlab = "Speed", 
               ylab = "Dist")


# Box Plot 
# Good for checking for outliers in the data

par(mfrow=c(1, 2))  # divide graph area into 1 row and  2 columns

# box plot for 'speed'
boxplot(simpleCarData$speed, 
        main="Speed", 
        sub=paste("Outlier rows: ", 
        boxplot.stats(simpleCarData$speed)$out)) 

# box plot for 'distance'
boxplot(cars$dist, main="Distance", 
        sub=paste("Outlier rows: ", 
        boxplot.stats(simpleCarData$dist)$out))  

#################
# Outlier Rule
#################
# An outlier is any datapoint that lies outside the 1.5 * inter quartile range 

installed.packages('e1071')
library(e1071)

par(mfrow=c(1, 2))  # divide graph area in 2 columns

# density plot for 'speed'
plot(density(simpleCarData$speed), 
     main="Density Plot: Speed", 
     ylab="Frequency", 
     sub=paste("Skewness:", 
     round(e1071::skewness(simpleCarData$speed), 2))) 

polygon(density(simpleCarData$speed), col="red")



plot(density(simpleCarData$dist), 
     main="Density Plot: Distance", 
     ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2))) 

polygon(density(simpleCarData$dist), col="blue")



#####################################
# Correlation Analysis 
#####################################
# Remember that correlation gives us a LINEAR dependence between two variables

# calculate correlation between speed and distance
cor(simpleCarData$speed, simpleCarData$dist) 


par(mfrow=c(1, 2))

plot( simpleCarData$speed, simpleCarData$dist,
      main="Dist ~ Speed",
      xlab = "Speed", 
      ylab = "Dist")

scatter.smooth(x=simpleCarData$speed, y=simpleCarData$dist,
               main="Dist ~ Speed",
               xlab = "Speed", 
               ylab = "Dist")


#####################################
# Building linear regression model
#####################################
#  y = b + mx
#  dist = intercept + beta * speed 

regressionModel <- lm(dist ~ speed, data=simpleCarData)  # build linear regression model on full data
print(regressionModel)

#  dist = -17.579  + 3.932 speed 


#####################################
# p Value To Check For Statistical Significance
#####################################
# linear model to be statistically significant only when both these p-Values 
# are less than the pre-determined statistical significance level of 0.05

summary(regressionModel)


#####################################
# Null and Alternate Hypothesis
#####################################
# The Null Hypothesis (H0) is that the beta coefficients 
# associated with the variables is equal to zero.

# The alternate hypothesis (H1) is that the coefficients are not equal to zero. 

# A larger t-value indicates that it is less likely that the coefficient 
# is not equal to zero purely by chance. 

# When p Value is less than significance level (< 0.05), you can safely 
# reject Null Hypothesis (H0)

#####################################
# KEY INSIGHTS
#####################################
# STATISTIC              CRITERION
#-----------------------------------
# R-Squared          Higher the better 
# Adj R-Squared      Higher the better
# F-Statistic        Higher the better
# Std. Error         Closer to zero the better
# t-statistic        Should be greater 1.96 for p-value to be less than 0.05



#####################################
# Predicting Linear Models
#####################################
# Create Training and Test data 

# setting seed to reproduce results of random sampling
set.seed(100)  

# row indices for training data
trainingRowIndex <- sample(1:nrow(simpleCarData), 0.8*nrow(simpleCarData)) 

# model training data
trainingData <- simpleCarData[trainingRowIndex, ] 

# test data
testData  <- simpleCarData[-trainingRowIndex, ]   

###################################
# Build the model on training data
###################################

# build the model
lmMod <- lm(dist ~ speed, data=trainingData) 

# predict distance
distPred <- predict(lmMod, testData)  # predict distance

summary (lmMod) 

par(mfrow=c(1, 2))

plot( simpleCarData$speed, simpleCarData$dist,
      main="Original Dist ~ Speed",
      xlab = "Speed", 
      ylab = "Dist")


plot( testData$speed, testData$dist,
      main="Test Data  Dist ~ Speed",
      xlab = "Speed", 
      ylab = "Dist")


#######################################
# Calculate prediction accuracy and error rates
#######################################

# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  

# A higher correlation accuracy implies that the actuals and predicted 
# values have similar directional movement,
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds)



#######################################
# k- Fold Cross validation
#######################################


install.packages('DAAG')
library(DAAG)

par(mfrow=c(1, 1))
cvResults <- suppressWarnings(CVlm(simpleCarData, form.lm=dist ~ speed,
                                   m=5, 
                                   dots=FALSE, 
                                   seed=29, 
                                   legend.pos="topleft",  
                                   printit=FALSE, 
                                   main="Small symbols are predicted values while bigger ones are actuals.")
                              )


attr(cvResults, 'ms')  





