#============================================================================= 
# PROGRAMMER: Alwahab Mohammad 
# PANTHER ID: 6130253 
# 
# CLASS: CAP4830 
# SECTION: U01 
# SEMESTER: Fall 2021 
# CLASSTIME: T/TH 12:30-1:45 pm 

# CERTIFICATION: I understand FIU's academic policies, and I certify that this  
#                work is my own and that none of it is the work of any other person. 
#============================================================================= 

library(triangle)

q1 = rtriangle(2000, a = 0, b = 1500, c = 1200)
q2 = rtriangle(2000, a = 0, b = 3500, c = 1000)
p = rtriangle(2000, a = 0, b = 17.50, c = 12.50)
s = rexp(2000, .01)

inputs <- data.frame(q1, q2, p, s)

#=============================================================================

hist(inputs$q1)
hist(inputs$q2)
hist(inputs$p)
hist(inputs$s)

par(mfrow=c(4, 1)) 

#=============================================================================

outputs <- data.frame(q1 = double(), q2 = double(), p = double(), s = double(), value = double())

for(i in 1:1000){
  
  q1 <- sample(inputs$q1 , 1 )
  q2 <- sample(inputs$q2 , 1 )
  p <- sample(inputs$p , 1 )
  s <- sample(inputs$s , 1 )
  value <- round((2700-q1-q2)*p-(s*p))
  
  outputs <- rbind(outputs, data.frame(q1, q2, p, s, value))
  
}

par(mfrow=c(1, 1))
hist(outputs$value)

#=============================================================================

empiricalCDF <- ecdf(outputs$value)

plot(empiricalCDF)

quantile(outputs$value, probs = c(seq(0,1,by = 0.1)))

px <- data.frame (value = quantile(outputs$value, probs = c(seq(0,1,by = 0.1))))
summary(px)

sprintf("Interval of Interest: [ %.2f , %.2f ]",  px$value[3], px$value[9])
sprintf("Min: %.2f    Center: %.2f   Max: %.2f", px$value[1], px$value[6], px$value[11])

#=============================================================================

storage <- matrix(ncol = 100, nrow = 250)

for(i in 1:100){
  outputs <- data.frame( q1 = double(), q2 = double(), p = double(), s = double(), value = double())
  for(j in 1:250){
    q1 <- sample(inputs$q1 , 1 )
    q2 <- sample(inputs$q2 , 1 )
    p <- sample(inputs$p , 1 )
    s <- sample(inputs$s , 1 )
    value <- round((2700-q1-q2)*p-(s*p))
    outputs <- rbind(outputs, data.frame(q1, q2, p, s, value))
  }
  storage[ , i] <- outputs$value
}

storage <- data.frame(storage)

#=============================================================================

cltData <- data.frame( mean = colMeans(storage))

par(mfrow=c(1, 1)) 

hist(cltData$mean)

shapiro.test(cltData$mean)
# the cltData is normally distributed as the p-value given by the Shapiro-Wilk
# normality test was >0.05. This means that the distribution of the data 
# are not significantly different from normal distribution.

#=============================================================================

n <- nrow(cltData)
cltMean <- mean(cltData$mean)
cltSD <- sd(cltData$mean)

margin <- qt(0.80, df = n-1 )* cltSD/sqrt(n)

lowerBound <- cltMean - margin
higherBound <- cltMean + margin

sprintf("Interval of Interest: [ %.2f , %.2f ]",  lowerBound, higherBound)