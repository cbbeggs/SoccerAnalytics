
# Chapter 11: R code
# Copyright: Clive Beggs - 6th April 2023

# Code for Example 11.1

rm(list = ls())    # This clears all variables from the work space

# Create data
Player <- c("Peter","Jane","John","Paul","Anne","Sarah","Lucy","Tom","Sean","David")
Sex <- c("Male","Female","Male","Male","Female","Female","Female","Male","Male","Male")
SexID <- c(1, 2, 1, 1, 2, 2, 2, 1, 1, 1) # Numerical classifier of gender
Age <- c(19, 21, 22, 24, 19, 21, 27, 25, 20, 18)
Height <- c(1.85, 1.64, 1.76, 1.83, 1.62, 1.57, 1.69, 1.80, 1.75, 1.81)
Weight <- c(80.4, 67.1, 75.4, 81.2, 65.2, 63.7, 66.3, 77.5, 73.4, 81.2)
BMI <- round((Weight/(Height)^2),2) # Computes BMI and rounds to 2 decimal places 

dat <- cbind.data.frame(Player, Sex, SexID, Age, Height, Weight, BMI)
print(dat)

# Inspect data
summary(dat)

# Look at the structure of the data
str(dat)

# Create some factors
dat$Sex <- as.factor(Sex)
dat$SexID <- as.factor(SexID)

# Review the structure
str(dat)

# Summary
summary(dat)

#####

# Code for Example 11.2

rm(list = ls()) # This clears all variables from the work space

# Load data (NB. Data acquired from https://fbref.com/en/comps/9/2021-2022/2021-2022-Premier-League-Stats)
mydata <- read.csv("C:/Datasets/EPL_regression_data_2020_2021.csv", sep=",")
rfdata <- mydata[,4:14]

# Inspect data
head(rfdata)

# Install 'randomForest' package 
# install.packages("randomForest")  # This installs the ‘randomForest’ package. 
# NB. This command only needs to be executed once to install the package. 
# Thereafter, the 'randomForest' library can be called using the ‘library’ command.

library(randomForest)
set.seed(123) # Set a seed so that the results are repeatable.
rf.mod1 = randomForest(Points ~., data = rfdata)
print(rf.mod1)

# Compte relative importance of predictor variables
importance(rf.mod1)
# Produce scree plot
varImpPlot(rf.mod1) 

# Build refined model
set.seed(123) # Set a seed so that the results are repeatable.
rf.mod2 = randomForest(Points ~ SoT + PassComp + Shots + AerialLost, data = rfdata)
print(rf.mod2)

# Prediction using full model
mod1.pred <- predict(rf.mod1, data = rfdata)

# Create vectors containing the new observed and predicted results 
obs.mod1 <- rfdata$Points
pred.mod1 <- mod1.pred
mod1.dat <- cbind.data.frame(obs.mod1,pred.mod1)

# Produce scatter plot of observed and predicted results for season 2020-21
library(ggplot2)  # Load package 'ggplot2' 
ggplot(mod1.dat, aes(x=pred.mod1, y=obs.mod1)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm, se = FALSE) +   # Add linear regression line 
  xlab("Predicted points") +
  ylab("Observed points") 

# Compute mean absolute error
mod1.mae <- mean(abs(obs.mod1 - pred.mod1)) # Mean absolute error (mae)
print(mod1.mae)

# Prediction using refined model
mod2.pred <- predict(rf.mod2, data = rfdata)

# Create vectors containing the new observed and predicted results 
obs.mod2 <- rfdata$Points
pred.mod2 <- mod2.pred
mod2.dat <- cbind.data.frame(obs.mod2,pred.mod2)

# Produce scatter plot of observed and predicted results for season 2021-22
library(ggplot2)  # Load package 'ggplot2' 
ggplot(mod2.dat, aes(x=pred.mod2, y=obs.mod2)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm, se = FALSE) +   # Add linear regression line 
  xlab("Predicted points") +
  ylab("Observed points") 

# Compute mean absolute error
mod2.mae <- mean(abs(obs.mod2 - pred.mod2)) # Mean absolute error (mae)
print(mod2.mae)

#####

# Code for Example 11.3

rm(list = ls())    # This clears all variables from the work space

# Country A
meanA <- 178 # Height in cm.
sdA <- 7 # Standard deviation in cm

# Country B
meanB <- 180 # Height in cm.
sdB <- 7 # Standard deviation in cm

npop <- 1000000 # Population size

# Create two vectors for each population (each npop long).
A <- rnorm(npop, meanA, sdA)
B <- rnorm(npop, meanB, sdB)

# Create table of descriptive statistics
CountryA <- c(mean(A),sd(A))
CountryB <- c(mean(B),sd(B))

results <- cbind.data.frame(CountryA,CountryB)
row.names(results) <- c("Mean","SD")
print(round(results,1))

# Specify sample size
nsamp <- 30

# 1000 simulations
nsim <- 1000
pval <- matrix(0,nsim,4)

# Install 'lsr' package 
# install.packages("lsr")  # This installs the ‘lsr’ package. 
# NB. This command only needs to be executed once to install the package. 
# Thereafter, the 'lsr' library can be called using the ‘library’ command.

library(lsr) # This library is required to compute Cohen's d.
set.seed(123) # Sets seed for repeatability
for(i in 1:nsim){
  sampA <- sample(A,nsamp)
  sampB <- sample(B,nsamp)
  dh <- mean(sampB)-mean(sampA) # Difference in sample means (the effect)
  es <-cohensD(sampA, sampB) # Computes Cohen's d effect size
  t <- t.test(sampA,sampB, pair=FALSE) # Independent t-test
  pval[i,1] <- round(dh,2)
  pval[i,2] <- round(t$p.value,3)
  if(pval[i,2]<0.05){
    pval[i,3] <-1
  }
  pval[i,4] <- round(es,2)
}

# Display first six rows of pval
colnames(pval) <- c("DiffMeans","p-value","Significant","CohansD")
head(pval)

colMeans(pval)

# Display the mean absolute effect, p-value, and Cohen’s d results.
round(mean(pval[,1]),3) # Mean difference between the sample means
round(mean(pval[,2]),3) # Mean computed p-value
round(mean(pval[,4]),3) # Mean effect size (Cohen's d)

# Compute percentage of significant results
sig.perc <- (sum(pval[,3])/nsim)*100
print(sig.perc)

# Display histogram
p_values <- pval[,2]
hist(p_values,100)
abline(v=0.05, lty=2, lwd=2)

#####








