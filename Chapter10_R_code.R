
# Chapter 10: R code
# Copyright: Clive Beggs - 3rd April 2023

# Code for Example 10.1

rm(list = ls())    # Clears all variables from the workspace

# Load data
regdata <- read.csv("C:/Datasets/EPL_regression_data_2020_2021.csv", sep=",")
head(regdata) # Displays the first six rows

# Split data into 2020-21 and 2021-22 seasons
season1 <- regdata[regdata$Season == 2020,]    # 2020-21
season2 <- regdata[regdata$Season == 2021,]    # 2021-22

# Remove the first three columns
s1 <- season1[,c(4:14)]   
s2 <- season2[,c(4:14)]  

# Inspect data
names(s1)

# Season 2020-21 descriptive statistics
library(psych)
s1.temp <- describeBy(s1) # These are the descriptive statistics
s1.stats <- cbind.data.frame(s1.temp$n, s1.temp$mean, s1.temp$sd) # NB. Only relevant statistics selected.
colnames(s1.stats) <- c("S1.n", "S1.mean", "S1.SD") # Rename variables

# Season 2021-22 descriptive statistics
s2.temp <- describeBy(s2) # These are the descriptive statistics
s2.stats <- cbind.data.frame(s2.temp$n, s2.temp$mean, s2.temp$sd) # NB. Only relevant statistics selected.
colnames(s2.stats) <- c("S2.n", "S2.mean", "S2.SD") # Rename variables

# Perform independent t-test
ttresults <- sapply(c(1:11), function(i) {t.test(s1[,i],s2[,i], paired=FALSE)}) # NB. The default is na.rm=TRUE
ttres <- as.data.frame(t(ttresults[3,])) # This selects just the p-values.
pval <- round(as.numeric(t(ttres)),3)

# Compile descriptive statistics results
Variables <- rownames(s1.temp)
stats <- cbind.data.frame(s1.stats,s2.stats)
stats <- round(stats,1)
stats.res <- cbind.data.frame(Variables,stats,pval)
print(stats.res)

#####

# Code for Example 10.2

# Install 'Hmisc' package 
# install.packages("Hmisc")  # This installs the ‘Hmisc’ package. 
# NB. This command only needs to be executed once to install the package. 
# Thereafter, the ‘Hmisc’ library can be called using the ‘library’ command.
library("Hmisc")

# Season 2020-21
s1.cor <- rcorr(as.matrix(s1))
s1.corres <- round(as.data.frame(s1.cor[1]),3)
colnames(s1.corres) <- c("Points","Shots","SoT","ShotDist","PassComp","Dribbles",
                         "Tackles","Crosses","Intercepts","AerialWon","AerialLost")
print(s1.corres)


# Season 2021-22
s2.cor <- rcorr(as.matrix(s2))
s2.corres <- round(as.data.frame(s2.cor[1]),3)
colnames(s2.corres) <- c("Points","Shots","SoT","ShotDist","PassComp","Dribbles",
                         "Tackles","Crosses","Intercepts","AerialWon","AerialLost")
print(s2.corres)

# Build OLS regression models for season 2020-21
# Using Shots
shots.lm <- lm(Points ~ Shots, data = s1)
summary(shots.lm)

# Compute AIC value
AIC(shots.lm)

# Using SoT
SoT.lm <- lm(Points ~ SoT, data = s1)
summary(SoT.lm)

# Compute AIC value
AIC(SoT.lm)

# Scatter plot with bets-fit lines
plot(s1$Shots, s1$Points, pch=4, col="black", xlim=c(0,800), 
     ylim=c(-40,100), 	ylab="Points", xlab="Shots & SoT")
points(s1$SoT, s1$Points, pch=20)
abline(lm(s1$Points ~ s1$Shots), lty=1)
abline(lm(s1$Points ~ s1$SoT), lty=2)
legend(450,15, c("Shots","Shots bestfit line","SoT","SoT bestfit line"),
       cex=0.8, col=c("black","black","black","black"), 
       lty=c(0,1,0,2), pch=c(4,NA,20,NA), bty = "n")  

#####

# Code for Example 10.3

# Remove Shots variables from data sets
build.dat <- s1[,c(1,3:11)] # Here we use season 2020-21 to build the linear model.
names(build.dat)

# Create multiple linear model for season 2020-21 using build.dat
# Base model
s1.lm1 <- lm(Points ~ SoT + ShotDist + PassComp + Dribbles + 
               Tackles + Crosses + Intercepts + AerialWon + AerialLost, data=build.dat)

# Alternative method
#s1.lm1 <- lm(Points ~ ., data=build.dat)

# Produce summary
summary(s1.lm1)

# Remove ‘PassComp’ and repeat process
s1.lm2 <- lm(Points ~ SoT + ShotDist + Dribbles + 
               Tackles + Crosses + Intercepts + AerialWon + AerialLost, data=build.dat)
summary(s1.lm2)

# Next, eliminate 'AerialWon'
s1.lm3 <- lm(Points ~ SoT + ShotDist + Dribbles + 
               Tackles + Crosses + Intercepts + AerialLost, data=build.dat)
summary(s1.lm3)

# Next, eliminate ‘Intercepts’
s1.lm4 <- lm(Points ~ SoT + ShotDist + Dribbles + 
               Tackles + Crosses + AerialLost, data=build.dat)
summary(s1.lm4)

#Finally, eliminate ‘ShotDist’
s1.lm5 <- lm(Points ~ SoT + Dribbles + Tackles + Crosses + AerialLost, data=build.dat)
summary(s1.lm5)

# Compute AIC
AIC(s1.lm5)

# Compute 95% confidence intervals of coefficients
confint(s1.lm5)

# Compare models
anova(s1.lm1, s1.lm5)   # This test uses the F-statistic   

# Use automatic ‘step’ function to perform backward exclusion
s1.lm6 <- step(s1.lm1, direction="backward")  # This uses a backwards exclusion methodology.
summary(s1.lm6)

# Compute AIC  
AIC(s1.lm6)

#####

# Code for Example 10.4

# relweights function 
# (Source: Kabacoff RI: R in action: data analysis and graphics with R:
# Simon and Schuster; 2015.)
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  lbls <- names(fit$model[2:nvar])
  rownames(import) <- lbls
  colnames(import) <- "Weights"
  barplot(t(import),names.arg=lbls,
          ylab="% of R-Square",
          xlab="Predictor Variables",
          main="Relative Importance of Predictor Variables",
          sub=paste("R-Square=", round(rsquare, digits=3)),
          ...)
  return(import)
}

# Relative weights for the refined model
relweights(s1.lm5)

# Relative weights for the base model
relweights(s1.lm1)

# Pearson correlation analysis of strongly correlated variables 
cor.temp <- round(cor(cbind(build.dat$Points,build.dat$SoT,build.dat$PassComp,build.dat$AerialLost)),3)
colnames(cor.temp) <- c("Points","SoT","PassComp","AerialLost")
rownames(cor.temp) <- c("Points","SoT","PassComp","AerialLost")
print(cor.temp)

# Regression model using PassComp as the predictor
s1.lm7 <- lm(Points ~ PassComp, data=build.dat)
summary(s1.lm7)

# Compute VIF values using the 'car' package
# install.packages("car")  # This installs the ‘car’ package. 
# NB. This command only needs to be executed once to install the package. 
# Thereafter, the ‘car’ library can be called using the ‘library’ command.

library(car)  
vif(s1.lm1) # Calculate the variance inflation factors.
vif(s1.lm5) # Calculate the VIF values for refined regression model. 

#####

# Code for Example 10.5

# Season 2020-21
s1.pred <- predict(s1.lm5, data = build.dat)
print(s1.pred)

# Create vectors containing the observed and predicted results 
obs.s1 <- build.dat$Points
pred.s1 <- s1.pred

# Produce scatter plot for season 2020-21
plot(pred.s1, obs.s1, pch=20, col="black", xlim=c(20,90), 
     ylim=c(0,100), ylab="Predicted points", xlab="Observed points")

# Compute model-fit metrics
s1.r2 <- cor(obs.s1,pred.s1)^2 # R^2
print(s1.r2)

s1.mae <- mean(abs(obs.s1 - pred.s1)) # Mean absolute error (mae)
print(s1.mae)

# Now we test the predictive ability of the linear regression model using the 
# data for season 2021-22 
test.dat <- s2[,c(1,3:11)]

s2.pred <- predict(s1.lm5, data = test.dat, type="response")
print(s2.pred)

# Create vectors containing the new observed and predicted results 
obs.s2 <- test.dat$Points
pred.s2 <- s2.pred

# Produce scatter plot for season 2021-22
plot(pred.s2, obs.s2, pch=20, col="black", xlim=c(20,90), 
     ylim=c(0,100), ylab="Predicted points", xlab="Observed points")

# Compute model-fit metrics
s2.r2 <- cor(obs.s2,pred.s2)^2 # R^2
print(s2.r2)

s2.mae <- mean(abs(obs.s2 - pred.s2)) # Mean absolute error (mae)
print(s2.mae)

#####

# Code for Example 10.6

# install.packages("lmtest")  # This installs the ‘lmtest’ package. 
# NB. This command only needs to be executed once to install the package. 
# Thereafter, the ‘lmtest’ library can be called using the ‘library’ command.

# Breusch-Pagan test
library(lmtest)   
bptest(s1.lm5)  

# Shapiro-Wilks test on the residuals.
residuals <- obs.s1 - pred.s1 # Computes the residuals for s1.lm5 using 2020-21 data.
shapiro.test(residuals) 

# Durbin-Watson test 
library(lmtest)    
dwtest(s1.lm5)

# Create diagnostic plots
par(mfrow=c(2,2))                    # Visualize four graphs at once
plot(s1.lm5)
par(mfrow=c(1,1))                    # Reset the graphics defaults

#####


