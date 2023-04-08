
# Chapter 2: R code
# Copyright: Clive Beggs 6th March 2023

# Code for Example 2.1

rm(list = ls())    # This clears all existing variables and data from the workspace.

MatchID <- c(1:10)  # This creates a vector (1,2,3,4,5,6,7,8,9,10) called 'MatchID'. 

print(MatchID)  # This displays the vector called 'MatchID'

GoalsFor <- c(0,2,4,1,3,0,2,2,3,1)   # This is the vector containing the goals scored.
print(GoalsFor)  # This displays the goals scored vector.

GoalsAgainst <- c(1,1,3,3,0,0,1,1,1,0)  # This is a vector containing the goals conceded.
print(GoalsAgainst)  # This displays the goals conceded vector.

GoalDiff <- GoalsFor-GoalsAgainst   # This is a vector containing the goal differences.
print(GoalDiff)   # This displays the goal difference vector.

# Goals scored:
mean(GoalsFor)  # Computes the mean.
median(GoalsFor)  # Computes the median.
sd(GoalsFor)  # Computes the standard deviation.
var(GoalsFor)  # Computes the variance.

# Goals conceded:
round(mean(GoalsAgainst),3)  # Computes the mean to 3 DP.
round(median(GoalsAgainst),3)  # Computes the median to 3 DP.
round(sd(GoalsAgainst),3)  # Computes the standard deviation to 3 DP.
round(var(GoalsAgainst),3)  # Computes the variance to 3 DP.

# Goal difference:
round(mean(GoalDiff),3)  # Computes the mean to 3 DP.
round(median(GoalDiff),3)  # Computes the median to 3 DP.
round(sd(GoalDiff),3)  # Computes the standard deviation to 3 DP.
round(var(GoalDiff),3)  # Computes the variance to 3 DP.

# Compile data frame
goals_dat <- cbind.data.frame(MatchID, GoalsFor, GoalsAgainst, GoalDiff) # Creates data frame
print(goals_dat) # Displays data frame

#####

# Code for Example 2.2

names(goals_dat)

head(goals_dat)

head(goals_dat, 8)  # This tells R to display the first eight rows.

tail(goals_dat)

nrow(goals_dat)  # This gives the number of rows in the data frame. 
ncol(goals_dat)  # This gives the number of columns in the data frame.

dim(goals_dat)  # This gives the dimensions of the data frame.

str(goals_dat) # Displays the structure of the data.


# Method 1: Specifying the variable name using the ‘$’ symbol.
goals_dat$GoalsFor  # Here, $ denotes that ‘GoalsFor’ is a variable belonging to ‘goals_dat’.

# Method 2: Specifying the variable position using square brackets.
goals_dat[,2]  # Here, the square brackets are used to select the second column.

# We use [,c(2,3)] to display both second and third columns in ‘goals_dat’.
goals_dat[,c(2,3)]  # This selects the second and third variables in ‘goals_dat’. 

# We can use [c(3,4,5),] to select rows 3,4 and 5 as follows:
goals_dat[c(3,4,5),]  # This selects the third, fourth and fifth rows in ‘goals_dat’. 

#####

# Code for Example 2.3

names(goals_dat)

# Method 1. Use the 'ifelse' function to specify the match outcome. 
outcome1 <- ifelse(goals_dat$GoalsFor > goals_dat$GoalsAgainst, "Win", "Did not win")
print(outcome1)

# Method 2. Use the 'if' function in conjunction with a ‘for loop’ to specify the match outcome.
outcome2 <- c()  # This creates an empty vector in which to store the results.
n <- nrow(goals_dat)  
for(i in 1:n){
  if(goals_dat$GoalsFor[i] > goals_dat$GoalsAgainst[i]){outcome2[i] <- "Win"}
  if(goals_dat$GoalsFor[i] < goals_dat$GoalsAgainst[i]){outcome2[i] <- "Lose"}
  if(goals_dat$GoalsFor[i] == goals_dat$GoalsAgainst[i]){outcome2[i] <- "Draw"}
  }

print(outcome2)

# Compile data frame
match_dat <- cbind.data.frame(goals_dat, outcome2)
names(match_dat)

# Rename 'outcome2'
colnames(match_dat)[colnames(match_dat) == 'outcome2'] <- 'Result'
print(match_dat)

# Find all matches where the team won
winResults <- match_dat[match_dat$Result == "Win",]    # Selects games in which the team won
print(winResults)
                                                                      
#####

# Code for Example 2.4

names(match_dat)

# Produce summary of descriptive statistics.
summary(match_dat) 

# Install the ‘psych’ library package.
# install.packages("psych")  # This installs the ‘psych’ package. 
# NB. This command only needs to be executed once to install the package.
# Thereafter, the ‘psych’ library can be called using the command:
library(psych)   # This loads the psych library into R.

# Producing descriptive statistics using the ‘describeBy’ function.
describeBy(match_dat)

# Select just the relevant variables and produce data frame of descriptive results.
des_res <- describeBy(match_dat[,c(2:4)])
print(des_res)

#####

# Code for Example 2.5

rm(list = ls())    # This clears all variables and data from the workspace.

dat <- read.csv("C:/Datasets/Arsenal_Chelsea_comparison.csv")
print(dat)

# Produce descriptive statistics
library(psych)   # This loads the psych library into R.
des_results <- describeBy(dat[,c(2:7)])
print(des_results)

# Export results as CSV file
# write.csv(des_results, "C:/AnalysisResults/descriptive_results.csv")  

#####

# Code for Example 2.6

seasons <- c("2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")

plot(seasons, dat$Arsenal_GF, type="o", lty=1, pch=20, col="black", ylim=c(0,140), 
     ylab="Goals", xlab="Season")
lines(seasons, dat$Chelsea_GF, type="o", lty=2, pch=20)
lines(seasons, dat$Arsenal_GA, type="o", lty=1, pch=4)
lines(seasons, dat$Chelsea_GA, type="o", lty=2, pch=4)
legend(2011,145, c("Arsenal goals for","Arsenal goals against","Chelsea goals for",
      "Chelsea goals against"), cex=0.8, col=c("black","black","black","black"),
       lty=c(1,1,2,2), pch=c(20,4,20,4), bty = "n")
title("Arsenal and Chelsea comparison")

#####

# Code for Example 2.7

# Produce box plot
boxplot(dat[,c(2,3,5,6)], ylab="Goals")

# Produce summary
summary(dat[,c(2,3,5,6)])

#####

# Code for Example 2.8

plot(dat$Chelsea_GA, dat$Chelsea_points, pch=20, col="black", xlim=c(0,60), 
     ylim=c(0,100), ylab="Points", xlab="Goals conceded")
points(dat$Arsenal_GA, dat$Chelsea_points, pch=4)
abline(lm(dat$Chelsea_points ~ dat$Chelsea_GA), lty=1)
abline(lm(dat$Arsenal_points ~ dat$Arsenal_GA), lty=2)
legend(5,40, c("Arsenal goals conceded","Arsenal bestfit line","Chelsea goals conceded","Chelsea bestfit line"),
       cex=0.8, col=c("black","black","black","black"), 
       lty=c(0,1,0,2), pch=c(4,NA,20,NA), bty = "n")

#####

# Code for Example 2.9

# Paired t-tests
t.test(dat$Arsenal_GF, dat$Chelsea_GF, paired=TRUE) 
t.test(dat$Arsenal_GA, dat$Chelsea_GA, paired=TRUE)

# Pearson correlation tests
cor.test(dat$Arsenal_GA,dat$Arsenal_points) # Arsenal
cor.test(dat$Chelsea_GA,dat$Chelsea_points) # Chelsea

#####

# Code for Example 2.10

# Building the linear regression models

# Arsenal
Arsenal.lm <- lm(Arsenal_points ~ Arsenal_GA + Arsenal_GF, data = dat)
summary(Arsenal.lm) # This produces a summary.

# Chelsea
Chelsea.lm <- lm(Chelsea_points ~ Chelsea_GA + Chelsea_GF, data = dat)
summary(Chelsea.lm) # This produces a summary.

# Making predictions

# Arsenal
Arsenal.pred <- round(predict(Arsenal.lm, data = dat, type="response"),1)

# Chelsea
Chelsea.pred <- round(predict(Chelsea.lm, data = dat, type="response"),1)

# Create data frame including predictions
new.dat <- cbind.data.frame(dat,Arsenal.pred,Chelsea.pred)
print(new.dat)

# Produce scatter plots

# Arsenal
obs.Arsenal <- new.dat$Arsenal_points
pred.Arsenal <- new.dat$Arsenal.pred

# Chelsea
obs.Chelsea <- new.dat$Chelsea_points
pred.Chelsea <- new.dat$Chelsea.pred

# install.packages("ggplot2") # NB. This only has to be used once to install ggplot2.

library(ggplot2)  # Call the 'ggplot2' package.

# Produce scatter plot of observed and predicted results for Arsenal
ggplot(new.dat, aes(x=pred.Arsenal, y=obs.Arsenal)) +
  geom_point(color="black", size=3) +    # Specify data point size and colour
  geom_smooth(method=lm, se = FALSE) +   # Add linear regression line
  xlab("Predicted points") +
  ylab("Observed points") 

# Produce scatter plot of observed and predicted results for Chelsea
ggplot(new.dat, aes(x=pred.Chelsea, y=obs.Chelsea)) +
  geom_point(color="black", size=3) +    # Specify data point size and colour
  geom_smooth(method=lm, se = FALSE) +   # Add linear regression line
  xlab("Predicted points") +
  ylab("Observed points")

#####
