
# Chapter 6: R code
# Copyright: Clive Beggs 15th March 2023

# Code for Example 6.1

rm(list = ls())    # Clears all variables from the workspace

# Import historical match data
mydata <- head(read.csv('https://www.football-data.co.uk/mmz4281/1819/E0.csv'),380)

# Inspect data
names(mydata)

# We select the following variables for inclusion in a working data frame called ‘dat’.
#	Date = Match date (dd/mm/yy)
#	HomeTeam = Home team
#	AwayTeam = Away team
#	FTHG = Full time home team goals
#	FTAG = Full time away team goals
#	FTR = Full time result (H=Home Win, D=Draw, A=Away Win)
#	PSH = Decimal odds for a home win as computed by Pinnacle
#	PSH = Decimal odds for a draw as computed by Pinnacle
#	PSH = Decimal odds for a away win as computed by Pinnacle

dat <- mydata[,c("Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","PSH","PSD","PSA")] 
names(dat)

# Rename column names
colnames(dat)[colnames(dat) == 'FTHG'] <- 'Hgoals'
colnames(dat)[colnames(dat) == 'FTAG'] <- 'Agoals'
colnames(dat)[colnames(dat) == 'FTR'] <- 'Result'
colnames(dat)[colnames(dat) == 'PSH'] <- 'HWodds'
colnames(dat)[colnames(dat) == 'PSD'] <- 'Dodds'
colnames(dat)[colnames(dat) == 'PSA'] <- 'AWodds'

# Display the first six rows of dat
head(dat)

# Produce table of home and away goal frequencies
n <- nrow(dat)
location <- rep(c("Home", "Away"), each = n)
goals <- c(dat$Hgoals, dat$Agoals)
goal.dat <- cbind.data.frame(location, goals)
table(goal.dat)

# Plot home and away goal frequencies.
hg.freq <- table(dat$Hgoals)
ag.freq <- table(dat$Agoals)
hgoals.frac <- hg.freq/n
agoals.frac <- ag.freq/n

# Combined plot
yy <- rbind(hgoals.frac, agoals.frac)
barplot(yy, beside=TRUE,col=c("darkgray","lightgray"), names.arg=c(0:6),ylim=c(0,0.4), axis.lty=1, xlab="Goals scored", ylab="Fraction")
legend(12,0.35, c("Home team","Away team"), col=c("darkgray","lightgray"), pch=c(15,15), bty="n")

# Compute and display the expected goals scored.
hgoals.mean <- mean(dat$Hgoals)
agoals.mean <- mean(dat$Agoals)

# Display results
print(hgoals.mean)
print(agoals.mean)

# Plot Poisson distributions
scale <- c(0:6)
plot(scale, dpois(x=0:6, lambda=hgoals.mean), type="o", lty=1, pch=20, ylim=c(0,0.4), xlab="Goals", ylab="Fraction")
lines(scale, dpois(x=0:6, lambda=agoals.mean), type="o", lty=2, pch=21)
legend(3,0.35, c("Home team","Away team"), lty=c(1,2), pch=c(20,21), bty = "n")

# Correlation with home team goals
cor(hgoals.frac, dpois(x=0:6, lambda=hgoals.mean))

# Correlation with away team goals
cor(agoals.frac, dpois(x=0:6, lambda=agoals.mean))

#####

# Code for Example 6.2

# Inspect data
names(dat)

# Select variables for inclusion in model
build.dat <- dat[1:370,2:5] # Select the first 370 matches and variables 2-5 for analysis.

# Rename variables
names(build.dat)[names(build.dat) == 'HomeTeam'] <- 'Home'
names(build.dat)[names(build.dat) == 'AwayTeam'] <- 'Away'
names(build.dat)

## [1] "Home"   "Away"   "Hgoals" "Agoals"

# Put data into long-form
long_dat <- rbind(
  data.frame(Home = 1,
             Team = build.dat$Home,
             Opponent = build.dat$Away,
             Goals = build.dat$Hgoals),
  data.frame(Home = 0,
             Team = build.dat$Away,
             Opponent = build.dat$Home,
             Goals = build.dat$Agoals))

# Inspect long-form data
head(long_dat)
tail(long_dat)

# Build Poisson model
pois.mod <- glm(Goals ~ Home + Team + Opponent, family=poisson(link=log), data=long_dat)
summary(pois.mod)

# Review remining matches
remain_matches <- dat[371:380,1:5]
print(remain_matches)

# Apply Poisson model to predict match result
# Specify teams
HTeam <- "Burnley"
ATeam <- "Arsenal"

# Predict expected (average) goals.
# Home Team
Hgoals.exp <- predict(pois.mod, data.frame(Home=1, Team=HTeam, Opponent=ATeam), type="response")
print(Hgoals.exp)

# Away Team
Agoals.exp <- predict(pois.mod, data.frame(Home=0, Team=ATeam, Opponent=HTeam), type="response")
print(Agoals.exp)

# Compute probability matrix
max_goals <- 8  # This specifies the maximum number of goals to be scored.
#prob.mat <- round(dpois(0:max_goals, Hgoals.exp) %o% dpois(0:max_goals, Agoals.exp),4)
#print(prob.mat)

prob.mat <- round(dpois(0:max_goals, Hgoals.exp) %*% t(dpois(0:max_goals, Agoals.exp)),4)
print(prob.mat)

# Compute probabilities
# Home win
sum(prob.mat[lower.tri(prob.mat)]) # Sum of lower triangle is the probability of a home win.

# Draw
sum(diag(prob.mat)) # Sum of diagonal is the probability of a draw.

# Away win
sum(prob.mat[upper.tri(prob.mat)]) # Sum of upper triangle is the probability of an away win.

#####

# Code for Example 6.3
# (NB. Code adapted from Opisthokonta.net; https://opisthokonta.net/?p=1685)

# Fit model
expected <- fitted(pois.mod)

# Compile and display fitted results
exp.dat <- cbind.data.frame(long_dat, expected)
head(exp.dat)
tail(exp.dat)

# Create home.exp and away.exp vectors
home.exp <- expected[1:nrow(build.dat)]
away.exp <- expected[(nrow(build.dat)+1):(nrow(build.dat)*2)]

# Inspect these vectors
head(home.exp)
tail(away.exp)

# Construct user-defined function is called ‘Tau’
tau <- Vectorize(function(x, y, lambda, mu, rho){
  if (x == 0 & y == 0){return(1 - (lambda*mu*rho))
  } else if (x == 0 & y == 1){return(1 + (lambda*rho))
  } else if (x == 1 & y == 0){return(1 + (mu*rho))
  } else if (x == 1 & y == 1){return(1 - rho)
  } else {return(1)}
})

# Construct user-defined function called ‘logLike’
logLike <- function(y1, y2, lambda, mu, rho=0){
  sum(log(tau(y1, y2, lambda, mu, rho)) + log(dpois(y1, lambda)) + log(dpois(y2, mu)))
} # Here, y1 is the home goals and y2 is the away goals.

# Construct user-defined function 'optRho'
optRho <- function(par){
  rho <- par[1]
  logLike(build.dat$Hgoals, build.dat$Agoals, home.exp, away.exp, rho)
}

# Run optimization process using the Broyden–Fletcher–Goldfarb–Shanno (BFGS) algorithm
res <- optim(par=c(0.1), fn=optRho, control=list(fnscale=-1), method='BFGS')
Rho <- res$par # This is the optimum value of rho.
print(Rho)

# Adjust to the match probability matrices 
HT <- "Burnley"  # Home team
AT <- "Arsenal"  # Away team

# Expected goals home
lambda <- predict(pois.mod, data.frame(Home=1, Team=HT, Opponent=AT), type='response')

# Expected goals away
mu <- predict(pois.mod, data.frame(Home=0, Team=AT, Opponent=HT), type='response')

# Display results
print(lambda)
print(mu)

# Compute the raw probability matrix without the Dixin-Coles adjustment.
maxgoal <- 8
prob_mat1 <- dpois(0:maxgoal, lambda) %*% t(dpois(0:maxgoal, mu))
print(round(prob_mat1,4)) # This is the unadjusted probability matrix

# Apply the Dixon-Coles adjustment by computing a [2 x 2] scaling matrix
scale_mat <- matrix(tau(c(0,1,0,1), c(0,0,1,1), lambda, mu, Rho), nrow=2)
prob_mat2 <- prob_mat1 # Makes copy of raw probability matrix
prob_mat2[1:2, 1:2] <- prob_mat1[1:2, 1:2] * scale_mat # Replaces original values
print(round(prob_mat2,4)) # This is the adjusted probability matrix

# Compute the probabilities of the various match outcomes. 
# Home win
sum(prob_mat2[lower.tri(prob_mat2)]) # Sum of lower triangle is probability of a home win.

# Draw
sum(diag(prob_mat2)) # Sum of diagonal is probability of a draw.

# Away win
sum(prob_mat2[upper.tri(prob_mat2)]) # Sum of upper triangle is probability of an away win.

#####

# Code for Example 6.4

# Inspect data
head(dat) # Displays first six rows of dat

# Select the variables for inclusion in random forest model. 
rf.dat <- dat[,c("Result","HWodds","Dodds","AWodds")]
rf.dat$Result <- as.factor(rf.dat$Result) # Ensure that the results are treated as a factor.
str(rf.dat)

# Split data into the training and testing data sets
rf_train.dat <- rf.dat[1:370,] # First 370 matches is the training data set
rf_test.dat <- rf.dat[371:380,] # Last 10 matches is the testing data set

#install.packages("randomForest")  # This installs the ‘randomForest’ package. 
# NB. This command only needs to be executed once to install the package. 
# Thereafter, the ‘randomForest’ library can be called using the ‘library’ command:

# Load 'randomForest' library
library(randomForest)
set.seed(123) # Set a seed so that the results are repeatable.
rf.mod = randomForest(Result ~ HWodds + Dodds + AWodds, data=rf_train.dat)
print(rf.mod)

# Assess the relative importance of the various predictor variables 
importance(rf.mod)

# Produces variable importance plot
varImpPlot(rf.mod)

# Make predictions
rf.pred <- predict(rf.mod, newdata=rf_test.dat, type="response") # NB. Testing data set used. 
rf.pred

# Compare predictions with actual results
pred.comp <- cbind.data.frame(rf_test.dat,rf.pred)
print(pred.comp)

# Calculate accuracy and kappa value for the predictions using 'caret' 
#install.packages("caret")  # This installs the ‘caret’ package. 
# NB. This command only needs to be executed once to install the package. 
# Thereafter, the ‘caret’ library can be called using the ‘library’ command.

library(caret)
confusionMatrix(rf.pred, rf_test.dat$Result)

#####

# Code for Example 6.5

# install.packages("party")  # This installs the ‘party’ package. 
# NB. This command only needs to be executed once to install the package. 
# Thereafter, the ‘party’ library can be called using the ‘library’ command.

# Create conditional inference tree
library(party)  # Load 'party' library
ctree.mod = ctree(Result ~ HWodds + Dodds + AWodds, data = rf_train.dat)
print(ctree.mod)

# Plot the tree
plot(ctree.mod)

# Produce table of results
table(predict(ctree.mod), rf_train.dat$Result)

# Produce predictions
cif.pred <- predict(ctree.mod, newdata=rf_test.dat, type="response")
print(cif.pred)

# Apply test criteria
n <- nrow(rf_test.dat)
res.vec <- matrix(0,n,1) # This creates an empty vector of length n to store the results.

for(i in 1:n){
  if(rf_test.dat$HWodds[i] <=1.97){res.vec[i] <- "H"}
  else if(rf_test.dat$HWodds[i] >2.89 & rf_test.dat$AWodds[i] <=1.76){res.vec[i] <- "A"}
  else {res.vec[i] <- "NA"}
}

# Display results
print(cbind(rf_test.dat, res.vec))

#####

# Code for Key Concepts Box 6.1

data(iris)
library(randomForest)
set.seed(123)
RF.model = randomForest(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
print(RF.model)

importance(RF.model)

####


