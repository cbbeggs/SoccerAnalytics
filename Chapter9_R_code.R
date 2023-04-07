
# Chapter 9: R code
# Copyright: Clive Beggs - 31st March 2023

# Code for Example 9.1

rm(list = ls())    # Clears all variables from the workspace

# First we create the results for a mini-soccer league competition.
match1 <- c("Team A","Team E",3,2,"H") # Team A v Team E (score: 3-2)
match2 <- c("Team B","Team F",1,1,"D") # Team B v Team F (score: 1-1)
match3 <- c("Team C","Team G",5,2,"H") # Team C v Team G (score: 5-2)
match4 <- c("Team D","Team H",0,1,"A") # Team D v Team H (score: 0-1)
match5 <- c("Team E","Team D",2,3,"A") # Team E v Team D (score: 2-3)
match6 <- c("Team F","Team C",2,1,"H") # Team F v Team C (score: 2-1)
match7 <- c("Team G","Team B",0,0,"D") # Team G v Team B (score: 0-0)
match8 <- c("Team H","Team A",1,3,"A") # Team H v Team A (score: 1-3)
match9 <- c("Team A","Team F",4,2,"H") # Team A v Team F (score: 4-2)
match10 <- c("Team G","Team D",2,2,"D") # Team G v Team D (score: 2-2)

mini <- rbind.data.frame(match1,match2,match3,match4,match5,
                         match6,match7,match8,match9,match10)
colnames(mini) <- c("HomeTeam","AwayTeam","HG","AG","Results")
mini$HG <- as.numeric(mini$HG) # Convert to integers.
mini$AG <- as.numeric(mini$AG) # Convert to integers.
print(mini)

# Assign numerical values to individual teams
HT <- as.factor(mini$HomeTeam)
levels(HT) <- 1:length(levels(HT))
HT <- as.numeric(HT)  

AT <- as.factor(mini$AwayTeam)
levels(AT) <- 1:length(levels(AT))
AT <- as.numeric(AT) 

# Create new matrix
X <- cbind(HT,AT,mini[,3:4])

# Now we harvest team names and collate them into a vector
teams <- unique(mini$HomeTeam)
teams <- sort(teams)
n <- length(teams)

# Populate adjacency matrix with weights
adj1 <- matrix(0,n,n)
adj2 <- matrix(0,n,n)
p <- nrow(mini)

for (k in 1:p){
  i = X[k,1]
  j = X[k,2] 
  if (adj1[i,j] == 0){adj1[i,j] <- X[k,3]}
  if (adj2[j,i] == 0){adj2[j,i] <- X[k,4]}
}

adj.mat <- adj1+adj2
rownames(adj.mat) <- teams
colnames(adj.mat) <- teams
print(adj.mat) # NB. This adjacency matrix is asymmetrical, indicating a directional graph.

# Plot directional graph using 'qgraph' package. 
# install.packages("qgraph")  # This installs the ‘qgraph’ package. 
# NB. This command only needs to be executed once to install the package. 
# Thereafter, the ‘qgraph’ library can be called using the ‘library’ command.

library(qgraph)
mini.graph1 <- qgraph(adj.mat, labels = teams, label.cex = 2, edge.labels = TRUE, 
                      edge.color="black", edge.label.cex = 2)  # Who-scored-against-who network
title("Goals scored", adj=0.5, line=3)

#####

# Code for Example 9.2

# Create adjacency matrix
WPWadj.mat <- matrix(0,n,n) 

for (k in 1:p){
  i = X[k,1]
  j = X[k,2] 
  if (WPWadj.mat[j,i] == 0){WPWadj.mat[j,i] = 1}
  else {WPWadj.mat[j,i] = WPWadj.mat[j,i] + 1} 
  if (WPWadj.mat[i,j] == 0){WPWadj.mat[i,j] = 1}
  else {WPWadj.mat[i,j] = WPWadj.mat[i,j] + 1} 
}

rownames(WPWadj.mat) <- teams
colnames(WPWadj.mat) <- teams
print(WPWadj.mat) # NB. This adjacency matrix is symmetric and not directional.

# Produce the Colley matrix
sumrowplus2 <- (rowSums(WPWadj.mat)) + 2    # Compute the sum of the rows
d <- diag(sumrowplus2)
C <- (-1*WPWadj.mat) + d  # This is the Colley matrix.
print(C)

# Create win-lose vector
homeResults <- table(mini$HomeTeam,mini$Result)
colnames(homeResults) <- c("HL","HD","HW")
print(homeResults)

awayResults <- table(mini$AwayTeam,mini$Result)
colnames(awayResults) <- c("AW","AD","AL")
print(awayResults)

# Compute win-lose vector
w <- homeResults[,3] + awayResults[,1]
l <- homeResults[,1] + awayResults[,3]
e <- matrix(1,n,1)
v <- e + 0.5*(w-l)
print(v) # This is the win-lose vector.

# Solve equation using the ‘ginv’ function in the ‘MASS’ package
# install.packages("MASS")  # This installs the ‘MASS’ package. 
# NB. This command only needs to be executed once to install the package. 
# Thereafter, the ‘MASS’ library can be called using the ‘library’ command.

library(MASS)
Colley.r <- ginv(C) %*% v

# Compile team ratings
Colley.rat <- cbind.data.frame(teams, round(Colley.r,3))
colnames(Colley.rat) <- c("Team","Rating")
print(Colley.rat)

# Rank teams according to Colley rating score
Colley.temp <- Colley.rat[order(Colley.rat[,2]),]
Colley.rank <- Colley.temp[n:1,]
print(Colley.rank)

# Plot bar chart
teams <- Colley.rank$Team
Rating <- Colley.rank$Rating
par(las=2) # Makes text labels perpendicular to axis
barplot(Rating, horiz=TRUE, names.arg = teams, xlim=c(0,0.8), xlab="Rating")
title("Colley ratings of teams")

#####

# Code for Example 9.3

# Create identify matrix x2
i2 <- diag(n)*2

# Create Massey matrix
M <- C - i2
print(M)

# Compute overall goal differences for each club
y <- matrix(0,n,1) 

for (i in 1:n){
  tempH <- mini[mini$HomeTeam == teams[i],]
  tempA <- mini[mini$AwayTeam == teams[i],]
  GFH <- sum(tempH$HG)
  GFA <- sum(tempA$AG)
  GAH <- sum(tempH$AG)
  GAA <- sum(tempA$HG)
  GF <- GFH+GFA
  GA <- GAH+GAA
  y[i] <- GF-GA
}

print(y) # This is the vector of goal differences.

# Compute Massey ratings
library(MASS)
Massey.r <- ginv(M) %*% y

# Compile team ratings
Massey.rat <- cbind.data.frame(teams, round(Massey.r,3))
colnames(Massey.rat) <- c("Team","Rating")
print(Massey.rat)

# Rank teams according to Massey rating score
Massey.temp <- Massey.rat[order(Massey.rat[,2]),]
Massey.rank <- Massey.temp[n:1,]
print(Massey.rank)

#####

# Code for Example 9.4

Elo.dat <- mini
nobs = nrow(Elo.dat) # Here nobs is the number of observations. 

Elo.dat["HTEP"] <- NA # That creates the new column for the home team Elo points
Elo.dat["ATEP"] <- NA # That creates the new column for the away team Elo points

for(i in 1:nobs){
  if(Elo.dat$Result[i] == "H"){Elo.dat$HTEP[i] <- 1; Elo.dat$ATEP[i] <- 0}
  else if(Elo.dat$Result[i] == "D"){Elo.dat$HTEP[i] <- 0.5; Elo.dat$ATEP[i] <- 0.5}
  else {Elo.dat$HTEP[i] <- 0; Elo.dat$ATEP[i] <- 1}
}

print(Elo.dat)

# Create a ranking matrix and set all teams' initial ranking to zero
team.A <- Elo.dat$HomeTeam
team.B <- Elo.dat$AwayTeam

# Create empty matrix to store Elo rank results
ranks <- matrix(0,n,(nrow(Elo.dat)+1))  
row.names(ranks) <- teams
ranks[,1] <- 1200    # This primes ranks matrix with an initial Elo score of 1200
m <- ncol(ranks)

# Specify K-factor.
K = 18.5

# Create some new columns to store Elo results.
Elo.dat["HomeProb"] <- NA # To store home win expected probability
Elo.dat["AwayProb"] <- NA # To store away win expected probability
Elo.dat["HomeRating"] <- NA # To store updated home team Elo rating 
Elo.dat["AwayRating"] <- NA # To store updated away team Elo rating

# Now we populate the ranks matrix using the Elo algorithm.
for(i in 1:(nrow(Elo.dat))){
  fA <- match(Elo.dat[i,1],teams) # Assign numerical identifier to home team.
  fB <- match(Elo.dat[i,2],teams) # Assign numerical identifier to away team.
  if(ranks[fA,i] == ranks[fB,i]){
    expA <- 0.5 # Expected probability of home team
    expB <- 0.5 # Expected probability of home team 
  }
  else{
    expA <- 1/(1+10^(-((ranks[fA,i]-ranks[fB,i])/400)))  # Expected probability of A
    expB <- 1/(1+10^(-((ranks[fB,i]-ranks[fA,i])/400)))  # Expected probability of B
  }
  rA <- ranks[fA,i] + K*(Elo.dat[i,6]-expA)  # Elo algorithm applied to home team.
  rB <- ranks[fB,i] + K*(Elo.dat[i,7]-expB)  # Elo algorithm applied to away team.
  ranks[,(i+1)] <- ranks[,i]
  ranks[fA,(i+1)] <- rA
  ranks[fB,(i+1)] <- rB
  Elo.dat$HomeProb[i] <- round(expA,3)
  Elo.dat$AwayProb[i] <- round(expB,3)
  Elo.dat$HomeRating[i] <- rA
  Elo.dat$AwayRating[i] <- rB
}

# Display results
print(Elo.dat) # Overall results
print(round(ranks,3)) # Elo ranks

# Compile results
er <- as.data.frame(ranks[,m]) # Select final ratings only
colnames(er) <- c("Elo_Rating")

# Now we use the 'dplyr' package to arrange the teams in descending order. 
# install.packages("dplyr")  # This installs the ‘dplyr’ package. 
# NB. This command only needs to be executed once to install the package. 
# Thereafter, the ‘dplyr’ library can be called using the ‘library’ command.

library(dplyr)
Elo.rank <- er %>% arrange(desc(Elo_Rating))
print(Elo.rank)

#####

# Code for Example 9.5

# Now we use the 'elo' package to arrange the teams in descending order. 
# install.packages("elo")  # This installs the ‘elo’ package. 
# NB. This command only needs to be executed once to install the package. 
# Thereafter, the ‘elo’ library can be called using the ‘library’ command.

library(elo)
mini_EloMod <- elo.run(data = Elo.dat,
                       formula = HTEP ~ HomeTeam + AwayTeam, 
                       k=18.5, initial.elos=1200)

mini_EloRes <- mini_EloMod %>% as.data.frame()
print(mini_EloRes)

# Now we rank the teams.
rank.teams(mini_EloMod, ties.method = "min",)

#####

# Code for Example 9.6

rm(list = ls())    # Clears all variables from workspace

# Load data 
mydata <- head(read.csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv'),380)[,1:11]
names(mydata)

dat <- mydata[,c(4:8)]
colnames(dat) <- c("HomeTeam","AwayTeam","HG","AG","Results")
dat["HTEP"] <- NA # That creates the new column for the home team Elo points
dat["ATEP"] <- NA # That creates the new column for the away team Elo points
ng <- nrow(dat) # Determine number of matches

# Populate empty columns 
for(i in 1:ng){
  if(dat$Result[i] == "H"){dat$HTEP[i] <- 1; dat$ATEP[i] <- 0}
  else if(dat$Result[i] == "D"){dat$HTEP[i] <- 0.5; dat$ATEP[i] <- 0.5}
  else {dat$HTEP[i] <- 0; dat$ATEP[i] <- 1}
}

head(dat, 10)

# Divide data into two sub-groups: training data and testing data. 
dat.train <- dat[1:210,]
dat.test <- dat[211:230,]

# Build model using the training data set, allowing for home advantage.
library(elo)
HGA <- 30 # Home ground advantage
EPL_mod <- elo.run(formula = HTEP ~ adjust(HomeTeam, HGA) + AwayTeam,                    
                   data = dat.train, k=18.5, initial.elos=1200)

EPL_EloRes <- EPL_mod %>% as.data.frame()
head(EPL_EloRes,15) # This displays the first 15 matches.

# Predictions
pred.train <- predict(EPL_mod) # Training data set
head(pred.train,15) # These are the predicted win probabilities for Team A.

# Specify thresholds
p.win <- 0.6
p.lose <- 0.4

# Compile match prediction results for training data set
train.pred <- cbind.data.frame(dat.train[,1:5], pred.train)
train.pred["Prediction"] <- NA # That creates the new column for the home team Elo points
train.pred["Outcome"] <- NA # That creates the new column for the away team Elo points
n.train <- nrow(train.pred)

for(i in 1:n.train){
  if(train.pred$pred.train[i] >= 0.6){train.pred$Prediction[i] <- "H"}
  else if(train.pred$pred.train[i] <= 0.4){train.pred$Prediction[i] <- "A"}
  else {train.pred$Prediction[i] <- "D"}
  if(train.pred$Results[i] == train.pred$Prediction[i]){train.pred$Outcome[i] <- 1}
  else{train.pred$Outcome[i] <- 0}
}

# Display the prediction results
head(train.pred,10)
tail(train.pred,10)

# Check performance of the model.
temp <- train.pred[,c(7,8)]
train.check <- temp[!(temp$Prediction == "D"),] # This removes the 'draws'.
n.pred <- nrow(train.check)
n.correct <- sum(train.check$Outcome)
train.accuracy <- n.correct/n.pred
print(train.accuracy) # This is prediction accuracy with the training data set. 

# Now we use the mode to predict the outcome of the matches in the testing data set.
pred.test <- predict(EPL_mod, newdata = dat.test) # Testing data set

# Compile match prediction results
test.pred <- cbind.data.frame(dat.test[,1:5], pred.test)
test.pred["Prediction"] <- NA # That creates the new column for the home team Elo points
test.pred["Outcome"] <- NA # That creates the new column for the away team Elo points
nm <- nrow(test.pred)

for(i in 1:nm){
  if(test.pred$pred.test[i] >= 0.6){test.pred$Prediction[i] <- "H"}
  else if(test.pred$pred.test[i] <= 0.4){test.pred$Prediction[i] <- "A"}
  else {test.pred$Prediction[i] <- "D"}
  if(test.pred$Results[i] == test.pred$Prediction[i]){test.pred$Outcome[i] <- 1}
  else{test.pred$Outcome[i] <- 0}
}

# Display test data results
print(test.pred)

#####









