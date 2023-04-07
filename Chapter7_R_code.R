
# Chapter 7: R code
# Copyright: Clive Beggs 21st March 2023

# Code for Example 7.1

rm(list = ls())    # Clears all variables from the workspace

# Specify number of players
nplayers <- 10  # Number of players playing roulette
nspins <- 10 # Number of spins per player
wager <- 10 # Wager per spin (pounds)


# Simulate the roulette wheel  
# Create empty matrix to store results
outcome <- matrix(0,nplayers,nspins)
wheel <- c(rep(1,18),rep(-1,19)) # European roulette wheel (i.e. 18 black, 18 red & 1 green)
print(wheel) # This displays the roulette wheel vector.

# Simulate random spins of the roulette wheel for 10 players 
set.seed(234) # This sets the seed so that the results are reproducible.
for (i in 1:nplayers){
  for (j in 1:nspins){
    outcome[i,j] <- sample(wheel,1) # This randomly samples one value from the wheel vector.
  }
}

# Display the results for each player
results <- outcome*wager # This computes the value of the wins and losses
print(results) # Here negative values represent losses and positive values represent a wins

# Compute total profit for each player
profit <- colSums(results) 
print(profit)

# Computes profit for the casino
house.profit <- -1*sum(results) # NB. This converts the negative values into positive ones.
print(house.profit)

# Compute the overall number of wins and losses 
win <- matrix(0,nplayers,nspins)
lose <- matrix(0,nplayers,nspins)

for (i in 1:nplayers){
  for (j in 1:nspins){
    if(outcome[i,j] <0){lose[i,j] <- outcome[i,j]}
    if(outcome[i,j] >0){win[i,j] <- outcome[i,j]}
  }
}

# Display overall wins and loses 
print(sum(win)) # Total number of winning spins.
print(sum(lose)) # Total number of losing spins.

# Compute win probability
winprob <- sum(win)/(sum(win)-sum(lose))
print(winprob)
                                                                                                                                                                                                                                 
#####

# Code for Example 7.2

rm(list = ls())    # Clears all variables from the workspace

b.odds <- 2.93 # Odds offered by bookmaker
t.odds <- 2.50 # Assumed 'true' odds

# Compute probabilities
# Bookmaker's estimate
b.prob <- 1/b.odds
print(b.prob)

# Assumed true probability
t.prob <- 1/t.odds
print(t.prob)

# Create a virtual roulette wheel to simulate the match outcomes
n <- round(t.prob*1000) # This is the total number of segments required on the virtual wheel.
sims <- 10 # Number of simulations (i.e. spins of the wheel).
res <- matrix(0,sims,1)
vir_wheel <- c(rep(1,n),rep(-1,(1000-n))) # Virtual roulette wheel
bet <- 10 # Amount wagered (i.e. £10)

set.seed(234)
for (i in 1:sims){
  res[i] <- sample(vir_wheel,1)
}

# Display random match outcomes
print(res)

# Compute the potential winnings
winnings <- matrix(0,sims,1)
for (i in 1:sims){
  if(res[i] <0){winnings[i] <- -1*bet}
  if(res[i] >0){winnings[i] <- bet*(b.odds-1)}
}

# Display winnings
print(winnings)

# Compute expected profit over ten matches
prof <- sum(winnings)
print(prof)

# Compute expected winnings per match
exp.winnings <- mean(winnings)
print(exp.winnings)

#####

# Code for Example 7.3

rm(list = ls())    # Clears all variables from the workspace

# Load data 
mydata <- head(read.csv('https://www.football-data.co.uk/mmz4281/1819/E0.csv'),380)

# For ease of use we will select only the variables in which we are interested.
dat <- mydata[,c("Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","B365H","B365D","B365A",
                 "BWH","BWD","BWA","IWH","IWD","IWA","PSH","PSD","PSA","WHH","WHD",
                 "WHA","VCH","VCD","VCA")] 

# Inspect data
names(dat)

# Group odds
hwin <- dat[,c("B365H","BWH","IWH","PSH","WHH","VCH")]
draw <- dat[,c("B365D","BWD","IWD","PSD","WHD","VCD")]
awin <- dat[,c("B365A","BWA","IWA","PSA","WHA","VCA")]

# Create value vectors
val <- matrix(0,nrow(dat),1) # Value indicator
odds <- matrix(0,nrow(dat),1) # Bet odds
OC <- matrix(-1,nrow(dat),1) # Outcome indicator
W <- matrix(0,nrow(dat),1) # Wins
L <- matrix(0,nrow(dat),1) # Losses

# Select the appropriate data set (i.e. hwin, draw, or awin)
data <- hwin   # Select when evaluating home win bets. 
#data <- draw # Select when evaluating draw bets. 
#data <- awin  # Select when evaluating away win bets. 

# Inspect data
head(data, 20)

# Identify matches with potential value
for(i in 1:nrow(dat)){
  odds[i] <- max(data[i,])
  if(data$PSH[i] < odds[i]){val[i] <- 1} # Select for home win.
  #if(data$PSD[i] < odds[i]){val[i] <- 1} # Select for draw.
  #if(data$PSA[i] < odds[i]){val[i] <- 1} # Select for away win.
}

# Specify wager value on each bet.
stake <- 10 # £10 wagered on the bet

# Compute winnings and losses 
for(i in 1:nrow(dat)){
  if(dat$FTR[i] == "H" & val[i] >0){OC[i] <- 1} # Select for home win.
  #if(dat$FTR[i] == "D" & val[i] >0){OC[i] <- 1} # Select for draw.
  #if(dat$FTR[i] == "A" & val[i] >0){OC[i] <- 1} # Select for away win.
  if(OC[i] >0){W[i] <- stake*(odds[i]-1)}
  else if(val[i] >0 & OC[i] <0){L[i] <- stake}
}

results <- cbind(dat[,c(1:6)],data,val,odds,OC,W,L)
head(results,20)

# Compute how much won and lost and total profit
sum(W)
sum(L)

Profit <- sum(W)-sum(L)
print(Profit)

#####

# Code for Example 7.4

# Create empty vectors to store results
hmax <- matrix(0,nrow(dat),2) # Best home win odds offered
dmax <- matrix(0,nrow(dat),2) # Best draw odds offered
amax <- matrix(0,nrow(dat),2) # Best away win odds offered
prob <- matrix(0,nrow(dat),1) # Sum of implied probabilities
arb <- matrix(0,nrow(dat),1) # Arbitrage opportunity indicator

# Identify best odds offered on each match
for(i in 1:nrow(dat)){
  hmax[i,1] <- round(1/(max(hwin[i,])),3) # Identifies best odds and computes the probability
  hmax[i,2] <- which.max(hwin[i,]) # Identifies location of best odds in ‘hwin’ data frame
  dmax[i,1] <- round(1/(max(draw[i,])),3)
  dmax[i,2] <- which.max(draw[i,])
  amax[i,1] <- round(1/(max(awin[i,])),3)
  amax[i,2] <- which.max(awin[i,])
  prob[i] <- hmax[i,1]+dmax[i,1]+amax[i,1]
  if(prob[i] <1){arb[i] <- 1}
}

# Compile arbitrage opportunity data frame
arb.temp <- cbind.data.frame(hmax,dmax,amax,prob,arb)
colnames(arb.temp) <- c("Hprob","Hbm","Dprob","Dbm","Aprob","Abm","Prob","Arb")
arb.ops <- cbind.data.frame(dat[,c(1:3,6)],arb.temp)
head(arb.ops,20)  # NB. ‘Hbm’ is the bookmaker offering the best home win odds.

# Compute odds from probabilities
arb.bets <- arb.ops
arb.bets[,5] <- round(1/arb.ops[,5],3)
arb.bets[,7] <- round(1/arb.ops[,7],3)
arb.bets[,9] <- round(1/arb.ops[,9],3)
colnames(arb.bets) <- c("Date","HomeTeam","AwayTeam","FTR",
                        "Hodds","Hbm","Dodds","Dbm","Aodds","Abm","Prob","Arb")
head(arb.bets,20) 

# Specify nominal wager on each bet
nom.wager <- 1000 # i.e. £1000

# Compute stakes to be placed on each bet
Hstake <- matrix(0,nrow(dat),1)
Dstake <- matrix(0,nrow(dat),1)
Astake <- matrix(0,nrow(dat),1)
wager <- matrix(0,nrow(dat),1)

for(i in 1:nrow(dat)){
  Hstake[i] <- round((arb.ops$Hprob[i]*nom.wager),2)
  Dstake[i] <- round((arb.ops$Dprob[i]*nom.wager),2)
  Astake[i] <- round((arb.ops$Aprob[i]*nom.wager),2)
  wager[i] <- round((Hstake[i]+Dstake[i]+Astake[i]),2)
}

# Compile data frame of monies wagered.
money.bet <- cbind.data.frame(Hstake,Dstake,Astake,wager)

# Display ‘money.bet' data frame
head(money.bet, 20)

# Compute profits from arbitrage bets
profit <-  matrix(0,nrow(dat),1)

for(i in 1:nrow(dat)){
  if(arb.bets$Arb[i] >0){
    if(arb.bets$FTR[i] == "H"){profit[i] <- (Hstake[i]*(arb.bets$Hodds[i]-1))-Dstake[i]-Astake[i]}
    if(arb.bets$FTR[i] == "D"){profit[i] <- (Dstake[i]*(arb.bets$Dodds[i]-1))-Hstake[i]-Astake[i]}
    if(arb.bets$FTR[i] == "A"){profit[i] <- (Astake[i]*(arb.bets$Aodds[i]-1))-Hstake[i]-Dstake[i]}
  }
}

# Display results
profit <- round(profit,2)
arb.res <- cbind(arb.bets[,c(1:10,12)],money.bet,profit)
head(arb.res,20)

# Compute number of arbitrage bets made and profit for the season
nbets <-sum(arb.res$Arb) # This is number of arbitrage bets made.
print(nbets)

tot.profit <- sum(profit) # This is the profit.
print(tot.profit)

# Average yield
approx.yield <- round((100*tot.profit/(nom.wager*nbets)),2) # This is the approximate yield per bet (%).
print(approx.yield) # Expressed as a percentage.

#####


