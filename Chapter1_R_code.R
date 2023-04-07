
# Chapter 1: R code
# Copyright: Clive Beggs 6th March 2023

# Code snippet 1

Clubs <- c("Arsenal","Blackburn Rovers","Chelsea","Leicester City","Liverpool","Man City","Man United")
Titles <- c(3,1,5,1,1,6,13)
epl.dat <- cbind.data.frame(Clubs,Titles)
print(epl.dat)

# Bar plot
barplot(epl.dat$Titles, names.arg=epl.dat$Clubs, ylim=c(0,14))
title("Bar chart of EPL champions 1992-2021")

# Pie chart
colours = gray.colors(length(epl.dat$Clubs))
pie(Titles, labels = Clubs , col=colours,main="Pie Chart of EPL champtions 1992-2021")

#####

# Code snippet 2

Manager <- c("Ferguson","Moyes","van Gaal","Mourinho","Solskjaer","Rangnick")
Wins <- c(895,27,54,84,91,11)
Draws <- c(338,9,25,32,37,10)
Losses <- c(267,15,24,28,40,8)

mu.record <- cbind.data.frame(Manager,Wins,Draws,Losses)
print(mu.record)

man1 <- "Ferguson"
man2 <- "Rangnick"

Manager1 <- mu.record[mu.record$Manager == man1,]   
Manager2 <- mu.record[mu.record$Manager == man2,]   

# Create a table called ContTab
temp = as.matrix(rbind(Manager1[,c(2:4)], Manager2[,c(2:4)]))
ContTab <- as.table(temp)
dimnames(ContTab) = list(Manager = c("Manager 1", "Manager 2"),
                         Outcome = c("Wins","Draws", "Loses"))
print(ContTab)

chsqRes = chisq.test(ContTab)  # This displays the results summary 
print(chsqRes)

#####

# Code snippet 3

Points <- c(61, 55, 41, 39, 67, 44, 59, 28, 59, 66, 69, 86, 74, 45, 23, 43, 62, 26, 65, 45)
Shots <- c(455,518,476,383,553,346,395,440,524,472,600,590,517,387,319,417,442,336,462,462)

dat2020 <- cbind.data.frame(Points, Shots)
head(dat2020)  # NB. The ‘head’ function displays the first 6 rows of the data frame.

# Build OLS regression models for season 2020-21
# Using Shots
mod2020 <- lm(Points ~ Shots, data = data.frame(dat2020))
summary(mod2020)

# Scatter plot with bets-fit lines
plot(dat2020$Shots, dat2020$Points, pch=20, col="black", xlim=c(0,800), 
     ylim=c(0,100), 	ylab="Points", xlab="Shots")
abline(lm(dat2020$Points ~ dat2020$Shots), lty=1)

# Chelsea (who attempted 585 shots and achieved 74 points in season 2021-22)
ChelPts.2021 <- predict(mod2020, list(Shots=585))
print(ChelPts.2021)

# Manchester City (who attempted 704 shots and achieved 93 points in season 2021-22)
MCPts.2021 <- predict(mod2020, list(Shots=704))
print(MCPts.2021)

# Norwich City (who attempted 374 shots and achieved 22 points in season 2021-22)
NCPts.2021 <- predict(mod2020, list(Shots=374))
print(NCPts.2021)
                      
#####

# Code snippet 4

# William Hill match odds
wh_hwodds <- 2.15 # Odds offered by bookmaker for a home win
wh_dodds <- 3.30 # Odds offered by bookmaker for a draw
wh_awodds <- 3.50 # Odds offered by bookmaker for a away win

# Pinnacle match odds
p_hwodds <- 2.13 # Odds offered by bookmaker for a home win
p_dodds <- 3.61 # Odds offered by bookmaker for a draw
p_awodds <- 3.64 # Odds offered by bookmaker for a away win

# Compile the data frame
WH_odds <- c(wh_hwodds,wh_dodds,wh_awodds) # William Hill
Pin_odds <- c(p_hwodds,p_dodds,p_awodds) # Pinnacle
bet.dat <- cbind.data.frame(WH_odds,Pin_odds)

# Compute implied probabilities
bet.dat$WH_prob <- round(1/bet.dat$WH_odds,3)
bet.dat$Pin_prob <- round(1/bet.dat$Pin_odds,3)
rownames(bet.dat) <- c("Home win","Draw","Away win")
print(bet.dat)

# William Hill’s over-round
WH_or <- sum(bet.dat$WH_prob)-1
print(WH_or)

# Pinnacle’s over-round
Pin_or <- sum(bet.dat$Pin_prob)-1
print(Pin_or)

# Bet profits
wager <- 10 # £10 wager with WH on Tottenham to win
profit <- wager * (wh_hwodds-1)
print(profit)

#####

# Code snippet 5

teams <- c(1:16)
print(teams)

# Create matrix to store results
cup.draw <- as.data.frame(matrix(0,8,2))
colnames(cup.draw) <- c("HomeTeam", "AwayTeam")

# Randomly sample eight home teams
set.seed(123) # This makes the draw results repeatable
samp.HT <- sample(teams, size=8, replace=FALSE)
samp.AT <- sample(teams[-samp.HT])

cup.draw$HomeTeam <- samp.HT
cup.draw$AwayTeam <- samp.AT
cup.draw

#####


