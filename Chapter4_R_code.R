
# Chapter 4: R code
# Copyright: Clive Beggs 7th March 2023

# Code for Example 4.1

rm(list = ls())    # This clears all variables from the workspace

# Load data (NB. Here we select just the first 16 variables).
EPL2020_data <- head(read.csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv'),380)[,1:16]

# Inspect data
names(EPL2020_data)

# Create a working data frame
dat <- EPL2020_data[,c("Date","HomeTeam","AwayTeam","FTHG","FTAG",
                       "FTR","HS","AS","HST","AST")]   # This creates a working data frame
head(dat) # Displays first six rows of dat

# Create new variables populated with NAs and zeros, in which to store the derived metrics.
dat["GD"] <- NA # Creates a new column for goal difference populated with NAs
dat["TG"] <- NA # Creates a new column for total goals scored populated with NAs
dat["HTSR"] <- NA # Creates a home team shots ratio variable populated with NAs
dat["ATSR"] <- NA # Creates a away team shots ratio variable populated with NAs 
dat["HPts"] <- 0 # Creates a home team points variable populated with zeros
dat["APts"] <- 0 # Creates an away team points variable populated with zeros

# Populate the variables
dat$GD <- dat$FTHG - dat$FTAG
dat$TG <- dat$FTHG + dat$FTAG
dat$HTSR <- round(dat$HS/(dat$HS+dat$AS),3)  # Here we round to 3 decimal places 
dat$ATSR <- round(dat$AS/(dat$AS+dat$HS),3)  # Here we round to 3 decimal places

# Compute home and away points awarded per match
for(i in 1:nrow(dat)){
  if(dat$FTR[i] == "H"){dat$HPts[i] <- 3}
  if(dat$FTR[i] == "A"){dat$APts[i] <- 3}
  if(dat$FTR[i] == "D") {dat$HPts[i] <- 1}
  if(dat$FTR[i] == "D") {dat$APts[i] <- 1}
}

# Rename variables
colnames(dat)[colnames(dat) == 'FTHG'] <- 'HG'
colnames(dat)[colnames(dat) == 'FTAG'] <- 'AG'
colnames(dat)[colnames(dat) == 'FTR'] <- 'Result'

# Display augmented data frame
head(dat)  # This displays only the first six rows of the data frame. 

#####

# Code for Example 4.2

# Specify target team
study.team <- "Liverpool" # NB. The team name should be in quotes.

# Extract data for the target team
home.matches <- dat[dat$HomeTeam == study.team,] # Selects target team's home matches.
away.matches <- dat[dat$AwayTeam == study.team,] # Selects target team's away matches.

# Add a 'status' variable to denote whether a match is home or away.
home.matches["Status"] <- "Home" # Creates the new column to denote home matches.
away.matches["Status"] <- "Away" # Creates the new column to denote away matches.

# Change the variable names to be club specific (i.e. for and against rather than home and away)
home <- home.matches # Make duplicate copy of the data frame on which to make changes
away <- away.matches # Make duplicate copy of the data frame on which to make changes

# Inspect data
head(home)
head(away)

# Change multiple variable names in the 'home' data frame using the 'dplyr' package.

#install.packages("dplyr")  # This installs the ‘dplyr’ package. 
# NB. This command only needs to be executed once to install the package. 
# Thereafter, the ‘dplyr’ library can be called using the ‘library’ command:

# First, we change the variable names in the 'home' data frame.
library(dplyr) # Load library package 'dplyr'
home <- rename(home, c("GF"="HG","GA"="AG","SF"="HS","SA"="AS",
                       "STF"="HST","STA"="AST","TSRF"="HTSR","TSRA"="ATSR",
                       "PF"="HPts","PA"="APts"))

# Now, we replace the ‘H’, ‘A’ and ‘D’ elements in Results vector with 'W', 'L' and 'D'.
home$Result <- recode(home$Result, "H"="W", "A"="L", "D"="D")
head(home)

# Now we repeat the process for the 'away' data frame.
away <- rename(away, c("GA"="HG","GF"="AG","SA"="HS","SF"="AS",
                       "STA"="HST","STF"="AST","TSRA"="HTSR","TSRF"="ATSR",
                       "PA"="HPts","PF"="APts"))

away$GD <- -1*away$GD # Change sign on goal difference to reflect use of 'for' and 'against'.

# Replace elements in Results vector with 'W', 'L', 'D'.
away$Result <- recode(away$Result, "H"="L", "A"="W", "D"="D")
head(away)

#####

# Code for Example 4.3

# Inspect data
names(home)

# Use 'psych' package to produce the descriptive statistics for home and away matches.
library(psych)

# Home matches descriptive statistics
home.temp <- home[,c(4,5, 7:16)] # Variables selected for statistical analysis
H.stats <- describeBy(home.temp) # These are the descriptive statistics
H.sums <- colSums(home.temp) # Column sums
home.des <- cbind.data.frame(H.stats$n, H.stats$mean, H.stats$median, H.stats$sd, H.sums) 
colnames(home.des) <- c("Pld", "Mean", "Median", "SD", "Total") # Rename variables
print(round(home.des,3)) # Display the home match descriptive statistics

# Away matches descriptive statistics
away.temp <- away[,c(4,5, 7:16)] # Variables selected for statistical analysis
A.stats <- describeBy(away.temp) # These are the descriptive statistics
A.sums <- colSums(away.temp) # Column sums
away.des <- cbind.data.frame(A.stats$n, A.stats$mean, A.stats$median, A.stats$sd, A.sums) 
colnames(away.des) <- c("Pld", "Mean", "Median", "SD", "Total") # Rename variables
print(round(away.des,3)) # Display the home match descriptive statistics

# Produce descriptive statistics for all the matches in the season.
all <- rbind(home, away)

# All matches descriptive statistics
all.temp <- all[,c(4,5, 7:16)] # Variables selected for statistical analysis
all.stats <- describeBy(all.temp) # These are the descriptive statistics
all.sums <- colSums(all.temp) # Column sums
all.des <- cbind.data.frame(all.stats$n, all.stats$mean, all.stats$median, all.stats$sd, all.sums) 
colnames(all.des) <- c("Pld", "Mean", "Median", "SD", "Total") # Rename variables
print(round(all.des,3)) # Display the home match descriptive statistics


# Produce box plot of home and away shots on target.
HSTF <- home$STF
HSTA <- home$STA
ASTF <- away$STF
ASTA <- away$STA
SoT <- cbind.data.frame(HSTF,HSTA,ASTF,ASTA)
boxplot(SoT, ylab="Shots on target")

#####

# Code for Example 4.4

rm(list = ls())    # This clears all variables from workspace

# Download the historical match data for the 5 seasons 2016-17 to 2020-21.
seasons = c(rep("1617",1), rep("1718",1), rep("1819",1), rep("1920",1), rep("2021",1))
division = c(rep(c("E0"),5)) # NB. "E0" is the EPL and 5 is the number of seasons.

urls = paste(seasons, division, sep="/")
urls = paste("https://www.football-data.co.uk/mmz4281", urls, sep="/")

# Download data for each season
download_data = NULL
for(i in 1:length(urls)){
  temp = read.csv(urls[i])
  temp = temp[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG",
                 "FTR","HS","AS","HST","AST")]  
  download_data = rbind(download_data, temp)
}

# Inspect downloaded data
head(download_data) # This displays the first six rows.
tail(download_data) # This displays the last six rows.

# Specify the home and away teams involved in the fixture.
teamH <- "Arsenal"  # This is the home team.
teamA <- "Chelsea"  # This is the away team.

# Identify H2H matches in previous seasons
n <- nrow(download_data)  # This determines the length of the indicator vector.
ind <- matrix(0,n,1)  # This created an [n,1] matrix (vector) populated with zeros.
ndat <- cbind.data.frame(download_data, ind)

for (i in  1:n){
  if(ndat$HomeTeam[i] == teamH & ndat$AwayTeam[i] == teamA){ndat$ind[i] <- 1}
}

# Select only those matches for which the indicator variable is 1.
H2H <- ndat[ndat$ind == 1,]

# For completeness we will also add the shots ratios for the respective matches.
H2H["HTSR"] <- NA # This creates the new column to store home team shots ratio.
H2H["ATSR"] <- NA # This creates the new column to store away team shots ratio.
H2H$HTSR <- round(H2H$HS/(H2H$HS+H2H$AS),3)  # Here we round to 3 decimal places.
H2H$ATSR <- round(H2H$AS/(H2H$AS+H2H$HS),3)  # Here we round to 3 decimal places.

# Display the H2H match results.
print(H2H)

#####

# Code for Example 4.5

rm(list = ls())    # Clears all variables from the workspace

# Load data (NB. 98 matches includes the matches on 30/11/2020 and equates to
# approx. 10 rounds of competition)
PiT_dat <- head(read.csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv'),98)[,1:7] 

# Inspect data
names(PiT_dat)

# Define the four variables that are to be loaded into the user-defined function. 
HomeTeam <- PiT_dat$HomeTeam
AwayTeam <- PiT_dat$AwayTeam
HomeGoals <- PiT_dat$FTHG
AwayGoals <- PiT_dat$FTAG

# Create user-defined functions 
# Code adapted from http://opisthokonta.net/?p=18

# Function 1. (This creates a vector of match outcomes.)
outcome <- function(hGoals, aGoals){
  nMatches <- length(hGoals)
  results <- matrix(NA,nMatches,1) # Creates an empty vector to store match outcome results.
  
  # This populates the results vector with match outcomes (i.e. H, A or D)
  for(i in 1:nMatches){
    if(hGoals[i] > aGoals[i]){results[i] <- "H"}
    if(hGoals[i] < aGoals[i]){results[i] <- "A"}
    if(hGoals[i] == aGoals[i]){results[i] <- "D"}
  }
  return(results)
}

# Function 2. (This creates a current league table from the match results data.)
create.table <- function(hTeam, aTeam, hGoals, aGoals){
  
  # Harvest team names and collate in to a vector
  teams.temp <- unique(hTeam)
  (teams <- sort(teams.temp)) # Arrange in alphabetical order.
  nTeams = length(teams) # This identifies the number of teams in the league.
  
  # Create a vector containing the match outcomes (i.e. H, A or D)
  results <- outcome(hGoals, aGoals)
  
  # Create empty vectors to store results.
  x <- numeric(nTeams)
  hWins <- x; hLoss <- x; hDraws <- x;
  aWins <- x; aLoss <- x; aDraws <- x;
  goals.for <- x; goals.against <- x; goal.diff <- x;
  matches.played <- x; pts <- x;
  
  # Populate vectors
  for (i in 1:nTeams) {
    hResults <- results[hTeam == teams[i]]
    aResults <- results[aTeam == teams[i]]
    matches.played[i] <- length(hResults) + length(aResults)
    goals.H <- sum(hGoals[hTeam == teams[i]])
    goals.A <- sum(aGoals[aTeam == teams[i]])
    goals.for[i] <- goals.H + goals.A
    conceded.H <- sum(aGoals[hTeam == teams[i]])
    conceded.A <- sum(hGoals[aTeam == teams[i]])
    goals.against[i] <- conceded.H + conceded.A
    goal.diff[i] <- goals.for[i] - goals.against[i]
    hWins[i] <- sum(hResults == "H")
    hDraws[i] <- sum(hResults == "D")
    hLoss[i] <- sum(hResults == "A")
    aWins[i] <- sum(aResults == "A")
    aDraws[i] <- sum(aResults == "D")
    aLoss[i] <- sum(aResults == "H")
    
    # Compute total points from the number of wins and draws for the respective teams.
    # Points awarded for the match outcomes
    win.pts <- 3
    draw.pts <- 1
    pts[i] <- (win.pts*(hWins[i] + aWins[i])) + (draw.pts * (hDraws[i] + aDraws[i]))
  }
  
  table <- data.frame(cbind(matches.played, hWins, hDraws,hLoss, aWins, aDraws, aLoss,
                            goals.for, goals.against, goal.diff, pts), row.names=teams)
  
  names(table) <- c("PLD", "HW", "HD", "HL", "AW", "AD", "AL", "GF", "GA", "GD", "PTS")
  ord <- order(-table$PTS, -table$GD, -table$GF)
  table <- table[ord, ]
  return(table)
}

# Execute user-defined functions to produce PiT league table
League.table <- create.table(HomeTeam, AwayTeam, HomeGoals, AwayGoals)
print(League.table)

#####

# Code for Example 4.6

rm(list = ls())    # Clears all variables from the workspace

# Load data
fb_data <- head(read.csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv'),98)[,1:20]

# Select variables for inclusion in the analysis.
soc_dat <- fb_data[,c("Date","HomeTeam","AwayTeam","FTHG","FTAG",
                      "FTR","HS","AS","HST","AST","HC","AC")]

# Add some new derived variables
soc_dat["HPts"] <- 0 # This creates the new column to store home team points per match 
soc_dat["APts"] <- 0 # This creates the new column to store away team points per match 

for(i in 1:nrow(soc_dat)){
  if(soc_dat$FTR[i] == "H"){soc_dat$HPts[i] <- 3}
  if(soc_dat$FTR[i] == "A"){soc_dat$APts[i] <- 3}
  if(soc_dat$FTR[i] == "D") {soc_dat$HPts[i] <- 1}
  if(soc_dat$FTR[i] == "D") {soc_dat$APts[i] <- 1}
}

# Rename variables
colnames(soc_dat)[colnames(soc_dat) == 'FTHG'] <- 'HG'
colnames(soc_dat)[colnames(soc_dat) == 'FTAG'] <- 'AG'
colnames(soc_dat)[colnames(soc_dat) == 'FTR'] <- 'Result'

# Inspect data
head(soc_dat)

# Create user-defined function ‘feature.Calc’
feature.Calc <- function(df, team){
  Hmatches <- df[df$HomeTeam == team,] # This selects the target team's home matches.
  Amatches <- df[df$AwayTeam == team,] # This selects the target team's away matches.
  all <- rbind.data.frame(Hmatches,Amatches)
  n <- nrow(all) # Number of matches
  
  # Create empty vectors to store results.
  x <- numeric(n)
  GF <- x; GA <- x; SF <- x; SA <- x; STF <- x; STA <- x;
  CF <- x; CA <- x; Pts <- x;
  
  # Goals for
  for(i in 1:n){
    if(all$HomeTeam[i] == team){GF[i] <- all$HG[i]}
    else {GF[i] <- all$AG[i]}
  }
  
  # Goals against
  for(i in 1:n){
    if(all$HomeTeam[i] == team){GA[i] <- all$AG[i]}
    else {GA[i] <- all$HG[i]}
  }
  
  # Shots for
  for(i in 1:n){
    if(all$HomeTeam[i] == team){SF[i] <- all$HS[i]}
    else {SF[i] <- all$AS[i]}
  }
  
  # Shots against
  for(i in 1:n){
    if(all$HomeTeam[i] == team){SA[i] <- all$AS[i]}
    else {SA[i] <- all$HS[i]}
  }
  
  # Shots on target for
  for(i in 1:n){
    if(all$HomeTeam[i] == team){STF[i] <- all$HST[i]}
    else {STF[i] <- all$AST[i]}
  }
  
  # Shots on target against
  for(i in 1:n){
    if(all$HomeTeam[i] == team){STA[i] <- all$AST[i]}
    else {STA[i] <- all$HST[i]}
  }
  
  # Corners for
  for(i in 1:n){
    if(all$HomeTeam[i] == team){CF[i] <- all$HC[i]}
    else {CF[i] <- all$AC[i]}
  }
  
  # Corners against
  for(i in 1:n){
    if(all$HomeTeam[i] == team){CA[i] <- all$AC[i]}
    else {CA[i] <- all$HC[i]}
  }
  
  # Points awarded
  for(i in 1:n){
    if(all$HomeTeam[i] == team){Pts[i] <- all$HPts[i]}
    else {Pts[i] <- all$APts[i]}
  }
  
  Pld <- matrix(1,n,1) # Vector containing matches played
  GD <- GF-GA
  TG <- GF+GA
  TSRF <- SF/(SF+SA)
  TSRA <- SA/(SF+SA)
  
  feats <- cbind.data.frame(Pld,GF,GA,GD,TG,SF,SA,STF,STA,TSRF,TSRA,CF,CA,Pts)
  featsSums <- colSums(feats)
  featsRes <- featsSums
  nOb <- nrow(feats)
  featsRes[10] <- featsSums[10]/nOb # This compute the average TSRF.
  featsRes[11] <- featsSums[11]/nOb # This computes the average TSRA.
  return(round(featsRes,2))
}

# Apply the ‘feature.Calc’ function to Tottenham and Manchester City.

Tot.features <- feature.Calc(soc_dat, "Tottenham")
print(Tot.features)

MC.features <- feature.Calc(soc_dat, "Man City")
print(MC.features)

# Now we apply the ‘feature.Calc’ function to all the teams in the EPL.
Teams <- unique(soc_dat$HomeTeam)
Teams <- sort(Teams) # Put them in alphabetical order
nTeams <- length(Teams) # Number of teams
print(Teams)

featureRes <- matrix(NA,nTeams,14)
for(i in 1:nTeams){
  featureRes[i,] <- feature.Calc(soc_dat, Teams[i])
}

# Compile feature table results
featureTab <- cbind.data.frame(Teams, featureRes)
colnames(featureTab) <- c("Team","Pld","GF","GA","GD","TG","SF","SA","STF",
                          "STA","TSRF","TSRA","CF","CA","Pts")
print(featureTab)

#####

# Code for Example 4.7

# Scatter plot of TSRF and points earned
x <- featureTab$TSRF
y <- featureTab$Pts

# Correlation between TSRF and points earned
cor.test(x,y)

# Scatter plot
plot(x,y, pch=20, xlim=c(0.2,0.8), ylim=c(0,25), xlab="Average TSRF score", ylab="Points earned")
text(y~x, labels=Teams, cex=0.8, font=1, pos=4)  # This puts team names on the data points. 
abline(lm(y~x), lty=2) # This draws a least squares best fit line through the data points.

#####

# Code for key concept box 4.1

rm(list = ls())    # This clears all variables from the workspace

# Create function called ‘calc_rectangle_area'
calc_rectangle_area <- function(length, width) {
  area <- length * width
  return(area)
}

# Specify input values
length <- 5
width <- 3

# Apply function
calc_rectangle_area(length,width)

#####







