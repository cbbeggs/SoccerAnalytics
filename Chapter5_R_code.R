
# Chapter 5: R code
# Copyright: Clive Beggs 13th March 2023

# Code for Example 5.1

rm(list = ls())    # This clears all variables from the workspace

# Load data - assumed that data file is stored in a directory called ‘Datasets’.
tabdat <- read.csv("C:/Datasets/EPL_final_table_2021.csv") 

# Compute average points per match for each team.
tabdat["avPPM"] <- NA # This creates the new column to store new data
tabdat$avPPM <- round(tabdat$PTS/tabdat$PLD, 4) # NB. Rounded to 3 dp

# Display data
head(tabdat) # Display the first six rows


# Compute the number of matches and teams in the league. 
m <- mean(tabdat$PLD) # Number of matches played per team
n <- nrow(tabdat) # Number of teams in league

# Compute the observed points average and variance
m <- mean(tabdat$PLD) # Number of matches played per team
n <- nrow(tabdat) # Number of teams in league

# Compute the observed points average and variance.
LPPM.av <- mean(tabdat$avPPM)  # Average number of points per match for the league
LPTS.av <- mean(tabdat$PTS) # Average team points total over entire season

LPTS.error <- c()  # Creates an empty vector to store results

for(i in 1:n){
  LPTS.error[i] <- tabdat$PTS[i] - LPTS.av
}

LPTS.var <- (1/n)*sum(LPTS.error^2) # Variance in team points 

# Specify the probabilities of a home win, and away win and a draw.
pH <- 0.46 # Probability of a home win
pA <- 0.29 # Probability of an away win
pD <- 0.25 # Probability of a draws

# Compile league statistics
league.stats <- round(c(m,pH,pA,pD,sum(tabdat$PTS),LPTS.av,LPTS.var,LPPM.av),3)
names(league.stats) <- c("Pld","HWprob","AWprob","Dprob","Pts.tot","Pts.av",
                         "Pts.var","Ptspm.av")

print(league.stats)


# Expected average points total per team
exP <- (m/2)*(3-pD) # Expected average points total if league completely random

# Expected points variance for all the teams in the league
exVar <- (m/2)*(9-(7*pD)-(((3*pH)+pD)^2)-(((3*pA)+pD)^2)) # Expected variance in points

# But in reality the league points variance is:
Lvar <- LPTS.var

# Therefore, the proportion of the league points variance that is due to chance is:
fchance <- exVar/Lvar  # NB. This is expressed as a fraction.

# Display results
print(exP)
print(exVar)
print(Lvar)
print(fchance)

#####

# Code for Example 5.2

rm(list = ls())    # Clears all variables from the workspace

# Load data
download_dat <- read.csv("C:/Datasets/EPL_standings_2013_14.csv")

standings <- download_dat[,2:39]
row.names(standings) <- download_dat[,1]  # This names the rows of the data frame.
head(standings)

# Perform Spearman correlation analysis using 'Hmisc' package
#install.packages("Hmisc")  # This installs the ‘Hmisc’ package. 
# NB. This command only needs to be executed once to install the package. 
# Thereafter, the ‘Hmisc’ library can be called using the ‘library’ command:

library(Hmisc)
cor <- rcorr(as.matrix(standings, type="spearman")) # type can be pearson or spearman
cor_r <- as.data.frame(cor[1]) # Convert to a data frame
cor_p <- as.data.frame(cor[3]) # Convert to a data frame

# Select the final column for round 38 of competition and round to 3 decimal places.
round38_r <- round(as.data.frame(cor_r[,38]),3)
round38_p <- round(as.data.frame(cor_p[,38]),3)

# Display results
print(t(round38_r)) # Transposed for ease of display.
print(t(round38_p)) # Transposed for ease of display.

# Plot results
plot(cor_r[,38],type = "l",col = "black", ylim=c(0,1), xlab="Round of competition", 
       ylab = "Spearman correlation (r value)")

#####

# Code for Example 5.3

rm(list = ls())    # Clears all variables from the workspace

# Load data
EoStab <- read.csv("C:/Datasets/EPL_final_table_2021.csv")
names(EoStab)

# Specify value of coefficients
a = 2.78
b = 1.24
c = 1.24
d = 1.25

# Compute key metrics.
pygFrac <- ((EoStab$GF^b)/((EoStab$GF^c)+(EoStab$GA^d)))
pygPts <- a * pygFrac * EoStab$PLD
pygDiff <- EoStab$PTS - pygPts

# Round to specified decimal places
pygFrac <- round(pygFrac, 3)
pygPts <- round(pygPts, 2)
pygDiff <- round(pygDiff, 2)

# Compile table
pygtab <- cbind.data.frame(EoStab, pygFrac, pygPts, pygDiff)
print(pygtab)

# Evaluate correlation between the Pythagorean expected points and actual points achieved.
cor.test(pygtab$PTS, pygtab$pygPts) # This give the correlation r value.

# Produce scatter plot
plot(pygtab$PTS, pygtab$pygPts, col = "black", pch=21, xlab="EPL points", 
     ylab = "Pythagorean points")
abline(lm(pygtab$pygPts ~ pygtab$PTS), lty=1)

#####

# Code for Example 5.4

# Pythagorean expected points function. 
pythag_pred <- function(PLD,GF,GA,PTS,nGames){
  
  # Coefficients (NB. These can be fine tuned to suit the particular league.)
  a = 2.78
  b = 1.24
  c = 1.24
  d = 1.25
  
  # Compute key metrics
  pythag_frac <- (GF^b)/((GF^c)+(GA^d))
  pythag_pts <- a * pythag_frac * PLD
  pythag_diff <- PTS - pythag_pts
  points_avail <- (nGames - PLD)*3
  pred_pts <- pythag_frac * a * (nGames - PLD)
  pred_total <- PTS + pred_pts
  pythag_total <- pythag_pts + pred_pts
  results <- round(c(PLD,GF,GA,PTS,pythag_frac,pythag_pts,pythag_diff,points_avail,
                     pred_pts,pred_total,pythag_total),1) # Creates results vector
  return(results)
}

# Specify input parameters

# Compute the total number of matches played.
nTeams <- 20 # Number of teams in EPL
nGames <- (nTeams-1)*2 # Computes the total number of games each team plays in a season.

# Specify the teams performance so far.
PLD <- 10 # Number of games played
GF <- 22 # Number of goals scored
GA <- 17 # Number of goals conceded
PTS <- 21 # Number of points achieved

# Apply the ‘pythag_pred’ function 
pred_res <- pythag_pred(PLD,GF,GA,PTS,nGames)
var_names <- c("PLD","GF","GA","PTS","PythagFrac","PythagPTS",
               "PythagDiff","AvailPTS","PredPTS","PredTot","PythagTot")
Liv_pred10 <-cbind.data.frame(var_names, pred_res)
print(Liv_pred10)

#####

# Code for Example 5.5

# Load data (assume file is stored in a directory called ‘Datasets’).
# Select data file for either 98, 185 or 290 matches.
part_tab <- read.csv("C:/Datasets/EPL_after_98_matches_2021.csv") # Approx. 10 rounds
#part_tab <- read.csv("C:/Datasets/EPL_after_185_matches_2021.csv") # Approx. 19 rounds
#part_tab <- read.csv("C:/Datasets/EPL_after_290_matches_2021.csv") # Approx. 29 rounds

# Create empty matrix to store the results
nTeams <- 20 # Number of teams in EPL
nGames <- (nTeams-1)*2 # Computes total number of games each team plays in a season.
predPTS <- matrix(NA, nTeams, 11) # Creates a [20 x 11] empty matrix populated with NAs.

# Populate the empty matrix by applying the ‘pythag_pred’ function using a ‘for loop’.
for(i in 1:nTeams){
  predPTS[i,] <- pythag_pred(part_tab$PLD[i], part_tab$GF[i],
                             part_tab$GA[i], part_tab$PTS[i], nGames)
}

# Compile results and display them.
Pred_EoSPTS <- cbind.data.frame(part_tab$Club, predPTS)
colnames(Pred_EoSPTS) <- c("Club", var_names)
head(Pred_EoSPTS) # This displays the top six rows.

# Download the EoS table for season 2020-21
EoS_tab <- read.csv("C:/Datasets/EPL_final_table_2021.csv")

# Sort team alphabetically
sortedEoS <- EoS_tab[order(EoS_tab$Club), ]
head(sortedEoS)

sortedPred <- Pred_EoSPTS[order(Pred_EoSPTS$Club), ]
head(sortedPred)

# Compile the EoS table with the Pythagorien predicted points added.
Pred <- sortedPred$PredTot
EoS_predtab <- cbind.data.frame(sortedEoS, Pred)
print(EoS_predtab)

# Evaluate accuracy of the predictions made
cor.test(sortedEoS$PTS,sortedPred$PredTot) # This computes the correlation r value.
mae <- mean(abs(sortedEoS$PTS - sortedPred$PredTot)) # Mean absolute error
print(mae)

# Scatter plot of EoS_PTS and PredTot
plot(sortedEoS$PTS, sortedPred$PredTot, col = "black", pch=21, xlab="Actual EoS points", 
     ylab = "Predicted EoS points")
abline(lm(sortedPred$PredTot ~ sortedEoS$PTS), lty=1)

#####

# Code for Example 5.6

rm(list = ls())    # Clears all variables from the workspace

# Here we assume that the file being loaded is stored in a directory called ‘Datasets’.
perf_dat <- read.csv("C:/Datasets/EPL_EoS_indicators_2021.csv")

# Legend
# "Team" - club
# "Rank" - EoS standing in the league
# "Pld" - number of matches played
# "Pts" - points earned
# "GF" - goals for
# "GA" - goals against
# "GD" - goal difference
# "ShF" - number of shots made
# "SoTF" - number of shots on target made
# "ShA" - number of shots conceded
# "SoTA" - number of shots on target conceded
# "Poss" - percentage possession
# "PassComp" - percentage of passes completed
# "xG" - expected goals

# Inspect data
names(perf_dat)

ind1 <- perf_dat[,c(1:4)]

# Now we add some new derived variables
ind1["GR"] <- NA # Create empty vector to store goal ratio results
ind1["TSR"] <- NA # Create empty vector to store total shots ratio results
ind1["TSoTR"] <- NA # Create empty vector to store total shots on target ratio results

# Now we populate these new variables and round to 3 decimal places.
ind1$GR <- round(perf_dat$GF/(perf_dat$GF+perf_dat$GA),3)
ind1$TSR <- round(perf_dat$SF/(perf_dat$SF+perf_dat$SA),3)
ind1$TSoTR <- round(perf_dat$SoTF/(perf_dat$SoTF+perf_dat$SoTA),3)

# Display results
print(ind1)

# Compute correlation r-values
ind1_cors <- round(cor(ind1[,c(4:7)]),3)
print(ind1_cors)

#####

# Code for Example 5.7
 
# Create second indicator data frame 
ind2 <- perf_dat[,c(1:5,12:14)]

# Add new derived variable ‘goal-to-shots ratio’
ind2["GSR"] <- NA # Create empty vector to store goal ratio results

# Populate new variables and round to 3 decimal places.
ind2$GSR <- round(perf_dat$GF/perf_dat$SF,3)

# Display results
print(ind2)

# Compute the the Pearson correlation r-values. 
cor.dat <- ind2[,c(4:9)]
ind2_cors <- round(cor(cor.dat),3)
print(ind2_cors)

# Alternative correlation method using the 'Hmisc' package 
library("Hmisc")
cor.res <- rcorr(as.matrix(cor.dat))
cor.res

#install.packages("ggcorrplot")  # This installs the ‘ggcorrplot’ package.
#install.packages("PerformanceAnalytics")  # This installs the ‘PerformanceAnalytics’ package.
# NB. These commands only needs to be executed once to install the package. 
# Thereafter, the libraries can be called using the ‘library’ command:

# Produce correlation plot using 'ggcorrplot' package
library(ggcorrplot)
ggcorrplot(as.matrix(ind2_cors), type = "lower", ggtheme = theme_bw, lab = TRUE) 

# Preduce advanced correlation plot using 'PerformanceAnalytics' package 
library("PerformanceAnalytics")
chart.Correlation(cor.dat, histogram=TRUE, pch=19)

#####






