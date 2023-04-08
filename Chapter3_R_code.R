
# Chapter 3: R code
# Copyright: Clive Beggs 7th March 2023

# Code for Example 3.1

rm(list = ls())    # This clears all variables from workspace

# Load data in form of CSV file
ArsenalHome <- read.csv("C:/Datasets/Arsenal_home_2020.csv")
print(ArsenalHome)

# Inspect data
names(ArsenalHome)  # This displays the variable names
str(ArsenalHome)  # This displays the structure of the data frame.

Ar_dat <- ArsenalHome    # This makes a working copy of the data frame.

# Step 1 – Create the new empty variables populated with NAs.
Ar_dat["GD"] <- NA  # This creates a new column to store goal difference. 
Ar_dat["TG"] <- NA  # This creates the new column to store total goals scored.
Ar_dat["HTSR"] <- NA  # This creates the new column to store home team shots ratio.
Ar_dat["ATSR"] <- NA  # This creates the new column to store away team shots ratio. 

# Step 2 - Populate the new columns with the calculated values.
Ar_dat$GD <- Ar_dat$HG - Ar_dat$AG   # Goal difference
Ar_dat$TG <- Ar_dat$HG + Ar_dat$AG   # Total goals
Ar_dat$HTSR <- round(Ar_dat$HS/(Ar_dat$HS + Ar_dat$AS),3)  # HTSR rounded to 3dp
Ar_dat$ATSR <- round(Ar_dat$AS/(Ar_dat$AS + Ar_dat$HS),3)   # ATSR rounded to 3dp

names(Ar_dat)

head(Ar_dat,8)  # This displays the first eight rows of the modified data frame.
  
# Export results as CSV file
#write.csv(Ar_dat, "C:/AnalysisResults/Arsenal_home_shots_ratio.csv")  

#####

# Code for Example 3.2

# Inspect data
head(Ar_dat)

# This splits the data into separate win, lose and draw data frames.
win <- Ar_dat[Ar_dat$Result == "W",]    # Wins
lose <- Ar_dat[Ar_dat$Result == "L",]    # Lose
draw <- Ar_dat[Ar_dat$Result == "D",]    # Draw

# Display sub-groups
print(win)
print(lose)
print(draw)

#####

# Code for Example 3.3

# Create data with some missing data entries
players <- c("Paul","Stephen","James","Kevin","Tom","Edward","John","David") # Players
shots <- c(2.4,3.6,0.3,1.1,4.2,2.3,NA,0.6) # Average number of shots per game
goals <- c(0.2,0.6,0.0,0.1,0.7,0.3,0.1,0.0) # Average number of goals per game
passes <- c(23.1,NA,39.2,25.5,18.6,37.4,28.3,28.3) # Average number of passes per game
tackles <- c(6.3,4.5,10.6,9.8,4.1,5.3,11.2,7.8) # Average number of tackles per game

# Create data frame
perf_dat <- cbind.data.frame(players,shots,goals,passes,tackles) # Creates data frame
print(perf_dat)

# Completely remove lines containing NAs
na.omit(perf_dat)

# Try out 'mean' function on data
mean(perf_dat$shots) # This does not work.
mean(perf_dat$goals) # This works.
mean(perf_dat$passes) # This does not work.
mean(perf_dat$tackles) # This works.

# Now adding na.rm = TRUE
mean(perf_dat$shots, na.rm = TRUE) # This now works.
mean(perf_dat$goals, na.rm = TRUE) # This now works.
mean(perf_dat$passes, na.rm = TRUE) # This now works.
mean(perf_dat$tackles, na.rm = TRUE) # This now works.

# Alternatively, use the 'describeBy' function in the “psych” package.
# install.packages("psych")  # This installs the ‘psych’ package. 
# NB. This command only needs to be executed once to install the package.
# Thereafter, the ‘psych’ library can be called using the command.
library(psych)
describeBy(perf_dat[,c(2:5)])

#####

# Code for Example 3.4

rm(list=ls())  # This clear existing variables and data from the workspace.

# Load data from Internet (all variables)
# EPL2020_dat <- read.csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv')

# Load data from Internet (first variables)
EPL2020_dat <- head(read.csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv'),380)[,1:16]

# Inspect data
names(EPL2020_dat)  # This lists the names of the variables in the data frame.  
head(EPL2020_dat,10)  # This displays the first ten lines of the data frame.

# Export data
#write.csv(EPL2020_dat, "C:/AnalysisResults/EPL_results_2021.csv")  

#####

# Code for Example 3.5

rm(list=ls())  # Clear existing variables and data from the workspace. 

# Download results from website for the 5 seasons 2016-2020.
seasons <- c(rep("1617",1), rep("1718",1), rep("1819",1), rep("1920",1), rep("2021",1))

division <- c(rep(c("E0"),5)) # NB. "E0" is the EPL and 5 refers to the number of seasons

urls = paste(seasons, division, sep="/")
urls = paste("https://www.football-data.co.uk/mmz4281", urls, sep="/")

# Load all the data using a loop and selecting just a few variables.
download_data = NULL
for(i in 1:length(urls)){
temp = read.csv(urls[i])
temp = temp[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR")]
download_data = rbind(download_data, temp)
}

# Inspect data frame
head(download_data,10) # This displays the first 10 rows.
tail(download_data,10) # This displays the last 10 rows.

#####

# Code for Example 3.6

# First install the devtools package, which enables packages to be installed from GitHub. 
#install.packages("devtools")  

# Then install worldfootballR from GitHub.
#devtools::install_github("JaseZiv/worldfootballR", ref = "main")  

# Clear existing variables and data from the workspace.
rm(list=ls())

# Call the worldfootballR package
library(worldfootballR) # This calls up the library package.
match_urls <- fb_match_urls(country = "ENG",gender = "M",tier = "1st",season_end_year = c(2021))

# Inspect match URLs
head(match_urls,10)

# Create match summary data frame
match_summary <- fb_match_summary(match_url = 
                "https://fbref.com/en/matches/bf52349b/Fulham-Arsenal-September-12-2020-Premier-League") 

# Display match summary
print(match_summary[,c(19:25)]) # This displays the variables of interest (i.e. columns 19-25). 

#####

# Code for Example 3.7

library(worldfootballR)

# Load data
EPL_2020_standard <- fb_season_team_stats(country = "ENG", gender = "M", 
                                          season_end_year = "2021", tier = "1st", stat_type = "standard")

# Inspect data
names(EPL_2020_standard)

# Display data
print(EPL_2020_standard[,c(4:9,14,15)])

#write.csv(EPL_2020_standard, "C:/AnalysisResults/EPL_2020_standard.csv")  

#####

# Code for Example 3.8

library(worldfootballR)
Ronaldo_shooting <- fb_player_season_stats("https://fbref.com/en/players/dea698d9/Cristiano->Ronaldo", 
                                           stat_type = 'shooting')

# Inspect data
names(Ronaldo_shooting)

# Selected variables from this data frame can be displayed using:
head(Ronaldo_shooting[,c(1,3:5,8,9,11)], 20) # This displays the top 20 rows in the data frame.

#####

# Code for Example 3.8

# install.packages("rvest")  # NB. This only needs to be run once to install the package.

# Read html
library(rvest)
tran_window <- read_html("https://en.wikipedia.org/wiki/Transfer_window")

# Create CSS selector
css_selector <- ".wikitable"

# Scrape table and convert it to a data frame
tw_df <- as.data.frame(tran_window %>%
                         html_element(css = css_selector) %>%
                         html_table(fill=TRUE))

print(tw_df)

#####




