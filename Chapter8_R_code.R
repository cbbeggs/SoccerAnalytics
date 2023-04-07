
# Chapter 8: R code
# Copyright: Clive Beggs - 31st March 2023

# Code for Example 8.1

rm(list = ls())    # Clears all variables from the workspace

# First we input the results for the matches in the mini-soccer competition.
match1 <- c("Midtown","Halton",3,2,"H") # Midtown v Halton (score: 3-2)
match2 <- c("Oldbury","Newtown",3,1,"H") # Oldbury v Newtown (score: 3-1)
match3 <- c("Longbury","Scotsway",4,2,"H") # Longbury v Scotsway (score: 1-3)
match4 <- c("Tilcome","Oldbury",0,1,"A") # Tilcome v Oldbury (score: 1-1)
match5 <- c("Scotsway","Tilcome",3,3,"D") # Scotsway v Tilcome (score: 3-3)
match6 <- c("Midtown","Longbury",2,2,"D") # Midtown v Longbury (score: 2-2)
match7 <- c("Halton","Midtown",1,2,"A") # Halton v Midtown (score: 1-2)
match8 <- c("Newtown","Longbury",1,3,"A") # Newtown v Longbury (score: 1-3)
match9 <- c("Halton","Scotsway",4,2,"H") # Halton v Scotsway (score: 4-2)
match10 <- c("Oldbury","Midtown",4,1,"H") # Oldbury v Midtown (score: 4-1)

mini <- rbind.data.frame(match1,match2,match3,match4,match5,match6,
                         match7,match8,match9,match10)
colnames(mini) <- c("HomeTeam","AwayTeam","HG","AG","Results")
mini$HG <- as.numeric(mini$HG) # Convert to integers.
mini$AG <- as.numeric(mini$AG) # Convert to integers.
print(mini)

# Harvest team names 
teams <- unique(mini$HomeTeam)
teams <- sort(teams)
n <- length(teams) # This identifies the length of the ‘teams’ vector. 

# Produce a adjacency table of who-played-who
HomeTeam <- mini$HomeTeam
AwayTeam <- mini$AwayTeam
adj.tab1 <- table(HomeTeam,AwayTeam)
print(adj.tab1)

# Convert table to an adjacency matrix
temp1 <- as.numeric(as.matrix(adj.tab1))
adj.mat1 <- matrix(temp1, nrow=n, ncol=n)
print(adj.mat1) # NB. This adjacency matrix is asymmetrical, indicating a directional graph.

# Build graph using 'qgraph' package. 
# install.packages("qgraph")  # This installs the ‘qgraph’ package. 
# NB. This command only needs to be executed once to install the package. 
# Thereafter, the ‘qgraph’ library can be called using the ‘library’ command.

library(qgraph)
mini.graph1 <- qgraph(adj.mat1, labels=teams, label.cex=1.3, esize=5, 
                      vsize=10, edge.color="black")  # Who-played-who network
title("Matches played", adj=1, line=3) # This puts title in top right hand corner

#####

# Code for Example 8.2

# Assign numerical values to individual teams
HT <- as.factor(mini$HomeTeam)
levels(HT) <- 1:length(levels(HT))
HT <- as.numeric(HT)       
print(HT)

AT <- as.factor(mini$AwayTeam)
levels(AT) <- 1:length(levels(AT))
AT <- as.numeric(AT)
print(AT)      

# Create new matrix
X <- cbind(HT,AT,mini[,3:4])

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

adj.mat2 <- adj1+adj2
rownames(adj.mat2) <- teams
colnames(adj.mat2) <- teams
print(adj.mat2) # NB. This adjacency matrix is asymmetrical, indicating a directional graph.

# Plot directional graph
library(qgraph)
mini.graph1 <- qgraph(adj.mat2, labels = teams, label.cex = 1.7, edge.labels = TRUE,
                      edge.color="black", edge.label.cex = 2)  # Who-scored-against-who network
title("Goals scored", adj=0.5, line=3)

#####

# Code for Example 8.3

# Load the data
Spain <- read.csv("C:/Datasets/Spain_2010_WC_final.csv", sep=",")
Netherlands <- read.csv("C:/Datasets/Netherlands_2010_WC_final.csv", sep=",")

# Display the data
print(Spain)
print(Netherlands)

# Create the adjacency matrices
# Make Spain team 1 (T1) and Netherlands team 2 (T2)
T1 <- Spain
T2 <- Netherlands

adj.T1 <- as.matrix(T1[1:14,2:15])
adj.T2 <- as.matrix(T2[1:14,2:15])

# Produce passing network graphs
# Spain
library(qgraph)
q.T1 <- qgraph(adj.T1, edge.color="black")  # Spanish passing network
title("Spain", adj=0.1, line=-1)

# Netherlands
q.T2 <- qgraph(adj.T2, edge.color="black")  # Dutch passing network
title("Netherlands", adj=0.8, line=-1)

# Alternative method using the ‘igraph’ package
# install.packages("igraph")  # This installs the ‘igraph’ package. 
# NB. This command only needs to be executed once to install the package. 
# Thereafter, the ‘igraph’ library can be called using the ‘library’ command.

# Build graph using 'igraph' package.
# Spain
library(igraph)
g.T1 <- graph_from_adjacency_matrix(adj.T1, weighted=TRUE)
plot(g.T1, vertex.color = "white", vertex.label.color = "black", layout=layout.kamada.kawai)
title("Spain", adj=0.2, line=-1)

# Netherlands
library(igraph)
g.T2 <- graph_from_adjacency_matrix(adj.T2, weighted=TRUE)
plot(g.T2, vertex.color = "white", vertex.label.color = "black", layout=layout.kamada.kawai)
title("Netherlands", adj=0.8, line=-1)

#####

# Code for Example 8.4

library(igraph)
# Spain
nv.T1 <- vcount(g.T1) # Number of vertices
ne.T1 <- ecount(g.T1) # Number of edges
den.T1 <- graph.density(g.T1) # Graph density
dia.T1 <- diameter(g.T1) # Diameter of graph
apl.T1 <- average.path.length(g.T1) # Average path length
res.T1 <- c(nv.T1, ne.T1, den.T1, dia.T1, apl.T1) # Combine into vector

# Netherlands
nv.T2 <- vcount(g.T2) # Number of vertices
ne.T2 <- ecount(g.T2) # Number of edges
den.T2 <- graph.density(g.T2) # Graph density
dia.T2 <- diameter(g.T2) # Diameter of graph
apl.T2 <- average.path.length(g.T2) # Average path length
res.T2 <- c(nv.T2, ne.T2, den.T2, dia.T2, apl.T2) # Combine into vector

# Compile descriptive statistics for the graphs
results <- round(cbind.data.frame(res.T1,res.T2),2)
colnames(results) <- c("Spain","Netherlands")
rownames(results) <- c("Number of vertices","Number of edges","Graph density",
                       "Graph diameter","Average path length")
print(results)

# Compile descriptive statistics for the individual players
library(qgraph)

# Spain
outdeg.qT1 <- centrality(q.T1)$OutDegree
indeg.qT1 <- centrality(q.T1)$InDegree
between.qT1 <- centrality(q.T1)$Betweenness
close.qT1 <- centrality(q.T1)$Closeness
players.qT1 <- cbind.data.frame(outdeg.qT1,indeg.qT1,between.qT1,round(close.qT1,3))
colnames(players.qT1) <- c("OutDegrees","InDegrees","Betweenness","Closeness")
print(players.qT1)

# Netherlands
outdeg.qT2 <- centrality(q.T2)$OutDegree
indeg.qT2 <- centrality(q.T2)$InDegree
between.qT2 <- centrality(q.T2)$Betweenness
close.qT2 <- centrality(q.T2)$Closeness
players.qT2 <- cbind.data.frame(outdeg.qT2,indeg.qT2,between.qT2,round(close.qT2,3))
colnames(players.qT2) <- c("OutDegrees","InDegrees","Betweenness","Closeness")
print(players.qT2)

# Comparison between the results for the two teams.
res.qT1 <- colSums(players.qT1)
res.qT2 <- colSums(players.qT2)
res <- rbind(res.qT1,res.qT2)
rownames(res) <- c("Spain","Netherlands")
print(res)

# igraph can be used to rate the performance of the players
library(igraph)
# Spain
nodes.T1 <- V(g.T1)$name # Names of Spanish players
pr.T1 <- page_rank(g.T1, directed = TRUE)$vector
auth.T1 <- authority.score(g.T1)$vector
players.gT1 <- round(cbind.data.frame(pr.T1,auth.T1),3)
rownames(players.gT1) <- nodes.T1
colnames(players.gT1) <- c("PageRank","Authority")
print(players.gT1)

# Netherlands
nodes.T2 <- V(g.T2)$name # Names of Spanish players
pr.T2 <- page_rank(g.T2, directed = TRUE)$vector
auth.T2 <- authority.score(g.T2)$vector
players.gT2 <- round(cbind.data.frame(pr.T2,auth.T2),3)
rownames(players.gT2) <- nodes.T2
colnames(players.gT2) <- c("PageRank","Authority")
print(players.gT2)

#####

# Code for Example 8.5

# Create two vectors showing the relationships between managers and clubs.
V1 <- c("A","A","A", "B","B","B","C","D","D","E","F") # Clubs
V2 <- c("m1","m2","m7","m3","m4","m5","m5","m6","m7","m2","m6") # Managers

# Combine vectors into a data frame
d <- cbind.data.frame(V1,V2)

# Convert to graph object
library(igraph)
g <- graph_from_data_frame(d, directed=FALSE) # Create graph
V(g)$label <- V(g)$name # Set labels
print(V(g)$label) # Show unique nodes

# Set type
V(g)[name %in% V1]$type <- 1
V(g)[name %in% V2]$type <- 2
print(V(g)$type) # Display types

# Define colour and shape mappings.
col <- c("lightgray", "white")
shape <- c("square","circle")

# Plot bivariate graph
plot(g, vertex.label.color = "black",
     vertex.color = col[as.numeric(V(g)$type)],
     vertex.shape = shape[as.numeric(V(g)$type)]
)

#####



