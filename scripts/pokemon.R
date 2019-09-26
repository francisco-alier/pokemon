##### Challenge - try to cluster the different type of pokemons based on their statuses

#------------- Load packages ------------------

require(tidyverse)
require(dplyr)
require(data.table)
library(corrplot) # to draw correlation maps

# Clustering
library(cluster)
library(factoextra)
library(mlbench)
library(clustertend)
library(NbClust)

# Read data
library(readxl)
library(xlsx)

# Colors
library(RColorBrewer)

#------------- Load the Data ------------- 

df.pokemon <- read.csv(file = "./data/Pokemon.csv")

View(df.pokemon)

dim(df.pokemon)
summary(df.pokemon)
str(df.pokemon)
#-------------  Data ------------- 

# I will use the stats of the pokemon

data <- df.pokemon %>% 
  select(Name, HP, Attack, Defense, Sp..Atk, Sp..Def, Speed)

rownames(data) <- data$Name 

data$Name <- NULL

# scale data
df.scalled <- scale(data)

#------------- Correlation analysis ---------------------------------------------------------------------

# Calculate correlations between pathologies

correlations <- cor(df.scalled)

dim(correlations)  

correlations[1:4, 1:4]

corrplot(correlations, method = "number", type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdBu"), main = "Pokemon stats - correlations")

#------------------- Validate Clusters ----------------------------------------------------------------

## check https://en.wikipedia.org/wiki/Hopkins_statistic ##
# Measure the quality of clustering of dataset using Hopkins statistic

# Compute Hopkins statistic
set.seed(123)

hopkins(data, n = nrow(data)-1)

# It can be seen that the data set is highly clusterable (the H values are
# far below the threshold 0.5).

# Check the distance matrix
fviz_dist(dist(df.scalled), show_labels = FALSE)+
  labs(title = "Pokemon distances")


# Elbow method
fviz_nbclust(df.scalled, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(main = "Pokemon") # 3 to 4 clusters are optimal

# Silhouette method
fviz_nbclust(df.scalled, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")


# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(df.scalled, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method") # 3 clusters are optimal


# Evaluates the optimal number of clusters for each dataset
nb <- NbClust(df.scalled, distance = "euclidean", min.nc = 2,
                  max.nc = 10, method = "kmeans")

fviz_nbclust(nb) # Optimal of 2

#------------------- Clustering ----------------------------------------------------------------

new_hkmeans <- function (x, k, hc.metric = "euclidean", hc.method = "ward.D2", 
                         iter.max = 10, km.algorithm = "Hartigan-Wong") {
  res.hc <- stats::hclust(factoextra::get_dist(x, method = hc.metric), 
                          method = hc.method)
  grp <- stats::cutree(res.hc, k = k)
  clus.centers <- stats::aggregate(x, list(grp), mean)[, -1]
  res.km <- kmeans(x, centers = clus.centers, iter.max = iter.max, 
                   algorithm = km.algorithm)
  class(res.km) <- "hkmeans"
  res.km$data <- x
  res.km$hclust <- res.hc
  res.km
}

# clustering time
set.seed(123)

# 3 clusters
res.hk <-new_hkmeans(df.scalled, 18, hc.metric = "euclidean", hc.method = "ward.D2")

# Visualize the tree and hkmeans final cluster

fviz_dend(res.hk, 
          cex = 0.6 ,
          rect = TRUE,  
          rect_fill = TRUE, 
          main = "Pokemon - 18 clusters")
fviz_cluster(res.hk,
             repel = TRUE, 
             ggtheme = theme_classic(), 
             main = "Pokemon - 18 clusters")


# Analysis of results
df.analysis <- cbind(data,  
                     cluster = res.hk$cluster,
                     type1 = df.pokemon$Type.1)

df.analysis %>% 
  group_by(cluster) %>% 
  summarise(number = n(),
            hp = mean(HP),
            attack = mean(Attack),
            defense = mean(Defense),
            sp.attack = mean(Sp..Atk),
            sp.deffense = mean(Sp..Def),
            speed = mean(Speed))
