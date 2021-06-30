#Bevan Vollenhoven
#Environmental Distance


library(vegan)
library(ggplot2)
library(geodist) # for calculating geographical distances between lats/lons
library(ggpubr) # to arrange the multipanel graphs
library(dplyr)
library(readr) #to read in data sets from a url. 

xyz_data <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/diversity/Euclidian_distance_demo_data_xyz.csv"
xyz <- read_csv(url(xyz_data))
dim(xyz)
glimpse(xyz)
xyz


# euclidean distance ------------------------------------------------------
#Euclidean distance is analogous to dissimilarity index. 
#As with dissimilarity, the general rule is that distance equals dissimilarity. 
#the function vegdist() is used to calculate Euclidean distance. 
#Coordinates can be used (x, y, and z) to find euclidean distance. 


xyz_eu <- round(vegdist(xyz[, 2:4], method = "euclidian", 
                        upper = TRUE, diag = TRUE), 4) # select only columns 2, 3, and 4 as col 1 contains character data
xyz_eu

xyz_data <- as.data.frame(as.matrix(xyz_eu)) #converts the dissimilarity matrix xyz_eu into a data frame that can be used to create plots
xyz_data

#as with dissimilarity indices, distance equals dissimilarity
#the higher the number, the less identical the environmental conditions are.


# Euclidean distance based on environmental variables ----------------------
#Instead of using sites or coordinates, actual environmental variables can be 
#used in which species are present. 

load("data/SeaweedEnv.RData")
dim(env)

#each row represents a site
#each column represents an environmental variable.

env_1 <- env %>% 
  dplyr::select(febMean, febRange, febSD, augMean,
                augRange, augSD, annMean, annRange, annSD)
dim(env_1)
env_1
View(env_1)

#Since there variables are measured in various units, the data needs to be standardized. 
#This is done by calculating the z-scores
#Once the data is standardized, the overall mean will be equal to zero and the 
#overall standard deviation will be 1. 

stand <- round(decostand(env_1, method = "standardize"), 4)
stand
View(stand)

#The calculation of euclidean distance 

stand_euc <- round(vegdist(stand, method = "euclidian", upper = TRUE), 4)
stand_df <- as.data.frame(as.matrix(stand_euc)) #converts the dissimilarity matrix to a data frame.
View(stand_df)
dim(stand_df)

#Plot a graph on how environmentally different site one is to each other site.
eu_plot_data <- as_tibble(stand_df[1:58, 1]) #make the data into table data so a plot can be made.
#filtering out the first column of data only.
View(eu_plot_data)

eu_plot <- ggplot(data = eu_plot_data, aes(y = value, x = 1:58)) + 
  geom_line(col = "salmon") +
  labs(x = "Coastal section, west to east", y = "Environmetal distance",
       title = "Environmetal distance from site 1 to each \n proceeding site") +
  theme_bw()
eu_plot


# Euclidean distances of geographical data --------------------------------
#With the use of coordinates, actual distance can be calculated through 
#Euclidean distance (but it will be scaled)

geo_data <- ("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/diversity/sites.csv")
geo <- read_csv(url(geo_data))
dim(geo)
View(geo)

#Calculate geographic distances (in meters) between coordinate pairs:

distance <- geodist(geo, paired = TRUE, measure = "geodesic")
distance_df <- as.data.frame(as.matrix(distance))
dim(distance_df)
View(distance_df)

distance_plot_data <- as_tibble(distance_df[1:58, 1])
dim(distance_plot_data)
View(distance_plot_data)


plt1 <- ggplot(data = distance_plot_data, (aes(x = 1:58, y = `value`/1000))) + #converts the distance from km to meters
  geom_line(aes(col = "salmon")) +
  labs(x = "Coastal section, west to east", y = "Distance (m)",
       title = "Actual geographic distance") +
  theme_bw() +
  theme(legend.position = "none")
plt1

#euclidean distance
distance_euc <- vegdist(geo, method = "euclidian")
distance_euc_df <- round(as.data.frame(as.matrix(distance_euc)), 4)

euc_dist_data <- as_tibble(distance_euc_df[1:58, 1])
dim(distance_plot_data)
View(distance_plot_data)

plt2 <- ggplot(data = euc_dist_data, (aes(x = 1:58, y = `value`))) + #converts the distance from km to meters
  geom_line(aes(col = "salmon")) +
  labs(x = "Coastal section, west to east", y = "Distance",
       title = "Euclidian geographic distance") +
  theme_bw() +
  theme(legend.position = "none")
plt2

#The two plots look identical as they are. The euclidean plot is to scale
#however there might be some variations in some points, to to the globular shape
#of the earth, in the geographical distance plot. 



