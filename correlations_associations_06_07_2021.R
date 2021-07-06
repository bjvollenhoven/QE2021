#Correlations and Associations 
#Bevan Vollenhoven (3840572)

library(ggplot2)
library(dplyr)
library(tidyverse)
library(betapart)
library(vegan)
library(gridExtra)
library(BiodiversityR)
library(grid)
library(gridBase)
library(tidyr)
library(geodist)
library(readr)
library(Hmisc)
library(corrplot)


env_url <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsEnv.csv"
env_data <- read.csv(url(env_url))

# remove the first column
env <- as_tibble(env_data) %>% 
  select(-X)

head(env)
env

cor <- round(cor(env), 2) #used to find correlations between environmental variables. 
cor


pcore <- rcorr(as.matrix(env)) #used to find the p values between environmental gradients. 
pcore
view(pcore)

cor_p <- rcorr(as.matrix(env)) #show the p-values between variables in order to establish significance.
cor_p

# Questions A --------------------------------------------------------------
#1. Create a plot of pairwise correlations.
plt <- corrplot(cor, method = "circle",
                title = "Correltation between environmental variables", 
                mar = c(0, 0, 4, 0)) #in order to prevent the title from being cut. Adjusts margins to fit your title.
plt

#2. Name to two top positive and two top negative statistically-significant correlations.
#dfs and flo, and amm and pho are positive statistically-significant correlations.
#dfs and alt, and alt and flo are negative statistically-significant correlations.

#3. For each, discuss the mechanism behind the relationships. Why do these relationships exist?

# As the Distance from source (dfs) increases, so does the Mean minimum discharge (flo),
# showing their positive correlation. This is due to the increased in speed of water 
# flow as the river goes to lower latitudes and gains more power. 
# Ammonium concentration (amm) and Phosphate concentration (pho) increase
# together and have a positive correlation. These are chemicals that are associated 
# with the decomposition of plants. The increase of plant decay, and the influence 
# of anthropogenic factors (agriculture) as you move downstream all contribute to 
# the increase of these chemicals. 

# Dfs and Altitude (alt) showed a negative correlation. The source starts at the mouth
# of a rive, which in this case, is at a higher altitude. So naturally, as the one moves
# to lower altitudes, you move away from the source. Altitude and flow also show a 
# statistically negative correlation. As one move to high altitudes, the mean minimum
# discharge in the river is lower as you get closer to the source at higher 
# altitudes. As the river flows down the mountain side, the flow of water increases
# and so does the flo. 

#Questions B ---------------------------------------------------------------
sp_url <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsSpe.csv"
sp_data <- read.csv(url(sp_url)) %>% 
  select(-X)
head(sp_data)
glimpse(sp_data)
sp_data

#1. Why do we need to transpose the data?
#As the data is originally, if we run correlation tests, the correlations between 
#species would be found. We transpose the data set to make the sites the subject 
#of our correlation calculations. In this way, correlations between sites might be
#identified and you could possible associate those correlations to common 
#environmental characteristics. 
  
#2. What are the properties of a transposed species table?
#Instead of showing the quantity of species in each site, it shows the quantity 
#of sites in which each species is found. 



# Questions C --------------------------------------------------------------
sp_data_t <- t(sp_data)

assoc_1 <- as.matrix(vegdist(sp_data_t, method = "jaccard"))
view(assoc_1)

assoc_2 <- as.matrix(vegdist(sp_data_t, method = "jaccard", binary = TRUE))
view(assoc_2)

#1. What are the properties of an association matrix? How do these properties 
#differ from that of a i) species dissimilarity matrix and from a ii) correlation 
#matrix?

# Association matrices encompass multivariate analysis comparisons of all possible 
# pairs of descriptors or objects. These matrices are square and symmetrical and
# have dimensions where the number of rows equals the number columns (they may contain
# variables or objects) being compared. Association measures have different forms;
# Q-mode and R-mode. R-mode includes matrices that compares pares of descriptors
# whereas Q-mode focuses on objects being compared. 

# Q-mode includes dissimilarity and similarity indexes. These show how similar or
# different objects being compared are. R-mode includes correlation type coefficients.
# These are used to compare species distributions through space and time. 
# Binary coefficients (which are used in Q-mode) can be applied in R-mode to 
# compare species. 

#2. What is the difference between spp_assoc1 and spp_assoc2? Is the information 
#contained in each markedly different from the other?
  
# assoc_1 shows the similarity between sites based in the species abundances present
# between sites. assoc_2 shows the similarity between sites based on the species 
# richness between sites. The information contained in each matrix has slight discrepancies.
# assoc_1 puts more weight on common species to conclude similarity. assoc_2 puts more 
# weight in rare species as they are weighted the same as common species. 

#3. Explain the kind of insight we are able to glean from a species association matrix.

# Species association matrices can be used as a quick comparison between pairs of sites 
# and their associated species. This could give a quick glance to compare how species 
# are distributed over a specific area or time and give more information regrading
# common environmental conditions and species niches. 




