#Topic 8: Principle component analysis. 
#Bevan Vollenhoven (3840572)
#08 July 2021

library(ggplot2)
library(ggpubr)
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

data_url <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsEnv.csv"
env <- read.csv(url(data_url)) %>% 
  select(-X)
head(env)
glimpse(env)
env

env_pca <- rda(env, scale = TRUE) # scale = TRUE is used to standardize the variables 
env_pca
# In the case of PCA on a correlation matrix, inertia = sum of diag. values
# of the correlation matrix = the number of variables. 
# Eigenvalues shows the relative measures of importance of the axes in descending
# order. 

# Questions A -------------------------------------------------------------
# Ordinations extract main trends in data sets in the form of continuous axes.
# PCA only shows the linear relationship between data as it preserves euclidean 
# distance. Therefore, it cannot alone be sufficient to explain changes in gradients
# in the real world. Only the first few principle components are used as they show
# the most importance in variation according to their eigenvalues. e.g. The number of axes
# chosen must sow at least 75% of variance in the data. The rest of the axes will show
# the remaining 35%. The remaining data does not show as much of the variation and 
# focus can be placed on the first few axes. In this way, there is reduction in 
# complexity of the data set allowing for a good first insight on the data. 


summary(env_pca) #allows us to view the species scores and site scores. 

round(sum(env_pca$CA$eig[1:2]) / sum(env_pca$CA$eig) * 100, 1) # the proportion of variation explained by PC1 and PC2 is 73.9%

biplot(env_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(env_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

source("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/cleanplot.pca.R")
cleanplot.pca(env_pca, scaling = 1) # argument "scaling = 1" (distance biplot), scales 
                                    # eigenvalues to their aprox. euclidean distance
                                    # i.e. distance among points in the biplot = aprox. euclidean distance. 

# the radius of the circle in this plot (circle of equilibrium contribution), 
# represents the variable that would contribute equally to all dimensions in PCA. 
# Variables that have longer vectors than the circle have higher than average contributions
# and can be interpreted confidently. 

cleanplot.pca(env_pca, scaling = 2) # the argument "scaling = 2" (correlation biplot)
                                    # scales each eigenvalue to the square root of 
                                    # that eigenvalues. Angles between descriptors = correlation.


biplot(env_pca, type = c("text", "points"), col = c("black", "black"))
ordisurf(env_pca ~ bod, env, add = TRUE, col = "turquoise", knots = 1)
ordisurf(env_pca ~ alt, env, add = TRUE, col = "salmon", knots = 1)


# Questions B -------------------------------------------------------------
# 1 

# Bird communities in Yushan Mountains ---------

url_birds <- "https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_env.txt"
birds_data <- read.delim(url(url_birds)) %>% 
  select(-`Veg.`, -Veg_ext, -Station) # the variables are descriptive and not numeric. 
head(birds_data)
dim(birds_data)
view(birds_data)

bird_pca <- rda(birds_data, scale = TRUE) # scale = TRUE is used to standardize the variables 
bird_pca

round(sum(bird_pca$CA$eig[1:2]) / sum(bird_pca$CA$eig) * 100, 1) #71.2% of variation is explained in the first two PC's

summary(bird_pca)
# Ground cover had the highest species score in PC1, the lowest being T2C. 

biplot(bird_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(bird_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

cleanplot.pca(bird_pca, scaling = 1)
cleanplot.pca(bird_pca, scaling = 2)
# ELE  =	Elevation [m a.s.l.]
# ASP  =	Aspect (norhteness): 1-8; 1 - southwest (warmest), 8 - northeast (coldest)
# SLP  =	Slope
# EXP  =	Exposure: 0 - valley, 2 - lower slope, 4 - middle slope, 6 - upper slope, 8 - ridge, 10 - peak
# CH   =	Canopy height [m]
# GC   =	Ground cover
# HC   =	Herb cover
# SC   =	Shrub cover
# T2C  =	Secondary tree cover
# T1C  =	Canopy cover
# TFV  =	Total foliage volume
# FHD  =	Foliage height diversity index
# CP   =	Conifer percentage
# TD   =	Tree density
# MDB  =	Mean tree DBH
# SDDB =	Standard deviation of tree DBH
# TBA  =	Tree basal area
# TSD  =	Tree species diversity 

# A

# For the scaling 1 biplot, a gradient is present from left to right. A group 
# that includes sites 1-10, 12-16, and 22-23 make up the first group of sites. They
# have the lowest values in EXP, SLP, ELE, and CP. They highest values were TSD, 
# TD, T2C, ASP, and HC. 
# The second group includes sites includes sites 14, 17-21, and 24-31. This group has 
# the highest values in TFV, CH, T1C, FHD, SDDB, and TBA. Their lowest values were
# SC and GC. 
# The third group (sites 32-45), showed the inverse of the first group with their highest
# values being EXP, SLP, ELE, and CP. Their lowest values were TD, T2C, ASP, and HC. The 
# final group (sites 46-50) had their highest values in SC and GC with their lowest in 
# TFV, CH, T1C, FHD, SDDB, and TBA. Majority of the influence on variances within the ordination
# was due to SDDB, TBA, and MDB.
# Overall, the change of a tree dominated landscape to shrub dominated landscape can be seen
# with an increase in elevation. 

# In the scaling 2 biplot, correlations between variables. The variables can be
# organised into groups. In the upper-left comer TD, TSD, T2C, ASP, and HC all have
# have strong positive correlations, while having strong negative correlations with
# EXP, CP, ELE, and SLP. In the top right comer, SC and GC have a strong positive
# correlation, while being negatively correlated with TFV, CH, T1C, FHD, SDDB, and TBA. 
# MDB has a nearly orthogonal arrow, and SC has a shorter arrow showing its lesser
# importance in the ordination of sites. 

# B
# The change from tree dominated to a shrub dominated landscape over short distances
# can be explained by the presence of an elevation gradients. Changes in elevation 
# introduce more variation in temperature and precipitation over relatively short 
# distances. There can also be variation in species composition with north-facing
# and south-facing slopes. These factors can all contribute to changes in species
# composition along a mountain side. The colder the environment is the less variation
# in species composition is present. 

# C
# A strong negative correlation can be seen between group A (EXP, CP, ELE, and SLP),
# and group B (TD, TSD, T2C, ASP, and HC). An increase in elevation over a short
# distance facilitates the presence of a steep slope. The higher an area is, the more
# exposed it is to the elements. With the increase in elevation, tree density and 
# tree species diversity can decrease. This, as stated previously, is due to colder
# and less protected and covered environments. 

# Alpine plant communities in Aravo, France. ------------------
alpine_data <- read.csv("data/aravo_env.csv") %>% 
  select(-X, -ZoogD) # the variables are descriptive and not numeric. 
dim(alpine_data)
head(alpine_data)


alpine_pca <- rda(alpine_data, scale = TRUE)
alpine_pca

summary(alpine_pca)
#The highest species score in PC1 was Form, and the lowest was Slope. 

round(sum(alpine_pca$CA$eig[1:3]) / sum(alpine_pca$CA$eig) * 100, 1)
# #83.2% of variation is explained in the first three PC's

cleanplot.pca(alpine_pca, scaling = 1)
cleanplot.pca(alpine_pca, scaling = 2)

# Aspect =  Relative south aspect (opposite of the sine of aspect with flat coded 0)
# Slope  = Slope inclination (°)
# Form 	 = Microtopographic landform index [1 (convexity); 2 (convex slope); 3 (right slope); 4 (concave slope); 5 (concavity)]
# Snow 	 = Mean snowmelt date (Julian day) averaged over 1997-1999
# PhysD  = Physical disturbance, i.e., percentage of unvegetated soil due to physical processes

# A
# In the scaling 1 biplot, sites within the top-quadrant had the highest values in
# slope inclination and relative south aspect, but the lowest in mean snow melt. This does not include sites 46,
# 74 and 15, which can be grouped with sites in the bottom-left quadrant. These sites
# have the highest values in physical disturbances, and the lowest in microtopographic 
# land form index. This includes sites 66 and 72. Site in the bottom-right quadrant 
# had the highest values in mean snow melt and the lowest in slope inclination. Sites
# in the top-right quadrant showed the inverse from sites found in the bottom-left 
# quadrant. 

# In the scaling 2 biplot, slope inclination and mean snow melt had a strong, negative
# correlation. The same can be said for the microtopographic land form index and
# physical disturbances. relative south aspect had the shortest arrow indicating that is
# explains the least amount of variation within the ordination. 

# B and C
# No clear pattern was observed. However, correlations between variables can be explained.
# The more concave an area is the more vegetative physical disturbances are present.
# Concave regions offer protection from external stressors and have lower rates of soil erosion. 
# Concave regions may also have increased water input as a result of water draining into the 
# area allowing the soil to have a higher moisture content, facilitating plant growth.  
# Snow pack thermodynamic is strongly influenced by slope, which in turn affects snow accumulation
# and snow melting. Snow packing sensitivity to climate change may also change over relatively
# short distances depending on the aspect. This could explain the negative correlation seen between 
# mean snowmelt and aspect + slope inclination. 


