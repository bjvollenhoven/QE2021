#Bevan Vollenhoven 
#Gamma Diversity 

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

#Gamma diversity is amount of biodiversity on a regional/global scale.
#Species richness of all sample units make up the gamma diversity.
#Uses the same metrics as alpha diversity.

sppdata <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/diversity/SeaweedsSpp.csv"
spp <- read_csv(url(sppdata)) %>% 
  select(-X1)
head(spp)
view(spp)
dim(spp)

#The number of columns gives an indication to the number of species
ncol(spp)
#847 species according to the number of columns counted. 


#An alternative method can also be used. 
diversityresult(spp, index = 'richness', method = 'pooled')
#846 species according to the diversityresult() method 
sums <- colSums(spp)
view(sums)
#there is one species that does no occur within any sites.



# Questions ---------------------------------------------------------------

#1. Why is there a difference between the two? 
#ncol() simple counts the number of sites. In the diversityresult() function, we 
#are pooling the data together and finding the diversity across all sites. There is 
#one species that is not present within any of the sites, hence it is not part of the
#gamma diversity.

#2. Which is correct?
#The diversityresult() function that is pooling the data is correct as it accounts
#for species that are not present and removes them. 

  
