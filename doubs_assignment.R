#Topic 5: species dissimilarity 
#Duet date: 05 July 2021
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

#load in the dataset.
doubs_spp <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsSpe.csv"
d_spp <- read.csv(url(doubs_spp)) 

#1. Look at the dataset and explain its structure in words. -----------------
dim(d_spp) #assess the dimanesions of the dataset. 
head(d_spp)
d_spp
view(d_spp)

spp <- as_tibble(d_spp) %>%  #transformer the dataset into a table in order to filter out unwanted columns.
  select(-X)
view(spp)
dim(spp)

#The data is set up as a species abundance dataset. Each row represents a site and each column
#represents a species. In this way, the abundance (number of species) can be observed
#as a table. 

# 2. Would we use Bray-Curtis or Jaccard dissimilarities? -----------------
#Bray-Curtis dissimilarity index must be used as we are looking at dissimilarity between sites.
#Jaccard's index looks at similarity between sites. 


# 3. Apply the calculation. -----------------------------------------------
bray_curtis <- vegdist(spp, binary = FALSE, diag = TRUE) #used binary = false for abundance data when calculating dissimilarity index and apply the Bray-Curtis index.
bray <- as.matrix((bray_curtis)) #transformes the data back into a data matrix. 


# 4. Explain the meaning of the results in broad terms. -------------------
#The results show how dissimilar one site is to another. The further sites are 
#to one another, the more dissimilar they are. The data is square as each site is being compared to
#itself and every other site. The presence of the zero diagonal shows where each site is compared to itself.


# 5. Examine it more closely: what general pattern comes out? -------------
#There are spikes which occur in the dissimilarity. Some sites are completely
#dissimilar (dissimilarity = 1) compared to other creating spikes in the index.
#The sites are also occurring along a latitudinal gradient.
#There is also a site that has no fish species being studied. 


# 6. Plot this pattern. ---------------------------------------------------

dim(bray_curtis)
bc <- as_tibble(bray[1, 1:30]) #filters out only the first row and converts it into a table in order to plot the data. 
view(bc)

bc_plot <- ggplot(data = bc, aes(x = 1:30, y = value)) +
  geom_line(aes(colour = "salmon")) +
  labs(x = "Site along river side", y = "Dissimilarity",
       title = "Site species dissimilarity along sites ", 
       subtitle = "Abundance data") +
  theme_bw() +
  theme(legend.position = "none")

bc_plot


#7. What explanation can you offer for this pattern? ---------------------

#There are dips and spikes in dissimilarity between the sites. This can be attributes
#to the change of altitude that the sites occur in. Altitude has dramatic effects
#on diversity. The higher altitudes can decrease diversity due to colder temperatures and
#lower altitudes can effect diversity as well due to possible human interactions.


#8. create presence/absence data, and obtain a suitable dissimilarity matrix.-----

pa <- (decostand(spp, method = "pa")) #converts the abundance data to presence/absence data. 
pa

sorensen <- vegdist(spp, binary = TRUE) #binary = TRUE is used for presence absence data to produce a dissimilarity index and apply sorenson's index. 
sorensen_df <- as.matrix(as.matrix(sorensen))
dim(sorensen_df)
view(sorensen_df) 

sor <- as_tibble(sorensen_df[1, 1:30])
sor




#9. Create another plot and explain the pattern. --------------------------
sor_plot <- ggplot(data = sor, aes(x = 1:30, y = value)) +
  geom_line(aes(colour = "salmon")) +
  labs(x = "Site along river side", y = "Dissimilarity",
       title = "Site species dissimilarity along sites ",
       subtitle = "Presence/Absence data") +
  theme_bw() +
  theme(legend.position = "none")

sor_plot

#The dissimilarity for the presence absence data follows the same pattern as the dissimilarity index
#for abundance data. The pattern still shows the presence of dips in dissimilarity, followed 
#by a gradual increase. The patterns have slight discrepancies due to the weighting that the various 
#datasets have on species data. Presence absence data gives more weighting to rare species as they
#are weighted the same as common species. Abundance data give more weighting to commons species.




