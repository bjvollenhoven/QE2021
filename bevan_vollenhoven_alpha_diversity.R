#Bevan Vollenhoven
#3840572
#Measures of Biodiversity 
#Alpha biodiversity Questions

#The first measure of biodiversity (alpha diversity) was tested and worked through.
#Question asked about the specific section were answered accordingly. This section
#Mainly focuses on the codes needed to depict the various methods of showing 
#alpha diversity in R. 


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

 

sppdata <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/diversity/SeaweedsSpp.csv"
spp <- read_csv(url(sppdata)) %>% 
  select(-X1)
head(spp)
view(spp)
dim(spp)

#The alpha diversity of the data set can be presented in three ways:
#Species richness (S)
#Uni-variate diversity index, such as Shannon diversity or Simpson's diversity
#Dissimilarity index. 

# Species richness --------------------------------------------------------


#Species richness shows the number of species within each sample location

spp_richness <- diversityresult(spp, index = 'richness', method = 'each site') 
#species richness is calculated per site is the spp dataset as dictated by method = `each site`. 

spp_richness_plot <- ggplot(data = spp_richness, (aes(x = 1:58, y = richness))) +
  geom_line(aes(col = "salmon")) + 
  labs(x = "Coastal section, west to east", y = "Species richness",
       title = "Seaweed species richnest along the coastal region \nof South Africa") +
  theme_bw() +
  theme(legend.position = "none")
spp_richness_plot


# Univariate indices ------------------------------------------------------

#Alpha diversity can be expressed in one of two uni-variate diversity indices:
#Shannon's diversity - focuses on relative abundance and richness
#The Simpson index - puts emphasis on the evenness of species diversity 
#Uni-variate indices cannot be calculated with absence/presence data 
#abundance data is needed. 
#Instead of using the spp data, we will be using supplementary data consisting of a three differing communities of plants
#that are each one showing differing abundances in different light environments 

light_data <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/diversity/light_levels.csv"
light <- read_csv(url(light_data))
dim(light)
head(light)

light_div_indx <- data.frame(
  site = c("low_light", "mid_light", "high_light"),
  richness = specnumber(light[, 2:7], MARGIN = 1), #alternative method of calculating species richness in the event that diversityresult() is unable to work. 
  shannon = round(diversity(light[, 2:7], MARGIN = 1, index = "shannon"), 2),
  simpson = round(diversity(light[, 2:7], MARGIN = 1, index = "simpson"), 2))
light_div_indx
#according to the results, communities in found in mid light conditions were
#more species rich


# Dissimilarity indices -----------------------------------------------------

#Vegdigest() is used to calculated the sorensen dissimilarity index
view(spp)
dissimilar <- vegdist(spp, binary = TRUE) # binary = TRUE sets species that presence/absence data is being used, FALSE = abundance data. 
dissimilar_df <- round(as.matrix(dissimilar), 4)
dims(dissimilar_df)
view(dissimilar_df)

dissimilar_plot_data <- as_tibble(dissimilar_df[1, 1:58]) #filters out the first row
#as to only show how dissimilar site 1 is to all the other sites. 
view(dissimilar_plot_data)


dissimilar_plot <- ggplot(data = dissimilar_plot_data, aes(y = value, x = 1:58)) +
  geom_line(aes(col = "salmon")) +
  labs(x = "Coastal section, west to east", y = "Dissimilarity index",
       title = "Change in dissimilarity over distance") +
  theme_bw() +
  theme(legend.position = "none")
dissimilar_plot



# QUESTIONS ----------------------------------------------------------------

#1. Why is the matrix square, and what determines the number of rows/columns?
#The matrix is square due to the number of rows being compared equal to the 
#number of columns.

#2. What is the meaning of the diagonal?
#The diagonal section were the value is always zero shows where the current site
#is being compared to itself. Therefore, dissimilarity will always be zero.

#3. What is the meaning of the non-diagonal elements?
#The non-diagonal elements statistically depict the way in which one site differs
#to another through their dissimilarity index. The higher the index, the more 
#different the sites are.

#4. Take the data in row 1 and create a line graph that shows these values as a function of section number.
dissimilar_plot

#5. Provide a mechanistic (ecological) explanation for why this figure takes the 
#shape that it does.

#The figure shows as that the sites become increasingly dissimilar to site 1 as you move 
#further east. The dissimilarity index is also seen as analogous to a distance matrix.
#Thus, this graph shows as distance increases, so does dissimilarity. As you move
#along the east coast a temperature gradient can be observed. This temperature gradient effects
#the species presence at various sites as different species have their own optimal temperature. 
#As temperature changes, so does the species composition. Therefor, the species found in site 1
#will become more dissimilar as you compared it to sites up along the the east.






