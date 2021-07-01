#Bevan Vollenhoven (3840572)
#Measures of Biodiversity: Beta Diversity

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

#Beta diversity measures the diversity between communities/environments. 
#The first measure if beta diversity is called TRUE BETA DIVERSITY and it was 
#done by Whittaker (1960). 
#It is done by dividing gamma diversity with the alpha diversity of specific sites. 
sppdata <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/diversity/SeaweedsSpp.csv"
spp <- read_csv(url(sppdata)) %>% 
  select(-X1)
head(spp)
view(spp)
dim(spp)

#True beta diversity 

true_beta <- data.frame(beta = specnumber(spp, MARGIN = 1) / ncol(spp), # dividing the number of species per row by the total number of columns present.
                        #alpha diversity divided by gamma diversity. 
  section_no = c(1:58))
dim(true_beta)
view(true_beta) 
#it is sorted into a data frame


beta <- plot <- ggplot(data = true_beta, (aes(x = section_no, y = beta))) +
  geom_line() + 
  labs(x= "Coastal section, west to east", y = "True beta-diversity") +
  theme_bw()
beta

#The second measure of beta-diversity is called absolute beta diversity. 

abs_beta <- data.frame(beta = ncol(spp) - specnumber(spp, MARGIN = 1), #gamma diversity minus alpha diversity per site.
  section_no = c(1:58)) 
dim(abs_beta)
view(abs_beta)

ggplot(data = abs_beta, (aes(x = section_no, y = beta))) +
  geom_line() + xlab("Coastal section, west to east") + 
  ylab("Absolute beta-diversity")


# Contemporary definitions of beta diversity ------------------------------
#library(betapart) this package is needed to perform the follwing functions. 

# Decompose total Sørensen dissimilarity into turnover and nestedness-resultant components:
Y.core <- betapart.core(spp) #sets up the basic quantities needed to decompose soreness into species turnover and nestedness-resultant 
Y.pair <- beta.pair(Y.core, index.family = "sor") #this function breaks the data into beta diversity, species turnover and nestedness-resultan


# Let Y1 be the turnover component (beta-sim):
#Species turnover looks at processes that cause species composition to change without a change in alpha diversity. 
Y1 <- as.matrix(Y.pair$beta.sim)
dim(Y1)
view(Y1)

# Let Y2 be the nestedness-resultant component (beta-sne):
#Nestedness_resultant refers to the processes that cause a loss or gain in species without replacement (alpha diversity changes)
Y2 <- as.matrix(Y.pair$beta.sne)
dim(Y2)
view(Y2)

#Y3 <- as.matrix(Y.pair$beta.sor) this function is used to get total diversity i.e. sorensen's index.

# Questions ---------------------------------------------------------------
#1. Plot species turnover as a function of Section number, 
#and provide a mechanistic explanation for the pattern observed.

y1_plt_data <- as_tibble(Y1[1, 1:58])
view(y1_plt_data)

y1_plt <- ggplot(data = y1_plt_data, aes(x = 1:58, y = value)) +
  geom_line(aes(col = "salmnon")) + 
  labs(x = "Coastal section, west to east", y = "Species turnover",
       title = "Species turnover (Beta diversity)") +
  theme_bw() +
  theme(legend.position = "none")
y1_plt


