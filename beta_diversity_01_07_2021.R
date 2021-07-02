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

true_beta <- data.frame(beta = ncol(spp) / specnumber(spp, MARGIN = 1)  , # dividing the number of species per row by the total number of columns present.
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
#Overall beta diversity (sorensen index) must be decomposed into nestedness-resultant and
#species turnover in order to understand the processes that underlie beta diversity. 
#library(betapart) this package is needed to perform the following functions. 

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

#The graph shows increasing dissimilarity between sites as a result of species 
#turnover. As distance between sites increase so does their dissimilarity. 
#The sites are relatively close and similarity between sites decrease progressively
#due to the presence of a linear temperature gradient (Smit et al., 2017). 
#The species composition change process in taken into account independent
#of species richness.



#2. Based on an assessment of literature on the topic, provide a discussion of 
#nestedness-resultant β-diversity. Use either a marine or terrestrial example to 
#explain this mode of structuring biodiversity. 

#A high nestedness resultant beta diversity indicates that the mechanism of change 
#in diversity between sites is based non-gradient ecological processes. These
#would include ecological factors such as dispersal limitations, or species interactions, 
#as ass posed to environmental gradients (temperature, salinity). A research paper done by 
#Baselga (2010) set out how one would deconstruct over all beta diversity in order
#to properly analyse underlying mechanism driving diversity. In this paper, a clear 
#difference in mechanisms between nestedness-resultant beta diversity and species turnover
#was explained. While species turnover was driven by the presence of an environmental gradient,
#nestedness resultant indicated to a non-gradient influence on diversity. Here, alternative
#ecological explanations are needed (such as dispersal limitations) to explain changes 
#in diversity. 

#An example of this difference would be observed with the levels and changes in seaweed diversity along the South African
#coast. Beta diversity of seaweed species along the South African coast was studies 
#by Smit et al. in 2017. In the study, high nestedness resultant was observed along 
#the western coast region as apposed to the high turnover influenced beta diversity 
#of the east coast. A high species nestedness resultant indicates to a lack of 
#environmental gradient influence on diversity and favors alternative ecological 
#influences on diversity.

#Beta diversity would normally be dominated by species turnover but the west coast 
#is an exception. This can be as consequence of the Benguela current present on the
#west coast and the Agulhas current on the east. It was suggested that the reasoning 
#behind the nestedness dominated beta diversity on the west coast was as a result of
#a possible historic event that established a barrier, limiting connectivity between 
#the Agulhas and Benguela current. 

#Through this example, one can see how important it is to know the
#underlying mechanistic effects on beta diversity. In this way, a deeper understanding 
#of the functionality of beta diversity was highlighted and can be applied and studied
#in other ecological spaces. 








