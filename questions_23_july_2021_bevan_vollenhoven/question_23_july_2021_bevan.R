
# Bevan Vollenhoven (3840572)
# Questions accompanying nMDS worksheet
# Due date: 23 July 2021

# Using two unconstrained ordination techniques of your choice, analyse the mite data 
# in the vegan package. 
# Provide a brief description and discussion of what you have found, and produce the R code

library(vegan)
library(dplyr)
library(tidyverse)
library(tidyr)
library(cluster)

# 1. MITES data -------------------------------
data(mite) # load in mite data (species abundance data)
head(mite)
dim(mite)

data("mite.env") # load in mite env data. 
head(mite.env)
glimpse(mite.env)
dim(mite.env)


# PCoA ----------------------------------------
options(scipen = 999) 

spe_bray <- vegdist(mite) # dissimilarity 
spe_bray

pcoa <- capscale(mite ~ 1, distance = "bray")
pcoa
summary(pcoa) 

# PCoA orgination plot ------------------------

par(mfrow = c(2, 2))
plot(pcoa, scaling = 1, main = "PCoA mite abundances - biplot scaling 1")
plot(pcoa, scaling = 2, main = "PCoA mite abundances - biplot scaling 2")
plot(pcoa, choices = c(1, 3), scaling = 1, main = "PCoA mite abundances - biplot scaling 1")
plot(pcoa, choices = c(1, 3), scaling = 2, main = "PCoA mite abundances - biplot scaling 1")

pl1 <- ordiplot(pcoa, type = "none", scaling = 1, main = "PCoA mite abundances - biplot scaling 1")
points(pl1, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl1, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl1, "species", col = "blue4", cex = 0.9)
text(pl1, "sites", col = "red4", cex = 0.9)

pl2 <- ordiplot(pcoa, type = "none", scaling = 2, main = "PCoA mite abundances - biplot scaling 2")
points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl2, "species", col = "blue4", cex = 0.9)
text(pl2, "sites", col = "red4", cex = 0.9)

plt3 <- ordiplot(pcoa, choices = c(1, 3), type = "none", scaling = 1, main = "PCoA mite abundances - biplot scaling 2")
points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl2, "species", col = "blue4", cex = 0.9)
text(pl2, "sites", col = "red4", cex = 0.9)

plt4 <- ordiplot(pcoa, choices = c(1, 3), type = "none", scaling = 2, main = "PCoA mite abundances - biplot scaling 2")
points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl2, "species", col = "blue4", cex = 0.9)
text(pl2, "sites", col = "red4", cex = 0.9)
# PCoA biplot scaling 1 shows the relationship between sites. In biplot scaling one 
# for MDS1 and MDS2, all the sites are aggregated close to the origin (0, 0) which 
# means that according to the PCoA analysis, all the sites are more or less similar 
# in species composition. This could be as a possibility that environmental conditions 
# are similar. There are sights that are slightly further from the origin than other
# and this is due to species presence that are more abundant in those sites. Species 
# LCIL is more abundant and more associated with site 59 and 43 than any other site.
# In the plot, it can be seen that this species has the most impact on site variation for 
# those sites than other species. Thus, making these sites more dissimilar than the other sites,
# hence its positioning. 
# Since the sites are aggregated close to the origin, it can tell us one of two things 
# about the species:
# species are found across all gradients or species are occupying their optimum to 
# mid-range optimum environments. 

# PCoA biplot scaling 2 for MDS1 and MDS2 shows the relationship between species. The angle between
# species indicates to their correlation. LCIL is negatively correlated with 
# SUCT, HMIN, and ONOV. This relationship indicates that a high presence of LCIL results
# in a decrease abundance of its negatively correlated species. LRUG is negatively 
# correlated with ONOV meaning in sites where LRUG is high, ONOV is low. ONOV and SUCT
# have a positive correlation with one another, indicating that they are most abundant
# in similar sites, hence the positioning of similar sites around them within the biplot. 

# PCoA biplot scaling 1 for MDS1 and MDS2 show the same relationship as the biplot 
# for MDS1 and MDS2. Except, the sites are spread out from the origin. This can be 
# as a result of MDS3 explaining less proportion of variation than MDS2. However, 
# in this biplot, we can see the effect of certain species presence on site variation more 
# clearly. Sites more attributed for LCIL, for example, are plotted further away 
# from the other sites. 

# In PCoA biplot scaling 2 for MDS1 and MDS3, the same relationship is seen as in 
# the biplot for MDS1 and MDS2. 

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(mite, tmp <- ordisurf(pcoa ~ ONOV, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "ONOV"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(pcoa ~ SUCT, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "SUCT"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(pcoa ~ LRUG, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "LRUG"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(pcoa ~ LCIL, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "LCIL"))
abline(h = 0, v = 0, lty = 3)


(spe_pcoa_env <- envfit(pcoa, mite.env, scaling = 2)) 
plot(spe_pcoa_env, col = "grey40")
plot(spe_pcoa_env, p.max = 0.05, col = "red")

# As previously mentioned, in the the biplot fore MDS1 and MDS2 which shows the most 
# proportion of variation, the sites are all more or less the similar due to similar
# species abundances. In the above highlighted species, one can see that their variation
# and abundances are spread out across most of the sites while also showing that their 
# highest abundances are seen in sites stated previously. Environmental variables 
# were added to inspect their influence of the abundances of species. It can be seen 
# that certain env. variables can be favored by certain species. This might allows
# a species to fill in more niches easier than other species, resulting in them 
# being more abundant in those sites. 

# PCA -------------------------------

head(mite.env)
env <- mite.env %>% 
  select(-Topo, -Shrub, -Substrate)

env_pca <- rda(env, scale = TRUE) # scale = TRUE is used to standardize the variables 
env_pca

summary(env_pca) #allows us to view the species scores and site scores. 

par(mfrow = c(1, 2))
source("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/cleanplot.pca.R")
cleanplot.pca(env_pca, scaling = 1) 
cleanplot.pca(env_pca, scaling = 2)

# Due to lack of numeric environmental variables, only the relationship of water content 
# and substrate density on site similarity can be seen. Therefore, interpretations 
# are difficult. In biplot scaling 1, sites found the top and bottom left quadrants 
# are more similar and they are influenced by waterfront and substrate density. In
# biplot scaling 2, water content and substrate density are fairly motiveless correlated,
# indicating that they have a relationship that can have a conjoined effect on environmental 
# gradient and sites similarity. 

# 2. Dunes data ----------------------------------------
data("dune")
head(dune)

data("dune.env")
head(dune.env)
glimpse(dune.env)

# nMDS -------------------------------------------------   
library(FD)
# NMDS does not use exact distances, it uses ranks. 
# As a result, the ranks are ordered in a low dimensions, which causes stress
# Stress influences level of confidence that we have in positioning of objects 
# and values relative to each other. 

nmds <- metaMDS(dune, distance = "bray")
nmds 
# Stress value < 0.2 (0.118). Therefore, we can have trust the accuracy and 
# fairness of the way the data is represented in the ordination plots.  
# ordination plot ---------------------------------------

par(mfrow = c(2, 2))
stressplot(nmds, main = "Shepard plot") # Stress plot (R^2 gives relation...)
ordiplot(nmds, type = "t", cex = 1.5, main = paste0("nMDS stress = ", 
                                                        round(nmds$stress, 2))) 
# shows observed dissimilarities 
# shows relation ship between species and rank. Therefore, keeping in mind the low
# stress value, 

gof = goodness(nmds)
plot(nmds, type = "t", main = "Goodness of fit") # efficiency in capturing 
points(nmds, display = "sites", cex = gof * 200) # bigger bubbles indicate a worse fit

par(mfrow = c(1, 1))
pl <- ordiplot(nmds, type = "none", main = "nMDS fish abundances ")
points(pl, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl, "species", col = "blue4", cex = 0.9)
text(pl, "sites", col = "red4", cex = 0.9)

# CA ---------------------------------------------------------------------

ca <- cca(dune)

summary(ca)

round(sum(ca$CA$eig[1:3]) / sum(ca$CA$eig) * 100, 2) 
# 56.52% of variance can be explained by the first three ordinations. 

# ordination plots -----------------
par(mfrow = c(2, 2))
plot(ca, scaling = 1, main = "Dune species abundances - biplot scaling 1")
plot(ca, scaling = 2, main = "Dune species adundances - biplot scaling 2")
plot(ca, choices = c(1, 3), scaling = 1, main = "Dune species adundances - biplot scaling 1")
plot(ca, choices = c(1, 3), scaling = 2, main = "Dune species adundances - biplot scaling 2")

# In CA biplot scaling 1 for CA2 and CA1, the relationship between sites can be explained. Sites 
# are are more closely plotted are more similar than those further away. Sites 
# 17 and 19 are more similar and are more associated with having and higher abundance 
# of species Emprenigr, Airaprae, and Hyporadi. Sites 20, and 15-16 are mosre similar
# than the other sites due to the presence of Callsup, Juncarti, Eleopalu, and Ranuman.
# The sites that are plotted closely have similar species compositions, hence making them
# more similar to each other. They could also be exposed to similar environmental conditions. 

#In biplot scaling 2 for CA1 and CA2, the relationship between species in highlighted. Emprenigr, 
# Airaprae, Anthodor, and Hyporadi are all psotively correlated. This means thatr where 
# one of these species is present, similar or higher abundances of the rest of the 
# the mentioned species can be found in those sites as well. Indicating as to how these 
# sites are similar to one another. 

# The biplots for CA1 and CA3 show similar relationships. 

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(dune, tmp <- ordisurf(ca ~ Salirepe, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Salirepe"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(ca ~ Juncarti, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Juncarti"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(ca ~ Airaprae, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Airaprae"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(ca ~ Achimill, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Achimill"))
abline(h = 0, v = 0, lty = 3)

spe_ca_env <- envfit(ca, dune.env, scaling = 2)
plot(spe_ca_env, col = "grey40")
plot(spe_ca_env, p.max = 0.05, col = "red")

#In the above highlighter species, biplots where made to observe there abundances 
# across sites and with reference to environmental conditions. The abundances of the mentioned
# species is in agreement with the CA biplots. With these plots we can see how A1 
# environmental variables influences Juncarti. Sites that do not have high values in 
# A1 have low abundances of Juncarti. 