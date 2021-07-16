# Bevan VOllenhoven (3840572)
# Topic 9: Correspondence analysis (CA). 
# 14 July 2021

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

url <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsSpe.csv"
doubs_sp <- read.csv(url(url)) %>% 
  select(-X)
dim(doubs_sp)
head(doubs_sp)

# Correspondence analysis (CA) function -----------
# The function cca() is used for CA and Constrained Correspondence analysis. 

sp_ca <- cca(doubs_sp) #this method is used when constraints are not specified. 
# An error occurs as there is one row of sums that = 0. 

apply(doubs_sp, 1, sum) # calculates the sum of each row in order to identify
                        # which row = 0. 
# row nr. 8 as no species recorded in it. 

sp <- doubs_sp[rowSums(doubs_sp) > 0, ] #ommits any row that has a sum = 0
dim(sp)
head(sp)

sp_ca <- cca(sp)
sp_ca

summary(sp_ca)

round(sum(sp_ca$CA$eig[1:2]) / sum(sp_ca$CA$eig) * 100, 2) # 63.97% of variance
                                                           # can be explained by
                                                           # CA1 and CA2

# ORdination diagrams --------------
par(mfrow = c(1, 2))
plot(sp_ca, scaling = 1, main = "CA fish abundances - biplot scaling 1")
# Biplot scaling 1 shows the relationships between sites. Sites 1 - 10 are 
# closely associated as they would have similar abundances in species Satr, Babl, 
# and Phph. Sites 11 - 17 are closely related as they are closer together due
# to the common presence of Teso, Cogo, and Teth. The remaining sites all contain
# the remaining species. 

plot(sp_ca, scaling = 2, main = "CA fish abundances - biplot scaling 2")
# Biplot scaling 2 shows the relationship between species. Satr, Babl, and Phph
# have a close relationships due to their shared presence in several sites (mentioned previously).
# Teso, Cogo, and Teth have a close relationship due their shared presence in sites 11 - 17.
# The rest of the species all have close relations due to their similar abundant presense in
# the remaining sites. 


require('viridis')
library(viridis)

palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(sp, tmp <- ordisurf(sp_ca ~ Satr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Satr"))
abline(h = 0, v = 0, lty = 3)
with(sp, tmp <- ordisurf(sp_ca ~ Scer, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Scer"))
abline(h = 0, v = 0, lty = 3)
with(sp, tmp <- ordisurf(sp_ca ~ Teso, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Teso"))
abline(h = 0, v = 0, lty = 3)
with(sp, tmp <- ordisurf(sp_ca ~ Cogo, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Cogo"))
abline(h = 0, v = 0, lty = 3)

env_url <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsEnv.csv"
env <- read.csv(url(env_url)) %>% 
  select(-X)
dim(env)
head(env)

# we removed the 8th row in spe, so do it here too
env <- slice(env, -8)
dim(env)
head(env)

# the last plot produced (CA scaling 2) must be active
# scaling 2 is default
(sp_ca_env <- envfit(sp_ca, env, scaling = 2))
plot(sp_ca_env, col = "grey40")
plot(sp_ca_env, p.max = 0.05, col = "red") # plot significant variables with a different colour

# Questions ------------------
# 1. 
# The above figure shows where the isolated species are most abundant. Satr is mmost abundant
# near the start of the river (sites 1-18). They still occur at intermediate sites but 
# their abundance far less. The environmental data shows that variables most correlated
# with their presence is high altitude (alt), slope (slo), dissolved oxygen (oxy), and pH of water. 
# These environmental variables are negatively correlated with distance from source (dfs)
# mean minimum discharge (flo), hardness (har), phosphate concentration (pho), nitrate conc. (nit),
# ammonium conc. (amm), and biological oxygen demand (bod). Where these variables are, abundances
# of Satr are low, thus showing how the species abundance are possibly correlated and influenced 
# by the environmental variables measured. The contour lines connecting abundances show where
# similar abundances occur, providing evidence that species in these sites share similar
# environmental conditions. 

# Scer is most abundant in the sites furthest from the river start (sites 19 - 23).
# At these sites dfs, flo, har, pho, amm, and bod are the highest environmental variables
# contributing the most to variation. Site where the species are least abundant occur
# where environmental conditions are the negatively correlated env. variables (oxy,
# slo, alt, and pH) are higher. 

# Teso is most abundant in sites 13 - 18 where oxy and pH are most responsible for 
# variance in sites. Their abundances are lower in all other sites as environmental 
# conditions are possibly not favored by the species and them favoring an oligotrophic 
# environment. 

# Cogo is most abundant in sites 11-18. They share similar environmental preferences as
# Teso but occur at a wider range of sites. While they prefer oligotropic environments (higher
# pH levels and oxy), while also favoring areas in more higher altitudes with steeper slopes.  



# Birds ------------------------------------------------------

url_birds_env <- "https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_env.txt"
birds_env <- read.delim(url(url_birds_env)) %>% 
  select(-`Veg.`, -Veg_ext, -Station)
head(birds_env)

url_birds_sp <- "https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_spe.txt"
birds_sp <- read.delim(url(url_birds_sp)) %>% 
  select(-Station)
head(birds_sp)
dim(birds_sp)

birds_ca <- cca(birds_sp)
birds_ca

summary(birds_ca)

round(sum(birds_ca$CA$eig[1:5]) / sum(birds_ca$CA$eig) * 100, 2) # 71.35% of variance
# can be explained by
# CA1 to CA5

par(mfrow = c(1, 2))

plot(birds_ca, scaling = 1, main = "CA birds abundances - biplot scaling 1")
# In biplot scaling 1, sites that have more species in common are aggregated 
# closely together. Sites 1-23 have many species in common thus they are so condensed.
# Sites 45-50 are seperated from majority of the sites by their shared presence so species
# ALA, WRN, VRF, FLT, JBR. 
plot(birds_ca, scaling = 2, main = "CA birds abundances - biplot scaling 2")
# In biplot scaling 2, species relationships are shown. Species ALA, WRN, JBR, FLT,
# and ALA are more closely plotted and seen as having a stronger relationship than with 
# other species. This can be due to the fact as these sites occurred in a partially isolated
# section of the mountain ranges. 

palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(birds_sp, tmp <- ordisurf(birds_ca ~ ALA, bubble = 3,
                               family = quasipoisson, knots = 2, col = 6,
                               display = "sites", main = "ALA"))
abline(h = 0, v = 0, lty = 3)
with(birds_sp, tmp <- ordisurf(birds_ca ~ WRN, bubble = 3,
                               family = quasipoisson, knots = 2, col = 6,
                               display = "sites", main = "WRN"))
abline(h = 0, v = 0, lty = 3)
with(birds_sp, tmp <- ordisurf(birds_ca ~ BRD, bubble = 3,
                               family = quasipoisson, knots = 2, col = 6,
                               display = "sites", main = "BRD"))
abline(h = 0, v = 0, lty = 3)
with(birds_sp, tmp <- ordisurf(birds_ca ~ BLB, bubble = 3,
                               family = quasipoisson, knots = 2, col = 6,
                               display = "sites", main = "BLB"))
abline(h = 0, v = 0, lty = 3)

(birds_env_ca <- envfit(birds_ca, birds_env, scaling = 2))
plot(birds_env_ca, col = "grey40")
plot(birds_env_ca, p.max = 0.05, col = "red") 

#3.1 
# Species ALA, which had a strong influence on variation in CA 1, was most abundant 
# in sites 45 to 50. This is in agreement with the strong relationship that these
# sites have with their species absurdness and species composition. These 
# species prefer environmental conditions with high value in ground cover and low 
# tree density, total foliage volume, herb cover, and aspect. 

# Species WRN had its highest abundance in sites 37-50. This species's abundance 
# is linked to ground cover, exposure, slope, conifer percentage, elevation, and mean tree
# DBH. Higher values in these environmental variables positively influenced abundances. 

# Species BRD was most abundant in ~sites 1-17. The environmental variables positively influencing 
# their abundance would the high values in tree density, total foliage volume, herb cover,
# and aspect. 

# Species BLB was most abundant in the first few sites, which can be associated 
# with high values of tree density, shrub cover, total foliage volume, herb cover, 
# and aspect. 

# ALA and WRN are the highest negative eigenvectors affecting variance in the correlation
# analysis. These two species are also subjected to similar environmental variables of 
# similar intensities. The same can be said with BLB and BRD as they hold the two 
# highest positive eigenvectors. Except, these species favors conditions that are 
# negatively correlated with the first two mentioned species. This shows a change in the
# landscape from tree dominated to possibly low rising plant dominated. 


# Alpine plant community --------------------------------------------------


alpine_env <- read.csv("data/aravo_env.csv") %>% 
  select(-X, -ZoogD)
head(alpine_env)

alpine_data <- read.csv("data/aravo_sp.csv") %>% 
  select(-X) # the variables are descriptive and not numeric. 
dim(alpine_data)
head(alpine_data)

alpine_ca <- cca(alpine_data)
alpine_ca
summary(alpine_ca)

round(sum(alpine_ca$CA$eig[1:15]) / sum(alpine_ca$CA$eig) * 100, 2) # 76.87% of variance
# can be explained by
# CA1 to CA15

par(mfrow = c(1, 2))

plot(alpine_ca, scaling = 1, main = "CA alpine species abundances - biplot scaling 1")
# Due to the overcrowding of points interpreting the data is difficult. However, in biplot
# scaling 1, one can clearly see how the relationships between sites (black points) is influenced
# by species abundance and presence (red points). Sites that are more closely related will
# be plotted close together with their common species plotted close by. 
plot(alpine_ca, scaling = 2, main = "CA alpine species abundances - biplot scaling 2")
# In s=biplot scaling 2, species that are plotted close together show closer relationships 
# as they are found in similar sites, alluding to similar environmental conditions. 


palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(alpine_data, tmp <- ordisurf(alpine_ca ~ Bart.alpi, bubble = 3,
                                  family = quasipoisson, knots = 2, col = 6,
                                  display = "sites", main = "Bart.alpi"))
abline(h = 0, v = 0, lty = 3)


with(alpine_data, tmp <- ordisurf(alpine_ca ~ Sali.retu, bubble = 3,
                                  family = quasipoisson, knots = 2, col = 6,
                                  display = "sites", main = "Sali.retu"))
abline(h = 0, v = 0, lty = 3)

with(alpine_data, tmp <- ordisurf(alpine_ca ~ Alch.pent, bubble = 3,
                                  family = quasipoisson, knots = 2, col = 6,
                                  display = "sites", main = "Alch.pent"))
abline(h = 0, v = 0, lty = 3)

with(alpine_data, tmp <- ordisurf(alpine_ca ~ Poa.supi, bubble = 3,
                                  family = quasipoisson, knots = 2, col = 6,
                                  display = "sites", main = "Poa.supi"))
abline(h = 0, v = 0, lty = 3)

alpine_env_ca <- envfit(alpine_ca, alpine_env, scaling = 2)
plot(alpine_env_ca, col = "grey40")
plot(alpine_env_ca, p.max = 0.05, col = "red")

# 3.2
# Alch.pent and Poa.supi are the top two negative eigenvectors with regards to
# species scores contributing to variance in the correlation analysis. They 
# are most abundant in similar sites with Alch.pent having more abundance in a wider 
# range of sites. Their abundance has a positive correlation with more concave landscapes 
# and higher mean snow melt. They are negatively correlated with areas of steeper slopes and
# more physical disturbance not due to vegetation. The inverse can be said about the 
# top two positive eigenvector holders (Sali.retu and Bart.alpi). These species have a 
# higher abundance in environmental conditions that are more steeper and have more physical disturbances
# occurring. 

# The pattern that can be seen is that of a change in environmental conditions dependent on
# the side on which the site is found on the mountain range. As different sides are subjected 
# to different external forces. 
