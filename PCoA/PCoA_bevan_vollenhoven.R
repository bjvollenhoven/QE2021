# Bevan VOllenhoven (3840572)
# Topic 10: Principal coordinate analysis (PCoA) 
# 19 July 2021

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
spe <- read.csv(url(url)) %>% 
  select(-X) %>% 
  slice(-8)
glimpse(spe)
head(spe)
dim(spe)
spe


# PCoA attempts to represent dissimilarities as euclidean distance. 
spe_bray <- vegdist(spe)

pcoa <- capscale(spe_bray ~ 1)
pcoa

summary(pcoa)
# there are no species scores as individual species are not represented in dissimilarity
# matrix. Only dissimilarity between sites is present. 

pcoa <- capscale(spe ~ 1, distance = "bray") 
pcoa
summary(pcoa)

round(sum(pcoa$CA$eig[1:3]) / sum(pcoa$CA$eig) * 100, 2)
# 77.98% of the importance of data contributing to variantion is explained in the 
# first three eigenvalues. 

par(mfrow = c(1, 2))
plot(pcoa, scaling = 1, main = "PCoA fish abundances - biplot scaling 1")
plot(pcoa, scaling = 2, main = "PCoA fish abundances - biplot scaling 2")

pl1 <- ordiplot(pcoa, type = "none", scaling = 1, main = "PCoA fish abundances - biplot scaling 1")
points(pl1, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl1, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl1, "species", col = "blue4", cex = 0.9)
text(pl1, "sites", col = "red4", cex = 0.9)

pl2 <- ordiplot(pcoa, type = "none", scaling = 2, main = "PCoA fish abundances - biplot scaling 2")
points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl2, "species", col = "blue4", cex = 0.9)
text(pl2, "sites", col = "red4", cex = 0.9)


# -------------------------------------------------------------------------

env_url <- "https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsEnv.csv"
env <- read.csv(url(env_url)) %>% 
  select(-X) %>% 
  slice(-8)

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(pcoa ~ Satr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Satr"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(pcoa ~ Scer, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Scer"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(pcoa ~ Teso, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Teso"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(pcoa ~ Cogo, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Cogo"))
abline(h = 0, v = 0, lty = 3)


(spe_pcoa_env <- envfit(pcoa, env, scaling = 2)) 
plot(spe_pcoa_env, col = "grey40")
plot(spe_pcoa_env, p.max = 0.05, col = "red")
