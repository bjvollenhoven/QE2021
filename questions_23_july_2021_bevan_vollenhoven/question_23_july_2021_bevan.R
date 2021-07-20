
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


# CA -------------------------------

ca <- cca(mite)
ca
summary(ca)

round(sum(ca$CA$eig[1:3]) / sum(ca$CA$eig) * 100, 2) 
# 54.62% of variance can be explained by the first three ordinations. 

# ordination plots -----------------
par(mfrow = c(2, 2))
plot(ca, scaling = 1, main = "Mite abundances - biplot scaling 1")
plot(ca, scaling = 2, main = "Mite adundances - biplot scaling 2")
plot(ca, choices = c(1, 2), scaling = 1, main = "Mite adundances - biplot scaling 1")
plot(ca, choices = c(1, 2), scaling = 2, main = "Mite adundances - biplot scaling 2")


palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(mite, tmp <- ordisurf(pcoa ~ Trimalc2, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Trimalc2"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(pcoa ~ LCIL, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "LCIL"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(pcoa ~ Miniglmn, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Miniglmn"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(pcoa ~ RARD, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "RARD"))
abline(h = 0, v = 0, lty = 3)


(spe_pcoa_env <- envfit(pcoa, mite.env, scaling = 2)) 
plot(spe_pcoa_env, col = "grey40")
plot(spe_pcoa_env, p.max = 0.05, col = "red")

# 2. Dunes data ----------------------------------------
data("dune")
head(dune)

data("dune.env")
head(dune.env)
glimpse(dune.env)

# nMDS -------------------------------------------------   
library(FD)
dune_d <- gowdis(dune)

nmds <- metaMDS(dune_d, metric = "gower")

# ordination plot ---------------------------------------

par(mfrow = c(2, 2))
stressplot(nmds, main = "Shepard plot") # Stress plot (R^2 gives relation...)
ordiplot(nmds, type = "t", cex = 1.5, main = paste0("nMDS stress = ", 
                                                        round(nmds$stress, 2))) # Interpet as you would with CA
gof = goodness(spe_nmds)
plot(nmds, type = "t", main = "Goodness of fit") # efficiency in capturing 
points(nmds, display = "sites", cex = gof * 200) # bigger bubbles indicate a worse fit


pl <- ordiplot(nmds, type = "none", main = "nMDS fish abundances ")
points(pl, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl, "species", col = "blue4", cex = 0.9)
text(pl, "sites", col = "red4", cex = 0.9)



# PCA ---------------------------------------------------------------------



