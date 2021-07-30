
# Bevan Vollenhoven 
# Doubs Assignment 
# Due date: 30 July 2021

library(vegan)
library(tidyverse)
library(ggplot2)
library(betapart)
library(gridExtra)
library(grid)
library(gridBase)
library(tidyr)
library(dplyr)
library(cluster)
library(ggcorrplot)
library(factoextra)
library(ggpubr)


# PCoA -------------------------------------------------------------------------
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
# 77.98% of the importance of data contributing to variation is explained in the 
# first three eigenvalues. 

par(mfrow = c(2, 2))
plot(pcoa, scaling = 1, main = "PCoA fish abundances - biplot scaling 1")
plot(pcoa, scaling = 2, main = "PCoA fish abundances - biplot scaling 2")
plot(pcoa, scaling = 1, choices = c(1, 3), main = "PCoA fish abundances - biplot scaling 1")
plot(pcoa, scaling = 2, choices = c(1, 3), main = "PCoA fish abundances - biplot scaling 1")

# plot can be adjusted to allow for easier interpretations 
pl1 <- ordiplot(pcoa, type = "none", scaling = 1, main = "A. PCoA fish abundances - biplot scaling 1")
points(pl1, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl1, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl1, "species", col = "blue4", cex = 0.9)
text(pl1, "sites", col = "red4", cex = 0.9)

pl2 <- ordiplot(pcoa, type = "none", scaling = 2, main = "B. PCoA fish abundances - biplot scaling 2")
points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl2, "species", col = "blue4", cex = 0.9)
text(pl2, "sites", col = "red4", cex = 0.9)

pl3 <- ordiplot(pcoa, scaling = 1, choices = c(1, 3), main = "C. PCoA fish abundances - biplot scaling 1")
points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl2, "species", col = "blue4", cex = 0.9)
text(pl2, "sites", col = "red4", cex = 0.9)

pl4 <- ordiplot(pcoa, scaling = 2, choices = c(1, 3), main = "D. PCoA fish abundances - biplot scaling 1")
points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl2, "species", col = "blue4", cex = 0.9)
text(pl2, "sites", col = "red4", cex = 0.9)


# data interpretation
# Sites ordinated closer to one another in the PCoA biplot are considered more similar 
# than sites ordinated further away. 
# In PCoA biplot scaling 1 for MDS1 and MDS2, relationships between sites are emphasized. Sites ordinated 
# on the left side of the plot are more similar due to their similar species composition.
# The main species contributing to their similarity is Satr, Phph, and Babl. 
# Ththm Teso, and Cogo are also contributed to the similarity between these sites, 
# however they are not as "powerful" of an influence on the species composition. 
# The sites found the right are more similar to to their shared species composition, which 
# is made up of the remaining species. Notably, sites 22-24 are more similar than the other 
# sites ordinated to the right. In the PCoA biplot scaling 2 for MDS1 and MDS2, we
# can see the relationship between species, and the effect it has on site dissimilarity. 
# Satr, Phph Babl, Ththm Teso, and Cogo are all positively correlated with one another
# than all other species. As a result, species ordinated in the left quadrants on the 
# biplot have more abundances of the these species. All the sites to the right of the 
# ordination have low abundances and are negatively correlated with these species. 
# The rest of the species, are more negatively correlated with these species, signifying
# that they have lower abundances in the sites ordinated to the right. The sites 
# ordinated to the right are positively correlated with the remainder of the species 
# and are positively correlated with those species. Sites 22 - 23 however, are 
# are not strongly correlated with the species plotted to the right. These sites are
# most strongly positively correlated to species Alal. 

# The following subset plots were made in order to conduct and indirect analysis
# on the underlying environmental effect on species distribution and site dissimilarity. 
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
with(spe, tmp <- ordisurf(pcoa ~ Alal, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Alal"))
abline(h = 0, v = 0, lty = 3)


(spe_pcoa_env <- envfit(pcoa, env, scaling = 2)) 
plot(spe_pcoa_env, col = "grey40")
plot(spe_pcoa_env, p.max = 0.05, col = "red")
 
# data interpretation
# In the four subsets of data, Satr, Alal, Satr, and Teso abundance were analysed. 
# In the subset data, the circles represent the level of abundance (larger circle = 
# higher abundance). The green curves on the plots show areas with similar abundances
# (i.e. all points along the curve has similar abundances). Sites 22-24 have a large 
# abundance of Alal, Alal is most abundant in both sites 22-24 and the in some of the 
# remaining sites ordinated to the right. The environmental conditions these sites are 
# influenced by is high bod, amm, nit, and pho levels. Satr, on the other hand, is most 
# abundant in sites found the the top-left quadrant. These sites are exposed to high 
# levels of oxy, alt, and pH (howver pH is does not have a prominent significance on
# the abundance of Satr in those sites). In these plots, we can see a gradient resulting
# in a change from an oligtrophic environment to a eutrophic environment. 


# db-RDA -----------------------------------------------------------------------

spp <- read.csv(url("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsSpe.csv")) %>% 
  select(-1) %>% # first column is not needed
  slice(-8) # this row is empty
head(spp)
dim(spp)
glimpse(spp)

# data set up

spp_bray <- vegdist(spp, diag = TRUE) #calculate the bray-Curtis dissimilarity for abundance data. 
spp_bray

# load in the env. data.
env <- read.csv(url("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsEnv.csv")) %>% 
  select(-X) %>% 
  slice(-8) # must remove the same row that was removed in the species data. 
dim(env)
head(env)

E1 <- decostand(env, method = "standardize") # standardize the variables as they have differing units of measurement. 

# start the db-RDA

rda_full <- capscale(spp_bray~., E1, distance = "bray")
summary(rda_full)

anova(rda_full, parallel = 4)
# the fit is significant. 
round(rda_full_R2 <- RsquareAdj(rda_full)$adj.r.squared, 2)


# The variation explained by the full set environmental variables
round(sum(rda_full$CCA$eig) / rda_full$tot.chi * 100, 2) 

vif.cca(rda_full) # check co-linearity of all env variables. 

E2 <- E1 %>% select(-dfs)
rda_sel1 <- capscale(spp_bray~., E2, distance = "bray")
vif.cca(rda_sel1)

E3 <- E2 %>% select(-amm)
rda_sel2 <- capscale(spp_bray ~., E3, distance = "bray")
vif.cca(rda_sel2)

E4 <- E3 %>% select(-pho)
rda_sel3 <- capscale(spp_bray~., E4, distance = "bray")
vif.cca(rda_sel3)

rda_final <- rda_sel3

# significance of the model (the variance explained by all the constraints present in the final model)

anova(rda_final, parallel = 4)
# The it is significant

# In order to find which axis have significance, and must be plotted, the coding can be adjusted with one argument.
anova(rda_final, by = "axis", parallel = 4)
# With the results produced, it can be concluded that CAP1-3 are significant. 

# The same must be done regarding the env variables in E5. 
rda_final_axis <- anova(rda_final, by = "terms", parallel = 4)
rda_final_axis <- which(rda_final_axis[, 4] < 0.05)
rda_final_sign_ax <- colnames(E4[,rda_final_axis])
rda_final_sign_ax
# the significant env variables are "slo", "flo", "pho", "nit", and "oxy". 

# The adjusted $R^{2}$ for the constraints:
round(rda_final_R2 <- RsquareAdj(rda_final)$adj.r.squared, 2)

# variance explained by the final model 
round(sum(rda_final$CCA$eig) / rda_final$tot.chi * 100, 2)
# 74.48% of the variances is explained. 

# ordination plots ---------------------

# use scaling = 1 or scaling = 2 for site and species scaling, respectively
rda_final_scrs <- scores(rda_final, display = c("sp", "wa", "lc", "bp"))

# below I splot the wa (site) scores rather than lc (constraints) scores
site_scores <- data.frame(rda_final_scrs$site) # the wa scores
site_scores$section <- seq(1:29)

biplot_scores <- data.frame(rda_final_scrs$biplot)
biplot_scores$labels <- rownames(biplot_scores)
biplot_scores_sign <- biplot_scores[biplot_scores$labels %in% rda_final_sign_ax,]

par(mfrow = c(1, 1))
ggplot(data = site_scores, aes(x = CAP1, y = CAP2, colour = section)) +
  geom_point(size = 5.0, shape = 21, fill = "white") +
  geom_text(aes(label = section), size = 3.0, col = "black") +
  geom_label(data = biplot_scores_sign,
             aes(CAP1, CAP2, label = rownames(biplot_scores_sign)),
             color = "black") +
  geom_segment(data = biplot_scores_sign,
               aes(x = 0, y = 0, xend = CAP1, yend = CAP2),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               color = "lightseagreen", alpha = 1, size = 0.7) +
  xlab("CAP1") + ylab("CAP2") +
  ggtitle(expression(paste("db-RDA for the Doubs River species data"))) +
  theme_grey() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        aspect.ratio = 0.8)

# Data interpretations 

# The db-RDA is an extension of PCoA, but with multiple regressions. This is an 
# an unconstrained ordination that is able to run a "direct gradient analysis". 
# Constrained ordinations extract and summarize variation found within a set 
# of species data explained by constraints induced by environmental properties present. 
# db-RDAn allows for the analyzing of non-euclidean distance such as Bray-Curtis 
# distance. We are also able to model species distribution data as a function of 
# underlying environmental data. 

# Sites ordinated to the left are characterized by having high slop and oxy.While
# sites ordinated to the right are characterized by having high flo and nit. We
# are able to see the gradient from a oligotrophic to a eutrophic environment 
# as we move further down the river where flo increases and slope decreases. 


# Cluster analysis -------------------------------------------------------------

spp <- read.csv(url("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsSpe.csv")) %>% 
  select(-1) %>% # first column is not needed
 slice(-8) # this row is empty
head(spp)
dim(spp)
glimpse(spp)


# Single Linkage Agglomerative Clustering
# agglomerates objects on the basis of their shortest pairwise distances (or greatest similarities)
# In order for two groups to be connected on a similar dissimilarity level, only 
# one of the objects in the two groups need to be linked in order to aggregate
# the groups at the same level. 

# This common hierarchical method allows gradients to be interpreted clearly, but causes difficulty
# in the interpretations of partitions. 

par(mfrow = c(1,2))
spp.norm <- decostand(spp, method = "normalize")
spp.ch <- vegdist(spp, distance = "euc")

spp.ch.single <- hclust(spp.ch, method = "single") # single linkage clustering
plot(spp.ch.single, main = "A. Single linkage cluster")


# The "height" of the vertical lines represent the distance between clusters. In this 
# dendrogram, we are able to view the presence of a gradient from site 1 to the last 
# site. The clustering of sites is similar the to ordination of sites in previous 
# ordination plots. Sites 1 -15 from one cluster (with exception to discontinuities).
# The sites become progressively more different until they are all groupes together 
# with being equally different to the second cluster (sites 16 -25). Sites 22-25 
# form a cluster on their. This is in agreement with previous ordinations as these 
# sights were all significant differently ordinated from the other sites. 

spp.ch.complete <- hclust(spp.ch, method = "complete")
plot(spp.ch.complete, main = "B. Complete linkage cluster")
# Complete linkage clustering only forms groups with objects that has a distance
# corresponding to the furthest object in the group. 

# K-means clustering
corr <- round(cor(env), 1)
ggcorrplot(corr, type = 'upper', outline.col = "white",
           colors = c("#1679a1", "white", "#f8766d"),
           lab = TRUE)

# Compute the number of clusters 
fviz_nbclust(E4, FUNcluster, method = c("silhouette", "wss", "gap_stat"))

library(NbClust)
library(FunCluster)

# Elbow method
fviz_nbclust(E4, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(E4, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(E4, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")

# According to the above tests, 4 clusters are needed. 

E4_euc <- vegdist(E4, method = "euclidean")

E4_km <- kmeans(E4, centers = 4)

par(mfrow = c(1, 1))
fviz_cluster(E4_km, data = E4,
             geom = "text",
             ellipse.type = "confidence",)



