# Bevan Vollenhoven (3840572)
# Topic 13: Cluster analysis
# 02 August 2021

library(tidyverse) 
library(cluster)
library(ggcorrplot)
library(factoextra)
library(vegan)
library(ggpubr)

SDGs <- read_csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/SDG_complete.csv")
SDGs[1:5, 1:8]

unique(SDGs$ParentLocation)

length(SDGs$Location)

# a correalation matrix
corr <- round(cor(SDGs[3:ncol(SDGs)]), 1)
ggcorrplot(corr, type = 'upper', outline.col = "white", 
           colors = c("navy", "white", "#FC4E07"), 
           lab = TRUE)

SDGs_std <- decostand(SDGs[3:ncol(SDGs)], method = "standardize")
# SDGs_euc <- vegdist(SDGs_std, method = "euclidian")
rownames(SDGs_std) <- SDGs$Location # carry location names into output

# using silhouette analysis
plt1 <- fviz_nbclust(SDGs_std, cluster::pam, method = "silhouette") + theme_grey()

# total within cluster sum of square / elbow analysis
plt2 <- fviz_nbclust(SDGs_std, cluster::pam, method = "wss") + theme_grey()

# gap statistics
plt3 <- fviz_nbclust(SDGs_std, cluster::pam, method = "gap_stat") + theme_grey()

ggarrange(plt1, plt2, plt3, nrow = 3)

# cluster 
SDGs_pam <- pam(SDGs_std, metric = "euclidean", k = 3)

fviz_cluster(SDGs_pam, geom = "point", ellipse.type = "convex", 
             palette = c("#FC4E07", "violetred3", "deepskyblue3"), ellipse.alpha = 0.05) +
  geom_text(aes(label = SDGs$Location), size = 2.5)


# scale SA bigger for plotting
SDGs <- SDGs |> 
  mutate(col_vec = ifelse(Location == "South Africa", "black", "grey50"),
         scale_vec = ifelse(Location == "South Africa", 3.5, 2.5))

fviz_cluster(SDGs_pam, geom = "point", ellipse.type = "convex",
             palette = c("#FC4E07", "violetred3", "deepskyblue3"),
             ellipse.alpha = 0.05, pointsize = 2.0) +
  geom_text(aes(label = SDGs$Location), size = SDGs$scale_vec, col = SDGs$col_vec)

#alternate cluster 
fviz_cluster(SDGs_pam, palette = c("#FC4E07", "violetred3", "deepskyblue3"), 
             ellipse.type = "euclid", 
             star.plot = TRUE, repel = TRUE, 
             pointsize = SDGs$scale_vec * 0.8) + # SA, no 147, plotted slightly bigger
  theme_grey()

# Do a silhouette analysis to check cluster fidelity:
fviz_silhouette(SDGs_pam, palette = c("#FC4E07", "violetred3", "deepskyblue3"), 
                ggtheme = theme_grey())

# median value for each cluster:

SDGs_centroids <- SDGs %>% 
  mutate(cluster = SDGs_pam$clustering) %>% 
  group_by(cluster) %>% 
  summarise_at(vars(other_1:SDG3.b_5), median, na.rm = TRUE)
SDGs_centroids

pairs(SDGs[, 3:10], col = c("#FC4E07", "violetred3", "deepskyblue3")[SDGs_pam$clustering])

# Questions --------------------

# 1. What happens if we use pam() to create four, five, or even six clusters?

# If the number of clusters is increased past 3, the pam function produces and error
# stating that their are insufficient values in the manual scale. Depending on the 
# data, there are only a maximum amount of clusters that can be grouped together. As
# a result, you cannot create more clusters than the data is able to group. pam() has
# the same objective as kmeans, with the addition of using dissimilarity, and it minimizes 
# the sum of dissimilarities instead of the sum of squared euclidean distance. 
# If you choose to have more clusters than are available, the clusters would need to 
# overlap and the relationships between the variables in the locations would not make 
# sense as a locations cannot belong to more than one cluster. 

# 2. In your reasoned opinion, what would be the optimal number of clusters to use?

# Considering the data and knowledge knows regarding the financial situations of the
# various regions, I would say having 3 clusters work best, In this way, countries can be placed
# into clusters depending on their financial status. 
# This is shown in the cluster as countries of similar financial situations are placed together
# i.e., The 1st world countries in Europe and North America are placed
# in one cluster. 

# 3. 
SDGs_km <- kmeans(SDGs_std, centers = 3)
fviz_cluster(SDGs_km, data = SDGs_std,
             geom = "text",
             ellipse.type = "confidence",)

# The results results are not remarkably different from each other. I have chosen 
# to proceed with the pam() clustering method. Not only does it have the same 
# objective as kmeans, it also allows for the use of a dissimilarity matrix as well 
# as being more robust. 

# 4. Build upon the narrative that you have already developed in the previous assignment.

# The general pattern observed in the final cluster is that countries are grouped based
# on parent locations. All African countries are grouped together as well as the 
# South American countries. The European and North American countries are all grouped 
# together as well. The pattern seen shows that countries are not only clustered based 
# on their parent countries, but also based on their financial status. The African and 
# South American countries is majorly made of developing countries. Hence, these 
# countries are grouped together and are then further separated into their respective 
# parent locations. 

# 5. South Africa is a "wealthy" African country, but its resources are being mismanaged. 
# Hence, our country might have the access to resources to properly evaluate and 
# achieve our SDG goals, yet it's being mismanaged resulting in our country being 
# subjected to third world vulnerabilities while still having a part of the population experiencing 
# first world luxuries. This why South Africa is in this limbo that is seen in the clusters. s



