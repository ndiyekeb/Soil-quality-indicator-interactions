# Unsupervised self organizing maps ------------------------------------
# Set working directory
setwd("E:/21_SeOMap_Bots")

# clean memory ---------------------------------------------------------
gc()
rm(list=ls()) 

# Load libraries -------------------------------------------------------
library(kohonen)
library(aweSOM)
library(viridis)
library(ggplot2)
library(factoextra)
library(broom)

# Data -----------------------------------------------------------------
data <- read.csv('All_data.csv', header = TRUE, sep = ',')
str(data)
head(data)

# Store the data in X and normalize it using scale --------------------- 
X <- scale(data[, 3:14])
summary(X)

# Self Organizing Map --------------------------------------------------
set.seed(222)
map <- som(X,
           grid = kohonen::somgrid(xdim = 4, ydim = 7, topo = 'hexagonal'),
           rlen = 100, 
           alpha = c(0.05, 0.01), 
           radius = 1, 
           dist.fcts = "sumofsquares")

# Map quality assessment -----------------------------------------------
somQuality(map, X)

# Map customization ---------------------------------------------------- 
plot(map, type = 'changes', lwd = 4, col = 'darkred')
plot(map, shape = 'straight')
plot(map, 
     type = 'codes',
     shape = 'straight',
     palette.name = inferno)

# Map plots for different variables ------------------------------------

par(mfrow=c(3,4)) # Plot all SOMs in one plot

# K normalized
K <- plot(map, type = 'property', shape = 'straight', 
              property = getCodes(map)[,1], 
              main = colnames(getCodes(map))[1], palette.name = inferno)

# Mn normalized
Mn <- plot(map, type = 'property', shape = 'straight',
              property = getCodes(map)[,2], 
              main = colnames(getCodes(map))[2], palette.name = inferno)

# Fe normalized
Fe <- plot(map, type = 'property', shape = 'straight',
              property = getCodes(map)[,3], 
              main = colnames(getCodes(map))[3], palette.name = inferno)

# Zn normalized
Zn <- plot(map, type = 'property', shape = 'straight',
              property = getCodes(map)[,4], 
              main = colnames(getCodes(map))[4], palette.name = inferno)

# Sand normalized
Sand <- plot(map, type = 'property', shape = 'straight',
              property = getCodes(map)[,5], 
              main = colnames(getCodes(map))[5], palette.name = inferno)

# Silt normalized
Silt <- plot(map, type = 'property', shape = 'straight',
               property = getCodes(map)[,6], 
               main = colnames(getCodes(map))[6], palette.name = inferno)

# Clay normalized
Clay <- plot(map, type = 'property', shape = 'straight',
             property = getCodes(map)[,7], 
             main = colnames(getCodes(map))[7], palette.name = inferno)

# P normalized
P <- plot(map, type = 'property', shape = 'straight',
             property = getCodes(map)[,8], 
             main = colnames(getCodes(map))[8], palette.name = inferno)

# EC normalized
EC <- plot(map, type = 'property', shape = 'straight',
             property = getCodes(map)[,9], 
             main = colnames(getCodes(map))[9], palette.name = inferno)

# SOM normalized
SOM <- plot(map, type = 'property', shape = 'straight',
             property = getCodes(map)[,10], 
             main = colnames(getCodes(map))[10], palette.name = inferno)

# CEC normalized
CEC <- plot(map, type = 'property', shape = 'straight',
             property = getCodes(map)[,11], 
             main = colnames(getCodes(map))[11], palette.name = inferno)

# pH_CaCl normalized
pH_CaCl <- plot(map, type = 'property', shape = 'straight',
             property = getCodes(map)[,12], 
             main = colnames(getCodes(map))[12], palette.name = inferno)

# -----------------------------------------------------------------------
# Determining the number of clusters by k-means ------------------------- 
# -----------------------------------------------------------------------

dev.off() # Resetting the combined plot function set above

# silhouette method for finding the optimal number of clusters ----------
set.seed(123)
fviz_nbclust(X, kmeans, method = "silhouette")

# Color coding for individual clusters (based on number of clusters) ----
palette <- c('gold', 'chartreuse3')

# Visualizing cluster results
# use hierarchical clustering to cluster the codebook vectors
map_cluster <- cutree(hclust(dist(getCodes(map))), 2) # No. of clusters obtained from silhouette clustering at the end of code

# plot these results (showing actual sample points) -------------------- 
set.seed(109)
plot(map, 
     type = 'mapping', 
     shape = 'straight', 
     pch = 20, 
     bgcol = palette[map_cluster], 
     main = "Clusters") 
add.cluster.boundaries(map, map_cluster)

# plot these results (showing actual sampleIDs) ------------------------
set.seed(109)
plot(map, 
     type = 'mapping', 
     shape = 'straight', 
     labels = data$Pedon_sample,
     font = 1,
     cex = 0.7,
     bgcol = palette[map_cluster], 
     main = 'Clusters') 
add.cluster.boundaries(map, map_cluster)

legend('right', 
       title = 'Cluster',
       c('1', '2'), 
       fill = palette, 
       horiz = FALSE, 
       cex = 1)

rm(list=ls())
#----------------------------------End----------------------------------
