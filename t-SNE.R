# t-SNE-----------------------------------------------------------------
# Set working directory
setwd("E:/21_SeOMap_Bots")

# clean memory ---------------------------------------------------------
gc()
rm(list=ls()) 

# Load libraries -------------------------------------------------------
library(Rtsne)

# Data -----------------------------------------------------------------
data <- read.csv('All_data.csv', header = TRUE, sep = ',')
str(data)
head(data)

## Split data into two objects: 1) containing measurements 2) containing species type
data_data <- data[, 3:14] # We are sub-setting data object such as to include 'all rows' and columns 3 to 14.
data_profile <- data[, 2] # We are sub-setting data object such as to include 'all rows' and column 2.

## t-SNE algorithm run-------------------------------------------------- 
set.seed(1267)
tsne_results <- Rtsne(data_data, 
                      perplexity = 10, 
                      check_duplicates = TRUE, 
                      pca = TRUE, 
                      max_iter = 1000,
                      theta = 0.0,
                      verbose = TRUE) # Perplexities can be altered as per the data (minimum = 5) 

# t-SNE results plot----------------------------------------------------
plot(tsne_results$Y, 
     col = "black", 
     bg= data_profile,
     xlim = c(-45,30),
     ylim = c(-60,60),
     pch = 21, 
     cex = 2) # Second plot: Color the plot by the profile type

# Add text--------------------------------------------------------------
text(tsne_results$Y, 
     labels = data$Pedon_sample,
     cex = 0.7, 
     font = 1, 
     pos = 1)

# Add legend------------------------------------------------------------
legend(-47, -5,
       c("Pedon 1", "Pedon 2", "Pedon 3", 
         "Pedon 4", "Pedon 5","Pedon 6",
         "Pedon 7", "Pedon 8"),
       fill=c("black",
              "red",
              "green3",
              "blue",
              "cyan",
              "magenta",
              "yellow",
              "gray"), 
       cex=1,
       box.lty=0)

rm(list=ls())
#-----------------------------------End----------------------------------