# Load necessary libraries
library(readxl)      
library(dplyr)       
library(cluster)     
library(factoextra)  

# ----------------------------
# Data Import and Preparation
# ----------------------------

# Read the dataset (Ensure the file exists in the working directory)
data <- read_excel("txt.xlsx")

# Check the structure of the data
str(data)

# Select relevant columns
attributes <- data %>% select(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style)

# Remove missing values
attributes <- na.omit(attributes)

# Standardize the data
attributes_scaled <- scale(attributes)

# -----------------------------------
# Determine Optimal Number of Clusters
# -----------------------------------

set.seed(123)  # For reproducibility

# 1️⃣ **Elbow Method Plot**
png("elbow_method.png", width = 800, height = 600)
fviz_nbclust(attributes_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = "dashed", color = "red") +
  labs(title = "Elbow Method: Optimal k", x = "Number of Clusters", y = "Total WSS") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

# 2️⃣ **Silhouette Analysis Plot**
png("silhouette_analysis.png", width = 800, height = 600)
fviz_nbclust(attributes_scaled, kmeans, method = "silhouette") +
  geom_vline(xintercept = 3, linetype = "dashed", color = "red") +
  labs(title = "Silhouette Analysis: Optimal k", x = "Number of Clusters", y = "Average Silhouette Width") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

# -----------------------------------
# Apply K-means Clustering with k = 3
# -----------------------------------
set.seed(123)
km_res <- kmeans(attributes_scaled, centers = 3, nstart = 25)

# 3️⃣ **K-Means Clustering Scatter Plot**
png("kmeans_clustering.png", width = 800, height = 600)
fviz_cluster(km_res, data = attributes_scaled, 
             ellipse.type = "norm",
             geom = "point", stand = FALSE) +
  labs(title = "K-means Clustering Results (k = 3)") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
