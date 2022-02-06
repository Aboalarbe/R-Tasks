library(readr)
library(ggpubr)
library(factoextra)
library (vegan)
library (cluster)

df_original <- read_csv("shopping_Customers.csv")

# we will remove the first 2 columns (CustomerId, Gender)
df_original<- select(df_original, -c(CustomerID, Gender))
# check missing values
sum(is.na(df_original))
View(df_original)

# Run k-means cluster on the data set
set.seed(123)
cluster_kmean <- kmeans(df_original[,1:3], 2, nstart = 20)

# plot the k-means clusters
fviz_cluster(cluster_kmean, data = df_original,
             palette = c("#2E9FDF", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

# add cluster labels to the original dataset 
df <- df_original 
df$labels <- cluster_kmean$cluster
#calculate correlation matrix
correlationMatrix <- cor(df)
View(correlationMatrix)
# determine attributes that are highly corrected (ideally > 0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.70)
hc = sort(highlyCorrelated)
hc
reduced_Data = df[ ,-c(hc)]
View (reduced_Data)
# plot heatmap
rquery.cormat(df)
cormat<-rquery.cormat(df, graphType="heatmap")
corrplot(cor(df),                       
         method = "circle",                
         type = "full",                   
         diag = TRUE,                   
         tl.col = "black",                
         bg = "white",                    
         title = "",                     
         col = NULL,                      
         tl.cex =0.7,
         cl.ratio =0.2)

# plot elbow method to determine the best k
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}

wssplot(df, nc = 10)

# Run k-means with k = 6
set.seed(123)
cluster_kmean2 <- kmeans(df_original[,1:3], 6, nstart = 20)

# plot the k-means clusters
fviz_cluster(cluster_kmean2, data = df_original,
             palette = c("#2E9FDF", "#E7B800", "#32e7c8", "#a75e11", "#134e41","#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

# Silhouette Analysis

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(df_original, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df_original))
  mean(ss[, 3])
}

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(2:15, avg_sil)

plot(2:15, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")
fviz_nbclust(df_original, kmeans, method = "silhouette")

# Apply hierarchical clustering 

# Compute distances and hierarchical clustering (Complete)
complete_dis <- dist(scale(df_original), method = "euclidean")
complete_hc <- hclust(complete_dis, method = "complete")
plot(complete_hc, hang = -1, cex = 0.6)


# Compute distances and hierarchical clustering (single)
single_dis <- dist(scale(df_original), method = "euclidean")
single_hc <- hclust(complete_dis, method = "single")
plot(single_hc, hang = -1, cex = 0.6)

