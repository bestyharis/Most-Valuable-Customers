

getwd()
setwd("D:/Simplilearn/SimpliR")

library(tidyverse)
ec <- read_csv("Ecommerce.csv")
ec


head(ec)
summary(ec)



# Wrangle the data

## Create TotalPrice column
## Change CustomerID into string
ec <- ec %>% mutate(TotalPrice = Quantity * UnitPrice)
ec$CustomerID <- as.character(ec$CustomerID)
ec


## Remove obs whose CustomerID are NA
na_cust <- ec %>% filter(is.na(CustomerID))
ec <- anti_join(ec, na_cust)



# Create a new df
ec_sales <- ec %>% filter(TotalPrice > 0) %>% group_by(CustomerID) %>%
  summarize(Sales=sum(TotalPrice)) %>% arrange(CustomerID)
ec



# K-Means Clustering
KMC <- kmeans(as.matrix(ec_sales), centers = 4, iter.max = 1000)
KC1 <- subset(ec_sales, KMC$cluster==1)
mean(KC1$Sales)
KC2 <- subset(ec_sales, KMC$cluster==2)
mean(KC2$Sales)
KC3 <- subset(ec_sales, KMC$cluster==3)
mean(KC3$Sales)
KC4 <- subset(ec_sales, KMC$cluster==4)
mean(KC4$Sales)





# Hierarchical Clustering
distance <- dist(ec_sales, method='euclidean')
clusterIntensity <- hclust(distance, method="ward.D")
plot(clusterIntensity)



# Select 4 clusters
salesClusters <- cutree(clusterIntensity, k = 4)
table(salesClusters)
tapply(ec_sales$Sales, salesClusters, mean)


