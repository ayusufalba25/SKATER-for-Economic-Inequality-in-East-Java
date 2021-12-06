# install.packages(c("ape", "rgdal", "spdep", "tidyverse", "readxl"))
# Import libraries
library(ape) # Moran.I
library(rgdal) # Read shp file
library(spdep) # Spatial weight and SKATER
library(tidyverse) # Data wrangling
library(readxl) # Read data

# Latitude: Y
# Longitude: X
# Jatim (Jawa Timur): East Java

# Read the shp file
jatim_map <- readOGR(dsn = "Data/SHP", layer = "35jatim")
longlat <- coordinates(jatim_map)

# Create weight matrix
weight <- as.matrix(dist(cbind(longlat[,1], longlat[,2])))
weight <- 1 / weight
weight[is.infinite(weight)] <- 0

# Check for the number of infinite values
sum(is.infinite(weight))

# Read the data
jatim_data <- read_xlsx("Data/Economic Inequality.xlsx")
View(jatim_data)

# Calculate moran I test
morGin <- Moran.I(jatim_data$GINI, weight)
morGin
(morGin$observed - morGin$expected) / morGin$sd # Z statistic

morL <- Moran.I(jatim_data$L, weight)
morL
(morL$observed - morL$expected) / morL$sd # Z statistic

morTheil <- Moran.I(jatim_data$THEIL, weight)
morTheil
(morTheil$observed - morTheil$expected) / morTheil$sd # Z statistic

# Standardizing
jatim_std <- jatim_data %>% 
  select(GINI, L, THEIL) %>% 
  scale()
jatim_std

# Plot the point coordinates
plot(longlat[,1], longlat[,2])

# Create neighbours with K nearest neighbours
jatim_nb <- knearneigh(x = longlat, k = 4, longlat = TRUE) %>% 
  knn2nb()
summary(jatim_nb)

# Plot point coordinates for the neighbours
plot(jatim_map, border = gray(.5))
plot(jatim_nb, longlat, col = "blue", add = T)

# Edge costs
lcosts <- nbcosts(jatim_nb, jatim_std)
head(lcosts)

# Convert the neighbor list to a list weight object
jatim_w <- nb2listw(jatim_nb, lcosts, style = "B")
summary(jatim_w)

# MST (Minimum Spanning Tree)
set.seed(100)
jatim_mst <- mstree(jatim_w)
class(jatim_mst)
head(jatim_mst)

# Plot MST
plot(jatim_map, border = gray(.5))
plot(jatim_mst, longlat, col = "blue", cex.lab = .7, cex.circles = 0, add = T)

# Create n cluster: n-1 cuts
clus2 <- skater(jatim_mst[, 1:2], jatim_std, 1)
clus3 <- skater(jatim_mst[, 1:2], jatim_std, 2)
clus4 <- skater(jatim_mst[, 1:2], jatim_std, 3)
clus5 <- skater(jatim_mst[, 1:2], jatim_std, 4)

# Check the cluster assignment
ccs2 <- clus2$groups
ccs3 <- clus3$groups
ccs4 <- clus4$groups
ccs5 <- clus5$groups

table(ccs2)
table(ccs3)
table(ccs4)
table(ccs5)

# Adding cluster
jatim_data <- jatim_data %>% 
  mutate(CLUST2 = ccs2,
         CLUST3 = ccs3,
         CLUST4 = ccs4,
         CLUST5 = ccs5)
View(jatim_data)

# MANOVA
man2 <- manova(cbind(GINI, L, THEIL) ~ CLUST2, data = jatim_data)
summary(man2)

man3 <- manova(cbind(GINI, L, THEIL) ~ CLUST3, data = jatim_data)
summary(man3)

man4 <- manova(cbind(GINI, L, THEIL) ~ CLUST4, data = jatim_data)
summary(man4)

man5 <- manova(cbind(GINI, L, THEIL) ~ CLUST5, data = jatim_data)
summary(man5)

# Calculate each mean value for the variables for each cluster when k = 5
jatim_data %>% 
  group_by(CLUST5) %>% 
  summarise(GINI = mean(GINI),
            L = mean(L),
            THEIL = mean(THEIL))

# Plot the map
plot(jatim_map, col = c("red", "green", "blue", "yellow", "brown")[ccs5])
legend("topright", as.character(1:5), fill = c("red", "green", "blue", "yellow", "brown"),
       title = "Cluster")

# Saving result
write_csv(jatim_data, "Result/Clustered Data.csv")

jatim_result_all <- list(
  neighbour = jatim_nb,
  edge_cost = lcosts,
  weight = jatim_w,
  mst = jatim_mst
)
saveRDS(jatim_result_all, "Result/Result_all.rds")

