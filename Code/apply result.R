# Apply result

# Read result data
result <- readRDS("Result/Result_all.rds")
df_result <- read_csv("Result/Clustered Data.csv")

# Plot point coordinates for the neighbours
plot(jatim_map, border = gray(.5))
plot(result$neighbour, longlat, col = "blue", add = T)

# Plot MST
plot(jatim_map, border = gray(.5))
plot(result$mst, longlat, col = "blue", cex.lab = .7, cex.circles = 0, add = T)

# Plot cluster
plot(jatim_map, col = c("red", "green")[df_result$CLUST2])
legend("topright", as.character(1:2), fill = c("red", "green"),
       title = "Cluster")

plot(jatim_map, col = c("red", "green", "blue")[df_result$CLUST3])
legend("topright", as.character(1:3), fill = c("red", "green", "blue"),
       title = "Cluster")

plot(jatim_map, col = c("red", "green", "blue", "yellow")[df_result$CLUST4])
legend("topright", as.character(1:4), fill = c("red", "green", "blue", "yellow"),
       title = "Cluster")

plot(jatim_map, col = c("red", "green", "blue", "yellow", "brown")[df_result$CLUST5])
legend("topright", as.character(1:5), fill = c("red", "green", "blue", "yellow", "brown"),
       title = "Cluster")

# Check Kabupaten Kota for each cluster
kabkot2 <- df_result %>% 
  group_by(CLUST2) %>% 
  summarise(all_kabkot = paste(KABKOT, collapse = ", "))

kabkot3 <- df_result %>% 
  group_by(CLUST3) %>% 
  summarise(all_kabkot = paste(KABKOT, collapse = ", "))

kabkot4 <- df_result %>% 
  group_by(CLUST4) %>% 
  summarise(all_kabkot = paste(KABKOT, collapse = ", "))

kabkot5 <- df_result %>% 
  group_by(CLUST5) %>% 
  summarise(all_kabkot = paste(KABKOT, collapse = ", "))

kabkot2[,2] %>% unlist()

# Manova
man2 <- manova(cbind(GINI, L, THEIL) ~ CLUST2, data = df_result)
summary(man2)

man3 <- manova(cbind(GINI, L, THEIL) ~ CLUST3, data = df_result)
summary(man3)

man4 <- manova(cbind(GINI, L, THEIL) ~ CLUST4, data = df_result)
summary(man4)

man5 <- manova(cbind(GINI, L, THEIL) ~ CLUST5, data = df_result)
summary(man5)

# Calculate each mean value for the variables for each cluster when k = 5
df_result %>% 
  group_by(CLUST5) %>% 
  summarise(GINI = mean(GINI),
            L = mean(L),
            THEIL = mean(THEIL))
