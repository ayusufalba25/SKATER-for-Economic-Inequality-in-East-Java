# Create a natural break map
library(classInt)

# Create classes with jenks natural breaks algorithm
classes <- classIntervals(jatim_data$GINI, n = 5, style = "jenks")
classes$brks

# Data manipulation
jatim_natbreak <- jatim_data %>% 
  mutate(ncGin = cut(GINI, classes$brks, include.lowest = T))
View(jatim_natbreak)

# Check the data
jatim_natbreak %>% 
  group_by(ncGin) %>% 
  summarise(n = n())

jatim_natbreak %>% 
  group_by(ncGin) %>% 
  summarise(n = n()) %>% 
  select(ncGin) %>% 
  unlist()

# Plot the data
plot(jatim_map, col = c("red", "green", "blue", "yellow", "brown")[jatim_natbreak$ncGin])
legend("topright", legend = jatim_natbreak %>% 
         group_by(ncGin) %>% 
         summarise(n = n()) %>% 
         select(ncGin) %>% 
         unlist(), fill = c("red", "green", "blue", "yellow", "brown"),
       title = "Gini")

