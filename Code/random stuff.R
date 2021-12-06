natBreakGraph("GINI", 5)


classes <- classIntervals(df_result$"GINI", n = 5, style = "jenks")
df_result$"GINI"


natBreakGraph <- function(var_name, n_cut){
  classes <- classIntervals(df_result[, var_name] %>% unlist(), n = n_cut, style = "jenks")
}


sym('height')


# The environment variable `var` refers to the data-variable
# `height`
var <- sym("height")

# We force `var`, which substitutes it with `height`
starwars %>%
  summarise(avg = mean(var, na.rm = TRUE))

natBreakGraph <- function(var_name, n_cut){
  classes <- classIntervals(df_result[, var_name] %>% unlist(), n = n_cut, style = "jenks")
  
  var_col <- sym(var_name)
  jatim_natbreak <- df_result %>% 
    mutate(ncGin = cut(!!var_col, classes$brks, include.lowest = T))
}
natBreakGraph("GINI", 5)
clustplot(4)
