# Import libraries
library(shiny)
library(ape)
library(rgdal)
library(spdep)
library(tidyverse)
library(readxl)
library(classInt)

# Global
jatim_map <- readOGR(dsn = ".", layer = "35jatim")
result <- readRDS("Result_all.rds")
df_result <- read_csv("Clustered Data.csv")
color_identifier <- c("red", "green", "blue", "yellow", "brown")

natBreakGraph <- function(var_name, n_cut){
  classes <- classIntervals(df_result[, var_name] %>% unlist(), n = n_cut, style = "jenks")
  
  var_col <- sym(var_name)
  jatim_natbreak <- df_result %>% 
    mutate(ncGin = cut(!!var_col, classes$brks, include.lowest = T))
  
  plot(jatim_map, col = color_identifier[1:n_cut][jatim_natbreak$ncGin])
  legend("topright", legend = jatim_natbreak %>% 
           group_by(ncGin) %>% 
           summarise(n = n()) %>% 
           select(ncGin) %>% 
           unlist(), fill = color_identifier[1:n_cut],
         title = var_name)
}

clustplot <- function(n_clust){
  cl_col <- paste0("CLUST", n_clust)
  cl <- df_result[, cl_col] %>% unlist()
  plot(jatim_map, col = color_identifier[1:n_clust][cl])
  legend("topright", as.character(1:n_clust), fill = color_identifier[1:n_clust],
         title = "Cluster")
}

clusttable <- function(n_clust){
  cl_col <- paste0("CLUST", n_clust) %>% sym()
  df <- df_result %>% 
    group_by(!!cl_col) %>% 
    summarise(GINI = mean(GINI),
              L = mean(L),
              THEIL = mean(THEIL)) %>% 
    rename("CLUSTER" = !!cl_col)
  return(df)
}


# UI
ui <- fluidPage(
  h1("SKATER for Economic Inequality in East Java"),
  h4(tags$a(href = "https://www.linkedin.com/in/ahmad-yusuf-a-ab5696130/", "Ahmad Yusuf Albadri")),
  tabsetPanel(
    tabPanel(
      "SKATER",
      sidebarLayout(
        sidebarPanel(
          h4(em(strong("Natural Breaks Map Parameter"))),
          selectInput("varnbg", "Select Variable", colnames(df_result)[2:4]),
          sliderInput("nnbg", "Number of Breaks", value = 3, min = 2, max = 5),
          hr(),
          h4(em(strong("SKATER Parameter"))),
          sliderInput("nskat", "Number of Clusters", value = 3, min = 2, max = 5)
        ),
        mainPanel(
          h4(em(strong("Natural Breaks Map"))),
          plotOutput("natbmap"),
          hr(),
          fluidRow(
            column(8,
                   h4(em(strong("SKATER Map"))),
                   plotOutput("clustmap")),
            column(4,
                   h4(em(strong("Characteristics (Mean)"))),
                   tableOutput("clustab"))
          )
        )
      )
    ),
    tabPanel(
      "Data",
      tableOutput("data")
    ),
    tabPanel(
      "About",
      p('I created this web apps based on my project called', strong("Grouping Districts and Cities in East Java Based on Its Economic Inequality Metrics Through the Implementation of the SKATER Method."),
      " This particular web apps aims to visualize the data that has been analyzed using SKATER in R.
        There are two different sections in the visualization panel that you can explore with its parameters:"),
      tags$ol(tags$li("Natural breaks map for the choosen variable."),
              tags$li("SKATER results that contains the spatial cluster map and its characteristics based on variable mean.")),
      p("Note: Five clusters is the best number of clusters to grouped each district and cities spatially using SKATER method.")
    )
  )
)

# Server
server <- function(input, output){
  output$natbmap <- renderPlot(natBreakGraph(input$varnbg, input$nnbg))
  output$clustmap <- renderPlot(clustplot(input$nskat))
  output$clustab <- renderTable(clusttable(input$nskat))
  output$data <- renderTable(df_result[1:4] %>% 
                               rename("District/City" = KABKOT))
}

shinyApp(ui, server)