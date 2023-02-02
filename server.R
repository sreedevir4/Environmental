library(tidyverse)
library(ggplot2)
#library(countrycode)
#library(leaflet)
#library(ggmap)
library(dplyr)
#library(tidygeocoder)
library(readxl)
#library(sf)
#library(sp)
#install.packages("rgdal")
#library(rgdal)
#install.packages("mapview")
#library(mapview)
#install.packages("boot")
#library(boot)
#install.packages("table1")
#library(table1)
library(corrplot)
#library(htmlwidgets)
library(highcharter)  


########################
####Replace C:\\Users\\ravis6\\Documents\\ with your file path
env1 <- read_excel("C:\\Users\\sreed\\OneDrive\\Documents\\calenviroscreen40resultsdatadictionary_F_2021.xlsx",sheet = "CES4.0FINAL_results")
env2 <- read_excel("C:\\Users\\sreed\\OneDrive\\Documents\\calenviroscreen40resultsdatadictionary_F_2021.xlsx",sheet = "Demographic Profile")
########################

nrow(env1)
nrow(env2)
colnames(env1)
env <- merge(env1,env2,by = "Census Tract") 
########################

server <- function(input,output,session) 
{

  output$myplot <- renderPlot({
    if (input$dist == "corr") {
      
      env_num2 <- select_if(env1, is.numeric)
      req <- env_num2[,c(9,11,13,15,17,19,21,23,25,27,29,31,34,36,38,40)]
      req$LowBirth <- as.numeric(env1$`Low Birth Weight Pctl`)
      
      M = cor(req,use = "complete.obs")
      corrplot(M,tl.cex = 0.5)
    }
    else if (input$dist == "scatter") {
    plot(env1$Poverty, env1$`Low Birth Weight`, pch = 19, col = "blue",cex=.8,ylab="Low Birth Weight",xlab="Poverty",main = "Low Birth Weight vs Poverty")
      
    }
    else if (input$dist == "heat") {
      env1 <- read_excel("C:\\Users\\sreed\\OneDrive\\Documents\\calenviroscreen40resultsdatadictionary_F_2021.xlsx",sheet = "CES4.0FINAL_results")
      env2 <- read_excel("C:\\Users\\sreed\\OneDrive\\Documents\\calenviroscreen40resultsdatadictionary_F_2021.xlsx",sheet = "Demographic Profile")
      env <- merge(env1,env2,by = "Census Tract") 
      
      env_num2 <- select_if(env, is.numeric)
      colnames(env_num2)
      req <- env_num2[,c(9,11,13,15,17,19,21,23,25,27,29,31,34,36,38,40)]
      req$LowBirth <- as.numeric(env$`Low Birth Weight Pctl`)
      ##########
      req <- na.omit(req)
      
      df <- scale(req)
      heatmap(df, scale = "none")
    }
    else
      
      output$hc <- renderHighchart({
        env_dem <- env2 %>%
          rename(County = `California County`)
        env_tree <- env_dem %>%
          select(c('County','Total Population')) %>%
          group_by(County) %>%
          summarise('Total Population' = sum(`Total Population`,na.rm = FALSE))
        env_tree <- env_tree %>%
          rename(Population = `Total Population`)
        env_tree %>%
          hchart(
            "treemap", 
            hcaes(x = County, value = Population, color = Population)
          )
        
        
        
      })
  })
  
  
 
  
}

