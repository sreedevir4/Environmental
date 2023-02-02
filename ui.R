library(highcharter)
#install.packages("shiny")
library(shiny)

library(readxl)
#########
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel(p("CalEnviroScreen 4.0", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      radioButtons("dist", "Visualization type:",
                   c("Correlation Plot" = "corr",
                     "Treemap" = "tree",
                     "Scatter plot" = "scatter",
                     "Heatmap" = "heat"))
    ),
   # mainPanel(highchartOutput("hc"),plotOutput("myplot"))
    
   # mainPanel(highchartOutput("hc"))
   mainPanel( conditionalPanel(
     condition = "input.dist == 'corr' || input.dist == 'heat' || input.dist == 'scatter'",
     plotOutput("myplot")
   ),
   conditionalPanel(
     condition = "input.dist == 'tree'",
     highchartOutput("hc")
     )
  ))

)

