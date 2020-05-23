library(shiny)
library(ggplot2)
library(tidyverse)
require(nCov2019)
library(dplyr)
library(forcats)
daily_data <- get_nCov2019()

shinyUI(pageWithSidebar(
  
  headerPanel("covid-19"),
  selectInput('By YIYUE(Joanna) QIAO', 'By YIYUE(Joanna) QIAO', daily_data),
  mainPanel(
    tabsetPanel(
      tabPanel("Countries", plotOutput("countries")), 
      tabPanel("Bar chart(Top 20)", plotOutput("barchart")), 
      tabPanel("Line chart(Top seven)", plotOutput("linechart")),
      tabPanel("SEIR(personal opinion)", plotOutput("SEIR"))
    )
  )
))
