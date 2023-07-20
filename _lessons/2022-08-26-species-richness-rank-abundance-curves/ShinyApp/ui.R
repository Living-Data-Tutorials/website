#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# To deploy (using Egor's laptop)
# rsconnect::deployApp('C:/Users/egor/Documents/Projects/LivingDataWebsite/_lessons/2022-08-26-species-richness-rank-abundance-curves/ShinyApp', appName= "TurkeyLakesInverts")

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
source("plotRankAbundance.R", encoding = "UTF-8")

# Get options for year, season and catchment
df = read.csv("TLW_invertebrateDensity.csv")
year_options      = df %>% pull(year) %>% unique
month_options     = df %>% pull(month) %>% unique
catchment_options = df %>% pull(catchment) %>% unique

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Rank-Abundance Curve for Benthic Invertebrates in Turkey Lakes"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", h5("Year"),
                  min = min(year_options), max = max(year_options), value = c(1998, 1999), step=1, sep=""),
      # checkboxGroupInput("year", 
      #                    h3("Year"), 
      #                    choices = c(),
      #                    selected = 1998), 
      checkboxGroupInput("month", 
                         h5("Month"), 
                         choices = stats::setNames(month_options, stringr::str_to_title(month_options)),
                         selected = month_options),         
      radioButtons("catchment", 
                         h5("Catchment"), 
                         choices = catchment_options,
                         selected = "34L"), 
      checkboxInput("log", "Log Counts", value = TRUE)),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("RankAbundPlot", height = "800px")
    )
  )
))
