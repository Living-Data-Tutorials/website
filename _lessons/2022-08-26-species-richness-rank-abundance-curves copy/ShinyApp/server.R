#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("plotRankAbundance.R", encoding = "UTF-8")

tidy_df = 
  read.csv("TLW_invertebrateDensity.csv") %>%
  tidyr::pivot_longer( "Aeshna":"Trichoptera", names_to = "Species", values_to = "Density" ) %>%
  mutate(
    Count = Density * 0.33, 
    Count = tidyr::replace_na(Count, 0))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$RankAbundPlot <- renderPlot({
      plotRankAbundance(df = tidy_df,
                        select_catchment = input$catchment, 
                        select_month = input$month,
                        select_year=input$year, 
                        log=input$log)
    })

})
