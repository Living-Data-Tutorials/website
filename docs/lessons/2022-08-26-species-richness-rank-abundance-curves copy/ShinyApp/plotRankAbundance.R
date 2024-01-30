

# source("R/DataProcessing/getDataSubset.R")

## The following function could be used as a part of a Shiny app.
plotRankAbundance <- function (
  df = df,
  select_catchment = "34L",             ## 31 & 34L most heavily deforested
  select_year      = c(1998,1998),     ## Harvesting operation: 1997
  select_month     = c("june"),   
  log              = T, 
  SpName           = T, 
  Barcol           = T
)
{
  # Load & filter to options selected above
  df <- df %>% 
    filter(catchment %in% !! select_catchment, 
           year   >= select_year[1], 
           year   <= select_year[2],
           month     %in% !! select_month) %>%
    group_by(Species) %>%
    summarise(
      TotalCount = sum(Count))  %>%
    arrange(desc(TotalCount)) %>% 
    mutate( Rank = seq_along(Species) )
    
    

  # PLOT
  
  if(SpName) {
    plot = df %>%
      ggplot(aes(x=reorder(Species, -1*TotalCount), y=TotalCount)) + 
      xlab("Taxon")
  } else {
    plot = df %>%
      ggplot(aes(x=reorder(Rank, -1*TotalCount), y=TotalCount)) + 
      xlab("Rank")
  }
  if(Barcol) {
    plot = plot + geom_col() 
  } else {
    plot = plot + geom_line() 
  }
  
  plot = plot +  
    ylab("Total count (sum of replicates)") + 
    # coord_flip() +
    theme_classic(base_size=16) +
    theme(axis.text.x = element_text(face=ifelse(SpName, "italic", "plain"), size=9, angle=90))
  if(log) {
    plot = plot + scale_y_log10() + ylab("log Total count (sum of replicates)")
  }
  plot
}

## DEBUG
# plotRankAbundance()
