library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(purrr)

df = read_csv("Data/TLW_litterTraps.csv") %>% 
  mutate(
    treatment = catchment %in% c("33", "34L", "34U")
    # harvested = ! (catchment %in% c("32", "37", "38", "39", "42", "60", "96", "97", "98", "99", "99U", "99L", "99UU", "99UL", "99U","100", "101", "102", "NC", "NCU", "NCL"))
  )

# df %>% filter(catchment=="34M")

# df %>% pull(catchment) %>% unique()
resp = 
  c("dryleaf_g" = "dry mass of leaves (g)", 
    "dryother_g"= "dry mass of non-leaf biomass (g)",
    "organicleaf_gperm2"  = "organic or ash-free dry weight input \n from leaf litter per area (g/m2)",
    "organicother_gperm2" = "organic or ash-free dry weight input \n from non-leaf litter per area (g/m2)"
  )

plotLitter <- function(df, r) {
  # browser()
  m = max(df%>% pull(r), na.rm = T)
  df %>%
    ggplot(aes(y= !! sym(r), x=year, color=treatment)) + 
    ylab(resp[r]) +
    stat_summary() + 
    scale_color_brewer(type="seq", palette="Dark2") +
    annotate("polygon", x=c(1996.5, 1997.5, 1997.5, 1996.5), y=c(0, 0, 20 , 20), fill=rgb(0.5, 0, 0, 0.20))
}

map(names(resp), ~plotLitter(df, .x)) %>% wrap_plots() + plot_layout(guides = "collect")

