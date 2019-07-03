library(ggplot2)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(maps)

#function to floor the decades
floor_dec= function(myyear){ 
  return(myyear - myyear %% 10) 
}
floor_dec_v <- Vectorize(floor_dec)

## Data exploration
media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

#add a decade column
media_franchises <- media_franchises %>% mutate(decade_created=floor_dec_v(year_created))

#stacked barplot of the revenues category
bplot <- media_franchises %>% ggplot(aes(franchise,revenue, fill = revenue_category)) + 
  geom_bar(stat="identity")+
  coord_flip()
bplot

panel <- bplot + facet_grid(decade_created ~ .,scales="free_y",space="free")
panel
