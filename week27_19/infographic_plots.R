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

#number of franchise by decades
data_plot <- franchise_summary %>% select(decade_created,n, franchise, original_media) %>% distinct()

bplot2 <- data_plot %>% ggplot(aes(x=decade_created, fill=original_media)) + 
  geom_bar(width=10)+
  theme_classic()
bplot2

out_file="week27.png"
ggsave(out_file, width = 10, height = 7, units = "cm")
