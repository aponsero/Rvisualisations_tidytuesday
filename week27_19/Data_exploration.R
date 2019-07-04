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
  labs(y = "revenue (in billion)", x = "Franchise name") +
  coord_flip()
bplot

panel <- bplot + facet_grid(decade_created ~ .,scales="free_y",space="free")
panel

#addition of the type of revenues by decade
franchise_summary <- media_franchises %>% group_by(decade_created, revenue_category) %>% summarise(sum=sum(revenue))
#number of franchise by decades
numb_by_decade <- media_franchises %>% group_by(decade_created) %>% tally()
franchise_summary <- full_join(numb_by_decade, franchise_summary, by="decade_created")

franchise_summary

bplot2 <- franchise_summary %>% ggplot(aes(x=decade_created)) + 
  geom_bar(aes(y=sum, fill = revenue_category), stat="identity")+
  geom_point(aes(y=n*3,size = n))+
  geom_text(aes(y=n*3, label=round(franchise_summary$n,1)), nudge_y = 20) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 2, name = "number of franchises"))+
  labs(fill="Categories", y = "revenue (in billion)", x = "year of creation")+
  theme_classic(base_size = 10)
bplot2

# stacked barplot by owner