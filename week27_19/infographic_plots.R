library(ggplot2)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(maps)
library(svglite)

#function to floor the decades
floor_dec= function(myyear){ 
  return(myyear - myyear %% 10) 
}
floor_dec_v <- Vectorize(floor_dec)

## Data loading
media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

#add a decade column
media_franchises <- media_franchises %>% mutate(decade_created=as.character(floor_dec_v(year_created)))

#number of franchise by decades
simple <- media_franchises %>% select(decade_created, franchise, original_media) %>% distinct()

#merge film and animated film in one section
simple <- simple %>% mutate_if(is.character, 
                str_replace_all, pattern = "Animated film", replacement = "Film")
#merge Anime and manga
simple <- simple %>% mutate_if(is.character, 
                               str_replace_all, pattern = "Manga", replacement = "Japan")
simple <- simple %>% mutate_if(is.character, 
                               str_replace_all, pattern = "Anime", replacement = "Japan")
simple <- simple %>% mutate_if(is.character, 
                               str_replace_all, pattern = "Japan", replacement = "Anime/Manga")
#merge Cartoon, Cartoon character and Animated cartoon
simple <- simple %>% mutate_if(is.character, 
                               str_replace_all, pattern = "Cartoon character", replacement = "Cartoon")
simple <- simple %>% mutate_if(is.character, 
                               str_replace_all, pattern = "Animated cartoon", replacement = "Cartoon")
#merche comic book and comic strip
simple <- simple %>% mutate_if(is.character, 
                               str_replace_all, pattern = "Comic book", replacement = "Comic book/strip")
simple <- simple %>% mutate_if(is.character, 
                               str_replace_all, pattern = "Comic strip", replacement = "Comic book/strip")
#merge Novel, book and visual novel
simple <- simple %>% mutate_if(is.character, 
                               str_replace_all, pattern = "Novel", replacement = "Book")
simple <- simple %>% mutate_if(is.character, 
                               str_replace_all, pattern = "Visual novel", replacement = "Book")
#merge Television series and Animated series
simple <- simple %>% mutate_if(is.character, 
                               str_replace_all, pattern = "Television series", replacement = "Series")
simple <- simple %>% mutate_if(is.character, 
                               str_replace_all, pattern = "Animated series", replacement = "Series")
#merge Digital pet and video game
simple <- simple %>% mutate_if(is.character, 
                               str_replace_all, pattern = "Digital pet", replacement = "Video game")
#merge greeting card and Musical theater
simple <- simple %>% mutate_if(is.character, 
                               str_replace_all, pattern = "Greeting card", replacement = "Other")
simple <- simple %>% mutate_if(is.character, 
                               str_replace_all, pattern = "Musical theatre", replacement = "Other")



bplot1 <- simple %>% ggplot(aes(x=decade_created, fill=original_media)) + 
  geom_bar()+
  theme_classic()
bplot1

out_file="/Users/aponsero/Documents/Rvisualisations_tidytuesday/week27_19/week27.svg"
ggsave(out_file, width = 10, height = 7, units = "cm")

#get count owners
dis <- media_franchises %>% select(franchise, original_media, owners) %>% distinct() %>% group_by(owners) %>% tally()
dis

#pie chart for Winnie, Star wars and Frozen
myfranchises=c("Winnie the Pooh", "Star Wars","Frozen")
three <- media_franchises %>% filter(franchise %in% myfranchises) %>% 
  group_by(franchise) %>% mutate(Percent = revenue / sum(revenue))
  
three$franchise_f = factor(three$franchise, 
                           levels=c("Winnie the Pooh", "Star Wars","Frozen"))

pie <- three %>% ggplot(aes(x = 2, y = Percent, fill = revenue_category)) + 
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5)+
  facet_grid(facets = . ~ franchise_f)  +
  theme_classic()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid  = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = 'bottom') +    
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
pie
