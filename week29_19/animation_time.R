library(ggplot2)
library(dplyr)
library(gganimate)
library(gapminder)
library(tidyverse)
library(lubridate)
library(extrafont)
font_import()
loadfonts()


#Load dataset
r4ds_members <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv")
r4ds_members

data_plot <- r4ds_members %>% 
                    select(date, total_membership,daily_active_members,weekly_active_members) %>% 
                    gather(members, count, -date) 



# static plot time-serie
data_plot %>% ggplot(aes(x = date, y = count)) + 
  labs(y="number of members", x="date",
       title="Number of members in R4DS\n(2017 to 2019)")+
  geom_line(aes(color = members), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "red")) +
  geom_point(aes(color = members), size = 3) +
  theme_minimal(base_size = 16)+
  theme(text=element_text(family="Times New Roman"))

# animation ! exciting times !
data_plot %>% ggplot(aes(x = date, y = count)) + 
  labs(y="number of members", x="date",
       title="Number of members in R4DS",
       subtitle = "(2017 to 2019)")+
  geom_line(aes(color = members), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "red")) +
  geom_point(aes(color = members), size = 3) +
  theme_minimal(base_size = 16)+
  theme(text=element_text(family="Times New Roman"))+
# Here comes the gganimate stuff
  transition_reveal(date,
                    range = as.Date(c("2017-08-27", "2020-01-01")))
