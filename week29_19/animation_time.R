library(ggplot2)
library(dplyr)
library(gganimate)
library(gapminder)
library(tidyverse)


#####################tutorial from gganimate###################
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')


########################### Tidydata ###########################
r4ds_members <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv")
r4ds_members

data_plot <- r4ds_members %>% select(date, total_membership, 
                                     daily_active_members,
                                     weekly_active_members) %>% gather(members, count, -date)


# static plot time-serie
data_plot %>% ggplot(aes(x = date, y = count)) + 
  geom_line(aes(color = members), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "red")) +
  theme_minimal()

# animation galore
data_plot %>% ggplot(aes(x = date, y = count)) + 
  geom_line(aes(color = members), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "red")) +
  theme_minimal()+
# Here comes the gganimate specific bits
  geom_point() +
  transition_reveal(date)
