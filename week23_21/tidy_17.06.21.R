library(tidyverse)

# load the dataset
summary <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/summary.csv')

# subset and pivot the table for ggplot
summary_view <- summary %>% select(season, viewers_premier, viewers_finale, viewers_reunion, viewers_mean) %>%
  pivot_longer(cols = -season, names_to="type_of_viewers", values_to="nb_viewers")

# plot the evolution of viewership per season
summary_view %>% ggplot(aes(x= season, y=nb_viewers, colour=type_of_viewers)) +
  geom_line() +
  geom_point(size=2) +
  theme_minimal() +
  labs(
    title = "surivor viewers over time",
    y= "number of viewers in million",
    colour = "type of viewers"
  ) 

# ca
summary_delta <- summary %>% select(season, viewers_premier, viewers_finale) %>%
  mutate(delta= viewers_finale- viewers_premier)

summary_delta %>% ggplot(aes(y=delta, x=season)) + 
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(
    title = "Loss or gain of viewers between premiere and finale",
    x = "Season",
    y = "Delta of Viewers (Millions)",
  )


