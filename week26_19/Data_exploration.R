library(ggplot2)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(maps)

## Data exploration
ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

# divide date-time to get month, year and hours:min
date_corr_ufo <- ufo_sightings %>% separate(date_time,c("month","day","year","time") ,sep="([ /])", remove=FALSE)
date_corr_ufo %>% select(date_time, month,day,year,time)

# Make a map of the sightings in the world in 1950 vs 2000
world_map <- ufo_sightings %>% ggplot()+ geom_polygon(aes(long, lat, group = group),
                  data = map_data("world"), fill = NA, colour = "black", inherit.aes = FALSE)+
                  geom_point(aes(x = longitude, y = latitude, color=state), size = 2)


world_map 


# Plot the number of encounter at night vs day, as the data is expressed in local time-zone, the division is pretty straightforward

## dummy function to return the period of the day from the time
time_of_the_day <- function(time) {
  time_split <- strsplit(time, ":")[[1]]
  hour<-as.numeric(time_split[1])
  if (hour >= 0 & hour <= 7 | hour == 24) {
      period="early morning"
    } else if (hour > 7 & hour < 19 ) {
      period="day"
    } else if (hour >= 19 & hour <= 21 ) {
      period="early night"
    } else if (hour >= 21 & hour <= 23 ) {
      period="late night"
    } else {
      period="unknown"
    }
  return(period)
}
#vectorize the function to work with mutate
time_of_the_day_v <- Vectorize(time_of_the_day)
#plot the data
data_day_night <- date_corr_ufo %>% mutate(period_day=time_of_the_day_v(time))

data_day_night %>% ggplot(aes(period_day)) + geom_bar()

                    