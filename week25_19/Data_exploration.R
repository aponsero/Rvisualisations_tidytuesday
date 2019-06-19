library(ggplot2)
library(tidyverse)
library(ggthemes)

## Data exploration
bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")

#sum of species counted
spec_counts<- bird_counts %>% group_by(species_latin) %>% summarize(count_by_species=sum(how_many_counted)) %>% filter(count_by_species!=0)
spec_counts
##194 different species observed over the years

#Add a genus variable
bird_genus <- bird_counts %>% separate(species_latin, c("genus", NA))
#sum of genus counted
gen_counts<- bird_genus %>% group_by(genus) %>% summarize(count_by_genus=sum(how_many_counted)) %>% filter(count_by_genus!=0)
gen_counts
##126 different genus observed over the years

#median of time spent counting
year_count <- bird_counts  %>% select(year, total_hours) %>% distinct()
median_hours=median(year_count$total_hours, na.rm=TRUE)
median_hours
##171 hours

#Normalize count over total count of the year -> Not really appropriate, the count/hour is more pertinent
norm_bird <- bird_counts %>% group_by(year) %>% mutate(total_count=sum(how_many_counted)) %>% mutate(norm_count=how_many_counted/total_count)

#Look at the evolution of the hours spent counting over year
bird_counts %>% ggplot(aes(year, total_hours)) + geom_line()

#get the most interesting species to plot (species with the largest change over time, using the count over time)
range_years<-bird_counts %>% group_by(species_latin)  %>% summarise(min=min(how_many_counted_by_hour, na.rm=TRUE), max=max(how_many_counted_by_hour, na.rm=TRUE), delta=max(how_many_counted_by_hour, na.rm=TRUE)-min(how_many_counted_by_hour, na.rm=TRUE))

#most important delta in count
range_years %>% top_n(3, delta) 

#plot all species delta
range_years %>% ggplot(aes(delta)) + geom_histogram()

####evolution between 1921 and 2017
evolution<-bird_counts %>% filter(year==c(1921,2017)) %>% group_by(species_latin) %>% summarise(min=min(how_many_counted_by_hour), max=max(how_many_counted_by_hour), increase=max(how_many_counted_by_hour)-min(how_many_counted_by_hour))

#most important decrease in count between 1921 and 2017
evolution %>% top_n(-3, increase)

#most important increase in count
evolution %>% top_n(3, increase)

#plot all increase in counts
evolution %>% ggplot(aes(increase)) + geom_histogram()

#get the most interesting genus to plot (genus with the largest change over time, using the count over time)
range_years<-bird_genus %>% group_by(genus)  %>% summarise(min=min(how_many_counted_by_hour, na.rm=TRUE), max=max(how_many_counted_by_hour, na.rm=TRUE), delta=max(how_many_counted_by_hour, na.rm=TRUE)-min(how_many_counted_by_hour, na.rm=TRUE))

#most important delta in count
range_years %>% top_n(3, delta) 

#plot all species delta
range_years %>% ggplot(aes(delta)) + geom_histogram()

####evolution between 1921 and 2017
evolution<-bird_genus %>% filter(year==c(1921,2017)) %>% group_by(genus) %>% summarise(min=min(how_many_counted_by_hour), max=max(how_many_counted_by_hour), increase=max(how_many_counted_by_hour)-min(how_many_counted_by_hour))

#most important decrease in count between 1921 and 2017
evolution %>% top_n(-3, increase)

#most important increase in count
evolution %>% top_n(3, increase)

#plot all increase in counts
evolution %>% ggplot(aes(increase)) + geom_histogram()


## PLOTS INFOGRAPHIE

#look at the diversity of species over time
nb_species_by_year<-bird_counts %>% filter(how_many_counted!=0) %>% group_by(year) %>% summarize(nb_species=n_distinct(species_latin))

species_diversity<-full_join(year_count,nb_species_by_year,by="year") %>% filter(year>1949) %>% mutate(nb_species_by_hour=nb_species/total_hours)

plot1<-species_diversity %>% ggplot(aes(year, nb_species_by_hour)) + geom_line()
plot1

#Look at the nb of birds over time
nb_birds_by_year<-bird_counts %>% filter(how_many_counted!=0) %>% group_by(year) %>% summarize(nb_bird_year=sum(how_many_counted))

bird_diversity<-full_join(year_count,nb_birds_by_year,by="year") %>% filter(year>1949) %>% mutate(nb_bird_by_hour=round(nb_bird_year/total_hours))

plot2<-bird_diversity %>% ggplot(aes(year, nb_bird_by_hour)) + geom_line()
plot2

#Plot Anas platyrhynchos
plot3<-bird_counts %>% filter(year>1949 & species_latin=="Anas platyrhynchos") %>% ggplot(aes(year, how_many_counted_by_hour)) + geom_line()
plot3

#Plot Passer domesticus
plot4<-bird_counts %>% filter(year>1949 & species_latin=="Passer domesticus") %>% ggplot(aes(year, how_many_counted_by_hour)) + geom_line()
plot4

#Plot Sturnus vulgaris
plot5<-bird_counts %>% filter(year>1949 & species_latin=="Sturnus vulgaris") %>% ggplot(aes(year, how_many_counted_by_hour)) + geom_line()
plot5

#plot the two birds together
plot6<-bird_counts %>% filter(year>1949 & species_latin==c("Anas platyrhynchos","Passer domesticus")) %>% ggplot(aes(year, how_many_counted_by_hour, color=species_latin)) + geom_line()
plot6 + theme_classic()

#plot composition evolution
low_abundance<- bird_counts %>% filter(year>1949 & how_many_counted_by_hour<20) %>% group_by(year) %>% summarize(other=sum(how_many_counted_by_hour)) %>% gather(-"year", key = species, value = how_many_counted_by_hour)
high_abundance<- bird_counts %>% filter(year>1949 & how_many_counted_by_hour>=20) %>% select(year, species, how_many_counted_by_hour)
abundance_data<-bind_rows(low_abundance,high_abundance)

plot7<- abundance_data %>% ggplot(aes(year, how_many_counted_by_hour)) + geom_bar(stat="identity",aes(fill=species)) + scale_colour_tableau() 
plot7 + theme_classic()
