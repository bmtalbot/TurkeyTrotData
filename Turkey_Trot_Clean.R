library(tidyverse)
library(jsonlite)
library(ggplot2)
library(gganimate)
library(cowplot)

### DATA PULLING ###

##data were pulled from inspecting the site: http://results.xacte.com/?kw=mcmtt# 
## http://results.xacte.com/json/search?eventID=2222&offset=0&limit=100

### pulled helpful code from https://stackoverflow.com/questions/20925492/how-to-import-json-into-r-and-convert-it-to-table

##How I saved each year
my.JSON_2018 <- fromJSON('C:/Users/.../TT_2018_search.json')
my.JSON_2017 <- fromJSON('C:/Users/.../TT_2017_search.json')
my.JSON_2016 <- fromJSON('C:/Users/.../TT_2016_search.json')
my.JSON_2015 <- fromJSON('C:/Users/Brooke/...TT_2015_search.json')

## RESHAPING DATA INTO TABLES 

library(dplyr)
data_2018 <- bind_rows(my.JSON_2018, .id = 'aaData')
data_2017 <- bind_rows(my.JSON_2017, .id = 'aaData')
data_2016 <- bind_rows(my.JSON_2016, .id = 'aaData')
data_2015 <- bind_rows(my.JSON_2015, .id = 'aaData')
#data_2014 <- bind_rows(my.JSON_2014, .id = 'aaData')

## EXTRACTING TABLES AND VARIABLE CREATION FOR EACH YEAR
dt_2018 <- as.data.frame(data_2018$aaData) %>% mutate(age_group = case_when(age < 10 ~ "Under 10",
                                                                            age >= 10 & age <= 19 ~ "10-19",
                                                                            age >= 20 & age <= 29 ~ "20-29",
                                                                            age >= 30 & age <= 39 ~ "30-39",
                                                                            age >= 40 & age <= 49 ~ "40-49",
                                                                            age >= 50 & age <= 59 ~ "50-59",
                                                                            age >= 60 & age <= 69 ~ "60-69",
                                                                            age >= 70 & age <= 79 ~ "70-79",
                                                                            age >= 80 & age <= 89 ~ "20-29")) %>%
  mutate(newpace = (chiptime/(1000*60))/(10*.621)) %>% 
  mutate(year = format(as.Date('2018', "%Y"), "%Y")) %>%
  select(firstname, lastname, chiptime, city, state, country, age, age_group, sex, newpace, year)

dt_2017 <- as.data.frame(data_2017$aaData) %>% mutate(age_group = case_when(age < 10 ~ "Under 10",
                                                                            age >= 10 & age <= 19 ~ "10-19",
                                                                            age >= 20 & age <= 29 ~ "20-29",
                                                                            age >= 30 & age <= 39 ~ "30-39",
                                                                            age >= 40 & age <= 49 ~ "40-49",
                                                                            age >= 50 & age <= 59 ~ "50-59",
                                                                            age >= 60 & age <= 69 ~ "60-69",
                                                                            age >= 70 & age <= 79 ~ "70-79",
                                                                            age >= 80 & age <= 89 ~ "20-29")) %>%
  mutate(newpace = (chiptime/(1000*60))/(10*.621)) %>%
  mutate(year = format(as.Date('2017', "%Y"), "%Y")) %>%
  select(firstname, lastname, chiptime, city, state, country, age, age_group, sex, newpace, year)

dt_2016 <- as.data.frame(data_2016$aaData) %>% mutate(age_group = case_when(age < 10 ~ "Under 10",
                                                                                age >= 10 & age <= 19 ~ "10-19",
                                                                                age >= 20 & age <= 29 ~ "20-29",
                                                                                age >= 30 & age <= 39 ~ "30-39",
                                                                                age >= 40 & age <= 49 ~ "40-49",
                                                                                age >= 50 & age <= 59 ~ "50-59",
                                                                                age >= 60 & age <= 69 ~ "60-69",
                                                                                age >= 70 & age <= 79 ~ "70-79",
                                                                                age >= 80 & age <= 89 ~ "20-29")) %>%
  mutate(newpace = (chiptime/(1000*60))/(10*.621)) %>%
  mutate(year = format(as.Date('2016', "%Y"), "%Y")) %>%
  select(firstname, lastname, chiptime, city, state, country, age, age_group, sex, newpace, year)

dt_2015 <- as.data.frame(data_2015$aaData) %>% mutate(age_group = case_when(age < 10 ~ "Under 10",
                                                                                age >= 10 & age <= 19 ~ "10-19",
                                                                                age >= 20 & age <= 29 ~ "20-29",
                                                                                age >= 30 & age <= 39 ~ "30-39",
                                                                                age >= 40 & age <= 49 ~ "40-49",
                                                                                age >= 50 & age <= 59 ~ "50-59",
                                                                                age >= 60 & age <= 69 ~ "60-69",
                                                                                age >= 70 & age <= 79 ~ "70-79",
                                                                                age >= 80 & age <= 89 ~ "20-29")) %>%
  mutate(newpace = (chiptime/(1000*60))/(10*.621)) %>%
  mutate(year = format(as.Date('2015', "%Y"), "%Y")) %>%
  select(firstname, lastname, chiptime, city, state, country, age, age_group, sex, newpace, year)
# #dt_2014 <- as.data.frame(data_2014$aaData) %>% mutate(age_group = case_when(age < 10 ~ "Under 10",
#                                                                                 age >= 10 & age <= 19 ~ "10-19",
#                                                                                 age >= 20 & age <= 29 ~ "20-29",
#                                                                                 age >= 30 & age <= 39 ~ "30-39",
#                                                                                 age >= 40 & age <= 49 ~ "40-49",
#                                                                                 age >= 50 & age <= 59 ~ "50-59",
#                                                                                 age >= 60 & age <= 69 ~ "60-69",
#                                                                                 age >= 70 & age <= 79 ~ "70-79",
#                                                                                 age >= 80 & age <= 89 ~ "20-29")) %>%
#   mutate(newpace = (chiptime/(1000*60))/(10*.621)) %>%
#   mutate(year = format(as.Date('2014', "%Y"), "%Y")) %>%
#   select(firstname, lastname, chiptime, city, state, country, age, age_group, sex, newpace,year)


## COMBINE ALL OF THE DATA INTO A SINGLE TABLE

all_TT <- rbind(dt_2015, dt_2016, dt_2017, dt_2018)

## DATA SUBSETTING ##

## Filter out all the registered names, non-runners

all_TT_runners <- all_TT %>% filter(!is.na(newpace)) %>% mutate(fullname = str_to_upper(paste(firstname,lastname, sep = " ")))

## Find runners with the same name 
duplicated <- all_TT_runners[all_TT_runners$fullname %in% all_TT_runners$fullname[duplicated(all_TT_runners$fullname)],]

## Removing names that are not the same person by age/location
duplicated <- duplicated %>%
  filter(!(fullname %in% c("ANDREW JOHNSON", "DAVID JONES", "EVELYNE SMITH", "JULIE SMITH", "NICOLE MARTIN",
                         "RAFAEL MAS","SARAH FLORES")) | (fullname %in% "CHARLES BALL" & age_group == '40-49')) %>%
  mutate(person_ID = if_else(fullname %in% c("GILBERT GUERIN", "TIMOTHY PHILLIPS"), paste(fullname,str_to_upper(city)), fullname))

## Identify the first race for each unique person

library(data.table)
duplicated <- duplicated[order(duplicated$person_ID, duplicated$year),]

## 0 or NA indicated that this row was the firt race for this person, so they don't have a record of this being a returning race
duplicated <- duplicated %>% mutate(count = 0)
duplicated$count <- ifelse(duplicated$person_ID == shift(duplicated$person_ID), 1, 0)  



#### DATA VISUALIZATION PLOTS ###

## MY NICE LITTLE HEX CODES FOR FALL THEMED COLORS: #F7EFD4, #FADDAF, #EB712F, #91371B, #472C25, #D4C2B2

## Visualize the number of returning runners by year

returners <- ggplot(duplicated %>% filter(count > 0 & (!is.na(count))), aes(x=year, fill = year)) + geom_bar() +
  labs(title = "Number of Returning Runners", x = "Year", y = "Number of Runners") +
  scale_fill_manual( values = c("#FADDAF", "#EB712F", "#91371B")) + 
  geom_text(stat='count', aes(label=..count..), vjust= 10, size = 5, fontface = "bold") +
 theme(panel.background = element_blank(), legend.position = "none",
       plot.title = element_text(hjust = 0.5)) +
transition_states(year,  transition_length = 2, state_length = 1) + 
  shadow_mark() +
  enter_grow() 

returners

## SIMPLE VISUALIZATION PLOTS

ggplot(data = data_2018, aes(y= data_2018$aaData$clocktime, x = data_2018$aaData$sex)) + 
  geom_boxplot() +
  scale_y_time()

## NUMBER OF MALE AND FEMALE PARTICIPANTS BY AGE GROUP
ggplot(data = dt_2018, aes(x=age_group, fill = sex)) + geom_bar(position = "dodge")

  
## CONVERT THE NET TIME INTO CLOCK TIME FOR AXES
y <- format(as.POSIXct((data_2018$aaData$chiptime/(1000)), origin="1970-01-01 00:00:00"), 
                       format="%H:%M:%OS", tz="UTC")


format(as.POSIXct(x, origin="1970-01-01 00:00:00"), format="%H:%M:%OS", tz="UTC")

#For making animation objects
library(gganimate)
library(gifski)

## Trying out gganimate with age groups
library(gganimate)
sex <- ggplot(data = all_TT_runners, aes(x=age_group, fill = sex)) + geom_bar(position = "dodge") +
  labs(title = '{closest_state}', x = "Age Group", y = "Participants") +
  scale_fill_manual(values = c("#EB712F", "#91371B")) +
  transition_states(as.factor(year), transition_length = 2, state_length = 1)
sex

splits <- all_TT_runners %>% mutate(newpace = (chiptime/(1000*60))/(10*.621))
splits_gif <- ggplot(splits, aes(x = as.factor(age_group), y = newpace, fill = sex)) + geom_boxplot() +
  scale_fill_manual( values = c("#EB712F", "#91371B")) +
  labs(y = "Pace (min/mile)", x= "Age Group", title = "Racer's Paces {closest_state}") + 
  theme(panel.background = element_blank()) +
  transition_states(year, transition_length = 2, state_length = 1)
splits

## DIFFERENT VISUALIZATIONS

#### MULTIPANEL PLOT ###

## PLOT 1 ##

sex <- ggplot(data = all_TT, aes(x=sex, fill = sex)) + geom_bar(position = "dodge") +
  labs(title = 'Age Group: {frame_time}', x = "Sex", y = "Participants") +
  transition_states(as.factor(age_group), transition_length = 2, state_length = 1)
sex

## PLOT 2 ##

### fastest runners each year
xx <- all_TT_runners %>% 
  arrange(chiptime) %>% 
  group_by(year,sex) %>%
  slice(1:3)

# NUMBER OF RETURNERS EACH YEAR

returners


## Preparing the animated plot

## Magick package for outputting the gif 
library(magick)
a_gif <- animate(sex, renderer = magick_renderer())
b_gif <- animate(splits_gif, renderer = magick_renderer())
c_gif <- animate(returners, renderer = magick_renderer())

a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)
c_mgif <- image_read(c_gif)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]), stack = TRUE)
for(i in 1:66){
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = TRUE)
  new_gif <- c(new_gif, combined)
}

new_gif

a_gif <- animate(sex, renderer = magick_renderer())
b_gif <- animate(splits_gif, renderer = magick_renderer())
c_gif <- animate(returners, renderer = magick_renderer())

tdir <- tempdir()
for(i in 1:100){
  new_gif <- plot_grid(ggdraw() + draw_image(a_gif[i],), 
                       ggdraw() + draw_image(b_gif[i]),
                       ggdraw() + draw_image(c_gif[i]),
                       nrow =1)
## Saving the set of images together 
  ggsave(
    filename = file.path(tdir, paste0("out_", sprintf("%03d", i), ".png")),
    plot = new_gif, width = 2.4, height = 3.6, device = "png")
}

png_files <- sort(list.files(path = tdir, pattern = "out_", full.names = TRUE))
gifski(png_files, gif_file = "out.gif", width = 3500, height = 3000, delay = .1,
       progress = TRUE)






