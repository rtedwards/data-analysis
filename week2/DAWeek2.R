## Data Analysis
## Week 2: Data Wrangling & Transformations

#########################################
## 1. Getting Started
#########################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)

## Packages containing interesting data
library(nycflights13)
library(fivethirtyeight)

#########################################
## 2.  Introducing 'Tidy' Data
#########################################
## Data in 'Tidy' Format
# country      alcohol type   servings
# Canada       beer           240
# Canada       spirit         122
# Canada       wine           100
# South Korea  beer           140
# South Korea  spirit         16
# South Korea  wine           9
# USA          beer           249
# USA          spirit         158
# USA          wine           84

#########################################
## 3.  Observational Units
#########################################
## 1. Each variable forms a column.
## 2. Each observation forms a row.
## 3. Each type of observational unit forms a table.
dim(flights)
head(flights)
glimpse(flights)

## How many different types of planes are in the nycflights13 package?
nrow(planes)
length(unique(planes$model))


#########################################
## 4.  Identification vs Measurement Variables
#########################################
glimpse(airports)

## lat & lon are the geographic position given latitude and longitude
## alt = the altitude in feet
## tz = timezone offset from GMT
## dst = Daylight Savings Time

##From the data sets listed above, find an example where combinations of 
## variables are needed to uniquely identify each observational unit.
airports$name[duplicated(airports$name)]
airports$name[airports$name == "Penn Station"]
airports$name[airports$name == "Dillingham"]


#########################################
## 5.  Importing Spreadsheets in R
#########################################
library(readr)
dem_score <- read_csv("https://moderndive.com/data/dem_score.csv")
dem_score

## read from local file
read_csv("dem_score.csv")

## Import Graphically
library(readxl)
dem_score <- read_excel("dem_score.xlsx")

## Read in life expectancy data
library(readr)
le_mess <- read_csv("https://moderndive.com/data/le_mess.csv")
View(le_mess)


#########################################
## 6. Converting to 'tidy' Data Format
#########################################
guat_dem <- dem_score %>%
  filter(country == "Guatemala")
guat_dem

## Convert data from 'wide' to 'tidy'
guat_tidy <- gather(data = guat_dem,
                    key = year, ## select column to collapse
                    value = democracy_score, ## column name for values
                    - country) ## don't touch this column
guat_tidy

## Plot data: 
## parse_number() needed to convert from chr
## o use mutate()
ggplot(data = guat_tidy, mapping = aes(x = parse_number(year), y= democracy_score)) +
  geom_line() +
  labs(x = "year")

## Convert the dem_score data frame into a tidy data frame and assign the name of  
## dem_score_tidy to the resulting long-formatted data frame.
dem_score_tidy <- gather(data = dem_score,
                         key = year,
                         value = democracy_score,
                         - country)
head(dem_score_tidy)

## Convert the life expectancy data set you created above into 
## a tidy data frame.
life_exp_scores <- read_csv("https://moderndive.com/data/le_mess.csv")
life_exp_tidy <- gather(data = life_exp_scores,
                        key = year,
                        value = life_expectancy,
                        -country)
head(life_exp_tidy)


#########################################
## 7. Introducing Data Wrangling
#########################################
## filter(): Pick rows based on conditions about their values
## summarize(): Compute summary measures known as "summary statistics" of variables
## group_by(): Group rows of observations together
## mutate(): Create a new variable in the data frame by mutating existing ones
## arrange(): Arrange/sort the rows based on one or more variables
## join(): Join/merge two data frames by matching along a "key" variable. There are many different join()s available. Here, we will focus on the  inner_join() function.


#########################################
## 8.  Filter Observations Using Filter
#########################################
portland_flights <- flights %>%
  filter(dest == "PDX")
head(portland_flights[, seq(-6,-12)])
#We leave out columns 6-11 from the display so we can see the "dest" variable

## select all flights that left JFK airport heading to Burlington, Vermont ("BTV") 
## or Seattle, Washington ("SEA") in the months of October, November, or December. 
## Run the following
btv_sea_flights_fall <- flights %>%
  filter(origin == "JFK", (dest == "BTV" | dest == "SEA"), month >= 10)
head(btv_sea_flights_fall[,-6:-12])

## Here we are selecting rows corresponding to flights that didn't 
## go to Burlington, VT or Seattle, WA.
not_BTV_SEA <- flights %>% 
  filter(!(dest == "BTV" | dest == "SEA"))
head(not_BTV_SEA[,-6:-12]) 

#########################################
## 9. Summarize Variables Using Summarize
#########################################
summary_temp <- weather %>%
  summarize(mean = mean(temp), std_dev = sd(temp))
summary_temp

## remove NA values
summary_temp <- weather %>%
  summarize(mean = mean(temp, na.rm=T), std_dev = sd(temp, na.rm=T))
summary_temp

## Other functions that can be used in a summary
## mean(): the mean or average
## sd(): the standard deviation, which is a measure of spread
## min() and max(): the minimum and maximum values respectively
## IQR(): Interquartile range
## sum(): the sum
## n(): a count of the number of rows/observations in each group


#########################################
## 10. Group Rows Using group_by
#########################################
## mean and standard deviation of temperatures in each month
summary_monthly_temp <- weather %>%
  group_by(month) %>%
  summarize(mean = mean(temp, na.rm=T),
            std_dev = sd(temp, na.rm=T))
summary_monthly_temp

## remove group stucture meta data using ungroup()
summary_monthly_temp <- weather %>%
  group_by(month) %>%
  ungroup() %>%
  summarize(mean = mean(temp, na.rm=T),
            std_dev = sd(temp, na.rm=T))
summary_monthly_temp

## get a sense for how many flights departed each of the 
## three airports in New York City
by_origin <- flights %>%
  group_by(origin) %>%
  summarize(count = n())
by_origin

## number of flights leaving each of the three New York City airports for each month
by_origin_monthly <- flights %>% 
  group_by(origin, month) %>% 
  summarize(count = n())
by_origin_monthly


## reverse grouping order
by_monthly_origin <- flights %>% 
  group_by(month, origin) %>% 
  summarize(count = n())
by_monthly_origin

## if you want to group_by() two or more variables, you should include all these variables in a 
## single group_by() function call.

## Produce mean & SD of temp for each day in nyc2013
summary_daily_temp <- weather %>%
  group_by(day) %>%
  summarize(mean = mean(temp, na.rm=T),
            std_dev = sd(temp, na.rm=T))
summary_daily_temp

## How could we identify how many flights left each of the three airports for each  carrier
summary_origin_carrier <- flights %>%
  group_by(origin, carrier) %>%
  summarize(count = n())
summary_origin_carrier

## filter() picks out rows from the original data set without modifying them, 
## whereas group_by %>% summarize computes summaries of numerical variables, 
## and hence reports new values.


#########################################
## 11. Creat/Change Variables with mutate
#########################################
## the mutate() command outputs a new data frame consisting of the original data frame with the 
## addition of the new variable gain, which then replaces the original flights data frame.
flights1 <- flights %>%
  mutate(gain = dep_delay - arr_delay)
flights1

flights1 %>%
  select(dep_delay, arr_delay, gain) %>%
  slice(1:5)

## recreate summary function
gain_summary <- flights1 %>%
  summarize(
    min = min(gain, na.rm = TRUE),
    q1 = quantile(gain, 0.25, na.rm = TRUE),
    median = quantile(gain, 0.5, na.rm = TRUE),
    q3 = quantile(gain, 0.75, na.rm = TRUE),
    max = max(gain, na.rm = TRUE),
    mean = mean(gain, na.rm = TRUE),
    sd = sd(gain, na.rm = TRUE),
    missing = sum(is.na(gain))
  )
gain_summary

ggplot(data = flights1, mapping = aes(x = gain)) +
  geom_histogram(color = "white", fill = "skyblue", bins = 20)

## We can also create multiple columns at once and even refer to columns that were just created in a new column.
flights1 <- flights %>%
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours
  )
flights1 %>%
  select(gain, hours, gain_per_hour) %>%
  slice(1:5)


#########################################
## 12. Reorder Dataframe using arrange()
#########################################
freq_dest <- flights %>% 
  group_by(dest) %>% 
  summarize(num_flights = n())
freq_dest

freq_dest %>% 
  arrange(num_flights)

## arrange in descending order
freq_dest %>% 
  arrange(desc(num_flights))


#########################################
## 13. Joining Data Frames
#########################################
head(airlines)

## In both flights and airlines, the key variable we want to join/merge/match 
## the two data frames with has the same name in both data sets: carriers
flights_joined <- flights %>%
  inner_join(airlines, by = "carrier")
names(flights)
names(flights_joined)
flights_joined %>% select(flight, carrier, name) 

## Joining by 'key' variables with different names
head(airports)

flights1 %>%
  inner_join(airports, by = c("dest" = "faa"))

## compute the number of flights from NYC to each destination
named_dests <- flights %>%
  group_by(dest) %>%
  summarize(num_flights = n()) %>%
  arrange(desc(num_flights)) %>%
  inner_join(airports, by = c("dest" = "faa")) %>%
  rename(airport_name = name)
named_dests

## Joining multiple 'key' variables
flights_weather_joined <- flights %>%
  inner_join(weather, by = c("year", "month", "day", "hour", "origin"))
head(flights_weather_joined)#[,c(1:4,10:11,22:32)])

#########################################
## 14. Other Verbs
#########################################
glimpse(flights)
names(flights)

flights1 %>%
  select(carrier, flight)

## remove year variable since it is constant
flights_no_year <- flights %>% 
  select(-year)
names(flights_no_year)

## specify a ranges of columns
flight_arr_times <- flights %>% 
  select(month:dep_time, arr_time:sched_arr_time)
flight_arr_times

## The select function can also be used to reorder columns in 
## combination with the  everything() helper function
## everything() picks up all remaining variables
flights_reorder <- flights %>% 
  select(month:day, hour:time_hour, everything())
names(flights_reorder)

## starts_with, ends_with, and contains
flights_begin_a <- flights %>% 
  select(starts_with("a"))
head(flights_begin_a)

flights_delays <- flights %>% 
  select(ends_with("delay"))
head(flights_delays)

flights_time <- flights %>% 
  select(contains("time"))
head(flights_time)

## Rename variables with rename()
flights_time <- flights %>% 
  select(contains("time")) %>% 
  rename(departure_time = dep_time,
         arrival_time = arr_time)
names(flights_time)

## Find the top number of values using top_n
## returns most frequent num_flights
named_dests %>%
  top_n(n = 10, wt = num_flights) %>%
  arrange(desc(num))

ten_freq_dests <- flights %>%
  group_by(dest) %>%
  summarize(num_flights = n()) %>%
  arrange(desc(num_flights)) %>%
  top_n(n = 10) 
ten_freq_dests







