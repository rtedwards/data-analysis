### Data Analysis Week 1
### Author: Robert Edwards


##############################
## 1.  Getting Started
##############################
library(ggplot2)
library(nycflights13)

print("Well done!  You've loaded the libraries")


##############################
## 2.  Viewing the Data
##############################
head(flights, n=3)  # prints first 'n=3' rows of the flights data set
dim(flights)        # check size of data set

## subset flights from Alaska Airlines
Alaska <- flights[flights$carrier == "AS", ]
head(Alaska, n=5)
dim(Alaska)


##############################
## 3.  Scatterplots
##############################
## Plot relationship b/t departure and arrival delays
## aes() relates the plot aesthetics
## dep_delay maps to the x coordinate
## arr_delay maps to the y coordinate
## geom_point() specifies that the geometric objects to add to the plot are the points
ggplot(data = Alaska, mapping = aes(x=dep_delay, y=arr_delay)) +
  geom_point() +
  labs(x="Departure delay (minutes)", 
       y= "Arrival delay (minutes)",
       title="Alaska Airlines flights leaving NYC in 2013")


### OVERPLOTTING ###
## Overplotting is when many points are plotted very close to one another
## making it impossible to distinguish the number of points
## 1. adjust the transparency of the plotted points using the alpha argument
ggplot(data = Alaska, mapping = aes(x=dep_delay, y=arr_delay)) +
  geom_point(alpha = 0.2) +
  labs(x="Departure delay (minutes)", 
       y= "Arrival delay (minutes)",
       title="Alaska Airlines flights leaving NYC in 2013")


## 2. jitter the points using the geom_jitter function
## The idea behind jittering is that each point is randomly moved, 
## or nudged, slightly from its original position in such a way that 
## clusters of points with the same coordinates can be observed, 
## instead of being plotted on top of one another
jitter.example <- matrix(0, nrow=10, ncol=2)
jitter.example

## ggplot only works with data frames
jitter.example <- as.data.frame(jitter.example)

ggplot(data = jitter.example, mapping=aes(x=V1, y=V2)) +
  geom_point()

ggplot(data=jitter.example, mapping=aes(x=V1, y=V2)) +
  geom_jitter(width=0.1, height=0.1)

ggplot(data=Alaska, mapping=aes(x=dep_delay, y=arr_delay)) +
  geom_jitter(width=30, height=30) +
  labs(x="Departure delay (minutes)", 
       y= "Arrival delay (minutes)",
       title="Alaska Airlines flights leaving NYC in 2013")


##############################
## 4.  Histograms
##############################
head(weather, n=3)

ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram()

## Specify number of bins
ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram(bins = 60, color = "red", fill = "blue")

## Specify binwidth
ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram(binwidth = 1.5, color = "turquoise", fill = "cyan") +
  labs(x = "Temperature (Hourly)",
       y = "count",
       title = "Hourly Temperatures from NYC in 2013")


##############################
## 5.  Boxplots
##############################
summary(weather$temp)

ggplot( data = weather, mapping = aes(x = factor(month), y = temp)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "Month", y = "Temperature (Hourly)",
       title = "Hourly temperatures from NYC in 2013 by month")  +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

## scale_x_discrete renames the labels of the boxplots

##############################
## 6.  Barplots
##############################
carrier.freq <- table(flights$carrier)
carrier.freq <- as.data.frame(carrier.freq)
colnames(carrier.freq) <- c("carrier", "number")

ggplot(data = carrier.freq, mapping = aes(x = carrier, y = number)) +
  geom_col() +
  labs(x = "Carrier",
       y = "Count",
       title = "Carriers who flew out of NYC in 2013")

carrier.origin <- table(flights$origin, flights$carrier)
carrier.origin <- as.data.frame(carrier.origin)
colnames(carrier.origin) <- c("origin", "carrier", "number")

### STACKED BARPLOT ###
ggplot(data = carrier.origin, mapping = aes(x = carrier, y = number, fill = origin)) +
  geom_col() +
  labs(x = "Carrier", 
       y = "Count",
       title = "Carriers who flew out of New York City in 2013") 

### SIDE-BY-SIDE (DODGED) BARPLOT ###
ggplot(data = carrier.origin, mapping = aes(x = carrier, y = number, fill = origin)) +
  geom_col(position = "dodge") +
  labs(x = "Carrier", y = "Count",
       title = "Carriers who flew out of New York City in 2013")

### FACETED BARPLOT ###
## provide an easier way to compare the carrier distributions by origin
ggplot(data = carrier.origin, mapping = aes(x = carrier, y = number, fill = origin)) +
  geom_col() +
  facet_wrap(~ origin, ncol = 1) +
  labs(x = "Carrier", y = "Count",
       title = "Carriers who flew out of New York City in 2013")


##############################
## 7.  Linegraphs
##############################
Newark.Jan <- weather[weather$origin == "EWR" & weather$month == 1, ]
head(Newark.Jan, n=5)
dim(Newark.Jan)

ggplot(data = Newark.Jan, mapping = aes(x = time_hour, y = temp)) +
  geom_line() +
  labs(x = "Time (Hours)", y = "Temperature",
       title = "Hourly Temperature at Newark Airport in January 2013")


##############################
## 8.  Further Tasks
##############################

## 1. From the flights data set, subset the data for the airline carrier
## JetBlue Airways and produce a scatterplot of their departure delays 
## against arrival delays using ggplot. Interpret the scatterplot.
JetBlue <- flights[flights$carrier == "B6", ]
ggplot(data = JetBlue, mapping = aes(x = dep_delay, y = arr_delay)) + 
  geom_point(alpha = 0.1) +
  labs(x = "Departure Delay (minutes)",
       y = "Arrival Delay (minutes)",
       title = "JetBlue Airlines flights leaving NYC in 2013")

## 2. Produce a histogram of the hourly temperature from Newark Liberty
## International (EWR) Airport in 2013 using ggplot. How does the 
## temperature distribution compare with that from all airports in 
## New York City in 2013?
EWR <- weather[weather$origin == "EWR" && weather$temp, ]
ggplot( data = weather, mapping = aes(x = temp)) +
  geom_histogram(fill = "steelblue", color = "white") +
  labs(x = "Hour", y = "Temperature (Hourly)",
       title = "Hourly temperatures from NYC in 2013 by month")


## 3. For John F. Kennedy Airport, produce boxplots (using a single  
## ggplot command) of the hourly temperature for the months May, June, 
## July, August and September. How does the hourly temperature change 
## during this period?
JFK <- weather[weather$origin == "JFK" & weather$month == 5:9,]
ggplot( data = weather, mapping = aes(x = factor(hour), y = month)) +
  geom_boxplot( fill = "steelblue") + 
  labs(x = "Hour", y = "Month",
       title = "Hourly Temperature May - Sept at JFK Airport")


## 4. Take a look at the mtcars data set within the datasets library 
## relating to data extracted from the 1974 Motor Trend US magazine. 
## Using ggplot, produce a faceted barplot of the categorical variables 
## relating to the number of cylinders (cyl) and the automobiles 
## transmission (am). Interpret the barplot




## 5.  Produce a linegraph of the hourly temperature at LAGuardia (LGA) 
## Airport for the month of October 2013. Interpret the linegraph.



