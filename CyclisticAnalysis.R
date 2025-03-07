library(tidyverse)
library(ggplot2)


## step 1 data
q1_2019 <- read_csv("Divvy Trips Q1_2019 (Cleaned).csv")
q1_2020 <- read_csv("Divvy Trips Q1_2020 (Cleaned).csv")

## clean all of the data and make it consistent with q1_2020

(q1_2019 <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
))

q1_2019 <- mutate(q1_2019, ride_id = as.character(ride_id)
                  ,rideable_type = as.character(rideable_type))

all_trips <- bind_rows(q1_2019, q1_2020)#, q3_2019)#, q4_2019, q1_2020)

all_trips <- all_trips %>%
  select(-c(day_of_week))


rm(q1_2019,q1_2020)
# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

## Eliminate abnormally high ride_lengths over 86,400 seconds (amount of time in a day) ##
all_trips_v2 <- all_trips[!(all_trips$ride_length>86400),]

# Should have done this earlier, but
# remove rows if member_casual is NA (data not available)
# This should be okay since we still have over 200,000 data points for analysis
all_trips_v2 <- all_trips_v2[!(is.na(all_trips_v2$member_casual)),]

## Analysis ##

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = sd)
all_trips = 0

# 5 Number Summary for Ride Length statistics for Subscriber vs. Customer
summary(all_trips_v2 %>% filter(all_trips_v2$member_casual == "Subscriber") %>% select(ride_length))
summary(all_trips_v2 %>% filter(all_trips_v2$member_casual == "Customer") %>% select(ride_length))

#Compare Subscriber vs. Customer average ride duration per day of week
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)

## Results ##
"
   all_trips_v2$member_casual all_trips_v2$day_of_week all_trips_v2$ride_length
1                    Customer                   Friday                2028.2774
2                  Subscriber                   Friday                 667.3852
3                    Customer                   Monday                1912.4960
4                  Subscriber                   Monday                 664.4115
5                    Customer                 Saturday                2254.1913
6                  Subscriber                 Saturday                 764.6247
7                    Customer                   Sunday                2115.6370
8                  Subscriber                   Sunday                 713.5414
9                    Customer                 Thursday                2042.1777
10                 Subscriber                 Thursday                 658.0234
11                   Customer                  Tuesday                2184.3286
12                 Subscriber                  Tuesday                 677.4490
13                   Customer                Wednesday                2081.8376
14                 Subscriber                Wednesday                 667.3358

- From this analysis, we can see that Customers, while having less rides, often ride
their bikes longer than average compared to subscribers. This may be in part because of 
planned usage, where customers will use Cyclistic's service if they really need to for
longer trips, compared to subscribers, who use it more casually and liberally.
"

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)

# analyze ridership data by type and weekday
View(all_trips_v2 %>%
       mutate(weekday = wday(started_at, label = TRUE)) %>% 
       group_by(member_casual, weekday) %>%  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% # calculates the averageduration
       arrange(member_casual, weekday))



# Let's visualize the number of rides by rider type
# (result saved in main directory)
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  labs(title = "Number of Rides vs. Day of the Week",
       caption=paste0("Data from 2019 Q1 and 2020 Q1")) +
  geom_col(position = "dodge")

ggsave('divvy_analysis_chart_weekday_vs_numofrides_membercasual.png',
       width=16,
       height=8)
# Let's visualize the length of rides by rider type
# (result saved in main directory)
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  labs(title = "Avg. Length of Ride vs. Day of the Week",
       caption=paste0("Data from 2019 Q1 and 2020 Q1")) +
  geom_col(position = "dodge")

ggsave('divvy_analysis_chart_weekday_vs_avgduration_membercasual.png',
       width=16,
       height=8)


## Post-Analysis 1
"
Upon looking in the graphs (especially the combined one), we can see the trend identified above.
Customers have higher average ride lengths (about ~2200 seconds or 36m, 40s) compared to subscribers
(about ~700 seconds or 11m, 40s). This infers that customers are on average likely to ride their Cyclistic
bike almost 3x the length as subscribers. Therefore, if the company wants to target less consistent, but 
longer-duration riders, they should focus on attracting more non-subscribing customers. This will be beneficial
if Cyclistic bills per minute/timewise compared to upfront flat-rate.

On the other hand, the number of rides chart show that Subscribers have higher 
"


# Final File will be in the .RMD file