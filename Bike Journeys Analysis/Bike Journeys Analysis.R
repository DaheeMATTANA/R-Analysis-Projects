# Credit : superdatascience.com


library(dplyr)


# ----- Data
data_dplyr <- read.csv("Bike Journey Data.csv")
data_base <- read.csv("Bike Journey Data.csv")


# ----- Encoding Factors
str(data_base)
# dplyr version
data_dplyr <- data_dplyr %>% 
  mutate(trip_id = as.factor(trip_id),
         user_id = as.factor(user_id),
         location = as.factor(location))

# base R version
data_base$trip_id <- as.factor(data_base$trip_id)
data_base$user_id <- as.factor(data_base$user_id)
data_base$location <- as.factor(data_base$location)


# ----- Longest trip taken
# dplyr version
data_dplyr %>% 
  summarise(longest_trip = max(time))

# base R version
max(data_base$time)


# ----- Longest trip by location
# dplyr version
data_dplyr %>% 
  group_by(location) %>% 
  summarise(longest_trip = max(time))

# base R version
aggregate(list(time = data_base$time),
          by = list(location = data_base$location),
          FUN = max)


# ----- New column with average time for each location
# dplyr version
# timing the procedure
start_dplyr <- Sys.time()
data_dplyr <- data_dplyr %>% 
  group_by(location) %>% 
  mutate(avg_time_location = mean(time))
time_dplyr <- Sys.time() - start_dplyr
# Time took for calcul
time_dplyr
# New dataframe
head(data_dplyr)

# base R version
start_base <- Sys.time()
avg_time_location <- aggregate(list(avg_time_location = data_base$time),
                               by = list(location = data_base$location),
                               FUN = mean)
data_base <- merge(data_base, avg_time_location, by = "location")
time_base <- Sys.time() - start_base
# Time Took for calcul
time_base
# New dataframe
head(data_base)

# Calculation time between the two
time_dplyr
time_base


# ----- New data frame with only riders who have taken over 100 trips
data_dplyr_over_100 <- data_dplyr %>% 
  group_by(user_id) %>% 
  mutate(number_rides = n()) %>% 
  filter(number_rides > 100)

# New dataframe
head(data_dplyr_over_100)


# ----- For riders who have taken over 100 trips, most popular location?
over_100_most_popular_location <- data_dplyr_over_100 %>% 
  group_by(location) %>% 
  summarise(num_rides = n()) %>% 
  arrange(desc(num_rides))

# Most popular location
head(over_100_most_popular_location)