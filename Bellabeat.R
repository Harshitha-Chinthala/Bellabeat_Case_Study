#Load necessary libraries
library("tidyverse")
library("lubridate")
library("janitor")
library("ggplot2")

#Set working directory
setwd("C:/Users/chint/OneDrive/Documents/Bellabeat_Case_Study")

#Load datasets
daily_activity <- read_csv("dailyActivity_merged.csv") %>%
  clean_names()

sleep_day <- read_csv("sleepDay_merged.csv") %>%
  clean_names()

heartrate_seconds <- read_csv("heartrate_seconds_merged.csv") %>%
  clean_names()

#Explore the data
glimpse(daily_activity)
summary(daily_activity)

glimpse(sleep_day)
summary(sleep_day)

glimpse(heartrate_seconds)
summary(heartrate_seconds)

#Convert date columns to proper date format
daily_activity <- daily_activity %>%
  mutate(activity_date = mdy(activity_date))

sleep_day <- sleep_day %>%
  rename(date = sleep_day) %>%
  mutate(date = as_date(parse_date_time(date, orders = "mdY IMS p")))

heartrate_seconds <- heartrate_seconds %>%
  mutate(time = parse_date_time(time, orders = c("mdY HMS p","m/d/Y H:M:S p","m/d/Y HM","m-d-y HM","m-d-Y HMS")),
         date = as_date(time))

#Remove Duplicates
daily_activity <- distinct(daily_activity)
sleep_day <- distinct(sleep_day)
heartrate_seconds <- distinct(heartrate_seconds)

#Check for missing values
colSums(is.na(daily_activity))
colSums(is.na(sleep_day))
colSums(is.na(heartrate_seconds))

#Aggregate heart rate data by day
daily_heart <- heartrate_seconds %>%
  group_by(id,date) %>%
  summarise(avg_heart_rate = mean(value,na.rm = TRUE), .groups="drop")

glimpse(daily_heart)
summary(daily_heart)

#Merge all cleaned data
combined_data <- daily_activity %>%
  left_join(sleep_day, by = c("id","activity_date" = "date")) %>%
  left_join(daily_heart, by = c("id","activity_date" = "date"))

#Add a weekday column
combined_data <- combined_data %>%
  mutate(weekday = weekdays(activity_date))

glimpse(combined_data)
summary(combined_data)

#Analyzing summary statistics
combined_data %>%
  summarize(avg_steps = mean(total_steps, na.rm = TRUE),
            avg_calories = mean(calories, na.rm = TRUE),
            avg_sleep = mean(total_minutes_asleep, na.rm = TRUE),
            avg_heart_rate = mean(avg_heart_rate, na.rm = TRUE))

#Steps over Time
ggplot(data = combined_data) +
  geom_smooth(mapping = aes(x = activity_date,y = total_steps),color = "steelblue")+
  labs(title = "Total Steps Over Time",x = "Date",y = "Total Steps")

#Calories over Time
ggplot(data = combined_data) +
  geom_smooth(mapping = aes(x = activity_date,y = calories),color = "darkgreen") +
  labs(title = "Calories Over Time",x = "Date", y = "Calories")

#Calories vs Steps
ggplot(data = combined_data) +
  geom_point(mapping = aes(x = total_steps, y = calories),color = "tomato") +
  labs(title = "Calories Burned vs Steps Takes", x = "Total Steps",y = "Calories")

#Average Steps by Weekday
weekday_steps <- combined_data %>%
  mutate(weekday = factor(weekday,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))) %>% 
  group_by(weekday) %>%
  summarise(avg_steps = mean(total_steps,na.rm = TRUE), .groups = "drop")

ggplot(data = weekday_steps) +
  geom_col(mapping = aes(x = weekday,y = avg_steps),fill = "mediumpurple") +
  labs(title = "Avearge Steps by Weekday", x = "Weekday", y = "Average Steps")

#Distribution of Total Steps
ggplot(data = combined_data) +
  geom_histogram(mapping = aes(x = total_steps),binwidth = 1000, fill = "skyblue",color = "black") +
  labs(title = "Distribution of Total Steps", x = "Steps", y = "Count")

#Sleep Duration Distribution
ggplot(data = combined_data) +
  geom_histogram(mapping = aes(x = total_minutes_asleep),binwidth = 50, fill = "forestgreen", color = "black") +
  labs(title = "Sleep Duration Distribution", x = "Minutes Asleep",y = "Number of Days")

#Sleep vs Time in Bed
ggplot(data = combined_data) +
  geom_point(mapping = aes(x = total_time_in_bed,y = total_minutes_asleep),color = "deepskyblue") +
  labs(title = "Time in Bed vs Actual Sleep", x = "Time in Bed(min)", y = "Minutes Asleep")

#Sleep vs Steps
ggplot(data = combined_data) +
  geom_point(mapping = aes(x = total_minutes_asleep, y = total_steps),color = "plum") +
  labs(title = "Sleep Distribution vs Total Steps", x = "Minutes Asleep", y = "Total Steps")

#Average Heart Rate by Weekday
weekday_hr <- combined_data %>%
  mutate(weekday = factor(weekday,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))) %>%
  group_by(weekday) %>%
  summarise(avg_hr = mean(avg_heart_rate,na.rm = TRUE), .groups = "drop")

ggplot(data = weekday_hr) +
  geom_col(mapping = aes(x = weekday,y = avg_hr), fill = "orange") +
  labs(title = "Average Heart Rate by Weekday", x = "Weekday",y = "Average Heart Rate")

#Heart Rate vs Steps
ggplot(data = combined_data) +
  geom_point(mapping = aes(x = avg_heart_rate,y = total_steps),color = "red") +
  labs(title = "Heart Rate vs Steps", x = "Average Heart Rate",y = "Total Steps")

#Time in Bed vs Calories Burned
ggplot(data = combined_data) +
  geom_point(mapping = aes(x = total_time_in_bed, y = calories),color = "darkblue") +
  labs(title = "Total Time in Bed vs Calories Burned", x = "Time in Bed(min)",y = "Calories")

#Boxplot: Steps per Weekday
combined_data <- combined_data %>%
  mutate(weekday = factor(weekday,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")))

ggplot(data = combined_data) +
  geom_boxplot(mapping = aes(x = weekday, y = total_steps),fill = "gold",outlier.color = "red",outlier.shape = 1) +
  labs(title = "Distribution of Steps per Weekday", x = "Weekday",y = "Total Steps")

#Export the cleaned dataset
write_csv(combined_data,"cleaned_combined_data.csv")
