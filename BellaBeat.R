---
  title: "Bella Beat Case Study - Capstone Project, Google Data Analytics Course"
author: "Blessing Ideh"
date: "`r Sys.Date()`"
output: html_document
---


# Loading the necessary packages
library(tidyverse)
library(lubridate)
library(scales)


# Importing files
daily_activity <-  read.csv("dailyActivity_merged.csv")
daily_steps <-  read.csv("dailySteps_merged.csv")
hourly_steps <- read.csv("hourlySteps_merged.csv")
sleep_day <- read.csv("sleepDay_merged.csv")


# Viewing the data
str(daily_activity)
head(daily_activity)
str(sleep_day)
head(sleep_day)



# Data Cleaning and Transformation

# The data is cleaned and transformed by employing different methods to ensure that the data is cleaned, consistent and ready for analysis. Inconsistences are removed, data types are checked to confirmed that they are of the right type and errors, where present dealth with.

# Converting date columns from string to date data type

# The date columns in the data are of the character data type. They are subsequently converted to the date data type.

daily_activity <- mutate(daily_activity, ActivityDate = as.Date(daily_activity$ActivityDate, format = "%m/%d/%Y"))
daily_steps <- mutate(daily_steps, ActivityDay = mdy(daily_steps$ActivityDay))
hourly_steps <- mutate(hourly_steps, ActivityHour = format(mdy_hms(as.character(ActivityHour, tz = "UTC")), "%H:%S"))
sleep_day <- mutate(sleep_day, SleepDay = as.Date(sleep_day$SleepDay, format = "%m/%d/%Y"))

# Confirming if they are converted correctly
class(daily_activity$ActivityDate)
class(daily_steps$ActivityDay)


# Renaming Columns

# The date columns are renamed to "Date" and "Hour" for consistency and readability

daily_activity <-  rename(daily_activity, Date = ActivityDate)
daily_steps <-  rename(daily_steps, Date = ActivityDay)
hourly_steps <-  rename(hourly_steps, Time = ActivityHour)
sleep_day <-  rename(sleep_day, Date = SleepDay)



# Data Analysis

# Checking for the number of respondents in each data

# To check for the number of respondents in each data set by counting distinct IDs
n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)
n_distinct(daily_steps$Id)
n_distinct(hourly_steps$Id)


# Summary statistics of the sleep data
sleep_day %>%
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()



# Analyzing the difference between total time in bed and total time asleep

# Inserting a new column, Difference to compute the difference between total time in bed and total time asleep in the sleep_day data
sleep_day <- mutate(sleep_day, Difference = TotalTimeInBed - TotalMinutesAsleep)

# Arranging by descending order to view the values on the higher end
head(sleep_day %>% 
       arrange(desc(Difference)))

# Arranging by ascending order to view the values on the lower end
head(sleep_day %>% 
       arrange(Difference))


# Counting and displaying rows where the difference is greater than or equal to 30 minutes

# Displaying differences greater than or equal to 30 minutes
head(sleep_day %>% 
       filter(Difference >= 30))

# Counting differences greater than or equal to 30 minutes
sleep_day %>%
  summarise(sum(Difference >= 30))

# Counting and displaying rows where the difference is less than 30 minutes

# Displaying differences lesser than 30 minutes
head(sleep_day %>%
       filter(Difference < 30))

# Counting differences lesser than 30 minutes
sleep_day %>%
  summarise(sum(Difference < 30))

# Calculating the percentage
more_than_or_equal_to_30 <- 171 /413
less_than_30 <-  242 /413

# Rounding to 2 decimal places
percent(more_than_or_equal_to_30, accuracy = 1)
percent(less_than_30, accuracy = 1)

# This can be visualized in a graph that shows the difference in respondents that spend more than or exactly 30 minutes before sleeping against those who spend less than 30 minutes before falling.

# What  time of the day are steps taken higher?
head(hourly_steps %>%
       group_by(Time) %>%
       summarise(sum(StepTotal)),13)

# Are respondents meeting the recommended 10,000 steps daily

# Checking for entries where steps are more than or equal to 10,000
head(daily_activity %>%
       select(Id,Date, TotalSteps, Calories) %>% 
       group_by(Id) %>% 
       filter(TotalSteps >= 10000),10)

#Checking for entries where steps are less than 10,000
head(daily_activity %>% 
       select(Id,Date, TotalSteps) %>% 
       filter(TotalSteps < 10000), 10)

# Are respondents meeting the average 8 hours of sleep daily?

#Checking the time period in the data
range(sleep_day$Date)

# Viewing the no of respondents, days and total entries in the data
n_distinct(sleep_day$Id)
n_distinct(sleep_day$Date)
n_distinct(sleep_day)

# Number of hours respondents sleep daily

# inserting a new column, HoursSlept to show how many hours respondents are sleeping
sleep_day <- mutate(sleep_day, HoursSlept = round(TotalMinutesAsleep/60))
head(sleep_day)


# Checking for Respondents who sleep exactly or more than 8 hours daily
by_id <- sleep_day %>%
  group_by(Id) %>%
  filter(HoursSlept >= 8) 

head(by_id,10)

# Calculating the sum of sleep on days when respondents slept more than 8 hours and grouping by Id
sleep_hours <- sleep_day %>%
  group_by(Id) %>%
  summarise(Days_sleepHours_more_than_or_8 = sum(HoursSlept >= 8)) 

head(sleep_hours)

# How many respondents sleep more than 8 hours of the entire time period

# Filtering
sleep_hours %>%
  select(Id,Days_sleepHours_more_than_or_8) %>% 
  filter(Days_sleepHours_more_than_or_8 >= 15)


# Summary statistics of the Activity data

# The minutes are grouped into two, active and sedentary with the active grouped even further into very active, fairly active and lightly active minutes showing different level of intensities.


# Summary statistics of the minutes
daily_activity %>%
  select(VeryActiveMinutes,
         FairlyActiveMinutes,
         LightlyActiveMinutes,
         SedentaryMinutes) %>%
  summary()



# Creating a new column, Active minutes to combine the total, fairly and lightly active minutes together
daily_activity <-  mutate(daily_activity, ActiveMinutes = VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes)

# Summary statistics of Active and Sedentary minutes
daily_activity %>%
  select(SedentaryMinutes,ActiveMinutes) %>%
  summary()


# Visualization

# Comparison of Total time in bed and total time asleep
ggplot(sleep_day, aes(y = TotalTimeInBed, x = TotalMinutesAsleep)) + 
  geom_jitter() +theme_grey() + labs(x= "Time Asleep (mins)", y = "Total Time in bed (mins)", title = "Relationship between time in bed and time asleep", subtitle =  "Are respondents falling asleep as soon as they they hit the sack?") + theme_light()


# Difference between total time in bed and total time asleep
ggplot(sleep_day) +
  geom_bar(aes(y = Difference >= 30), fill = "darkslateblue") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + guides(fill = "none")  +
  labs(y = "Conditions",x  ="Difference between time in bed and time asleep (mins)",  title = "Difference between total time in bed and total time asleep",subtitle = "True if respondents spend 30 or more minutes before falling asleep, false if they spend less") +
  theme_minimal()


# Visualizing the most busy hours by steps taken
ggplot(data= hourly_steps, aes(x= StepTotal, y = Time )) +
  geom_jitter() + labs(x = "Total steps taken", title = "Most Busy Hours of the Day", subtitle = "What  time of the day are steps taken higher?") +
  theme_light()


# Minutes Intensity
Intensity <- daily_activity %>%
  pivot_longer(cols = c("VeryActiveMinutes", "LightlyActiveMinutes", "FairlyActiveMinutes", "SedentaryMinutes"), names_to = "Intensities", values_to = "Count")

ggplot(Intensity) +
  geom_bar(aes(x = Count, y = (reorder(Intensities, Count))), fill = "darkslateblue", stat = "Identity") +
  labs(title = "Intensities", subtitle = "Classsification of minutes by how intense, the activities respondents engage in are.") + labs(x = "Sum of minutes", y = "Minute type") + theme_minimal() + theme(plot.title = element_text(size = 23, color = "black")) +
  scale_y_discrete(labels = c("Fairly Active", "Lightly Active", "Very Active", "Sedentary")) +
  theme(panel.grid.major.x = element_line(size = .1, color = "azure4"))


# Breakdown of how time spent is spent by activity intensities

#Sedentary Minutes vs Total Steps
ggplot(daily_activity, aes(SedentaryMinutes, TotalSteps)) + 
  geom_jitter() +
  labs(x= "Sedentary Minutes", y = "Total Steps",title = "Sedentary Minutes vs Total Steps", subtitle = "Respondents take lesser steps when they're inactive") + theme_light()


# Active Minutes vs Total Steps
ggplot(daily_activity, aes(ActiveMinutes, TotalSteps)) + 
  geom_jitter() +
  labs(x= "Active Minutes", y = "Total Steps",title = "Active Minutes vs Total Steps", subtitle = "Respondents take more steps when their bodies are in active states") + theme_light()


