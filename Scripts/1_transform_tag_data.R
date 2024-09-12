# Load packages

library(tidyverse)
library(dplyr)
library(lme4)
library(MASS)
library(stats)
library(Matrix)
library(MatrixModels)

# Load tag data

tags <- read.csv("Input/ABMI_Influence_of_wildfire_smoke_on_avian_singing_rates_tag_report.csv")


# Select relevant columns only

tags <- tags %>%
  select(location,recording_date_time,species_code,aru_task_status,individual_order,abundance,vocalization)

# Combine columns 1 and 2 to create recording name file

tags$recording_name <- paste(tags$location, tags$recording_date_time, sep = " ")

# Remove location and date columns

tags <-tags %>%
  select(recording_name,species_code,aru_task_status,individual_order,abundance,vocalization)

# Remove all call tags

tags <- tags %>%
  mutate(
    individual_order = ifelse(vocalization == "Call", 0, individual_order),
    abundance = ifelse(vocalization == "Call", 0, abundance)
  )

# Replace values in "individual_order" and "abundance" with zero where "species_code" is "NONE"

tags <- tags %>%
  mutate(
    individual_order = ifelse(species_code == "NONE", 0, individual_order),
    abundance = ifelse(species_code == "NONE", 0, abundance)
  )

# Change all "NONE" species codes to "WTSP"

tags <- tags %>%
  mutate(species_code = ifelse(species_code == "NONE", "WTSP", species_code))





### Summarize number of individuals and abundnace of songs in each recordings ###


# Summarize abundance column by recording_name
summary_abundance <- tags %>%
  group_by(recording_name) %>%
  summarize(total_abundance = sum(abundance))

# Count unique individual_order values by recording_name

summary_individual_order <- tags %>%
  group_by(recording_name) %>%
  summarize(unique_order_count = n_distinct(individual_order[individual_order != 0]))

# Merge the two summaries based on recording_name
tags <- merge(summary_abundance, summary_individual_order, by = "recording_name")

# Replace "your_column" with the actual column name you want to split
tags <- tags %>%
  separate(recording_name, into = c("location", "date", "time"), sep = " ")

# Combine location and date columns in a column called "recording name"

tags <- tags %>%
  mutate(recording_name = paste(location, date, sep = " "))

# Remove time column

tags <-tags %>%
  select(recording_name,location,total_abundance,unique_order_count)

# Creating "mean singing rate column"

tags <- tags %>%
  mutate(mean_singing_rate = total_abundance / unique_order_count)

# Change all NaN values to 0

tags <- tags %>%
  mutate(mean_singing_rate = ifelse(is.nan(mean_singing_rate), 0, mean_singing_rate))

# Reduce the number of decimal places in "mean_singing_rate" to 2

tags <- tags %>%
  mutate(mean_singing_rate = round(mean_singing_rate, 2))

# Save the files as "tags"

write.csv(tags,"Output/tags.csv")








### FILTER DATA FOR T-TEST ###

# Load data

tags <- read.csv("Output/tags.csv")

# Filter out "smoky" day recordings

smoky <- tags %>%
  filter(grepl("21$|11$", recording_name))

# Filter out "non-smoky" day recordings

non_smoky <- tags %>%
  filter(grepl("15$|24$|9$|13$", recording_name))

# Clean smoky and non-smoky data frames

smoky <- smoky %>%
  select(recording_name,mean_singing_rate,unique_order_count)

non_smoky <- non_smoky %>%
  select(recording_name,mean_singing_rate,unique_order_count)

# Change name of "mean_singing_rate" for smoky day recordings

smoky <- smoky %>%
  rename(mean_SR_smoky = mean_singing_rate)

# Change name of "mean_singing_rate" for smoky day recordings

non_smoky <- non_smoky %>%
  rename(mean_SR_non_smoky = mean_singing_rate)

# Change name of "mean_singing_rate" for smoky day recordings

smoky <- smoky %>%
  rename(num_WTSP_smoky = unique_order_count)

# Change name of "mean_singing_rate" for smoky day recordings

non_smoky <- non_smoky %>%
  rename(num_WTSP_non_smoky = unique_order_count)

# Split the first column of both files so that they can be merged based on location

smoky <- smoky %>%
  separate(recording_name, into = c("Location", "Date"), sep = " ")

non_smoky <- non_smoky %>%
  separate(recording_name, into = c("Location", "Date"), sep = " ")

# Remove time column

smoky <- smoky %>%
  select(Location, mean_SR_smoky, num_WTSP_smoky)

non_smoky <- non_smoky %>%
  select(Location, mean_SR_non_smoky, num_WTSP_non_smoky)

# Merge smoky and non-smoky data frames based on location

dat1 <- merge(smoky, non_smoky, by = "Location")

# Remove 1266-SE (started transcribing but decided too remove due to too much uncertainty in trancription)

dat1 <- dat1 %>% filter(Location != "1266-SE")

# Remove all sites in which there is a singing rate of zero in both the smoky and non-smoky recordings

dat1 <- dat1[!(dat1$mean_SR_smoky == 0 & dat1$mean_SR_non_smoky == 0), ]

# Save transformed data

write.csv(dat1, "Output/ttest_data.csv")














