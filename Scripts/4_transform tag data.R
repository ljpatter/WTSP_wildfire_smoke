# Load packages
install.packages("usethis")
library(tidyverse)
library(dplyr)
library(lme4)
library(MASS)
library(stats)
library(Matrix)
library(MatrixModels)


# Load tag data

tags <- read.csv("Input/ABMI_Influence_of_wildfire_smoke_on_avian_singing_rates_tag_report.csv")

### Data transformation for paired t-test

# Select relevant columns only

tags <-tags %>%
  select(location,recording_date_time,species_code,aru_task_status,individual_order,abundance,vocalization)

# Combine columns 1 and 2 to create recording name file

tags$recording_name <- paste(tags$location, tags$recording_date_time, sep = " ")

# Remove location and date columns

tags <-tags %>%
  select(recording_name,species_code,aru_task_status,individual_order,abundance,vocalization)

# Filter out all call tags

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

### Summarize number of individuals and abundnace of songs in each recordings

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

# Filter out "smoky" day recordings

smoky <- tags %>%
  filter(grepl("21$|11$", recording_name))

# Filter out "non-smoky" day recordings

non_smoky <- tags %>%
  filter(grepl("15$|24$|9$|13$", recording_name))

# Clean smoky and non-smoky data frames

smoky <- smoky %>%
  select(recording_name,mean_singing_rate)

non_smoky <- non_smoky %>%
  select(recording_name,mean_singing_rate)

# Change name of "mean_singing_rate" for smoky day recordings

smoky <- smoky %>%
  rename(mean_SR_smoky = mean_singing_rate)

# Change name of "mean_singing_rate" for smoky day recordings

non_smoky <- non_smoky %>%
  rename(mean_SR_non_smoky = mean_singing_rate)

# Split the first column of both files so that they can be merged based on location

smoky <- smoky %>%
  separate(recording_name, into = c("Location", "Date"), sep = " ")

non_smoky <- non_smoky %>%
  separate(recording_name, into = c("Location", "Date"), sep = " ")

# Remove time column

smoky <- smoky %>%
  select(Location, mean_SR_smoky)

non_smoky <- non_smoky %>%
  select(Location, mean_SR_non_smoky)

# Merge smoky and non-smoky data frames based on location

dat1 <- merge(smoky, non_smoky, by = "Location")

# Remove 1266-SE

dat1 <- dat1 %>% filter(Location != "1266-SE")

# Remove all sites in which there is a singing rate of zero in both the smoky and non-smoky recordings

dat1 <- dat1[!(dat1$mean_SR_smoky == 0 & dat1$mean_SR_non_smoky == 0), ]

# paired t-test 

result <- t.test(dat1$mean_SR_smoky, dat1$mean_SR_non_smoky, paired = TRUE)
print(result)

# save

write.csv(dat1, "Output/ttest.csv")








### Generalized linear model

# Split location column into 2

tags <- tags %>%
  mutate(site = ifelse(grepl("^\\d", location), sub("-.*", "", location), location),
         quadrant = ifelse(grepl("^\\d", location), sub(".*-", "", location), ""))

# Create new smoky / non-smoky column

tags <- tags %>%
  mutate(treatment = case_when(
    grepl("21$|11$", recording_name) ~ "smoky",
    grepl("15$|24$|13$|9$", recording_name) ~ "non-smoky",
    TRUE ~ NA_character_  # Handle other cases if needed
  ))

# Change site, treatment, and recording name from characters to factor variables

tags$treatment <- as.factor(tags1$treatment)
tags$site <- as.factor(tags1$site)
tags$location <- as.factor(tags1$location)

# Change column names

names(tags)[names(tags) == "total_abundance"] <- "total_song_count"
names(tags)[names(tags) == "unique_order_count"] <- "num_WTSP"
names(tags)[names(tags) == "treatment"] <- "treatment_name"

# Remove all sites in which there is a singing rate of zero in both the smoky and non-smoky recordings

tags <- tags %>%
  group_by(location) %>%
  filter(any(mean_singing_rate != 0))

# Assuming 'tags' is your data frame

tags$treatment <- ifelse(tags$treatment_name == "smoky", 1, 0)










########### MODEL ##############

model <- glmer(total_song_count ~ treatment + num_WTSP + (1|location),
               data = tags,
               family = negative.binomial(theta = 1, link = "log"))


# Compute dispersion parameter
dispersion_value <- dispersion_glmer(model)
print(dispersion_value)










# Fit a Poisson GLMM
poisson_glmm <- glmer(total_song_count ~ treatment + num_WTSP + (1 | location), data = tags, family = poisson(link = "log"))

# Fit a Negative Binomial GLMM
neg_binom_glmm <- glmer.nb(total_song_count ~ treatment + num_WTSP + (1 | location), data = tags)
}

# Compare models using Likelihood Ratio Test
lr_test <- anova(poisson_glmm, neg_binom_glmm)


########### MODEL ##############

model <- glmer(total_song_count ~ treatment + num_WTSP + (1|location),
               data = tags,
               family = negative.binomial(theta = 1, link = "log"))

git config --global user.name "ljpatter"
git config --global user.email ljpatter@ualberta.ca

