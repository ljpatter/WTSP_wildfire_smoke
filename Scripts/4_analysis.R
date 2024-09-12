# Load packages

library(tidyverse)
library(lme4)
library(dplyr)
library(lmtest)
library(glmmTMB)
library(emmeans)
library(ggplot2)
library(effsize)
library(Matrix)
library(MatrixModels)



# Load data

dat1 <- read.csv("Output/ttest_data.csv")




### Paired t-test

# Paired t-test - singing rate

pairedttest <- t.test(dat1$mean_SR_smoky, dat1$mean_SR_non_smoky, paired = TRUE)
pairedttest

# Paired t-test - num_WTSP

pairedttest2 <- t.test(dat1$num_WTSP_smoky, dat1$num_WTSP_non_smoky, paired = TRUE)
pairedttest2

dat1$WTSP_diff <- dat1$num_WTSP_smoky - dat1$num_WTSP_non_smoky









### Transformations for Generalized linear model

# Load data

tags <- read.csv("Output/tags.csv")

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

tags$treatment <- as.factor(tags$treatment)
tags$site <- as.factor(tags$site)
tags$location <- as.factor(tags$location)

# Change column names

names(tags)[names(tags) == "total_abundance"] <- "total_song_count"
names(tags)[names(tags) == "unique_order_count"] <- "num_WTSP"
names(tags)[names(tags) == "treatment"] <- "treatment_name"
names(tags)[names(tags) == "location"] <- "ARU"

# Remove all sites in which there is a singing rate of zero in both the smoky and non-smoky recordings

tags <- tags %>%
  group_by(ARU) %>%
  filter(any(mean_singing_rate != 0))

# Assign dummy variable to treatment to run GLM

tags$treatment <- ifelse(tags$treatment_name == "smoky", 0, 1)

# Create a data column

tags <- tags %>%
  tidyr::separate(recording_name, into = c("date", "time"), sep = " ", remove = FALSE) %>%
  dplyr::select(-date) %>%
  dplyr::rename(date = time)

# Remove 1266-SE sites (too hard to transcribe)

tags <- tags %>% 
  filter(ARU != "1266-SE")

# Create a julian date column

tags$date <- as.Date(tags$date)
to_julian_date <- function(date) {
  as.numeric(date) - as.numeric(as.Date("2023-01-01")) + 1
}
tags$julian_date <- to_julian_date(tags$date)

# Add date code column

tags <- tags %>%
  mutate(date_code = case_when(
    julian_date %in% c(141, 162) ~ 1,
    julian_date %in% c(135, 160) ~ 0,
    julian_date %in% c(144, 164) ~ 2,
    TRUE ~ NA_integer_  # Handle other cases if needed
  ))

write.csv(tags, "glm.csv")












########### GLM ##############

tags <- read.csv("Output/glm.csv")

tags <- tags %>%
  dplyr::select(recording_name,ARU,site,date,julian_date,date_code,total_song_count,num_WTSP,mean_singing_rate,treatment_name,treatment)

### GLLM w/ NEGATIVE BINOMIAL DISTRIBUTION ###


model <- glmmTMB(total_song_count ~ treatment + num_WTSP + julian_date + (1 | ARU),
                 family = nbinom2(link = "log"), data = tags)


model1 <- glmmTMB(total_song_count ~ num_WTSP + treatment + date_code + (1 | ARU) + (1 | site), 
                 family = nbinom2(link = "log"), data = tags)

# View the model summary

summary(model)
summary(model1)


### NB Confidence intervals

emmeans(model, ~ treatment)


### Check for heteroscedasticity in both models

# Extract residuals

residuals <- residuals(model1, type = "pearson")

# Create a data frame with fitted values and residuals

residual_data <- data.frame(
  fitted_values = fitted(model1),
  residuals = residuals
)

# Plot residuals against fitted values

residual_plot <- ggplot(residual_data, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  labs(x = "Fitted values", y = "Residuals") +
  ggtitle("Residual Plot") +
  theme_minimal()

# Display the plots

residual_plot


# Extract residuals

residuals <- residuals(model, type = "pearson")

# Create a data frame with fitted values and residuals

residual_data <- data.frame(
  fitted_values = fitted(model1),
  residuals = residuals
)

# Plot residuals against fitted values

residual_plot <- ggplot(residual_data, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  labs(x = "Fitted values", y = "Residuals") +
  ggtitle("Residual Plot") +
  theme_minimal()

# Display the plots

residual_plot








# Install and load the required packages
install.packages("lmerTest")
library(lmerTest)

# Fit the linear mixed-effects model with p-values
model1 <- lmer(mean_singing_rate ~ treatment + date_code + (1 | ARU), 
               data = tags)

# Print the model summary with p-values
summary(model1)







model1 <- lmer(mean_singing_rate ~ treatment + date_code + (1 | ARU), 
                        data = tags)
summary(model1)

model3 <- glmer(num_WTSP ~ treatment + date_code + (1 | ARU),
                family = poisson(link = "log"),
                data = tags)

summary(model3)

















### GLM w/ POISSON DISTRIBUTION ###

# Fit a Poisson GLM
model2 <- glmer(total_song_count ~ treatment + num_WTSP + julian_date (1 | location),
                family = poisson(link = "log"),
                data = tags)

# Get the residual deviance and degrees of freedom
residual_deviance <- deviance(model2)
residual_df <- df.residual(model2)


# Calculate the overdispersion ratio
overdispersion_ratio <- residual_deviance / residual_df

# Print the overdispersion ratio
cat("Overdispersion Ratio (Residual Deviance / Residual DF):", overdispersion_ratio, "\n")

# Obtain the Pearson residuals from the Poisson model
residuals <- residuals(model2, type = "pearson")

# Plot the residuals against the fitted values
plot(fitted(model2), residuals,
     xlab = "Fitted Values",
     ylab = "Pearson Residuals",
     main = "Residual Plot",
     pch = 16,
     col = "blue")

# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red", lty = 2)

# Summarize the Poisson model
summary(model2)
















##### LINEAR MODEL GROUPED #####

# Load data

dat1 <- read.csv("Output/ttest_data.csv")

# Calculate the differences between the smoky and non-smoky singing rates

dat1$mean_SR_difference <- dat1$mean_SR_smoky - dat1$mean_SR_non_smoky

### MODEL

model <- lm(difference ~ 1, data = dat1)
summary(model)

# Plot model

file_path <- "Graphics/lm_plot.png"
plot(dat1$difference, xlab = "Observation", ylab = "Difference in singing rate", ylim = c(floor(min(dat1$difference)), ceiling(max(dat1$difference))))
abline(h = coef(model), col = "red")
png(file_path)
dev.off()

# Residuals plot

plot(residuals(model), main = "Residuals Plot", xlab = "Index", ylab = "Residuals")

# QQ plot

qqnorm(residuals(model))
qqline(residuals(model))









### LINEAR MODEL, CALCULATING BY SPLITTING DATA INTO 2 GROUPS:
### 1) PAIRS OF RECORDINGS IN WHICH NON-SMOKY OCCURS BEFORE SMOKY and 2) PAIRS OF RECORDINGS IN WHICH NON-SMOKY OCCURS AFTER SMOKY 


tags1 <- read.csv("Output/tags.csv")

# Remove 1266-SE  (started transcribing but decided too remove due to too much uncertainty in trancription)

tags1 <- tags1 %>% 
  filter(location != "1266-SE")

# Split recording_name to create a new "date" column

tags1 <- transform(tags1, date = sub("^[^ ]+ ", "", recording_name))

# Assign 'smoky' and 'non-smoky' to respective dates

tags1$treatment <- ifelse(tags1$date %in% c("2023-05-21", "2023-06-11"), "smoky", 
                          ifelse(tags1$date %in% c("2023-05-24", "2023-05-15", "2023-06-09", "2023-06-13"), 
                                 "non-smoky", NA))

# Filter out rows where both of the recordings in a pair are equal to zero (no singing )

tags2 <- tags1 %>%
  group_by(location) %>%
  filter(!(all(mean_singing_rate == 0)))

# Split data two 'before_smoky' and 'after_smoky' groupings

split_pairs <- function(df) {
  df_pairs <- split(df, f = rep(1:(nrow(df) / 2), each = 2))
  return(df_pairs)
}
pairs <- split_pairs(tags2)
before_nonsmoky <- bind_rows(lapply(pairs, function(x) {
  x <- arrange(x, location)
  if (x$treatment[1] == "non-smoky") {
    return(x)
  }
}))
after_nonsmoky <- bind_rows(lapply(pairs, function(x) {
  x <- arrange(x, location)
  if (x$treatment[1] == "smoky") {
    return(x)
  }
}))





### BEFORE SMOKY TRANSFORMATIONS ####

# Filter out 'smoky' day recordings from 'before smoky pairs'

smoky1 <- before_nonsmoky %>%
  filter(grepl("21$|11$", date))

# Filter out 'non-smoky' day recordings from 'before smoky pairs'

non_smoky1 <- before_nonsmoky %>%
  filter(grepl("15$|9$", date))

# Clean smoky and non-smoky data frames

smoky1 <- smoky1 %>%
  dplyr::select(recording_name,mean_singing_rate)

non_smoky1 <- non_smoky1 %>%
  dplyr::select(recording_name,mean_singing_rate)

# Change name of "mean_singing_rate" for smoky day recordings

smoky1 <- smoky1 %>%
  rename(mean_SR_smoky = mean_singing_rate)

# Change name of "mean_singing_rate" for non-smoky day recordings

non_smoky1 <- non_smoky1 %>%
  rename(mean_SR_non_smoky = mean_singing_rate)

# Split the first column of both files so that they can be merged based on location

smoky1 <- smoky1 %>%
  separate(recording_name, into = c("Location", "Date"), sep = " ")

non_smoky1 <- non_smoky1 %>%
  separate(recording_name, into = c("Location", "Date"), sep = " ")

# Remove time column

smoky1 <- smoky1 %>%
  dplyr::select(Location, mean_SR_smoky)

non_smoky1 <- non_smoky1 %>%
  dplyr::select(Location, mean_SR_non_smoky)

# Merge smoky and non-smoky data frames based on location

before_nonsmoky <- merge(smoky1, non_smoky1, by = "Location")

# Calculate the differences between the smoky and non-smoky singing rates

before_nonsmoky$difference <- before_nonsmoky$mean_SR_smoky - before_nonsmoky$mean_SR_non_smoky

### MODEL

model1 <- lm(difference ~ 1, data = before_nonsmoky)
summary(model1)

# Plot model

file_path <- "Graphics/lm_plot_before_nonsmoky.png"
plot(before_nonsmoky$difference, xlab = "Observation", ylab = "Difference in singing rate", ylim = c(floor(min(before_nonsmoky$difference)), ceiling(max(before_nonsmoky$difference))))
abline(h = coef(model1), col = "red")
png(file_path)
dev.off()

# Residuals plot

plot(residuals(model1), main = "Residuals Plot", xlab = "Index", ylab = "Residuals")

# QQ plot

qqnorm(residuals(model1))
qqline(residuals(model1))

# Histogram of model residuals

png("Graphics/histogram_of_residuals.png")
hist(residuals(model1), main = "Histogram of Residuals", xlab = "Residuals", ylab = "Frequency")
dev.off()










### AFTER SMOKY TRANSFORMATIONS ####

# Filter out "smoky" day recordings

smoky2 <- after_nonsmoky %>%
  filter(grepl("21$|11$", date))

# Filter out "non-smoky" day recordings

non_smoky2 <- after_nonsmoky %>%
  filter(grepl("24$|13$", date))

# Clean smoky and non-smoky data frames

smoky2 <- smoky2 %>%
  dplyr::select(recording_name,mean_singing_rate)

non_smoky2 <- non_smoky2 %>%
  dplyr::select(recording_name,mean_singing_rate)

# Change name of "mean_singing_rate" for smoky day recordings

smoky2 <- smoky2 %>%
  rename(mean_SR_smoky = mean_singing_rate)

# Change name of "mean_singing_rate" for smoky day recordings

non_smoky2 <- non_smoky2 %>%
  rename(mean_SR_non_smoky = mean_singing_rate)

# Split the first column of both files so that they can be merged based on location

smoky2 <- smoky2 %>%
  separate(recording_name, into = c("Location", "Date"), sep = " ")

non_smoky2 <- non_smoky2 %>%
  separate(recording_name, into = c("Location", "Date"), sep = " ")

# Remove time column

smoky2 <- smoky2 %>%
  dplyr::select(Location, mean_SR_smoky)

non_smoky2 <- non_smoky2 %>%
  dplyr::select(Location, mean_SR_non_smoky)

# Merge smoky and non-smoky data frames based on location

after_nonsmoky <- merge(smoky2, non_smoky2, by = "Location")

# Calculate the differences between the smoky and non-smoky singing rates

after_nonsmoky$difference <- after_nonsmoky$mean_SR_smoky - after_nonsmoky$mean_SR_non_smoky

### MODEL

model2 <- lm(difference ~ 1, data = after_nonsmoky)
summary(model2)

# Plot model

getwd()
if (!file.exists("Graphics")) {
  dir.create("Graphics")
}
file_path <- "Graphics/lm_plot_after_nonsmoky.png"
png(file_path)
plot(after_nonsmoky$difference, 
     xlab = "Observation", 
     ylab = "Difference in singing rate", 
     ylim = c(floor(min(after_nonsmoky$difference)), ceiling(max(after_nonsmoky$difference))))
abline(h = coef(model2), col = "red")
dev.off()

# Residuals plot

plot(residuals(model2), main = "Residuals Plot", xlab = "Index", ylab = "Residuals")

# QQ plot

qqnorm(residuals(model2))
qqline(residuals(model2))




###### LM MODEL SUMMARY ######

summary(model) # before and after grouped
summary(model1) # before non-smoky
summary(model2) # after non-smoky














