# Load packages

library(dplyr)
library(tidyverse)
library(lubridate)
library(tidyr)
library(sf)

# Load files

dat1 <- read.csv("Input/potential_locations.csv")


### GET DISTANCE BETWEEN ABMI EH SITES AND WEATHER STATIONS ### ---------


# Create shapefile

ssyr.sf <- st_as_sf(x = dat1,                         
                    coords = c("longitude", "latitude"),
                    crs = "+init=epsg:4326 +proj=longlat +ellps=WGS84 
                    +datum=WGS84 +no_defs +towgs84=0,0,0")

print("Bounding box of new shapefile in lat/long:")

st_bbox(ssyr.sf)

ssyr.sf<-st_transform(ssyr.sf, "+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")
str(ssyr.sf)
st_write(ssyr.sf, "Output/BU_2023locations.shp", append=FALSE)
#we can use this shape-file as an asset to extract data in GEE


#now add weather stations
weatherStations<-read.csv("Input/AQHI_communities_lat_long.csv")
weatherStations <- weatherStations[-13, ]
weatherStations <- weatherStations[-33, ]
weather.sf <- st_as_sf(x = weatherStations,                         
                       coords = c("long", "lat"),
                       crs = "+init=epsg:4326 +proj=longlat +ellps=WGS84 
                       +datum=WGS84 +no_defs +towgs84=0,0,0")

weather.sf<-st_transform(weather.sf, "+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")
str(weather.sf)
st_write(weather.sf, "Output/weatherstation_2023locations.shp", append=FALSE)

print("Bounding box of new shapefile in UTM coordinates:")

weather.sf$town<-as.factor(weather.sf$town)


estimDist<-function(pointfile, polypointfile, maxdist){
  ss.sf.summary<-list()
  #for (i in 1:100){#first 100 point counts in this shapefile
  for (i in 1:nrow(pointfile)){#all of the point counts in this shapefile
    ss.sf.i<-pointfile[i,]
    SS.i<-ss.sf.i$location
    b.maxdist<-st_buffer(ss.sf.i,maxdist)
    p.maxdist<-st_intersection(polypointfile, b.maxdist) #replaced polypointfile.y with polypointfile
    nearest<-ifelse((nrow(p.maxdist)==0), maxdist+1, 
                    min(st_distance(ss.sf.i,p.maxdist)))
    p.maxdist$townDistance<-as.vector(st_distance(ss.sf.i, p.maxdist))
    
    nearestTown<-ifelse(nrow(p.maxdist)==0, "Too far away", p.maxdist %>% filter(townDistance==nearest)%>% st_drop_geometry() %>%group_by(town) %>% as_tibble %>%
                          select(town)%>%
                          filter(row_number()==1))
    
    ss.sf.summary[[i]]<-data.frame(location=SS.i,
                                   Town=nearestTown,
                                   NEAR.DIST=nearest)
    print(paste0("point ",i," done"))  #you can turn this on if you want to watch the progress in your analysis
    colnames(ss.sf.summary[[i]])<-c("location","town","NEAR.DIST")
  }
  summaries<-do.call(rbind, ss.sf.summary)
  return(summaries)
}


maxdist<-200000 #200 km buffer
pointfile<-ssyr.sf
pointfilebuffer<-st_buffer(pointfile, maxdist)
featuretype<-weather.sf

a<-Sys.time()
polypointfile<-st_intersection(featuretype, pointfilebuffer)

summaries<-estimDist(pointfile,polypointfile,maxdist)
summaries<-data.frame(summaries)
#n=100; the estimDist function by default just estimates distance to footprint for the first 100 points, but can be easily modifed to estimate distance to footprint for all points
str(summaries)
summaries$distWeatherStation<-summaries$NEAR.DIST
summaries$NEAR.DIST<-NULL
write.csv(summaries, file=paste0("Output/BUpotentialPoints_weatherStationDistancesNew.csv"))
b<-Sys.time()
print(b-a)








### JOIN WEATHER STATIONS AND DISTANCES w/ RECORDINGS -----------------------

# Load all 2023 recordings

allrecordings <- read.csv("Input/allcurrentyearWAVrecordings.csv")

# Read in distances file

distances <-read.csv("Output/BUpotentialPoints_weatherStationDistancesNew.csv")

# Remove unnecessary columns 

distances<-distances%>%
  select(location,town,distWeatherStation)

# Select only sites that are nearest to Caroline

distances <- distances[distances$town != "Airdrie", ]

# Change distances$site from an integer to a character

distances$location <- as.character(distances$location)

# Change "location" to all uppercase so that it can merge with allrecordings.csv

distances$location <- toupper(distances$location)

# Join "distances" and "all recordings" using location

dat1 <- inner_join(distances, allrecordings, by = "location")

# Split recording_date_time into recording_date and recording_time

split_date_time <- strsplit(dat1$recording_date_time, " ")

# Create new columns in the 'distances' data frame

dat1$recording_date <- sapply(split_date_time, "[", 1)
dat1$recording_time <- sapply(split_date_time, "[", 2)

# Rearrange column order

dat1<-dat1%>%
  select(location,town,distWeatherStation,recording_date,recording_time,time_index)

# Filter rows where time_index is equal to 7

dat2 <- dat1[dat1$time_index == 7, ]

# Remove all rows where time_index 7 is not equal to ~6:00 AM

dat3 <- dat2[!(dat2$recording_time %in% c("01:45:00", "11:40:00")), ]

# Combine 'recording_date' and 'recording_time' into a new column

dat3$combined_datetime <- as.POSIXct(sprintf("%s %s", dat3$recording_date, dat3$recording_time), format="%Y-%m-%d %H:%M:%S")

# Round the 'combined_datetime' to the nearest hour

dat3$recording_time_rounded <- round_date(dat3$combined_datetime, unit = "hour")

# Remove the intermediate 'combined_datetime' column if not needed

dat3 <- dat3[, !(names(dat3) %in% "combined_datetime")]

# Split 'recording_time_rounded' based on the space

split_time <- str_split(dat3$recording_time_rounded, " ", simplify = TRUE)

# Create a new column 'round_recording_time'

dat3$round_recording_time <- split_time[, 2]

# Remove the intermediate split date column

dat3 <- dat3[, !(names(dat3) %in% "recording_time_rounded")]

# Convert time values from hh:mm:ss to hh:mm

dat3$round_recording_time <- sprintf("%02d:%02d", hour(hms(dat3$round_recording_time)), minute(hms(dat3$round_recording_time)))

# Change town column to AQHI communities

colnames(dat3)[colnames(dat3) == "town"] <- "AQHI communities"











### TRANSFORM SMOKE DATA ----------------------------------------------------

# Load smoke data

smokedata <- read.csv("Input/BU_Communities_AQHI.csv")

# Remove column headers and replace with row 1

colnames(smokedata) <- as.character(smokedata[1, ])
smokedata <- smokedata[-1, ]

# Convert wide to long

#smokedata <- pivot_longer(smokedata, cols = c("Caroline", "Drayton Valley", "Genesee", "St. Albert", "Steeper"), names_to = "AQHI communities", values_to = "AQHI values")
smokedata <- pivot_longer(smokedata, cols = c("Caroline"), names_to = "AQHI communities", values_to = "AQHI values")

# Split 'date' column into a column for date and another for time

split_data <- strsplit(smokedata$Date, " ")
smokedata$date <- sapply(split_data, function(x) paste(x[1], x[2], x[3], collapse = " "))
smokedata$time <- sapply(split_data, function(x) paste(x[4], x[5], x[6], collapse = " "))

# Remove old date column

smokedata<-smokedata%>%
  select(date, time,`AQHI communities`,`AQHI values`)

# convert date values so that they match up, and can be merged with dat3

smokedata <- smokedata %>%
  mutate(date = as.Date(date, format = "%B %d %Y"))

# Remove time zone from the time column so that it can be merged with dat3

split_data <- strsplit(smokedata$time, " ")
smokedata$time <- sapply(split_data, `[`, 1)
smokedata$deleteme <- sapply(split_data, `[`, 2)

# Remove old time column

smokedata<-smokedata%>%
  select(date, time,`AQHI communities`,`AQHI values`)

# Change 'date' in smoke data to a character so it can merge w/ merged_data2_subset

smokedata$date <- as.character(smokedata$date)

# Write to file

write.csv(smokedata, "Output/transformed_smokedata.csv")









### FINALLY, MERGE SMOKE DATA W/ dat3 ------------------------


# Change date and time column name in dat2 to match those in smokedata

names(dat3)[names(dat3) == "round_recording_time"] <- "time"
names(dat3)[names(dat3) == "recording_date"] <- "date"

# merge "merged_data2_subset" with "smokedata"

common_columns <- c("AQHI communities", "date", "time")
master_file <- inner_join(dat3, smokedata, by = common_columns)

# Extract only relevant dates

selected_dates <- c("2023-06-09", "2023-06-11", "2023-06-13")
master_file <- master_file[master_file$date %in% selected_dates, ]

# Remove all sites that don't have a complete set of before / smoky / after

location_counts <- table(master_file$location)
valid_locations <- names(location_counts[location_counts >= 3])
master_file <- master_file[master_file$location %in% valid_locations, ]

# Remove dashes from the "date" column

master_file$date <- gsub("-", "", master_file$date)

# Remove colons from the "recording_time" column

master_file$recording_time <- gsub(":", "", master_file$recording_time)

# Combine "location", "date", and "recording_time" columns with underscores

master_file$recording_name <- paste(master_file$location, master_file$date, master_file$recording_time, sep = "_")

# Add .wav to the end of all file names

master_file <- master_file %>%
 mutate(recording_name = paste0(recording_name, ".wav"))

# Remove all sites that start with the prefix "H23" as they were not in BUpublic
rows_to_remove <- grepl("^H", master_file$location)
master_file <- master_file[!rows_to_remove, ]

# write "master recording file" to csv for mover script

write.csv(master_file, "Output/BUmaster_recording_list.csv")








### Recording list check -----------------------------------------

### Create list of recording that were manually extracted from Cirrus

# Load packages

library(openxlsx)
library(readr)

# Set working direction to "Recordings" folder where audio files are stored

setwd("/Users/leonardpatterson/Desktop/Recordings/new")

# List all files in the directory
recording_files <- list.files()

# Create a data frame with the file names
recording_data <- data.frame(RecordingName = recording_files)

# Set working directory back to R Project

setwd("/Users/leonardpatterson/Desktop/R Files/Biol 399 Project")

# Replace 'recordings.csv' with the desired name for your CSV file
write_csv(recording_data, "Output/recordings_folder.csv")







### Compare master_file & recording_data to ensure files were extracted accurately --------


# Read the first CSV file

data1 <- read.csv("Output/recordings_folder.csv")

# Read the second CSV file

data2 <- read.csv("Output/BUmaster_recording_list.csv")

# Remove row number column from data2

data2 <- data2[, -1]

# Extract the "file_name" columns from both data frames

file_names1 <- data1$RecordingName
file_names2 <- data2$file_name

# Find values that are in file1.csv but not in file2.csv

values_unique_to_file1 <- setdiff(file_names1, file_names2)

# Find values that are in file2.csv but not in file1.csv

values_unique_to_file2 <- setdiff(file_names2, file_names1)

# Display the values that are unique to each file

cat("Values unique to file1.csv: ", values_unique_to_file1, "\n")
cat("Values unique to file2.csv: ", values_unique_to_file2, "\n")

# Export data2 as datasheet for recording data

#write.xlsx(data2, "Output/datasheet.xlsx")




