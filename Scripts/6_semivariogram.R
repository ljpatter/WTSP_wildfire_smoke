# Load packages

library(gstat)
library(sp)
library(dplyr)
library(sf)
library(tidyr)

############## MAY 15 ###################


### Load May 15 smoke data

dat1 <- read.csv("Input/May15_AQD.csv")

### Remove first 6 rows

dat1 <- dat1[-c(1:6), ]

### Create new data frame with one column of station names

stations <- data.frame(Station_Info = as.character(dat1[1, seq(2, ncol(dat1), by = 4)]))

### Create new data frame with lat lon of air monitoring stations

coords <- dat1 %>%
  select(seq(2, ncol(dat1), by = 4)) %>% # Select every 4th column starting from column 2
  slice(c(5, 6)) %>% # Extract rows 5 and 6
  t() %>% # Transpose the data to switch rows and columns
  as.data.frame() %>% # Convert the matrix back to a data frame
  setNames(c("lat", "lon")) # Rename the columns

# Create new data frame with smoke values

smokevalues <- dat1 %>%
  select(seq(3, ncol(dat1), by = 4)) %>% # Select every 4th column starting from column 3
  slice(21) %>% # Extract row 21
  pivot_longer(everything(), names_to = "Column", values_to = "Value") %>% # Convert to long format
  select(Value) # Keep only the values column

### Combine station names, coordinates, and PM2.5 values

dat2 <- cbind(stations, coords, smokevalues)
names(dat2) <- c("station", "lat", "lon", "pm25")

### Remove any stations that are missing PM2.5 values

dat2 <- dat2 %>%
  filter(pm25 >= 1)



### Convert lat / long coordinates into UTMs


# Creat# Example data frame with latitude and longitude

df <- data.frame(
  latitude = c(dat2$lat), 
  longitude = c(dat2$lon)
)

# Convert to an sf object with WGS84 (EPSG:4326) CRS
sf_df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

# Transform to UTM Zone 12N (EPSG:32612)
utm_df <- st_transform(sf_df, crs = 32612)

# Extract the UTM coordinates and add them to the original data frame
dat2$utm_easting <- st_coordinates(utm_df)[,1]
dat2$utm_northing <- st_coordinates(utm_df)[,2]


### Run semivariogram


# Create a spatial object from your data

coordinates(dat2) <- ~utm_easting + utm_northing

# Compute the empirical semivariogram

vgm_model <- variogram(pm25 ~ 1, data = dat2)

# Plot the semivariogram

plot(vgm_model)

# Plot and save

file_path <- "Graphics/May15_semiv.png"
png(filename = file_path, width = 800, height = 600)
par(cex.axis = 2,    # Size of axis tick labels
    cex.lab = 2,     # Size of axis labels
    cex.main = 2)    # Size of plot title
plot(vgm_model, 
     main = "", 
     pch = 19,            # Filled circles
     col = "black",       # Color of the dots
     cex = 2)           # Size of the points
dev.off()











############## MAY 21 ###################


### Load May 21 smoke data

dat1 <- read.csv("Input/May21_AQD.csv")

### Remove first 6 rows

dat1 <- dat1[-c(1:6), ]

### Create new data frame with one column of station names

stations <- data.frame(Station_Info = as.character(dat1[1, seq(2, ncol(dat1), by = 4)]))

### Create new data frame with lat lon of air monitoring stations

coords <- dat1 %>%
  select(seq(2, ncol(dat1), by = 4)) %>% # Select every 4th column starting from column 2
  slice(c(5, 6)) %>% # Extract rows 5 and 6
  t() %>% # Transpose the data to switch rows and columns
  as.data.frame() %>% # Convert the matrix back to a data frame
  setNames(c("lat", "lon")) # Rename the columns


# Fix lat and lon that are incorrectly swapped in original downloaded data

negative_lat <- coords$lat < 0
coords[negative_lat, c("lat", "lon")] <- coords[negative_lat, c("lon", "lat")]

# Create new data frame with smoke values

smokevalues <- dat1 %>%
  select(seq(3, ncol(dat1), by = 4)) %>% # Select every 4th column starting from column 3
  slice(21) %>% # Extract row 21
  pivot_longer(everything(), names_to = "Column", values_to = "Value") %>% # Convert to long format
  select(Value) # Keep only the values column

### Combine station names, coordinates, and PM2.5 values

dat2 <- cbind(stations, coords, smokevalues)
names(dat2) <- c("station", "lat", "lon", "pm25")

### Remove any stations that are missing PM2.5 values

dat2 <- dat2 %>%
  filter(pm25 >= 1)



### Convert lat / long coordinates into UTMs


# Creat# Example data frame with latitude and longitude

df <- data.frame(
  latitude = c(dat2$lat), 
  longitude = c(dat2$lon)
)

# Convert to an sf object with WGS84 (EPSG:4326) CRS
sf_df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

# Transform to UTM Zone 12N (EPSG:32612)
utm_df <- st_transform(sf_df, crs = 32612)

# Extract the UTM coordinates and add them to the original data frame
dat2$utm_easting <- st_coordinates(utm_df)[,1]
dat2$utm_northing <- st_coordinates(utm_df)[,2]


### Run semivariogram


# Create a spatial object from your data

coordinates(dat2) <- ~utm_easting + utm_northing

# Compute the empirical semivariogram

vgm_model <- variogram(pm25 ~ 1, data = dat2)

# Plot the semivariogram

plot(vgm_model)

# If you want to fit a theoretical model to the empirical semivariogram:

#vgm_fit <- fit.variogram(vgm_model, model = vgm("Sph"))
#plot(vgm_model, vgm_fit)


# Plot and save

file_path <- "Graphics/May21_semiv.png"
png(filename = file_path, width = 800, height = 600)
#vgm_fit <- fit.variogram(vgm_model, model = vgm("Sph"))
par(cex.axis = 2,    # Size of axis tick labels
    cex.lab = 2,     # Size of axis labels
    cex.main = 2)    # Size of plot title
plot(vgm_model, 
     main = "", 
     pch = 19,            # Filled circles
     col = "black",       # Color of the dots
     cex = 2)           # Size of the points
dev.off()






############## MAY 24 ###################


### Load May 24 smoke data

dat1 <- read.csv("Input/May24_AQD.csv")

### Remove first 6 rows

dat1 <- dat1[-c(1:6), ]

### Create new data frame with one column of station names

stations <- data.frame(Station_Info = as.character(dat1[1, seq(2, ncol(dat1), by = 4)]))

### Create new data frame with lat lon of air monitoring stations

coords <- dat1 %>%
  select(seq(2, ncol(dat1), by = 4)) %>% # Select every 4th column starting from column 2
  slice(c(5, 6)) %>% # Extract rows 5 and 6
  t() %>% # Transpose the data to switch rows and columns
  as.data.frame() %>% # Convert the matrix back to a data frame
  setNames(c("lat", "lon")) # Rename the columns


# Fix lat and lon that are incorrectly swapped in original downloaded data

negative_lat <- coords$lat < 0
coords[negative_lat, c("lat", "lon")] <- coords[negative_lat, c("lon", "lat")]

# Create new data frame with smoke values

smokevalues <- dat1 %>%
  select(seq(3, ncol(dat1), by = 4)) %>% # Select every 4th column starting from column 3
  slice(21) %>% # Extract row 21
  pivot_longer(everything(), names_to = "Column", values_to = "Value") %>% # Convert to long format
  select(Value) # Keep only the values column

### Combine station names, coordinates, and PM2.5 values

dat2 <- cbind(stations, coords, smokevalues)
names(dat2) <- c("station", "lat", "lon", "pm25")

### Remove any stations that are missing PM2.5 values

dat2 <- dat2 %>%
  filter(pm25 >= 1)



### Convert lat / long coordinates into UTMs


# Creat# Example data frame with latitude and longitude
df <- data.frame(
  latitude = c(dat2$lat), 
  longitude = c(dat2$lon)
)

# Convert to an sf object with WGS84 (EPSG:4326) CRS
sf_df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

# Transform to UTM Zone 12N (EPSG:32612)
utm_df <- st_transform(sf_df, crs = 32612)

# Extract the UTM coordinates and add them to the original data frame
dat2$utm_easting <- st_coordinates(utm_df)[,1]
dat2$utm_northing <- st_coordinates(utm_df)[,2]


### Run semivariogram


# Create a spatial object from your data

coordinates(dat2) <- ~utm_easting + utm_northing

# Compute the empirical semivariogram

vgm_model <- variogram(pm25 ~ 1, data = dat2)

# Plot the semivariogram

plot(vgm_model)

# If you want to fit a theoretical model to the empirical semivariogram:

#vgm_fit <- fit.variogram(vgm_model, model = vgm("Sph"))
#plot(vgm_model, vgm_fit)


# Plot and save

file_path <- "Graphics/May24_semiv.png"
png(filename = file_path, width = 800, height = 600)
#vgm_fit <- fit.variogram(vgm_model, model = vgm("Sph"))
par(cex.axis = 2,    # Size of axis tick labels
    cex.lab = 2,     # Size of axis labels
    cex.main = 2)    # Size of plot title
plot(vgm_model, 
     main = "", 
     pch = 19,            # Filled circles
     col = "black",       # Color of the dots
     cex = 2)           # Size of the points
dev.off()












############## June 9 ###################


### Load June 9 smoke data

dat1 <- read.csv("Input/June9_AQD.csv")

### Remove first 6 rows

dat1 <- dat1[-c(1:6), ]

### Create new data frame with one column of station names

stations <- data.frame(Station_Info = as.character(dat1[1, seq(2, ncol(dat1), by = 4)]))

### Create new data frame with lat lon of air monitoring stations

coords <- dat1 %>%
  select(seq(2, ncol(dat1), by = 4)) %>% # Select every 4th column starting from column 2
  slice(c(5, 6)) %>% # Extract rows 5 and 6
  t() %>% # Transpose the data to switch rows and columns
  as.data.frame() %>% # Convert the matrix back to a data frame
  setNames(c("lat", "lon")) # Rename the columns


# Fix lat and lon that are incorrectly swapped in original downloaded data

negative_lat <- coords$lat < 0
coords[negative_lat, c("lat", "lon")] <- coords[negative_lat, c("lon", "lat")]

# Create new data frame with smoke values

smokevalues <- dat1 %>%
  select(seq(3, ncol(dat1), by = 4)) %>% # Select every 4th column starting from column 3
  slice(21) %>% # Extract row 21
  pivot_longer(everything(), names_to = "Column", values_to = "Value") %>% # Convert to long format
  select(Value) # Keep only the values column

### Combine station names, coordinates, and PM2.5 values

dat2 <- cbind(stations, coords, smokevalues)
names(dat2) <- c("station", "lat", "lon", "pm25")

### Remove any stations that are missing PM2.5 values

dat2 <- dat2 %>%
  filter(pm25 >= 1)



### Convert lat / long coordinates into UTMs


# Creat# Example data frame with latitude and longitude
df <- data.frame(
  latitude = c(dat2$lat), 
  longitude = c(dat2$lon)
)

# Convert to an sf object with WGS84 (EPSG:4326) CRS
sf_df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

# Transform to UTM Zone 12N (EPSG:32612)
utm_df <- st_transform(sf_df, crs = 32612)

# Extract the UTM coordinates and add them to the original data frame
dat2$utm_easting <- st_coordinates(utm_df)[,1]
dat2$utm_northing <- st_coordinates(utm_df)[,2]


### Run semivariogram


# Create a spatial object from your data

coordinates(dat2) <- ~utm_easting + utm_northing

# Compute the empirical semivariogram

vgm_model <- variogram(pm25 ~ 1, data = dat2)

# Plot the semivariogram

plot(vgm_model)

# If you want to fit a theoretical model to the empirical semivariogram:

#vgm_fit <- fit.variogram(vgm_model, model = vgm("Sph"))
#plot(vgm_model, vgm_fit)


# Plot and save

file_path <- "Graphics/June9_semiv.png"
png(filename = file_path, width = 800, height = 600)
#vgm_fit <- fit.variogram(vgm_model, model = vgm("Sph"))
par(cex.axis = 2,    # Size of axis tick labels
    cex.lab = 2,     # Size of axis labels
    cex.main = 2)    # Size of plot title
plot(vgm_model, 
     main = "", 
     pch = 19,            # Filled circles
     col = "black",       # Color of the dots
     cex = 2)           # Size of the points
dev.off()










############## June 11 ###################


### Load June 11 smoke data

dat1 <- read.csv("Input/June11_AQD.csv")

### Remove first 6 rows

dat1 <- dat1[-c(1:6), ]

### Create new data frame with one column of station names

stations <- data.frame(Station_Info = as.character(dat1[1, seq(2, ncol(dat1), by = 4)]))

### Create new data frame with lat lon of air monitoring stations

coords <- dat1 %>%
  select(seq(2, ncol(dat1), by = 4)) %>% # Select every 4th column starting from column 2
  slice(c(5, 6)) %>% # Extract rows 5 and 6
  t() %>% # Transpose the data to switch rows and columns
  as.data.frame() %>% # Convert the matrix back to a data frame
  setNames(c("lat", "lon")) # Rename the columns


# Fix lat and lon that are incorrectly swapped in original downloaded data

negative_lat <- coords$lat < 0
coords[negative_lat, c("lat", "lon")] <- coords[negative_lat, c("lon", "lat")]

# Create new data frame with smoke values

smokevalues <- dat1 %>%
  select(seq(3, ncol(dat1), by = 4)) %>% # Select every 4th column starting from column 3
  slice(21) %>% # Extract row 21
  pivot_longer(everything(), names_to = "Column", values_to = "Value") %>% # Convert to long format
  select(Value) # Keep only the values column

### Combine station names, coordinates, and PM2.5 values

dat2 <- cbind(stations, coords, smokevalues)
names(dat2) <- c("station", "lat", "lon", "pm25")

### Remove any stations that are missing PM2.5 values

dat2 <- dat2 %>%
  filter(pm25 >= 1)



### Convert lat / long coordinates into UTMs


# Creat# Example data frame with latitude and longitude
df <- data.frame(
  latitude = c(dat2$lat), 
  longitude = c(dat2$lon)
)

# Convert to an sf object with WGS84 (EPSG:4326) CRS
sf_df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

# Transform to UTM Zone 12N (EPSG:32612)
utm_df <- st_transform(sf_df, crs = 32612)

# Extract the UTM coordinates and add them to the original data frame
dat2$utm_easting <- st_coordinates(utm_df)[,1]
dat2$utm_northing <- st_coordinates(utm_df)[,2]


### Run semivariogram


# Create a spatial object from your data

coordinates(dat2) <- ~utm_easting + utm_northing

# Compute the empirical semivariogram

vgm_model <- variogram(pm25 ~ 1, data = dat2)

# Plot the semivariogram

plot(vgm_model)

# If you want to fit a theoretical model to the empirical semivariogram:

#vgm_fit <- fit.variogram(vgm_model, model = vgm("Sph"))
#plot(vgm_model, vgm_fit)


# Plot and save

file_path <- "Graphics/June11_semiv.png"
png(filename = file_path, width = 800, height = 600)
#vgm_fit <- fit.variogram(vgm_model, model = vgm("Sph"))
par(cex.axis = 2,    # Size of axis tick labels
    cex.lab = 2,     # Size of axis labels
    cex.main = 2)    # Size of plot title
plot(vgm_model, 
     main = "", 
     pch = 19,            # Filled circles
     col = "black",       # Color of the dots
     cex = 2)           # Size of the points
dev.off()











############## June 13 ###################


### Load June 13 smoke data

dat1 <- read.csv("Input/June13_AQD.csv")

### Remove first 6 rows

dat1 <- dat1[-c(1:6), ]

### Create new data frame with one column of station names

stations <- data.frame(Station_Info = as.character(dat1[1, seq(2, ncol(dat1), by = 4)]))

### Create new data frame with lat lon of air monitoring stations

coords <- dat1 %>%
  select(seq(2, ncol(dat1), by = 4)) %>% # Select every 4th column starting from column 2
  slice(c(5, 6)) %>% # Extract rows 5 and 6
  t() %>% # Transpose the data to switch rows and columns
  as.data.frame() %>% # Convert the matrix back to a data frame
  setNames(c("lat", "lon")) # Rename the columns


# Fix lat and lon that are incorrectly swapped in original downloaded data

negative_lat <- coords$lat < 0
coords[negative_lat, c("lat", "lon")] <- coords[negative_lat, c("lon", "lat")]

# Create new data frame with smoke values

smokevalues <- dat1 %>%
  select(seq(3, ncol(dat1), by = 4)) %>% # Select every 4th column starting from column 3
  slice(21) %>% # Extract row 21
  pivot_longer(everything(), names_to = "Column", values_to = "Value") %>% # Convert to long format
  select(Value) # Keep only the values column

### Combine station names, coordinates, and PM2.5 values

dat2 <- cbind(stations, coords, smokevalues)
names(dat2) <- c("station", "lat", "lon", "pm25")

### Remove any stations that are missing PM2.5 values

dat2 <- dat2 %>%
  filter(pm25 >= 1)



### Convert lat / long coordinates into UTMs


# Creat# Example data frame with latitude and longitude
df <- data.frame(
  latitude = c(dat2$lat), 
  longitude = c(dat2$lon)
)

# Convert to an sf object with WGS84 (EPSG:4326) CRS
sf_df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

# Transform to UTM Zone 12N (EPSG:32612)
utm_df <- st_transform(sf_df, crs = 32612)

# Extract the UTM coordinates and add them to the original data frame
dat2$utm_easting <- st_coordinates(utm_df)[,1]
dat2$utm_northing <- st_coordinates(utm_df)[,2]


### Run semivariogram


# Create a spatial object from your data

coordinates(dat2) <- ~utm_easting + utm_northing

# Compute the empirical semivariogram

vgm_model <- variogram(pm25 ~ 1, data = dat2)

# Plot the semivariogram

plot(vgm_model)

# If you want to fit a theoretical model to the empirical semivariogram:

#vgm_fit <- fit.variogram(vgm_model, model = vgm("Gau"))
#plot(vgm_model, vgm_fit)


# Plot and save

file_path <- "Graphics/June13_semiv.png"
png(filename = file_path, width = 800, height = 600)
#vgm_fit <- fit.variogram(vgm_model, model = vgm("Gau"))
par(cex.axis = 2,    # Size of axis tick labels
    cex.lab = 2,     # Size of axis labels
    cex.main = 2)    # Size of plot title
plot(vgm_model, 
     main = "", 
     pch = 19,            # Filled circles
     col = "black",       # Color of the dots
     cex = 2)           # Size of the points
dev.off()





































# Ensure dat2 is already loaded with the relevant columns

# Convert to spatial object using UTM coordinates
coordinates(dat2) <- ~ utm_easting + utm_northing

# Compute empirical semivariogram
variogram_model <- variogram(pm25 ~ 1, data = dat2)
plot(variogram_model)

# Fit a theoretical model
fitted_model <- fit.variogram(variogram_model, model = vgm(1, "Sph", 500, 1))
plot(variogram_model, fitted_model)

# Output the fitted model parameters
fitted_model











# Sample code to calculate and plot value differences
library(ggplot2)

# Calculate distances and value differences
distances <- as.vector(dist(coords2))  # Distances between points
value_differences <- abs(outer(smokevalues$pm25, smokevalues$pm25, "-"))  # Value differences

# Create a dataframe for plotting
df <- data.frame(
  distance = rep(distances, each = length(distances)),
  value_diff = as.vector(value_differences)
)

ggplot(df, aes(x = distance, y = value_diff)) +
  geom_point() +
  labs(title = "Value Differences vs. Distance", x = "Distance (km)", y = "Value Difference")




