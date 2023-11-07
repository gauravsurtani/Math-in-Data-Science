library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)


# Read data from CSV file
data <- read.csv("spotify-2023.csv")

# Print the first few rows of the data
head(data)

#Know the dimensions
dim(data)

#Rows and columns
nrow(data)
ncol(data)

#Checking for data quality issues
summary(data)
str(data)
head(data)

missing_values <- sum(is.na(data))
cat("Number of missing values:", missing_values, "\n")

null_values <- sum(is.null(data))
cat("Number of null values:", null_values, "\n")

cat("Proportion of missing values:", mean(is.na(data)), "\n")

spotify_data <- na.omit(data)
dim(spotify_data)

rows_cleaned <- sum(is.na(spotify_data))
rows_cleaned
# Cleared the data with NA values 

spotify_data_df <- as.data.frame(spotify_data)
summary(spotify_data_df)

# EDA Analysis
# We scale down to the stream to in millions to improve data clarity

spotify_data$streams <- as.numeric(as.character(spotify_data$streams))
spotify_data <- spotify_data[!is.na(spotify_data$streams), ]
spotify_data$streams_in_millions <- spotify_data$streams/1000000
summary(spotify_data$streams_in_millions)

# 1. Histograms for Streams in millions and Playlist Adds
ggplot(spotify_data, aes(x = streams_in_millions)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "gray") +
  labs(title = "Distribution of Streams")

ggplot(spotify_data, aes(x = in_spotify_playlists)) +
  geom_histogram(binwidth = 100, fill = "green", color = "black") +
  labs(title = "Distribution of playlist Adds")

# I chose to start with Histograms of Streams and Playlist Additions because:
# They provide an overview of the data distribution. You can immediately see if the data is skewed, has a normal distribution, or has multiple modes.
# They help identify outliers.If there's a long tail, that could suggest the presence of outliers. We may have to remove some outliers at the end because they might possibly skew the results towards them.
# They are a precursor to data cleaning. If you spot any anomalies, such as unexpected spikes that don't correspond to the real-world behavior of the data, this might indicate errors or noise in the data collection process that need to be addressed.

spotify_data_lessthan10000_playlist <- spotify_data %>% 
  filter(in_spotify_playlists <= 10000)

ggplot(spotify_data_lessthan10000_playlist, aes(x = energy_., y = in_spotify_playlists)) +
  geom_point(alpha = 0.75, color = "blue") +
  labs(title = "Song Added to Playlist  vs. Energy Level of the song",
       x = "Energy Level",
       y = "Playlists that song is present in")

ggplot(spotify_data, aes(x = danceability_., y = bpm )) +
  geom_point(alpha = 0.6, color = "purple") +
  labs(title = "Danceability vs. Tempo",
       x = "Danceability",
       y = "Tempo (BPM)")+
  xlim(0, 100) 

# Example: Average tempo by key with range
spotify_data_filtered_emptyKey <- spotify_data %>%
  filter(!is.na(key) & !is.na(bpm) & bpm != 0 & key!='')


ggplot(spotify_data_filtered_emptyKey, aes(x = as.factor(key), y = bpm)) +
  geom_pointrange(stat = "summary", fun.data = "mean_cl_normal", color = "blue") +
  labs(title = "Average Tempo by Musical Key",
       x = "Musical Key",
       y = "Tempo (BPM)")


