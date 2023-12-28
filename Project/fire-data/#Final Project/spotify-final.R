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


spotify_data <- data
sum(is.na(spotify_data))

spotify_data_scaled <- as.data.frame(scale(spotify_data[, c('bpm', 'danceability_.', 'valence_.', 'energy_.', 'acousticness_.', 'instrumentalness_.', 'liveness_.', 'speechiness_.')]))

# Selecting the relevant features
features <- spotify_data_scaled[, c('bpm', 'danceability_.', 'valence_.', 'energy_.', 'acousticness_.', 'instrumentalness_.', 'liveness_.', 'speechiness_.')]

# Calculating the correlation matrix
c_m <- cor(features, use = "complete.obs") # Handles missing values, if any

# Using corrplot for correlation heatmap
corrplot(c_m, method = "number", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, 
         title = "Features Correlations Heatmap", 
         mar = c(0,0,1,0), # Adjust margin for title space
         col = colorRampPalette(c("red", "lightblue", "darkblue"))(200)) 

# Performing PCA
pca_result <- prcomp(features)

# Summarize PCA results to see how much variance is explained by each component
summary(pca_result)

pca_data <- as.data.frame(pca_result$x)
pca_data$bpm <- spotify_data$bpm
pca_data$acousticness_. <- spotify_data$acousticness_.
pca_data$energy_. <- spotify_data$energy_.


ggplot(pca_data, aes(x = PC1, y = PC2, color = bpm)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "PCA of Spotify Data (Colored by BPM)",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()

ggplot(pca_data, aes(x = PC1, y = PC2, color = acousticness_.)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "green") +
  labs(title = "PCA of Spotify Data (Colored by Acousticness)",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()

ggplot(pca_data, aes(x = PC1, y = PC2, color = energy_.)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "green") +
  labs(title = "PCA of Spotify Data (Colored by Energy)",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()

# ALL Data
# Assuming spotify_data is your dataset
numeric_columns <- sapply(spotify_data, is.numeric)
spotify_data_numeric <- spotify_data[, numeric_columns]

spotify_data_scaled_all <- as.data.frame(scale(spotify_data_numeric))

# Calculating the correlation matrix
c_m_all <- cor(spotify_data_scaled_all, use = "complete.obs") # Handles missing values, if any

# Using corrplot for correlation heatmap
corrplot(c_m_all, method = "circle", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 90, 
         title = "Features Correlations Heatmap", 
         mar = c(0,0,2,0), # Adjust margin for title space
         col = colorRampPalette(c("red", "lightblue", "darkblue"))(200)) 


spotify_data_df
spotify_data_top_10_artists = spotify_data_df[, c('artist.s._name', 'track_name')]

spotify_data_artists_songs <- spotify_data_df %>%
  group_by(artist.s._name) %>%
  summarise(song_count = n()) %>%
  arrange(desc(song_count))

spotify_data_top_10_artists_songs <- head(spotify_data_artists_songs,10)

ggplot(spotify_data_top_10_artists_songs, aes(x = reorder(artist.s._name, song_count), y=song_count)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(title = "Top 10 Artist with Most songs", x = "Artists", y="Number of Songs") +
  coord_flip()

# PCA - https://www.datacamp.com/tutorial/pca-analysis-r

install.packages("factoextra")
install.packages("ggcorrplot")
library(ggcorrplot)
library(factoextra)

colSums(is.na(spotify_data))
spotify_data_nomralized <- scale(spotify_data[, c('bpm', 'danceability_.', 'valence_.', 'energy_.', 'acousticness_.', 'instrumentalness_.', 'liveness_.', 'speechiness_.')])
head(spotify_data_nomralized)

spotify_correlation_matrx <- cor(spotify_data_nomralized)
ggcorrplot(spotify_correlation_matrx)

spotify_data.pca <- princomp(spotify_correlation_matrx)
summary(spotify_data.pca)

spotify_data.pca$loadings[, 1:2]

fviz_eig(spotify_data.pca, addlabels = TRUE)

# Graph of the variables
fviz_pca_var(spotify_data.pca, col.var = "blue")

fviz_cos2(spotify_data.pca, choice = "var", axes = 1:2)

fviz_pca_var(spotify_data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)
