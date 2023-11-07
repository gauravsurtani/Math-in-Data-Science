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

missing_values_cleaned <- sum(is.na(spotify_data))
missing_values_cleaned
# Cleared the data with NA values 


constant_value <- 150000000  # Replace with the actual known constant value
# Remove missing values from 'streams'
streams <- data$streams[!is.na(data$streams)]
streams <- as.numeric(streams[!is.na(streams) & grepl("^\\d+$", streams)])
# Check if there are still missing values
if (any(is.na(streams))) {
  stop("There are still missing values in 'streams'. Please handle missing values.")
}

# Perform one-sample t-test
sample_hypothesis_result <- t.test(streams, mu = constant_value)

# Display test results
print(sample_hypothesis_result)

# Check if null hypothesis is rejected
if (sample_hypothesis_result$p.value < 0.05) {
  cat("Reject the null hypothesis\n")
} else {
  cat("Fail to reject the null hypothesis\n")
}

# Assuming 'danceability_.' is the variable of interest
# Assuming 'Group1' and 'Group2' are the two groups in your dataset
# Replace with the actual variable names
group1 <- data[data$artist_count == 1, "danceability_."]
group2 <- data[data$artist_count == 2, "danceability_."]

# Remove missing values from both groups
group1 <- group1[!is.na(group1)]
group2 <- group2[!is.na(group2)]

# Check if there are still missing values
if (any(is.na(group1)) || any(is.na(group2))) {
  stop("There are still missing values in 'danceability_.'. Please handle missing values.")
}

# Perform two-sample t-test
t_result <- t.test(group1, group2)

# Display test results
print(t_result)

# Check if null hypothesis is rejected
if (t_result$p.value < 0.05) {
  cat("Reject the null hypothesis\n")
} else {
  cat("Fail to reject the null hypothesis\n")
}


