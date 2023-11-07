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

fire_data <- na.omit(data)
dim(fire_data)

missing_values_cleaned <- sum(is.na(fire_data))
missing_values_cleaned
# Cleared the data with NA values 



