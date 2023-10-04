# Read data from CSV file
data <- read.csv("employee-compensation-2022.csv")

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
null_values <- sum(is.null(data))
cat("Number of missing values:", missing_values, "\n")
cat("Number of null values:", null_values, "\n")

cat("Proportion of missing values:", mean(is.na(data)), "\n")

employee_compensation <- na.omit(data)
dim(employee_compensation)

missing_values_cleaned <- sum(is.na(employee_compensation))
missing_values_cleaned


#Check for duplicates
duplicates <- duplicated(employee_compensation)
cat("Number of duplicate rows:", sum(duplicates), "\n")

# There are no duplicates in this data

# Now filter top 10 jobs in this dataset based on the count

# Count occurrences of each job title
job_title_counts <- table(employee_compensation$Job.Title..as.of.12.31.22.)

# Sort in descending order
sorted_job_title_counts <- sort(job_title_counts, decreasing = TRUE)

# Top 10 job titles
top_10_job_titles <- head(names(sorted_job_title_counts), 10)
print(top_10_job_titles)

#Filter data to display on only data with the top 10 jobs
employee_compensation_top10 <- subset(employee_compensation,`Job.Title..as.of.12.31.22.` %in% top_10_job_titles)
employee_compensation_top10

# convert all strings columns to float
columns_to_convert <- names(employee_compensation_top10)[4:ncol(employee_compensation_top10)]
columns_to_convert

# All the data is string, so we need to convert it to float or integer for numerical categorization
employee_compensation_top10[columns_to_convert] <- lapply(employee_compensation_top10[columns_to_convert], function(x) {
  numeric_values <- as.numeric(gsub(",", "", x))
  numeric_values[is.na(numeric_values)] <- 0
  return(numeric_values)
})

#Checking again for missing values after conversion from String to Integer
missing_values1 <- sum(is.na(employee_compensation_top10))
null_values1 <- sum(is.null(employee_compensation_top10))
cat("Number of missing values:", missing_values1, "\n")
cat("Number of null values:", null_values1, "\n")


