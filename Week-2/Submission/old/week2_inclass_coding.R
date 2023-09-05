# ISE 201 Week 1 In Class Coding Exercise
# Created by Shilpa Gupta
# Edited by Gaurav Surtani

# Include Tidyverse library
library(tidyverse)

# Import Data

indata <- read.csv("data/FertilityRates.csv")

# Check if data import was as expected
#help (head)
dim(indata)
head(indata)

# Is it a data frame or a tibble?
is.data.frame(indata)

indata <- as_tibble(indata)
is_tibble(indata)

# Summary of data
summary(indata)

# Convert Country Name into a factor
indata$Country.Name <- as.factor(indata$Country.Name)

summary(indata$Country.Name)

# Convert Indicator Name into a factor
indata$Indicator.Name <- as.factor(indata$Indicator.Name)
summary(indata$Indicator.Name)

# Remove extra columns
indata_cleaned <- select(indata, -(Country.Code:Indicator.Code))

head(indata_cleaned)

# Pivot to long dataset
indata_pivoted <- pivot_longer(indata_cleaned, c(str_c("X", c(1960:2011))), names_to = "Year", values_to="Fertility.Rates")

head(indata_pivoted)

# Format Year values
indata_pivoted$Year <- as.integer(str_sub(indata_pivoted$Year, 2,5))

# Check Missing Values
sum(is.na(indata_pivoted$Fertility.Rates))

# Check missing values by country

## Step 1: Filter by missing values
## Step 2: group by country
## Step 3: Count number of values

indata_pivoted %>%  filter(is.na(Fertility.Rates)) %>% group_by(Country.Name) %>% summarise(count = n())


# Fill missing values within countries

indata_filled <- indata_pivoted %>% group_by(Country.Name) %>% fill(Fertility.Rates, .direction ="downup") %>% ungroup()

## Check
indata_filled %>%  filter(is.na(Fertility.Rates)) %>% group_by(Country.Name) %>% summarise(count = n())

indata_filled <- indata_filled %>% filter(!is.na(Fertility.Rates))

ggplot(data = indata_filled) + geom_point(mapping = aes(x=Year, y = Fertility.Rates))

# Plot Fertility Rates by Year
ggplot(data = indata_filled) + geom_point(mapping = aes(x=Year, y = Fertility.Rates), position = "jitter", alpha = 0.1) + geom_smooth(mapping = aes(x=Year, y = Fertility.Rates)) + labs(x = "Year", y = "Fertility Rates", title = "Fertility Rates over the years", subtitle = "Global fertility rates have decreased since 1960")


# Need a way to categorize 210 countries

## Option 1: subset the data
## Filter the data to only use the Fertility Rates from USA , Mexico & Canada
indata_subset <- filter(indata_filled, Country.Name %in% c("United States", "Mexico", "Canada"))


ggplot(data = indata_subset) + geom_line(mapping = aes(x=Year, y = Fertility.Rates, color = Country.Name), alpha = 0.5) + labs(x = "Year", y = "Fertility Rates", title = "Fertility Rates over the years", subtitle = "Global fertility rates have decreased since 1960")


## Option 2: Select based on statistics

# Top 10
# Comment
indata_filled %>%  group_by(Country.Name) %>% summarise(avg = mean(Fertility.Rates)) %>% arrange(desc(avg)) %>% print(n = 10)


# Bottom 10

indata_filled %>%  group_by(Country.Name) %>% summarise(avg = mean(Fertility.Rates)) %>% arrange(desc(avg)) %>% top_n(10) %>% ggplot() + geom_bar(mapping = aes(x = Country.Name, y = avg), stat = "identity") +coord_flip()


### Option
library(countrycode)

indata_df <- as.data.frame(indata_filled)
indata_df$Continent.Name <- factor(countrycode(sourcevar = indata_df[,"Country.Name"], origin = "country.name", destination = "continent"))


indata_df$Region.Name <- factor(countrycode(sourcevar = indata_df[,"Country.Name"], origin = "country.name", destination = "region"))

head(indata_df)

indata <- as_tibble(indata_df)


ggplot(data = indata) + geom_point(mapping = aes(x=Year, y = Fertility.Rates, color = Continent.Name), position = "jitter", alpha = 0.6) + labs(x = "Year", y = "Fertility Rates", title = "Fertility Rates over the years", subtitle = "Global fertility rates have decreased since 1960")

# Facet Plotting
ggplot(data =  indata) + geom_point(mapping = aes(x=Year, y = Fertility.Rates, color = Country.Name), position = "jitter", alpha = 0.2, show.legend = FALSE) + facet_wrap(~ Region.Name)


# Filtering and Potting
indata %>% filter(Region.Name == "Middle East & North Africa" & Continent.Name == "Asia") %>% ggplot() + geom_line(mapping = aes(x =Year, y = Fertility.Rates, color =  Country.Name), size = 1, linetype = 2)

# Boxplot

ggplot(data =  indata, mapping = aes(x = Continent.Name, y = Fertility.Rates)) + geom_boxplot() + coord_flip()

# Histogram
ggplot(data = indata) + geom_histogram(mapping = aes(x = Fertility.Rates), binwidth = 0.5)


# Barchart

indata %>% group_by(Region.Name) %>% summarise(avg = mean(Fertility.Rates)) %>% ggplot(mapping = aes(x = reorder(Region.Name, -avg), y = avg, fill=Region.Name)) + geom_bar(stat = "identity") + coord_flip()
summary(indata$Continent.Name)
