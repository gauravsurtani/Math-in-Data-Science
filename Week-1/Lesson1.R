a = 5
b = 6
sum(a,b)

data()

rowdata <- airquality
head(rowdata,7)
dim(rowdata)
summary(rowdata)

plot(rowdata$Wind,rowdata$Temp)

library(tidyverse)

rowdata_t <- as_tibble(rowdata)
head(rowdata_t)

summary(rowdata_t)

ggplot(data=rowdata_t) = geom_point(mapping = aes(x = Wind, y = Temp))