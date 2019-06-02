library (tidyverse)
library(ggplot2)
library(RColorBrewer)
library(survival)
library(doBy)
library(dplyr)


#General look at the data to check data elements to cofirm initial accuracy

ggplot(data = cbWomen) + geom_bar(mapping = aes(x=runTime))

ggplot(data = cbWomen) + geom_bar(mapping = aes(x=year))

ggplot(data = cbWomen) + geom_bar(mapping = aes(x=age))

#Exploring Age by Year
#Summary table and dot plot (Year, Mean and Median Age, Count)
#table
cbWomen$year = as.factor(cbWomen$year)
ages = group_by(cbWomen, year)
ageSummary = summarize(ages, ageMean = mean(age, na.rm = TRUE),ageMedian = median(age, na.rm = TRUE), count=n())
ageSummary
#dot plot
ggplot(ageSummary, aes(x=year, y=ageMean, group=1)) +
  geom_line() +
  geom_point() + 
  ggtitle("Mean Age by Year") +
  xlab("Year") +
  ylab("Age") + 
  theme(plot.title = element_text(hjust = 0.5))

#Exploring age and time

smoothScatter(y = cbWomen$runTime, x = cbWomen$age, 
              ylim = c(40, 165), xlim = c(15, 85),
              xlab = "Age (years)", ylab = "Run Time (minutes)")


#Bin the ages by 10s and view boxplot of age vs runTime

cbWomenGroup <- cbWomen[cbWomen$runTime > 30 & !is.na(cbWomen$age) & cbWomen$age > 10, ]
ageGroup <- cut(cbWomenGroup$age, breaks = c(seq(10, 80, 10), 90))
table(ageGroup)

plot(cbWomenGroup$runTime ~ ageGroup, xlab = "Age (years)", ylab = "Run Time (minutes)")

#Looking at most variables together
ggplot(cbWomen, aes(year, runTime, col = age)) +
  geom_point()

#Linear model of runTime and age
linearAge <- lm(runTime ~ age, data = cbWomenGroup)
summary(linearAge)

#plot residuals of linear model
smoothScatter(x = cbWomenGroup$age, y = linearAge$residuals, xlab = "Age (years)", ylab = "Residuals")
abline(h = 0, col = "purple", lwd = 3)
