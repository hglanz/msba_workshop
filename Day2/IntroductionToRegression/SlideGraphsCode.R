library(tidyverse)
library(ggthemes)
library(ggLinearModel)
library(ISLR)

data <- Carseats


data %>%
    ggplot(aes(x = Sales)) +
    geom_histogram() +
    ggtitle("Carseat Sales") +
    xlab("Sales (thousands)")


data %>%
    ggplot(aes(y = Sales)) +
    geom_boxplot() +
    ggtitle("Carseat Sales") +
    ylab("Sales (thousands)") +
    theme_gray()

data %>%
    ggplot(aes(x = "Sales", y = Sales)) +
    geom_point() +
    ggtitle("Carseat Sales") +
    ylab("Sales (thousands)")


data %>%
    ggplot(aes(x = "Sales", y = Sales)) +
    geom_point() +
    geom_hline(yintercept = mean(data$Sales), col = "blue", size = 2) +
    ggtitle("Carseat Sales") +
    ylab("Sales (thousands)")


data %>%
    ggplot(aes(x = Sales, fill = US)) +
    geom_histogram() +
    facet_wrap(.~US, nrow = 2) +
    ggtitle("Carseat Sales in U.S. or Not") +
    xlab("Sales (thousands)")
    

data %>%
    ggplot(aes(x = US, y = Sales, fill = US)) +
    geom_boxplot() +
    ggtitle("Carseat Sales in U.S. or Not") +
    ylab("Sales (thousands)")


data %>%
    ggplot(aes(x = US, y = Sales, color = US)) +
    geom_point() +
    ggtitle("Carseat Sales in U.S. or Not") +
    ylab("Sales (thousands)")

data %>%
    ggplot(aes(x = US, y = Sales, color = US)) +
    geom_point() +
    geom_hline(yintercept = mean(data$Sales[data$US == "Yes"]), col = "blue", size = 2) +
    geom_hline(yintercept = mean(data$Sales[data$US == "No"]), col = "red", size = 2) +
    ggtitle("Carseat Sales in U.S. or Not") +
    ylab("Sales (thousands)")

mean_Sales <- data.frame(US = c("No", "Yes"), Sales = tapply(data$Sales, data$US, mean))
data %>%
    ggplot(aes(x = US, y = Sales, color = US)) +
    geom_point() +
    geom_hline(aes(yintercept = Sales, color = US), data = mean_Sales, size = 2) +
    facet_wrap(~US) +
    ggtitle("Carseat Sales in U.S. or Not") +
    ylab("Sales (thousands)")


data %>%
    ggplot(aes(x = Price, y = Sales)) +
    geom_point() +
    ggtitle("Carseat Sales vs. Price") +
    ylab("Sales (thousands)")


data %>%
    ggplot(aes(x = Price, y = Sales)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle("Carseat Sales vs. Price") +
    ylab("Sales (thousands)")


data %>%
    ggplot(aes(x = Price, y = Sales, color = US)) +
    geom_point(size = 2) +
    ggtitle("Carseat Sales vs. Price") +
    ylab("Sales (thousands)")


data %>%
    ggplot(aes(x = Price, y = Sales, color = US)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle("Carseat Sales vs. Price") +
    ylab("Sales (thousands)")

ggLinearModel(data, Price, Sales, US)

ggLinearModel(data, Price, Sales, US, same_slope = TRUE)

ggLinearModel(data, Price, Sales, US, same_intercept = TRUE)

ggLinearModel(data, Price, Sales, US, poly = 3)
