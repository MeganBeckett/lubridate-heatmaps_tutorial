# Load packages 
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

# Read data ----------------------------------------------
app_train <- read_csv("raw_data/application_train.csv")
app_test <- read_csv("raw_data/application_test.csv")

# Explore data -------------------------------------------
summary(app_train)

# Missing values
missing_val_abs <- colSums(is.na(app_train))
missing_val_per <- missing_val_abs/nrow(app_train)*100
colnames <- colnames(app_train)
missing_summary <- subset(data.frame(missing_val_abs, missing_val_per), missing_val_abs > 0)
### There are 67 variables with missing values

# Distribution of classification 
hist(app_train$TARGET)
### There are many more loans paid on time than there aren't

# Variable types
table(sapply(app_train, class))
### There are 16 categorcal variables



