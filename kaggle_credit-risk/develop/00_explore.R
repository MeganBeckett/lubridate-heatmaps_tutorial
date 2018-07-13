# Load packages 
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

# Read data ----------------------------------------------
app_train <- read_csv("raw_data/application_train.csv")
app_test <- read_csv("raw_data/application_test.csv")

# Explore data -------------------------------------------
glimpse(app_train)
str(app_train)
summary(app_train)

# Missing values
# Total missing values
sum(is.na(app_train))

# Missing values per variable
missing_val_abs <- colSums(is.na(app_train))
missing_val_per <- missing_val_abs/nrow(app_train)*100
colnames <- colnames(app_train)
missing_summary <- subset(data.frame(missing_val_abs, missing_val_per), missing_val_per > 40)
# >>> There are 67 variables with missing values
# >>> 49 variables have over 40% missing values

# Variable types
table(sapply(app_train, class))
# >>> There are 16 categorical variables

# Baseline model accuracyif predicted everyone repaid loan
sum(app_train$TARGET == 0)/nrow(app_train)

# Exploring some (important?) variable distributions
# Target variable - classification
app_train %>%  
  count(TARGET) %>%
  arrange(desc(n)) %>%
  ggplot(aes(reorder(TARGET, -n, FUN = min), n, fill = TARGET)) +
  geom_col() + 
  theme(legend.position = "none") + 
  labs(x = 'TARGET (0 = repaid, 1 = did not repay)',y = 'Count') +
  ggtitle("Distribution of classification")
# >>> Binary classification (0 if loan repaid, 1 if loan not repaid)
# >>> There are many more loans paid on time than there aren't

# Gender
app_train %>%  
  count(CODE_GENDER) %>%
  arrange(desc(n)) %>%
  ggplot(aes(reorder(CODE_GENDER, -n, FUN = min), n, fill = CODE_GENDER)) +
  geom_col() + 
  theme(legend.position = "none") + 
  labs(x = 'CODE_GENDER',y = 'Count') +
  ggtitle("No of loan applications per gender")
# >>> More females applying for loans than males

# Type of contract
app_train %>%  
  count(NAME_CONTRACT_TYPE) %>%
  arrange(desc(n)) %>%
  ggplot(aes(reorder(NAME_CONTRACT_TYPE, -n, FUN = min), n, fill = NAME_CONTRACT_TYPE)) +
  geom_col() + 
  theme(legend.position = "none") + 
  labs(x = 'NAME_CONTRACT_TYPE',y = 'Count') +
  ggtitle("No of contract type of loan applied for")
# >>> Predominat loan type is cash loan

# Own a car
app_train %>%  
  count(FLAG_OWN_CAR) %>%
  arrange(desc(n)) %>%
  ggplot(aes(reorder(FLAG_OWN_CAR, -n, FUN = min), n, fill = FLAG_OWN_CAR)) +
  geom_col() + 
  theme(legend.position = "none") + 
  labs(x = 'FLAG_OWN_CAR',y = 'Count') +
  ggtitle("No of people who own a car")
# >>> About 50% less people own a car than those who don't


S
appLog = glm(TARGET ~ ., data=app_train, family=binomial)


