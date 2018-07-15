# Load packages 
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(skimr)

# Read data ----------------------------------------------
app_train <- read_csv("raw_data/application_train.csv")
app_test <- read_csv("raw_data/application_test.csv")
# >>> read_csv is faster but prefer taking advantage of read.csv() coercing characters to factors from beginning

# Explore Application_train data set -------------------------------------------
glimpse(app_train)
str(app_train)
summary(app_train)
skim(app_train)

# Variable types
table(sapply(app_train, class))
# >>> There are 16 categorical (character) variables

# Missing values -------------------------------------------
# Total missing values
sum(is.na(app_train))

# Missing values per variable
missing_val_abs <- colSums(is.na(app_train))
missing_val_per <- missing_val_abs/nrow(app_train)*100
colnames <- colnames(app_train)
missing_summary <- subset(data.frame(missing_val_abs, missing_val_per), missing_val_per > 40)
# >>> There are 67 variables with missing values
# >>> 49 variables have over 40% missing values - these are mostly variables associated with having property

# Exploring some (important?) variable distributions with summaries and plots -------------------------------------------
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

# Type of loan contract applied for
app_train %>%  
  count(NAME_CONTRACT_TYPE) %>%
  arrange(desc(n)) %>%
  ggplot(aes(reorder(NAME_CONTRACT_TYPE, -n, FUN = min), n, fill = NAME_CONTRACT_TYPE)) +
  geom_col() + 
  theme(legend.position = "none") + 
  labs(x = 'NAME_CONTRACT_TYPE',y = 'Count') +
  ggtitle("No of contract type of loan applied for")
# >>> Predominat loan type is cash loan

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

# Own any property
app_train %>%  
  count(FLAG_OWN_REALTY) %>%
  arrange(desc(n)) %>%
  ggplot(aes(reorder(FLAG_OWN_REALTY, -n, FUN = min), n, fill = FLAG_OWN_REALTY)) +
  geom_col() + 
  theme(legend.position = "none") + 
  labs(x = '',y = 'Count') +
  ggtitle("No of people who own property")
# >>> unlike owning a car, there are many more who own property

# Level of education of loan applicants
app_train %>%  
  count(NAME_EDUCATION_TYPE) %>%
  arrange(desc(n)) %>%
  ggplot(aes(reorder(NAME_EDUCATION_TYPE, -n, FUN = min), n, fill = NAME_EDUCATION_TYPE)) +
  geom_col() + 
  theme(legend.position = "none") + 
  labs(x = '',y = 'Count') +
  ggtitle("Level of education")
# >>> The majority have completed secondary school. 
# >>> There are a few with incomplete highschool or only lower secondary. This is an interesting flag.

# Income level of applicants
app_train %>%  
  count(NAME_INCOME_TYPE) %>%
  arrange(desc(n)) %>%
  ggplot(aes(reorder(NAME_INCOME_TYPE, -n, FUN = min), n, fill = NAME_INCOME_TYPE)) +
  geom_col() + 
  theme(legend.position = "none") + 
  labs(x = '',y = 'Count') +
  ggtitle("Income level type")
# >>> The majority are working. 
# >>> There is a significant portion of pensioners, which is interesting. 
# >>> There are also some unemplyed, students and those on materynity leave.
  
# Age of clients
summary(app_train$DAYS_BIRTH)
# >>> Interesting, these are negative numbers
# >>> Indicate the relative age of person in number of days backwards from loan application date
# >>> Convert to years and positive
age = as.data.frame(app_train$DAYS_BIRTH/(-365))
colnames(age) = 'AGE_YEARS'
summary(age)
hist(age$AGE_YEARS)
# >>> Median age is in the 40's and there is a wide distribution in ages from 20 to 70 years old

# Days of employment
summary(app_train$DAYS_EMPLOYED)
# >>> As with age in number of days, days of emplyment is no days before the application the person started current employment
# >>> Median is -1213 which is about 3 years prior to application
# >>> But, positive numbers don't make sense and there are massive outliers equating to about 1000 years (365243 days)
hist(app_train$DAYS_EMPLOYED)
# >>> Create subset of these outliers
employed_outliers <- subset(app_train, DAYS_EMPLOYED > 0)
employed_normal <- subset(app_train, DAYS_EMPLOYED <= 0)
summary(employed_outliers$TARGET)
summary(employed_normal$TARGET)
# >>> The outliers (all have same value of 365243 days) only default on 5.4% of loans whereas those with reasonable employment records, don't repay 8.66% of time
# >>> Investigate later by replacing these values with NA before imputation to replace missing values and see if has an effect.

# Exploring correlations between each variable and TARGET  -------------------------------------------

ggcorr(app_train[,2:40])
ggcorr(app_train[,c(2,41:80)])
ggcorr(app_train[,c(2,81:122)])

cor(app_train$TARGET, app_train[])

glm(TARGET ~ ., data=app_train, family=binomial)
