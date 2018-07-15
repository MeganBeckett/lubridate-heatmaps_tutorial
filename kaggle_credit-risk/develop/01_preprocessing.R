# Clean data
# Replacing outlier for DAYS_EMPLOYED with NA 
# I saw this in the discssion from HomeCredit https://www.kaggle.com/c/home-credit-default-risk/discussion/57247

app_train$DAYS_EMPLOYED <- replace(app_train$DAYS_EMPLOYED,app_train$DAYS_EMPLOYED == 365243,NA)
app_test$DAYS_EMPLOYED <- replace(app_test$DAYS_EMPLOYED,app_test$DAYS_EMPLOYED == 365243,NA)

# Handling missing values
# Decision made to replace missing categi=orical variables with another level ("Not available") and convert to factors
# For numeric, decision made to replace with 0 for now as most NA values are related to not having aproperty so should be 0
# Note to self: Try in future to replace with median of column

# Training data
chr <- app_train[, sapply(app_train, is.character)]
num <- app_train[, sapply(app_train, is.numeric)]

chr[is.na(chr)] <- "Not Available"

fac <- chr %>% 
  lapply(as.factor) %>% 
  as_data_frame()

app_train <- bind_cols(fac, num)
app_train[is.na(app_train)] <- 0

# Testing data 
chr <- app_test[, sapply(app_test, is.character)]
num <- app_test[, sapply(app_test, is.numeric)]

chr[is.na(chr)] <- "Not Available"

fac <- chr %>% 
  lapply(as.factor) %>% 
  as_data_frame()

app_test <- bind_cols(fac, num)
app_test[is.na(app_test)] <- 0