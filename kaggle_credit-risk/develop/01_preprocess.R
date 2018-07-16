library(caret)

# Clean data ----------------------------------------------
# Replacing outlier for DAYS_EMPLOYED with NA 
# I saw this in the discssion from HomeCredit https://www.kaggle.com/c/home-credit-default-risk/discussion/57247

app_train$DAYS_EMPLOYED <- replace(app_train$DAYS_EMPLOYED,app_train$DAYS_EMPLOYED == 365243,NA)
app_test$DAYS_EMPLOYED <- replace(app_test$DAYS_EMPLOYED,app_test$DAYS_EMPLOYED == 365243,NA)

# Handling missing values ----------------------------------------------
# Decision made to replace missing categorical variables with another level ("Not available") and convert to factors
# Replacing NA with median of column values

# Define median function
NA2median <- function(x){
  if(is.numeric(x)){
    replace(x, is.na(x), median(x, na.rm=TRUE))
  }
}

# Training data
chr <- app_train[, sapply(app_train, is.character)]
num <- app_train[, sapply(app_train, is.numeric)]

chr[is.na(chr)] <- "Not Available"

fac <- chr %>% 
  lapply(as.factor) %>% 
  as_data_frame()

num <- replace(app_train, TRUE, lapply(app_train, NA2median))
app_train2 <- bind_cols(fac, num)

# Testing data 
chr <- app_test[, sapply(app_test, is.character)]
num <- app_test[, sapply(app_test, is.numeric)]

chr[is.na(chr)] <- "Not Available"

fac <- chr %>% 
  lapply(as.factor) %>% 
  as_data_frame()

num <- replace(app_test, TRUE, lapply(app_test, NA2median))
app_test2 <- bind_cols(fac, num)

# Removing near zero variance variables ----------------------------------------------
# Use raw data sets to find near zero variance predictors, setting conservative uniqueCut = 1
# And then remove columns from imputed datasets
nzv_summary <- nearZeroVar(app_train, names = TRUE, uniqueCut = 1, saveMetrics = TRUE)
remove_cols_nzv <- nearZeroVar(app_train, names = TRUE, uniqueCut = 1)

# Training data
all_cols <- names(app_train)
app_train3 <- app_train2[, setdiff(all_cols, remove_cols)]

# Testsing data
all_cols <- names(app_test)
app_test3 <- app_test2[, setdiff(all_cols, remove_cols)]
