library(caret)

# Clean data --------------------------------------------------------------------------------------------
# Replacing outlier for DAYS_EMPLOYED with NA 
# I saw this in the discssion from HomeCredit https://www.kaggle.com/c/home-credit-default-risk/discussion/57247
app_train$DAYS_EMPLOYED <- replace(app_train$DAYS_EMPLOYED,app_train$DAYS_EMPLOYED == 365243,NA)
app_test$DAYS_EMPLOYED <- replace(app_test$DAYS_EMPLOYED,app_test$DAYS_EMPLOYED == 365243,NA)

# Replace outlier for AMT_TOTAL_INCOME with NA
app_train$AMT_INCOME_TOTAL <- replace(app_train$AMT_INCOME_TOTAL,app_train$AMT_INCOME_TOTAL == 117000000,NA)
summary(app_train$AMT_INCOME_TOTAL)

# Missing values ----------------------------------------------------------------------------------------
# Decision made to replace missing categorical variables with another level ("Not available") and convert to factors
# And replace missing numerical values median of column values

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

# Removing near zero variance variables -----------------------------------------------------------------
# I did my first logistic regression model without removing any near zero variance variables 
# and then came back and performed this preprocessing step to further help with identifying key variables.
# Set conservative uniqueCut = 1
# And then remove columns from imputed datasets
nzv_summary <- nearZeroVar(app_train, names = TRUE, uniqueCut = 1, saveMetrics = TRUE)
remove_cols_nzv <- nearZeroVar(app_train, names = TRUE, uniqueCut = 1)

# Training data
all_cols <- names(app_train)
app_train3 <- app_train2[, setdiff(all_cols, remove_cols_nzv)]

# Testing data
all_cols <- names(app_test)
app_test3 <- app_test2[, setdiff(all_cols, remove_cols_nzv)]

# Adding features from Bureau dataset ----------------------------------------------------------------------
# Create a summary table of the previous loans each applicant has made that are listed in the Credit Bureau.
# Set NA values to 0 first in Bureau
# Group by the current loan ID, then calculate the following variables to be associated with each current loan application:
# - total previous loans made
# - total days past due on CB for all loans
# - total current credit amount for all loans in CB
# - total current debt on CB credit for all loans
# - total current amount overdue on CB credit for all loans

bureau[is.na(bureau)] <- 0

bureau_summary = bureau %>%
  group_by(SK_ID_CURR) %>%
  summarise(total_prev_loans = n(), 
            total_AMT_CREDIT_SUM_DEBT = sum(AMT_CREDIT_SUM_DEBT),
            total_AMT_CREDIT_SUM_OVERDUE = sum(AMT_CREDIT_SUM_OVERDUE),
            total_CREDIT_DAY_OVERDUE = sum(CREDIT_DAY_OVERDUE),
            total_AMT_CREDIT_SUM = sum(AMT_CREDIT_SUM))

# Combine new Bureau variables
app_train4 = app_train3 %>% 
  left_join(bureau_summary, by = 'SK_ID_CURR') 
app_train4[is.na(app_train4)] <- 0

app_test4 = app_test3 %>% 
  left_join(bureau_summary, by = 'SK_ID_CURR') 
app_test4[is.na(app_test4)]<-0

