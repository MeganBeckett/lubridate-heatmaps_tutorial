# Removing near zero variance variables ----------------------------------------------
# Use raw data sets to find near zero variance predictors, setting conservative uniqueCut = 1
# And then remove columns from imputed datasets
nzv_summary <- nearZeroVar(app_train, names = TRUE, uniqueCut = 1, saveMetrics = TRUE)
remove_cols_nzv <- nearZeroVar(app_train, names = TRUE, uniqueCut = 1)

# Training data
all_cols <- names(app_train)
app_train3 <- app_train2[, setdiff(all_cols, remove_cols_nzv)]

# Testing data
all_cols <- names(app_test)
app_test3 <- app_test2[, setdiff(all_cols, remove_cols_nzv)]

# Create a summary table of previous loans in the credit bureua where I group by each current loan ID,
# then calculate the total number of previous loans, the total current debt on the Credit Bureau and the total
# current amount overdue on Credit Bureau credit

bureau_summary = bureau %>%
  group_by(SK_ID_CURR) %>%
  summarise(total_prev_loans = n(), 
            total_AMT_CREDIT_SUM_DEBT = sum(AMT_CREDIT_SUM_DEBT),
            total_AMT_CREDIT_SUM_OVERDUE = sum(AMT_CREDIT_SUM_OVERDUE))

# Combine new Bureau variables
app_train4 = app_train3 %>% 
  left_join(bureau_summary, by = 'SK_ID_CURR') 
app_train4[is.na(app_train4)]<-0

app_test4 = app_test3 %>% 
  left_join(bureau_summary, by = 'SK_ID_CURR') 
app_test4[is.na(app_test4)]<-0
