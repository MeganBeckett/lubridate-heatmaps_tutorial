# Run model on final app_test data
TARGET <- predict(model, app_test4, type = "response")

# Create submission file
submission <- cbind(app_test, TARGET)
sub_vars <- c("SK_ID_CURR", "TARGET")
submission <- submission[, sub_vars]

write.csv(submission, "submission.csv", row.names = FALSE)
