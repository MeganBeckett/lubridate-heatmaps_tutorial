# Test model on training set
prediction <- predict(model, app_train, type = "response")
summary(prediction)

confusion = table(app_train$TARGET, prediction >= 0.0807)
confusion


# Run model on testing set
TARGET <- predict(model, app_test, type = "response")
