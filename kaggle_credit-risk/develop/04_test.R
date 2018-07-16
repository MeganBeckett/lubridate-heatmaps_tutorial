# Test model on training set
prediction <- predict(model, app_train4, type = "response")
summary(prediction)

confusion = table(app_train4$TARGET, prediction >= 0.0807)
confusion


# Run model on testing set
TARGET <- predict(model, app_test4, type = "response")
