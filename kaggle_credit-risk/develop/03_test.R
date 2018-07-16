# Test model on training set
prediction <- predict(model, app_train3, type = "response")
summary(prediction)

confusion = table(app_train2$TARGET, prediction >= 0.0807)
confusion


# Run model on testing set
TARGET <- predict(model, app_test3, type = "response")
