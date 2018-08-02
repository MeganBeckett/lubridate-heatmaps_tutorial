# Summarise and investigate model ----------------------------------------------------------------
summary(model)

importance <- data.frame(varImp(model, scale=TRUE))
importance

# Evaluate using classification rate ----------------------------------------------------------------
# I'm getting an error related to the data and reference and should be factors with the same levels
# Haven't resolved yet

prediction <- predict(model, newdata = testing, type = "response")
accuracy <- table(prediction, testing[, "TARGET"])
confusionMatrix(data=table(prediction, testing$TARGET))

#summary(prediction)

#confusion = table(testing$TARGET, prediction >= 0.5)
#confusion

# Evaluating using AUROC ----------------------------------------------------------------

library(ROCR)
prob <- predict(model, newdata = testing, type = "response")
pred <- prediction(prob, testing$TARGET)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc
