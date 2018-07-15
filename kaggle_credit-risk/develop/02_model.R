# Basic logistic regression model using first 40 "clean" variables - ie no/very few missing data

data = app_train[c(2:21)]
summary(data)

model <- glm(TARGET~., data=data, family=binomial)


summary(model)

library(caret)

importance <- data.frame(varImp(model, scale=TRUE))
importance

model2 = glm(TARGET~NAME_CONTRACT_TYPE + CODE_GENDER + FLAG_OWN_CAR + AMT_CREDIT + 
               AMT_GOODS_PRICE + NAME_EDUCATION_TYPE + REGION_POPULATION_RELATIVE + DAYS_BIRTH +
               DAYS_EMPLOYED + DAYS_REGISTRATION + DAYS_ID_PUBLISH, data = data, family = binomial )

prediction <- predict(model2, data, type = "response")
summary(prediction)

confusion = table(data$TARGET, prediction >= 0.0807)
confusion


TARGET <- predict(model2, app_test, type = "response")
submission <- cbind(app_test, TARGET)
sub_vars <- c("SK_ID_CURR", "TARGET")
submission <- submission[, sub_vars]

write.csv(submission, "submission.csv", row.names = FALSE)

full <- bind_rows(app_train, app_test)

target <- app_train$TARGET
id <- app_test$SK_ID_CURR
full[,c('SK_ID_CURR','TARGET')] <- NULL

chr <- full[,sapply(full, is.character)]
num <- full[,sapply(full, is.numeric)]

chr[is.na(chr)] <- "Not Available"

fac <- chr %>% 
  lapply(as.factor) %>% 
  as_data_frame()


full <- bind_cols(fac, num)
rm(chr, fac, num)

full[is.na(full)] <- 0

num <- train[, sapply(train,is.numeric)]

rm(train, test)

train <- full[1:length(Target),]
test <- full[(length(Target)+1):nrow(full),]