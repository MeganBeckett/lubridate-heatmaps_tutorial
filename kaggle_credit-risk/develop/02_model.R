library(caret)

# Model 1
# Basic logistic regression model using first 40 "clean" variables - ie no/very few missing data
data = app_train[c(2:21)]
summary(data)
model <- glm(TARGET~., data=data, family=binomial)
summary(model)

importance <- data.frame(varImp(model, scale=TRUE))
importance

# Model 2
# Select some of the important varaibles from Model 1
model <- glm(TARGET~NAME_CONTRACT_TYPE + CODE_GENDER + FLAG_OWN_CAR + AMT_CREDIT + 
               AMT_GOODS_PRICE + NAME_EDUCATION_TYPE + REGION_POPULATION_RELATIVE + DAYS_BIRTH +
               DAYS_EMPLOYED + DAYS_REGISTRATION + DAYS_ID_PUBLISH, data = data, family = binomial)

# Model 3
# Logistc regression 
model <- glm(TARGET ~., data=app_train2, family = binomial)


