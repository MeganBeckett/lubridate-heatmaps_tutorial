# Model 1
# Basic logistic regression model using first 40 "clean" variables - ie no/very few missing data
data = app_train[c(2:21)]
summary(data)
model <- glm(TARGET~., data=data, family=binomial)

# Model 2
# Select some of the important varaibles from Model 1 (data = app_train2)
model <- glm(TARGET~NAME_CONTRACT_TYPE + CODE_GENDER + FLAG_OWN_CAR + AMT_CREDIT + 
               AMT_GOODS_PRICE + NAME_EDUCATION_TYPE + REGION_POPULATION_RELATIVE + DAYS_BIRTH +
               DAYS_EMPLOYED + DAYS_REGISTRATION + DAYS_ID_PUBLISH, data = app_train2, family = binomial)

# Model 3
# Logistc regression using all data after removed near variance variables (data= app_train3)
model <- glm(TARGET ~., data=app_train3, family = binomial)

# Model 4
# Logistic regression using selected variables by evaluating importance and significance (data = app_train3)
model <- glm(TARGET ~ EXT_SOURCE_3 + EXT_SOURCE_2 + EXT_SOURCE_1 + DAYS_BIRTH + CODE_GENDER + FLAG_OWN_CAR + FLAG_OWN_REALTY +
               AMT_CREDIT + AMT_GOODS_PRICE + AMT_ANNUITY + DAYS_EMPLOYED + DAYS_REGISTRATION + DAYS_ID_PUBLISH +
               FLAG_WORK_PHONE + FLAG_PHONE + OCCUPATION_TYPE + NAME_EDUCATION_TYPE + DAYS_LAST_PHONE_CHANGE + 
               REGION_POPULATION_RELATIVE + DEF_30_CNT_SOCIAL_CIRCLE + REG_CITY_NOT_LIVE_CITY + AMT_REQ_CREDIT_BUREAU_QRT, 
             data = app_train3, family = binomial)

# Model 5
# Logistic regression with added variables from Bureau (data = training)
model <- glm(TARGET ~ EXT_SOURCE_3 + EXT_SOURCE_2 + EXT_SOURCE_1 + DAYS_BIRTH + CODE_GENDER + FLAG_OWN_CAR + FLAG_OWN_REALTY +
               AMT_CREDIT + AMT_GOODS_PRICE + AMT_ANNUITY + DAYS_EMPLOYED + DAYS_REGISTRATION + DAYS_ID_PUBLISH +
               FLAG_WORK_PHONE + FLAG_PHONE + OCCUPATION_TYPE + NAME_EDUCATION_TYPE + DAYS_LAST_PHONE_CHANGE + 
               REGION_POPULATION_RELATIVE + DEF_30_CNT_SOCIAL_CIRCLE + REG_CITY_NOT_LIVE_CITY + AMT_REQ_CREDIT_BUREAU_QRT +
               total_prev_loans + total_AMT_CREDIT_SUM_DEBT + total_AMT_CREDIT_SUM_OVERDUE + total_CREDIT_DAY_OVERDUE +
               total_AMT_CREDIT_SUM, data = training, family = binomial)

# Model 6
# Decision tree using rpart, RandomForest

