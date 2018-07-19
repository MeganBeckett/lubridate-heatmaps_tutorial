# Split app_train into training and testing data
set.seed(123)
app_train4$TARGET = factor(app_train4$TARGET)
in_train <- createDataPartition(app_train4$TARGET, p=0.9, list=FALSE)
training <- app_train4[ in_train, ]
testing <- app_train4[ -in_train, ]
