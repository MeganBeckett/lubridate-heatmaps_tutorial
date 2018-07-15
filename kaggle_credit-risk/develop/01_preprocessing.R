# Removing missing values
# Replace catageorical values 

data2 = app_train

m=nrow(data2);n=ncol(data2)
for (c in 1:ncol(data2))
{if(is.factor(data2[[c]])&& levels(data2[[c]])[1]=="")
{levels(data2[[c]])[1]='NS'}  #NS=not specified
}


full = bind_rows(app_train, app_test)
levels(data$NAME_FAMILY_STATUS)
unique(data$NAME_FAMILY_STATUS)
