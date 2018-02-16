## Read the Original csv file
titanic.users.data=read.csv(".\\data\\titanic_original.csv",stringsAsFactors = F);

## Copy to new dataframe
titanic.clean.data <- titanic.users.data;

## Change all missing embarked values as 'S'
titanic.clean.data$embarked <- ifelse(titanic.clean.data$embarked=="","S",titanic.clean.data$embarked);

## Calculate the mean value of the Age Column and use it to populate the missing Age(NA)
mean_age <- mean(titanic.clean.data$age,na.rm = TRUE)
titanic.clean.data[is.na(titanic.clean.data)] <- mean_age

## Change all missing boat values as 'None'
titanic.clean.data$boat <- ifelse(titanic.clean.data$boat=="","None",titanic.clean.data$boat);

## Create a new column has_cabin_number which has 1 if there is a cabin number, and 0 otherwise
titanic.clean.data$has_cabin_number <- ifelse(titanic.clean.data$cabin=="",0,1)

## Wtite the clean data into a CSV file
write.csv(titanic.clean.data,file=".\\data\\titanic_clean.csv")


#table(titanic.clean.data$embarked)
#unique(titanic.clean.data$age)
#unique(titanic.clean.data$boat)
#unique(titanic.clean.data$cabin)
#unique(titanic.clean.data$has_cabin_number)



