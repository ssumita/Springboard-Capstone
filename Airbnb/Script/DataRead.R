#age.gender.data=read.csv(".\\data\\age_gender_bkts.csv");
#countries.data=read.csv(".\\data\\countries.csv");
#ndf.data=read.csv(".\\data\\sample_submission_NDF.csv");

### Read the input datasets

sessions.data=read.csv(".\\data\\sessions.csv",stringsAsFactors = F);
test.users.data=read.csv(".\\data\\test_users.csv",stringsAsFactors = F);
train.users.data=read.csv(".\\data\\train_users_2.csv",stringsAsFactors = F)

## Install packages and load them

install.packages("dplyr")
library(dplyr)
library(openxlsx)

install.packages("caret")
library(caret)

install.packages("caTools")
library(caTools)

install.packages("ROCR")
library(ROCR)

install.packages("rpart")
library(rpart)

## replace -unknown- as unknown

sessions.data=sessions.data%>%mutate(action=ifelse(action=="-unknown-","unknown",action));

sessions.data=sessions.data%>%mutate(action_type=ifelse(action_type=="-unknown-","unknown",action_type));

sessions.data=sessions.data%>%mutate(action_detail=ifelse(action_detail=="-unknown-","unknown",action_detail));

sessions.data=sessions.data%>%mutate(device_type=ifelse(device_type=="-unknown-","unknown",device_type));

## Replace Blanks as Not Given

sessions.data=sessions.data%>%mutate(action=ifelse(action=="","Not Given",action));

sessions.data=sessions.data%>%mutate(action_type=ifelse(action_type=="","Not Given",action_type));

sessions.data=sessions.data%>%mutate(action_detail=ifelse(action_detail=="","Not Given",action_detail));

sessions.data=sessions.data%>%mutate(device_type=ifelse(device_type=="","Not Given",device_type));


################################################################################

#Getting count and percentage of counts for each categorical variable in sessions.data


df=as.data.frame(sessions.data%>%group_by(action)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

df=as.data.frame(sessions.data%>%group_by(action_type)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

df=as.data.frame(sessions.data%>%group_by(action_detail)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

df=as.data.frame(sessions.data%>%group_by(device_type)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

#########################################################################

# Getting counts and percentage and cumulative percentage of counts for categorical variables in Training user data

df=as.data.frame(train.users.data%>%group_by(gender)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

df=as.data.frame(train.users.data%>%group_by(signup_method)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

df=as.data.frame(train.users.data%>%group_by(language)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

df=as.data.frame(train.users.data%>%group_by(affiliate_channel)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

df=as.data.frame(train.users.data%>%group_by(affiliate_provider)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

df=as.data.frame(train.users.data%>%group_by(first_affiliate_tracked)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

df=as.data.frame(train.users.data%>%group_by(signup_app)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

df=as.data.frame(train.users.data%>%group_by(first_device_type)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

df=as.data.frame(train.users.data%>%group_by(first_browser)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

#################################################################################

# Getting counts and percentage and cumulative percentage of counts for categorical variables in Test user data

df=as.data.frame(test.users.data%>%group_by(gender)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

df=as.data.frame(test.users.data%>%group_by(signup_method)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

df=as.data.frame(test.users.data%>%group_by(language)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

df=as.data.frame(test.users.data%>%group_by(affiliate_channel)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

df=as.data.frame(test.users.data%>%group_by(affiliate_provider)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

df=as.data.frame(test.users.data%>%group_by(first_affiliate_tracked)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

df=as.data.frame(test.users.data%>%group_by(signup_app)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

df=as.data.frame(test.users.data%>%group_by(first_device_type)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))

df=as.data.frame(test.users.data%>%group_by(first_browser)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))


################################################################################

## Copying a dataframe from R to Excel

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}
write.excel(df)