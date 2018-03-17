################################################################

###     Data cleaning on Sessions.data 

################################################################

## replace -unknown- as unknown and ## Replace Blanks as Not Given
## Append each Action, type, detail and device type with a suffix to highlight duplicate col names

sessions.clean.data=sessions.data;

sessions.clean.data=sessions.clean.data%>%mutate(uid=ifelse(user_id=="","Not Given",user_id));

sessions.clean.data=sessions.clean.data%>%mutate(action=ifelse(action=="-unknown-","unknown",action));
sessions.clean.data=sessions.clean.data%>%mutate(action=ifelse(action=="","Not Given",action));
sessions.clean.data$action = paste(sessions.clean.data$action,"AC", sep = "_");

sessions.clean.data=sessions.clean.data%>%mutate(action_type=ifelse(action_type=="-unknown-","unknown",action_type));
sessions.clean.data=sessions.clean.data%>%mutate(action_type=ifelse(action_type=="","Not Given",action_type));
sessions.clean.data$action_type = paste(sessions.clean.data$action_type,"AT", sep = "_");

sessions.clean.data=sessions.clean.data%>%mutate(action_detail=ifelse(action_detail=="-unknown-","unknown",action_detail));
sessions.clean.data=sessions.clean.data%>%mutate(action_detail=ifelse(action_detail=="","Not Given",action_detail));
sessions.clean.data$action_detail = paste(sessions.clean.data$action_detail,"AD", sep = "_");

sessions.clean.data=sessions.clean.data%>%mutate(device_type=ifelse(device_type=="-unknown-","unknown",device_type));
sessions.clean.data=sessions.clean.data%>%mutate(device_type=ifelse(device_type=="","Not Given",device_type));
sessions.clean.data$device_type = paste(sessions.clean.data$device_type,"DT", sep = "_");

## Replace all NA or blanks in secs_elapsed column to 0

sessions.clean.data$secs[is.na(sessions.clean.data$secs_elapsed)] <- 0

#unique(sessions.clean.data$action)

#*************************************************************************************************************
##load openxlsx package

#library(openxlsx);

#open the data exploration sheets

### Rename all Actions with Cumulative Percentage > 95% as MISC

actions.data=read.xlsx("C:\\Users\\Sumita\\Desktop\\Capstone\\Data Exploration - Cumulative Percentages.xlsx",1,colNames = TRUE,rows=NULL,cols=NULL,na.strings="NA");

actions.data = actions.data%>%mutate(action=ifelse(action=="-unknown-","unknown",action));
actions.data$action = paste(actions.data$action,"AC", sep = "_");

labels.action=actions.data$action[1:52];
labels.action.misc=actions.data$action[53:NROW(actions.data)];

sessions.clean.data$action_new = sessions.clean.data$action;
sessions.clean.data$action_new[sessions.clean.data$action %in% labels.action.misc]="MISC_AC";

df_action=as.data.frame(sessions.clean.data%>%group_by(action_new)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)));

### Rename all Action_type with Cumulative Percentage > 98% as MISC

actiontype.data=read.xlsx("C:\\Users\\Sumita\\Desktop\\Capstone\\Data Exploration - Cumulative Percentages.xlsx",2,colNames = TRUE,rows=NULL,cols=NULL,na.strings="NA");

actiontype.data=actiontype.data%>%mutate(action_type=ifelse(action_type=="-unknown-","unknown",action_type));
actiontype.data$action_type = paste(actiontype.data$action_type,"AT", sep = "_");

labels.actiontype=actiontype.data$action_type[1:6];
labels.actiontype.misc=actiontype.data$action_type[7:NROW(actiontype.data)];

sessions.clean.data$action_type_new = sessions.clean.data$action_type;
sessions.clean.data$action_type_new[sessions.clean.data$action_type %in% labels.actiontype.misc]="MISC_AT";

df_ac_type=as.data.frame(sessions.clean.data%>%group_by(action_type_new)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)));


### Rename all Action_detail with Cumulative Percentage > 95% as MISC

actiondetail.data=read.xlsx("C:\\Users\\Sumita\\Desktop\\Capstone\\Data Exploration - Cumulative Percentages.xlsx",3,colNames = TRUE,rows=NULL,cols=NULL,na.strings="NA");

actiondetail.data=actiondetail.data%>%mutate(action_detail=ifelse(action_detail=="-unknown-","unknown",action_detail));
actiondetail.data$action_detail = paste(actiondetail.data$action_detail,"AD", sep = "_");

labels.actiondetail=actiondetail.data$action_detail[1:37];
labels.actiondetail.misc=actiondetail.data$action_detail[38:NROW(actiondetail.data)];

sessions.clean.data$action_detail_new = sessions.clean.data$action_detail;
sessions.clean.data$action_detail_new[sessions.clean.data$action_detail %in% labels.actiondetail.misc]="MISC_AD";

df_ac_det=as.data.frame(sessions.clean.data%>%group_by(action_detail_new)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)));

### Rename all Device_type with Cumulative Percentage > 96% as MISC

devicetype.data=read.xlsx("C:\\Users\\Sumita\\Desktop\\Capstone\\Data Exploration - Cumulative Percentages.xlsx",4,colNames = TRUE,rows=NULL,cols=NULL,na.strings="NA");

devicetype.data=devicetype.data%>%mutate(device_type=ifelse(device_type=="-unknown-","unknown",device_type));
devicetype.data$device_type = paste(devicetype.data$device_type,"DT", sep = "_");

labels.devicetype=devicetype.data$device_type[1:6];
labels.devicetype.misc=devicetype.data$device_type[7:NROW(devicetype.data)];

sessions.clean.data$device_type_new = sessions.clean.data$device_type;
sessions.clean.data$device_type_new[sessions.clean.data$device_type %in% labels.devicetype.misc]="MISC_DT";

df_dev_ty=as.data.frame(sessions.clean.data%>%group_by(device_type_new)%>%summarise(count=n())%>%mutate(pct=count/sum(count))%>%arrange(desc(count)))


################################################################

###     Data cleaning on Sessions.data complete

################################################################
