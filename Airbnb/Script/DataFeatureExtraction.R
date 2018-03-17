################################################################

###     Base feature Extraction

################################################################


## Summarize elapsed time and count for each variable in Sessions.data

agg_action_new=as.data.frame(sessions.clean.data%>%group_by(uid,action_new)%>%summarise(tm=sum(secs),cnt=n()))

agg_action_type_new=as.data.frame(sessions.clean.data%>%group_by(uid,action_type_new)%>%summarise(tm=sum(secs),cnt=n()))

agg_action_detail_new=as.data.frame(sessions.clean.data%>%group_by(uid,action_detail_new)%>%summarise(tm=sum(secs),cnt=n()))

agg_device_type_new=as.data.frame(sessions.clean.data%>%group_by(uid,device_type_new)%>%summarise(tm=sum(secs),cnt=n()))

## Reshape the dataframes to wide format

w=reshape(agg_action_new,timevar="action_new",idvar="uid",direction="wide")

x=reshape(agg_action_type_new,timevar="action_type_new",idvar="uid",direction="wide")

y=reshape(agg_action_detail_new,timevar="action_detail_new",idvar="uid",direction="wide")

z=reshape(agg_device_type_new,timevar="device_type_new",idvar="uid",direction="wide")
  
## Merge dataframes to a single base.data and replace all NA as 0

bd1 <- merge(w, x, by="uid")

bd2 <- merge(bd1, y, by="uid")

base.data = merge(bd2, z, by="uid")
base.data$X <- NULL
base.data[is.na(base.data)] <- 0

## Write final Base.sessions.data

write.csv(base.data,file=".\\data\\Base.sessions.data.csv")

base.data=read.csv(".\\data\\Base.sessions.data.csv",stringsAsFactors = F);

## Extract the user_id, country_destination columns from Training user dataset to a dataframe
train.users.dest = select(train.users.data, uid = id, dest = country_destination)

## Extract the user id column from Test user dataset to a dataframe
test.users.dest = select(test.users.data, uid = id)

## Merge Base.sessions.data with Country_dest dataframe on Userid to get country_destination
sess.train.data = merge(base.data, train.users.dest, by="uid")
sess.test.data = merge(base.data, test.users.dest, by="uid")

## Identify uids in Test user data that do not have a match in base.sessions.data.
## Do a left exclude join on base.sessions.data vs Test user data using dplyr

## Extract the user id column from Base sessions dataset and Test user data to a dataframe resp

base.data.id = select(base.data, id = uid)
test.users.id = select(test.users.data, id)

## Do Left Excluding join to get uids in Test user data that do not match with base.sessions.data
## The Test.diff.uid will be merged with the final Submission file with predictions. 

Test.diff.id = setdiff(test.users.id,base.data.id)  

