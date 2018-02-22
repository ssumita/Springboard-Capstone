## Summarize elapsed time and count for each variable

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
base.data[is.na(base.data)] <- 0

## Write final Base.sessions.data

write.csv(base.data,file=".\\data\\Base.sessions.data.csv")

## Extract the user_id, country_destination columns from Training user dataset to a dataframe
train.users.dest = select(train.users.data, uid = id, dest = country_destination)

## Extract the user id column from Test user dataset to a dataframe
test.users.dest = select(test.users.data, uid = id)

## Merge Base.sessions.data with Country_dest dataframe on Userid to get country_destination
sess.train.data = merge(base.data, train.users.dest, by="uid")
sess.test.data = merge(base.data, test.users.dest, by="uid")

write.csv(sess.train.data,file=".\\data\\Sessions.train.csv")
write.csv(sess.test.data,file=".\\data\\Sessions.test.csv")

## Replacing outliers with the 2nd and 98th percentile values in each variable,
## remove uid and dest column as they are characters and cannot be input to the quantile fn

winsorize <- function(x){
  quantiles <- quantile( x, probs=c(.02, .98 ))
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}
sess.tn.data = as.data.frame(apply(sess.train.data[,-c(1,212)],2,winsorize))
sess.tt.data = as.data.frame(apply(sess.test.data[,-c(1)],2,winsorize))

## Merge the character columns - Uid and Dest with the winsorized dataframe
uid = sess.train.data[,1]
dest = sess.train.data[,212]
sess.tn.data=cbind(uid,sess.tn.data,dest)

uid = sess.test.data[,1]
sess.tt.data=cbind(uid,sess.tt.data)

#colnames(sess.tn.data)=colnames(sess.train.data)
#colnames(sess.tt.data)=colnames(sess.test.data)

write.csv(sess.tn.data,file=".\\data\\sess.tn.data.csv")
write.csv(sess.tt.data,file=".\\data\\sess.tt.data.csv")

## Identify variables with constant value (~0) above 50%
##  eg., quantile(sess.dest.data$cnt.header_userpic_AC,c(1:100/100))

## drop the non-numeric columns - uid and country_destination

sess.quant.data1 = sapply(sess.train.data, quantile, probs=c(0.5,0.75))
sess.quant.data2 = sapply(sess.test.data, quantile, probs=c(0.5,0.75))

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}
write.excel(sess.quant.data1)

quantile(sess.dest.data$tm.view_search_results_AD,c(1:100/100))
quantile(sess.q.data$tm.view_search_results_AD,c(1:100/100))

quantile(sess.dest.data$tm.index_AC,c(1:100/100))
quantile(sess.q.data$tm.index_AC,c(1:100/100))

hist(sess.dest.data$tm.index_AC)
hist(sess.q.data$tm.index_AC)


