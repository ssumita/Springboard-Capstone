################################################

###        Build a Decision Tree model

################################################

## Get a character list of DestNames

DestNames <- c("US_NDF","FR_NDF","CA_NDF","GB_NDF","ES_NDF","IT_NDF","PT_NDF","NL_NDF","DE_NDF","AU_NDF","OT_NDF",
               "US_OT","FR_OT","CA_OT","GB_OT","ES_OT","IT_OT","PT_OT","NL_OT","DE_OT","AU_OT",
               "US_AU","FR_AU","CA_AU","GB_AU","ES_AU","IT_AU","PT_AU","NL_AU","DE_AU",
               "US_DE","FR_DE","CA_DE","GB_DE","ES_DE","IT_DE","PT_DE","NL_DE",
               "US_NL","FR_NL","CA_NL","GB_NL","ES_NL","IT_NL","PT_NL",
               "US_PT","FR_PT","CA_PT","GB_PT","ES_PT","IT_PT",
               "US_IT","FR_IT","CA_IT","GB_IT","ES_IT",
               "US_ES","FR_ES","CA_ES","GB_ES",
               "US_GB","FR_GB","CA_GB",
               "US_CA","FR_CA",
               "US_FR")

DestNames = sort(DestNames)

## Get the list of 66 dataframe extract files

df_list = mget(ls(pattern = "^sess.tn.extr_*"))
sort(df_list)

## Create the final probabilty file with userid (probabilty will be populated inside the loop) 
id = sess.tt.data$uid
ProbFile <- data.frame(id)
PName = "P_AU_NDF"

for(i in 1:66){   
  
  Train <- df_list[[i]]
  print(paste(names(df_list[i])))
  Test  <- sess.tt.data
  Train$dest <- as.factor(Train$dest)
  set.seed(88)
  model.rpart <- rpart(factor(dest) ~., data = Train[,!colnames(Train) %in% c("uid")], parms = list(prior = c(0.5, 0.5)), control = list(cp = 0.01, xval = 5, minsplit = 10))
  cpTable     <- data.frame(model.rpart$cptable)
  optimalCP   <- cpTable[which.min(cpTable$xerror),c("CP")]
  model.rpart <- prune(model.rpart, cp = optimalCP)
  predictTest <- predict(model.rpart, Test, type = "class")
  predictTest.df = as.data.frame(predictTest)
  
  PName[i] <- paste("P",DestNames[i], sep = "_")    ### Eg., Create PName as P_US_NDF
  print(PName[i])
  names(predictTest.df) <- PName[i]                 ### Eg., Name the predictTest col as P_US_NDF
  ProbFile <- cbind(ProbFile,predictTest.df)        ### Merge ProbFile with the predictions
}

write.csv(ProbFile,file=".\\data\\ProbFile.csv")