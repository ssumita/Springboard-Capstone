#sess.tn.data=read.csv(".\\data\\sess.tn.data.csv",stringsAsFactors = F) ## session train user data
#sess.tt.data=read.csv(".\\data\\sess.tt.data.csv",stringsAsFactors = F) ## session test user data

# Drop the first column X

sess.tn.data$X <- NULL
sess.tt.data$X <- NULL

#install.packages("caret")
#library(caret)

NCOL(sess.tn.data)
NZV <- caret::nearZeroVar(sess.tn.data[,c(2:211)], saveMetrics = TRUE)
NZV$names <- row.names(NZV)
removeNames <- NZV[which(NZV$zeroVar == "TRUE"),ncol(NZV)]

sess.tn.data[,c(removeNames)] <- NULL ## drop all columns in removeNames from sess.train data
NCOL(sess.tn.data)

sess.tt.data[,c(removeNames)] <- NULL ## drop all columns in removeNames from sess.test data
NCOL(sess.tt.data)

correlationMatrix <- cor(sess.tn.data[,c(2:202)])  ## sess.train user data
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.60, names = TRUE)

sess.tn.data[,c(highlyCorrelated)] <- NULL ## drop all cols in highlyCorrelated from sess.Train data
NCOL(sess.tn.data)

sess.tt.data[,c(highlyCorrelated)] <- NULL ## drop all cols in highlyCorrelated from sess.Test data
NCOL(sess.tt.data)

## Extract 2 destinations combos from sess.train and make them 0 and 1, biggest class/dest should be assigned to 0

sess.tn.extr_US_NDF <- sess.tn.data[which(sess.tn.data$dest %in% c("US","NDF")),]
sess.tn.extr_US_NDF$dest <- ifelse(sess.tn.extr_US_NDF$dest == "NDF", 0, 1)

sess.tn.extr_FR_NDF <- sess.tn.data[which(sess.tn.data$dest %in% c("FR","NDF")),]
sess.tn.extr_FR_NDF$dest <- ifelse(sess.tn.extr_FR_NDF$dest == "NDF", 0, 1)

sess.tn.extr_CA_NDF <- sess.tn.data[which(sess.tn.data$dest %in% c("CA","NDF")),]
sess.tn.extr_CA_NDF$dest <- ifelse(sess.tn.extr_CA_NDF$dest == "NDF", 0, 1)

sess.tn.extr_GB_NDF <- sess.tn.data[which(sess.tn.data$dest %in% c("GB","NDF")),]
sess.tn.extr_GB_NDF$dest <- ifelse(sess.tn.extr_GB_NDF$dest == "NDF", 0, 1)

sess.tn.extr_ES_NDF <- sess.tn.data[which(sess.tn.data$dest %in% c("ES","NDF")),]
sess.tn.extr_ES_NDF$dest <- ifelse(sess.tn.extr_ES_NDF$dest == "NDF", 0, 1)

sess.tn.extr_IT_NDF <- sess.tn.data[which(sess.tn.data$dest %in% c("IT","NDF")),]
sess.tn.extr_IT_NDF$dest <- ifelse(sess.tn.extr_IT_NDF$dest == "NDF", 0, 1)

sess.tn.extr_PT_NDF <- sess.tn.data[which(sess.tn.data$dest %in% c("PT","NDF")),]
sess.tn.extr_PT_NDF$dest <- ifelse(sess.tn.extr_PT_NDF$dest == "NDF", 0, 1)

sess.tn.extr_NL_NDF <- sess.tn.data[which(sess.tn.data$dest %in% c("NL","NDF")),]
sess.tn.extr_NL_NDF$dest <- ifelse(sess.tn.extr_NL_NDF$dest == "NDF", 0, 1)

sess.tn.extr_DE_NDF <- sess.tn.data[which(sess.tn.data$dest %in% c("DE","NDF")),]
sess.tn.extr_DE_NDF$dest <- ifelse(sess.tn.extr_DE_NDF$dest == "NDF", 0, 1)

sess.tn.extr_AU_NDF <- sess.tn.data[which(sess.tn.data$dest %in% c("AU","NDF")),]
sess.tn.extr_AU_NDF$dest <- ifelse(sess.tn.extr_AU_NDF$dest == "NDF", 0, 1)

sess.tn.extr_OT_NDF <- sess.tn.data[which(sess.tn.data$dest %in% c("other","NDF")),]
sess.tn.extr_OT_NDF$dest <- ifelse(sess.tn.extr_OT_NDF$dest == "NDF", 0, 1)

sess.tn.extr_US_OT <- sess.tn.data[which(sess.tn.data$dest %in% c("US","other")),]
sess.tn.extr_US_OT$dest <- ifelse(sess.tn.extr_US_OT$dest == "US", 0, 1)

sess.tn.extr_FR_OT <- sess.tn.data[which(sess.tn.data$dest %in% c("FR","other")),]
sess.tn.extr_FR_OT$dest <- ifelse(sess.tn.extr_FR_OT$dest == "other", 0, 1)

sess.tn.extr_CA_OT <- sess.tn.data[which(sess.tn.data$dest %in% c("CA","other")),]
sess.tn.extr_CA_OT$dest <- ifelse(sess.tn.extr_CA_OT$dest == "other", 0, 1)

sess.tn.extr_GB_OT <- sess.tn.data[which(sess.tn.data$dest %in% c("GB","other")),]
sess.tn.extr_GB_OT$dest <- ifelse(sess.tn.extr_GB_OT$dest == "other", 0, 1)

sess.tn.extr_ES_OT <- sess.tn.data[which(sess.tn.data$dest %in% c("ES","other")),]
sess.tn.extr_ES_OT$dest <- ifelse(sess.tn.extr_ES_OT$dest == "other", 0, 1)

sess.tn.extr_IT_OT <- sess.tn.data[which(sess.tn.data$dest %in% c("IT","other")),]
sess.tn.extr_IT_OT$dest <- ifelse(sess.tn.extr_IT_OT$dest == "other", 0, 1)

sess.tn.extr_PT_OT <- sess.tn.data[which(sess.tn.data$dest %in% c("PT","other")),]
sess.tn.extr_PT_OT$dest <- ifelse(sess.tn.extr_PT_OT$dest == "other", 0, 1)

sess.tn.extr_NL_OT <- sess.tn.data[which(sess.tn.data$dest %in% c("NL","other")),]
sess.tn.extr_NL_OT$dest <- ifelse(sess.tn.extr_NL_OT$dest == "other", 0, 1)

sess.tn.extr_DE_OT <- sess.tn.data[which(sess.tn.data$dest %in% c("DE","other")),]
sess.tn.extr_DE_OT$dest <- ifelse(sess.tn.extr_DE_OT$dest == "other", 0, 1)

sess.tn.extr_AU_OT <- sess.tn.data[which(sess.tn.data$dest %in% c("AU","other")),]
sess.tn.extr_AU_OT$dest <- ifelse(sess.tn.extr_AU_OT$dest == "other", 0, 1)

sess.tn.extr_US_AU <- sess.tn.data[which(sess.tn.data$dest %in% c("US","AU")),]
sess.tn.extr_US_AU$dest <- ifelse(sess.tn.extr_US_AU$dest == "US", 0, 1)

sess.tn.extr_FR_AU <- sess.tn.data[which(sess.tn.data$dest %in% c("FR","AU")),]
sess.tn.extr_FR_AU$dest <- ifelse(sess.tn.extr_FR_AU$dest == "FR", 0, 1)

sess.tn.extr_CA_AU <- sess.tn.data[which(sess.tn.data$dest %in% c("CA","AU")),]
sess.tn.extr_CA_AU$dest <- ifelse(sess.tn.extr_CA_AU$dest == "CA", 0, 1)

sess.tn.extr_GB_AU <- sess.tn.data[which(sess.tn.data$dest %in% c("GB","AU")),]
sess.tn.extr_GB_AU$dest <- ifelse(sess.tn.extr_GB_AU$dest == "GB", 0, 1)

sess.tn.extr_ES_AU <- sess.tn.data[which(sess.tn.data$dest %in% c("ES","AU")),]
sess.tn.extr_ES_AU$dest <- ifelse(sess.tn.extr_ES_AU$dest == "ES", 0, 1)

sess.tn.extr_IT_AU <- sess.tn.data[which(sess.tn.data$dest %in% c("IT","AU")),]
sess.tn.extr_IT_AU$dest <- ifelse(sess.tn.extr_IT_AU$dest == "IT", 0, 1)

sess.tn.extr_PT_AU <- sess.tn.data[which(sess.tn.data$dest %in% c("PT","AU")),]
sess.tn.extr_PT_AU$dest <- ifelse(sess.tn.extr_PT_AU$dest == "AU", 0, 1)

sess.tn.extr_NL_AU <- sess.tn.data[which(sess.tn.data$dest %in% c("NL","AU")),]
sess.tn.extr_NL_AU$dest <- ifelse(sess.tn.extr_NL_AU$dest == "NL", 0, 1)

sess.tn.extr_DE_AU <- sess.tn.data[which(sess.tn.data$dest %in% c("DE","AU")),]
sess.tn.extr_DE_AU$dest <- ifelse(sess.tn.extr_DE_AU$dest == "DE", 0, 1)

sess.tn.extr_US_DE <- sess.tn.data[which(sess.tn.data$dest %in% c("US","DE")),]
sess.tn.extr_US_DE$dest <- ifelse(sess.tn.extr_US_DE$dest == "US", 0, 1)

sess.tn.extr_FR_DE <- sess.tn.data[which(sess.tn.data$dest %in% c("FR","DE")),]
sess.tn.extr_FR_DE$dest <- ifelse(sess.tn.extr_FR_DE$dest == "FR", 0, 1)

sess.tn.extr_CA_DE <- sess.tn.data[which(sess.tn.data$dest %in% c("CA","DE")),]
sess.tn.extr_CA_DE$dest <- ifelse(sess.tn.extr_CA_DE$dest == "CA", 0, 1)

sess.tn.extr_GB_DE <- sess.tn.data[which(sess.tn.data$dest %in% c("GB","DE")),]
sess.tn.extr_GB_DE$dest <- ifelse(sess.tn.extr_GB_DE$dest == "GB", 0, 1)

sess.tn.extr_ES_DE <- sess.tn.data[which(sess.tn.data$dest %in% c("ES","DE")),]
sess.tn.extr_ES_DE$dest <- ifelse(sess.tn.extr_ES_DE$dest == "ES", 0, 1)

sess.tn.extr_IT_DE <- sess.tn.data[which(sess.tn.data$dest %in% c("IT","DE")),]
sess.tn.extr_IT_DE$dest <- ifelse(sess.tn.extr_IT_DE$dest == "IT", 0, 1)

sess.tn.extr_PT_DE <- sess.tn.data[which(sess.tn.data$dest %in% c("PT","DE")),]
sess.tn.extr_PT_DE$dest <- ifelse(sess.tn.extr_PT_DE$dest == "DE", 0, 1)

sess.tn.extr_NL_DE <- sess.tn.data[which(sess.tn.data$dest %in% c("NL","DE")),]
sess.tn.extr_NL_DE$dest <- ifelse(sess.tn.extr_NL_DE$dest == "NL", 0, 1)

sess.tn.extr_US_NL <- sess.tn.data[which(sess.tn.data$dest %in% c("US","NL")),]
sess.tn.extr_US_NL$dest <- ifelse(sess.tn.extr_US_NL$dest == "US", 0, 1)

sess.tn.extr_FR_NL <- sess.tn.data[which(sess.tn.data$dest %in% c("FR","NL")),]
sess.tn.extr_FR_NL$dest <- ifelse(sess.tn.extr_FR_NL$dest == "FR", 0, 1)

sess.tn.extr_CA_NL <- sess.tn.data[which(sess.tn.data$dest %in% c("CA","NL")),]
sess.tn.extr_CA_NL$dest <- ifelse(sess.tn.extr_CA_NL$dest == "CA", 0, 1)

sess.tn.extr_GB_NL <- sess.tn.data[which(sess.tn.data$dest %in% c("GB","NL")),]
sess.tn.extr_GB_NL$dest <- ifelse(sess.tn.extr_GB_NL$dest == "GB", 0, 1)

sess.tn.extr_ES_NL <- sess.tn.data[which(sess.tn.data$dest %in% c("ES","NL")),]
sess.tn.extr_ES_NL$dest <- ifelse(sess.tn.extr_ES_NL$dest == "ES", 0, 1)

sess.tn.extr_IT_NL <- sess.tn.data[which(sess.tn.data$dest %in% c("IT","NL")),]
sess.tn.extr_IT_NL$dest <- ifelse(sess.tn.extr_IT_NL$dest == "IT", 0, 1)

sess.tn.extr_PT_NL <- sess.tn.data[which(sess.tn.data$dest %in% c("PT","NL")),]
sess.tn.extr_PT_NL$dest <- ifelse(sess.tn.extr_PT_NL$dest == "NL", 0, 1)

sess.tn.extr_US_PT <- sess.tn.data[which(sess.tn.data$dest %in% c("US","PT")),]
sess.tn.extr_US_PT$dest <- ifelse(sess.tn.extr_US_PT$dest == "US", 0, 1)

sess.tn.extr_FR_PT <- sess.tn.data[which(sess.tn.data$dest %in% c("FR","PT")),]
sess.tn.extr_FR_PT$dest <- ifelse(sess.tn.extr_FR_PT$dest == "FR", 0, 1)

sess.tn.extr_CA_PT <- sess.tn.data[which(sess.tn.data$dest %in% c("CA","PT")),]
sess.tn.extr_CA_PT$dest <- ifelse(sess.tn.extr_CA_PT$dest == "CA", 0, 1)

sess.tn.extr_GB_PT <- sess.tn.data[which(sess.tn.data$dest %in% c("GB","PT")),]
sess.tn.extr_GB_PT$dest <- ifelse(sess.tn.extr_GB_PT$dest == "GB", 0, 1)

sess.tn.extr_ES_PT <- sess.tn.data[which(sess.tn.data$dest %in% c("ES","PT")),]
sess.tn.extr_ES_PT$dest <- ifelse(sess.tn.extr_ES_PT$dest == "ES", 0, 1)

sess.tn.extr_IT_PT <- sess.tn.data[which(sess.tn.data$dest %in% c("IT","PT")),]
sess.tn.extr_IT_PT$dest <- ifelse(sess.tn.extr_IT_PT$dest == "IT", 0, 1)

sess.tn.extr_US_IT <- sess.tn.data[which(sess.tn.data$dest %in% c("US","IT")),]
sess.tn.extr_US_IT$dest <- ifelse(sess.tn.extr_US_IT$dest == "US", 0, 1)

sess.tn.extr_FR_IT <- sess.tn.data[which(sess.tn.data$dest %in% c("FR","IT")),]
sess.tn.extr_FR_IT$dest <- ifelse(sess.tn.extr_FR_IT$dest == "FR", 0, 1)

sess.tn.extr_CA_IT <- sess.tn.data[which(sess.tn.data$dest %in% c("CA","IT")),]
sess.tn.extr_CA_IT$dest <- ifelse(sess.tn.extr_CA_IT$dest == "IT", 0, 1)

sess.tn.extr_GB_IT <- sess.tn.data[which(sess.tn.data$dest %in% c("GB","IT")),]
sess.tn.extr_GB_IT$dest <- ifelse(sess.tn.extr_GB_IT$dest == "IT", 0, 1)

sess.tn.extr_ES_IT <- sess.tn.data[which(sess.tn.data$dest %in% c("ES","IT")),]
sess.tn.extr_ES_IT$dest <- ifelse(sess.tn.extr_ES_IT$dest == "IT", 0, 1)

sess.tn.extr_US_ES <- sess.tn.data[which(sess.tn.data$dest %in% c("US","ES")),]
sess.tn.extr_US_ES$dest <- ifelse(sess.tn.extr_US_ES$dest == "US", 0, 1)

sess.tn.extr_FR_ES <- sess.tn.data[which(sess.tn.data$dest %in% c("FR","ES")),]
sess.tn.extr_FR_ES$dest <- ifelse(sess.tn.extr_FR_ES$dest == "FR", 0, 1)

sess.tn.extr_CA_ES <- sess.tn.data[which(sess.tn.data$dest %in% c("CA","ES")),]
sess.tn.extr_CA_ES$dest <- ifelse(sess.tn.extr_CA_ES$dest == "ES", 0, 1)

sess.tn.extr_GB_ES <- sess.tn.data[which(sess.tn.data$dest %in% c("GB","ES")),]
sess.tn.extr_GB_ES$dest <- ifelse(sess.tn.extr_GB_ES$dest == "GB", 0, 1)

sess.tn.extr_US_GB <- sess.tn.data[which(sess.tn.data$dest %in% c("US","GB")),]
sess.tn.extr_US_GB$dest <- ifelse(sess.tn.extr_US_GB$dest == "US", 0, 1)

sess.tn.extr_FR_GB <- sess.tn.data[which(sess.tn.data$dest %in% c("FR","GB")),]
sess.tn.extr_FR_GB$dest <- ifelse(sess.tn.extr_FR_GB$dest == "FR", 0, 1)

sess.tn.extr_CA_GB <- sess.tn.data[which(sess.tn.data$dest %in% c("CA","GB")),]
sess.tn.extr_CA_GB$dest <- ifelse(sess.tn.extr_CA_GB$dest == "GB", 0, 1)

sess.tn.extr_US_CA <- sess.tn.data[which(sess.tn.data$dest %in% c("US","CA")),]
sess.tn.extr_US_CA$dest <- ifelse(sess.tn.extr_US_CA$dest == "US", 0, 1)

sess.tn.extr_FR_CA <- sess.tn.data[which(sess.tn.data$dest %in% c("FR","CA")),]
sess.tn.extr_FR_CA$dest <- ifelse(sess.tn.extr_FR_CA$dest == "FR", 0, 1)

sess.tn.extr_US_FR <- sess.tn.data[which(sess.tn.data$dest %in% c("US","FR")),]
sess.tn.extr_US_FR$dest <- ifelse(sess.tn.extr_US_FR$dest == "US", 0, 1)

