################################################################

###     Feature Selection

################################################################

## Replacing outliers with the 2nd and 98th percentile values in each variable,
## Remove uid and dest column as they are characters and cannot be input to the quantile fn

winsorize <- function(x){
  quantiles <- quantile( x, probs=c(.02, .98 ))
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}

## Drop the uid and dest column before applying winsorize function
sess.tn.data = as.data.frame(apply(sess.train.data[,-c(1,212)],2,winsorize))
sess.tt.data = as.data.frame(apply(sess.test.data[,-c(1)],2,winsorize))

## Merge the character columns - id and Dest with the winsorized dataframe

id = sess.train.data[,1]
country = sess.train.data[,212]
sess.tn.data=cbind(id,sess.tn.data,country)

id = sess.test.data[,1]
sess.tt.data=cbind(id,sess.tt.data)

## Drop the X column

sess.tn.data$X <- NULL
sess.tt.data$X <- NULL

################################################################

###     Remove zero and near-zero variance features

################################################################

NCOL(sess.tn.data)
NZV <- caret::nearZeroVar(sess.tn.data[,c(2:211)], saveMetrics = TRUE)
NZV$names <- row.names(NZV)
removeNames <- NZV[which(NZV$zeroVar == "TRUE"),ncol(NZV)]

sess.tn.data[,c(removeNames)] <- NULL ## drop all columns in removeNames from sess.train data
NCOL(sess.tn.data)

sess.tt.data[,c(removeNames)] <- NULL ## drop all columns in removeNames from sess.test data
NCOL(sess.tt.data)

################################################################

###     Remove highly correlated features

################################################################

correlationMatrix <- cor(sess.tn.data[,c(2:202)])  ## sess.train user data
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.60, names = TRUE)

sess.tn.data[,c(highlyCorrelated)] <- NULL ## drop all cols in highlyCorrelated from sess.Train data
NCOL(sess.tn.data)

sess.tt.data[,c(highlyCorrelated)] <- NULL ## drop all cols in highlyCorrelated from sess.Test data
NCOL(sess.tt.data)

write.csv(sess.tn.data,file=".\\data\\sess.tn.data.csv")
write.csv(sess.tt.data,file=".\\data\\sess.tt.data.csv")


################################################################

###    Create 66 binary level classifiers for model building

################################################################

## Extract 2 destinations combos from sess.train and make them 0 and 1, biggest class/dest should be assigned to 0

sess.tn.extr_US_NDF <- sess.tn.data[which(sess.tn.data$country %in% c("US","NDF")),]
#sess.tn.extr_US_NDF$country <- ifelse(sess.tn.extr_US_NDF$country == "NDF", 0, 1)

sess.tn.extr_FR_NDF <- sess.tn.data[which(sess.tn.data$country %in% c("FR","NDF")),]
#sess.tn.extr_FR_NDF$country <- ifelse(sess.tn.extr_FR_NDF$country == "NDF", 0, 1)

sess.tn.extr_CA_NDF <- sess.tn.data[which(sess.tn.data$country %in% c("CA","NDF")),]
#sess.tn.extr_CA_NDF$country <- ifelse(sess.tn.extr_CA_NDF$country == "NDF", 0, 1)

sess.tn.extr_GB_NDF <- sess.tn.data[which(sess.tn.data$country %in% c("GB","NDF")),]
#sess.tn.extr_GB_NDF$country <- ifelse(sess.tn.extr_GB_NDF$country == "NDF", 0, 1)

sess.tn.extr_ES_NDF <- sess.tn.data[which(sess.tn.data$country %in% c("ES","NDF")),]
#sess.tn.extr_ES_NDF$country <- ifelse(sess.tn.extr_ES_NDF$country == "NDF", 0, 1)

sess.tn.extr_IT_NDF <- sess.tn.data[which(sess.tn.data$country %in% c("IT","NDF")),]
#sess.tn.extr_IT_NDF$country <- ifelse(sess.tn.extr_IT_NDF$country == "NDF", 0, 1)

sess.tn.extr_PT_NDF <- sess.tn.data[which(sess.tn.data$country %in% c("PT","NDF")),]
#sess.tn.extr_PT_NDF$country <- ifelse(sess.tn.extr_PT_NDF$country == "NDF", 0, 1)

sess.tn.extr_NL_NDF <- sess.tn.data[which(sess.tn.data$country %in% c("NL","NDF")),]
#sess.tn.extr_NL_NDF$country <- ifelse(sess.tn.extr_NL_NDF$country == "NDF", 0, 1)

sess.tn.extr_DE_NDF <- sess.tn.data[which(sess.tn.data$country %in% c("DE","NDF")),]
#sess.tn.extr_DE_NDF$country <- ifelse(sess.tn.extr_DE_NDF$country == "NDF", 0, 1)

sess.tn.extr_AU_NDF <- sess.tn.data[which(sess.tn.data$country %in% c("AU","NDF")),]
#sess.tn.extr_AU_NDF$country <- ifelse(sess.tn.extr_AU_NDF$country == "NDF", 0, 1)

sess.tn.extr_OT_NDF <- sess.tn.data[which(sess.tn.data$country %in% c("other","NDF")),]
#sess.tn.extr_OT_NDF$country <- ifelse(sess.tn.extr_OT_NDF$country == "NDF", 0, 1)

sess.tn.extr_US_OT <- sess.tn.data[which(sess.tn.data$country %in% c("US","other")),]
#sess.tn.extr_US_OT$country <- ifelse(sess.tn.extr_US_OT$country == "US", 0, 1)

sess.tn.extr_FR_OT <- sess.tn.data[which(sess.tn.data$country %in% c("FR","other")),]
#sess.tn.extr_FR_OT$country <- ifelse(sess.tn.extr_FR_OT$country == "other", 0, 1)

sess.tn.extr_CA_OT <- sess.tn.data[which(sess.tn.data$country %in% c("CA","other")),]
#sess.tn.extr_CA_OT$country <- ifelse(sess.tn.extr_CA_OT$country == "other", 0, 1)

sess.tn.extr_GB_OT <- sess.tn.data[which(sess.tn.data$country %in% c("GB","other")),]
#sess.tn.extr_GB_OT$country <- ifelse(sess.tn.extr_GB_OT$country == "other", 0, 1)

sess.tn.extr_ES_OT <- sess.tn.data[which(sess.tn.data$country %in% c("ES","other")),]
#sess.tn.extr_ES_OT$country <- ifelse(sess.tn.extr_ES_OT$country == "other", 0, 1)

sess.tn.extr_IT_OT <- sess.tn.data[which(sess.tn.data$country %in% c("IT","other")),]
#sess.tn.extr_IT_OT$country <- ifelse(sess.tn.extr_IT_OT$country == "other", 0, 1)

sess.tn.extr_PT_OT <- sess.tn.data[which(sess.tn.data$country %in% c("PT","other")),]
#sess.tn.extr_PT_OT$country <- ifelse(sess.tn.extr_PT_OT$country == "other", 0, 1)

sess.tn.extr_NL_OT <- sess.tn.data[which(sess.tn.data$country %in% c("NL","other")),]
#sess.tn.extr_NL_OT$country <- ifelse(sess.tn.extr_NL_OT$country == "other", 0, 1)

sess.tn.extr_DE_OT <- sess.tn.data[which(sess.tn.data$country %in% c("DE","other")),]
#sess.tn.extr_DE_OT$country <- ifelse(sess.tn.extr_DE_OT$country == "other", 0, 1)

sess.tn.extr_AU_OT <- sess.tn.data[which(sess.tn.data$country %in% c("AU","other")),]
#sess.tn.extr_AU_OT$country <- ifelse(sess.tn.extr_AU_OT$country == "other", 0, 1)

sess.tn.extr_US_AU <- sess.tn.data[which(sess.tn.data$country %in% c("US","AU")),]
#sess.tn.extr_US_AU$country <- ifelse(sess.tn.extr_US_AU$country == "US", 0, 1)

sess.tn.extr_FR_AU <- sess.tn.data[which(sess.tn.data$country %in% c("FR","AU")),]
#sess.tn.extr_FR_AU$country <- ifelse(sess.tn.extr_FR_AU$country == "FR", 0, 1)

sess.tn.extr_CA_AU <- sess.tn.data[which(sess.tn.data$country %in% c("CA","AU")),]
#sess.tn.extr_CA_AU$country <- ifelse(sess.tn.extr_CA_AU$country == "CA", 0, 1)

sess.tn.extr_GB_AU <- sess.tn.data[which(sess.tn.data$country %in% c("GB","AU")),]
#sess.tn.extr_GB_AU$country <- ifelse(sess.tn.extr_GB_AU$country == "GB", 0, 1)

sess.tn.extr_ES_AU <- sess.tn.data[which(sess.tn.data$country %in% c("ES","AU")),]
#sess.tn.extr_ES_AU$country <- ifelse(sess.tn.extr_ES_AU$country == "ES", 0, 1)

sess.tn.extr_IT_AU <- sess.tn.data[which(sess.tn.data$country %in% c("IT","AU")),]
#sess.tn.extr_IT_AU$country <- ifelse(sess.tn.extr_IT_AU$country == "IT", 0, 1)

sess.tn.extr_PT_AU <- sess.tn.data[which(sess.tn.data$country %in% c("PT","AU")),]
#sess.tn.extr_PT_AU$country <- ifelse(sess.tn.extr_PT_AU$country == "AU", 0, 1)

sess.tn.extr_NL_AU <- sess.tn.data[which(sess.tn.data$country %in% c("NL","AU")),]
#sess.tn.extr_NL_AU$country <- ifelse(sess.tn.extr_NL_AU$country == "NL", 0, 1)

sess.tn.extr_DE_AU <- sess.tn.data[which(sess.tn.data$country %in% c("DE","AU")),]
#sess.tn.extr_DE_AU$country <- ifelse(sess.tn.extr_DE_AU$country == "DE", 0, 1)

sess.tn.extr_US_DE <- sess.tn.data[which(sess.tn.data$country %in% c("US","DE")),]
#sess.tn.extr_US_DE$country <- ifelse(sess.tn.extr_US_DE$country == "US", 0, 1)

sess.tn.extr_FR_DE <- sess.tn.data[which(sess.tn.data$country %in% c("FR","DE")),]
#sess.tn.extr_FR_DE$country <- ifelse(sess.tn.extr_FR_DE$country == "FR", 0, 1)

sess.tn.extr_CA_DE <- sess.tn.data[which(sess.tn.data$country %in% c("CA","DE")),]
#sess.tn.extr_CA_DE$country <- ifelse(sess.tn.extr_CA_DE$country == "CA", 0, 1)

sess.tn.extr_GB_DE <- sess.tn.data[which(sess.tn.data$country %in% c("GB","DE")),]
#sess.tn.extr_GB_DE$country <- ifelse(sess.tn.extr_GB_DE$country == "GB", 0, 1)

sess.tn.extr_ES_DE <- sess.tn.data[which(sess.tn.data$country %in% c("ES","DE")),]
#sess.tn.extr_ES_DE$country <- ifelse(sess.tn.extr_ES_DE$country == "ES", 0, 1)

sess.tn.extr_IT_DE <- sess.tn.data[which(sess.tn.data$country %in% c("IT","DE")),]
#sess.tn.extr_IT_DE$country <- ifelse(sess.tn.extr_IT_DE$country == "IT", 0, 1)

sess.tn.extr_PT_DE <- sess.tn.data[which(sess.tn.data$country %in% c("PT","DE")),]
#sess.tn.extr_PT_DE$country <- ifelse(sess.tn.extr_PT_DE$country == "DE", 0, 1)

sess.tn.extr_NL_DE <- sess.tn.data[which(sess.tn.data$country %in% c("NL","DE")),]
#sess.tn.extr_NL_DE$country <- ifelse(sess.tn.extr_NL_DE$country == "NL", 0, 1)

sess.tn.extr_US_NL <- sess.tn.data[which(sess.tn.data$country %in% c("US","NL")),]
#sess.tn.extr_US_NL$country <- ifelse(sess.tn.extr_US_NL$country == "US", 0, 1)

sess.tn.extr_FR_NL <- sess.tn.data[which(sess.tn.data$country %in% c("FR","NL")),]
#sess.tn.extr_FR_NL$country <- ifelse(sess.tn.extr_FR_NL$country == "FR", 0, 1)

sess.tn.extr_CA_NL <- sess.tn.data[which(sess.tn.data$country %in% c("CA","NL")),]
#sess.tn.extr_CA_NL$country <- ifelse(sess.tn.extr_CA_NL$country == "CA", 0, 1)

sess.tn.extr_GB_NL <- sess.tn.data[which(sess.tn.data$country %in% c("GB","NL")),]
#sess.tn.extr_GB_NL$country <- ifelse(sess.tn.extr_GB_NL$country == "GB", 0, 1)

sess.tn.extr_ES_NL <- sess.tn.data[which(sess.tn.data$country %in% c("ES","NL")),]
#sess.tn.extr_ES_NL$country <- ifelse(sess.tn.extr_ES_NL$country == "ES", 0, 1)

sess.tn.extr_IT_NL <- sess.tn.data[which(sess.tn.data$country %in% c("IT","NL")),]
#sess.tn.extr_IT_NL$country <- ifelse(sess.tn.extr_IT_NL$country == "IT", 0, 1)

sess.tn.extr_PT_NL <- sess.tn.data[which(sess.tn.data$country %in% c("PT","NL")),]
#sess.tn.extr_PT_NL$country <- ifelse(sess.tn.extr_PT_NL$country == "NL", 0, 1)

sess.tn.extr_US_PT <- sess.tn.data[which(sess.tn.data$country %in% c("US","PT")),]
#sess.tn.extr_US_PT$country <- ifelse(sess.tn.extr_US_PT$country == "US", 0, 1)

sess.tn.extr_FR_PT <- sess.tn.data[which(sess.tn.data$country %in% c("FR","PT")),]
#sess.tn.extr_FR_PT$country <- ifelse(sess.tn.extr_FR_PT$country == "FR", 0, 1)

sess.tn.extr_CA_PT <- sess.tn.data[which(sess.tn.data$country %in% c("CA","PT")),]
#sess.tn.extr_CA_PT$country <- ifelse(sess.tn.extr_CA_PT$country == "CA", 0, 1)

sess.tn.extr_GB_PT <- sess.tn.data[which(sess.tn.data$country %in% c("GB","PT")),]
#sess.tn.extr_GB_PT$country <- ifelse(sess.tn.extr_GB_PT$country == "GB", 0, 1)

sess.tn.extr_ES_PT <- sess.tn.data[which(sess.tn.data$country %in% c("ES","PT")),]
#sess.tn.extr_ES_PT$country <- ifelse(sess.tn.extr_ES_PT$country == "ES", 0, 1)

sess.tn.extr_IT_PT <- sess.tn.data[which(sess.tn.data$country %in% c("IT","PT")),]
#sess.tn.extr_IT_PT$country <- ifelse(sess.tn.extr_IT_PT$country == "IT", 0, 1)

sess.tn.extr_US_IT <- sess.tn.data[which(sess.tn.data$country %in% c("US","IT")),]
#sess.tn.extr_US_IT$country <- ifelse(sess.tn.extr_US_IT$country == "US", 0, 1)

sess.tn.extr_FR_IT <- sess.tn.data[which(sess.tn.data$country %in% c("FR","IT")),]
#sess.tn.extr_FR_IT$country <- ifelse(sess.tn.extr_FR_IT$country == "FR", 0, 1)

sess.tn.extr_CA_IT <- sess.tn.data[which(sess.tn.data$country %in% c("CA","IT")),]
#sess.tn.extr_CA_IT$country <- ifelse(sess.tn.extr_CA_IT$country == "IT", 0, 1)

sess.tn.extr_GB_IT <- sess.tn.data[which(sess.tn.data$country %in% c("GB","IT")),]
#sess.tn.extr_GB_IT$country <- ifelse(sess.tn.extr_GB_IT$country == "IT", 0, 1)

sess.tn.extr_ES_IT <- sess.tn.data[which(sess.tn.data$country %in% c("ES","IT")),]
#sess.tn.extr_ES_IT$country <- ifelse(sess.tn.extr_ES_IT$country == "IT", 0, 1)

sess.tn.extr_US_ES <- sess.tn.data[which(sess.tn.data$country %in% c("US","ES")),]
#sess.tn.extr_US_ES$country <- ifelse(sess.tn.extr_US_ES$country == "US", 0, 1)

sess.tn.extr_FR_ES <- sess.tn.data[which(sess.tn.data$country %in% c("FR","ES")),]
#sess.tn.extr_FR_ES$country <- ifelse(sess.tn.extr_FR_ES$country == "FR", 0, 1)

sess.tn.extr_CA_ES <- sess.tn.data[which(sess.tn.data$country %in% c("CA","ES")),]
#sess.tn.extr_CA_ES$country <- ifelse(sess.tn.extr_CA_ES$country == "ES", 0, 1)

sess.tn.extr_GB_ES <- sess.tn.data[which(sess.tn.data$country %in% c("GB","ES")),]
#sess.tn.extr_GB_ES$country <- ifelse(sess.tn.extr_GB_ES$country == "GB", 0, 1)

sess.tn.extr_US_GB <- sess.tn.data[which(sess.tn.data$country %in% c("US","GB")),]
#sess.tn.extr_US_GB$country <- ifelse(sess.tn.extr_US_GB$country == "US", 0, 1)

sess.tn.extr_FR_GB <- sess.tn.data[which(sess.tn.data$country %in% c("FR","GB")),]
#sess.tn.extr_FR_GB$country <- ifelse(sess.tn.extr_FR_GB$country == "FR", 0, 1)

sess.tn.extr_CA_GB <- sess.tn.data[which(sess.tn.data$country %in% c("CA","GB")),]
#sess.tn.extr_CA_GB$country <- ifelse(sess.tn.extr_CA_GB$country == "GB", 0, 1)

sess.tn.extr_US_CA <- sess.tn.data[which(sess.tn.data$country %in% c("US","CA")),]
#sess.tn.extr_US_CA$country <- ifelse(sess.tn.extr_US_CA$country == "US", 0, 1)

sess.tn.extr_FR_CA <- sess.tn.data[which(sess.tn.data$country %in% c("FR","CA")),]
#sess.tn.extr_FR_CA$country <- ifelse(sess.tn.extr_FR_CA$country == "FR", 0, 1)

sess.tn.extr_US_FR <- sess.tn.data[which(sess.tn.data$country %in% c("US","FR")),]
#sess.tn.extr_US_FR$country <- ifelse(sess.tn.extr_US_FR$country == "US", 0, 1)

################################################################

###     Feature extraction complete

################################################################