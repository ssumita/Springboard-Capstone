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

###     Feature extraction complete

################################################################
