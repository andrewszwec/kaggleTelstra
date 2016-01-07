##########################################################
# Kaggle - Telstra Comp
#
# BUILD FAULT ANALYTIC RECORD
#
#
##########################################################
setwd("~/kaggle/telstra")


step1 <- merge(raw_train, event, by="id", all.x = TRUE, sort=TRUE) 

##########################################################
## Pivot log features (Only make this once and save it)
##########################################################
#require(reshape2)

## UNCOMMENT THE FOLLOWING CODE TO RERUN
#feature.pivot <- dcast(feature, id ~ log_feature, fun.aggregate = sum, value.var = "volume") # 18,552 records
#write.csv(feature.pivot, file="~/kaggle/telstra/out/feature.pivot.csv")

# id    feat1 feat2 feat3 ... 
# 5022    1     3     45  ...

#feature.pivot$id <- as.factor(feature.pivot$id)

## Get principal components of the sparse matrix and 
# feature.pivot.pca <- prcomp(feature.pivot[ , 2:length(feature.pivot)],
#                             center = TRUE,
#                             scale. = TRUE,
#                             retx   = TRUE) 
# 
# log_features <- data.frame(cbind(feature.pivot$id, feature.pivot.pca$x[ ,1:10 ]))
# 
# names(log_features)[1] <- "id"
# rm(feature, feature.pivot, feature.pivot.pca)
#save(log_features, file='log_features.RData')
load(file='log_features.RData')

##########################################################
## Merge log features
##########################################################
step2 <- merge(step1, log_features, by="id", all.x = TRUE, sort=TRUE) 
rm(step1) # no longer needed

##########################################################
## Merge resource data
##########################################################

step3 <- merge(step2, resource, by="id", all.x = TRUE, sort=TRUE) 
rm(step2) # no longer needed


##########################################################
## Merge serverity type data
##########################################################
step4 <- merge(step3, severity, by="id", all.x = TRUE, sort=TRUE) 
rm(step3) # no longer needed



##########################################################
## Prepare for modelling
##########################################################

# Make target a factor
step4$fault_severity <- as.factor(step4$fault_severity)

# replace words in some classes
step4$location <- as.numeric(gsub("location ","",step4$location, ignore.case = TRUE, perl=TRUE))
step4$event_type <- gsub("event_type ","",step4$event_type, ignore.case = TRUE, perl=TRUE)
step4$resource_type <- gsub("resource_type ","",step4$resource_type, ignore.case = TRUE, perl=TRUE)
step4$severity_type <- gsub("severity_type ","",step4$severity_type, ignore.case = TRUE, perl=TRUE)


## Bucket the categoricals up to reduce below 53
source("bucket_categoricals.r")


## Convert back to factors for modelling
step6$location      <- as.factor(step6$location )
step6$event_type    <- as.factor(step6$event_type )
step6$resource_type <- as.factor(step6$resource_type )
step6$severity_type <- as.factor(step6$severity_type )

## Move target variable to last col
movetolast <- function(data, move) {
  data[c(setdiff(names(data), move), move)]
}
step6 <- movetolast(step6,"fault_severity" )

# Remove to include location and event groups
# buffer <- subset(step6, select=c( -location, -id, -event_type) )
buffer <- subset(step6, select=c( -location, -id, -event_type, -location.group, -event_type.group) )
rm(step6, b)
## Divide into training Test and validation sets
require(caret)
set.seed(975)
inTrain     = createDataPartition(buffer$fault_severity, p = 0.6)[[1]]
training    = buffer[ inTrain,]      # 60% of records
temp        = buffer[-inTrain,]      # 40% of reocrds
  
inTemp      = createDataPartition(temp$fault_severity, p = 0.5)[[1]]
testing     = temp[inTemp,]      # 20% of reocrds
validation  = temp[-inTemp,]     # 20% of reocrds

rm(temp)
##########################################################
## Build parallel model on Windows
##########################################################
library(randomForest)
library(foreach)
library(doSNOW)

cores <- 2
cl <- makeCluster(cores, type = "SOCK",outfile="")
registerDoSNOW(cl)


total.tree <- 2000
num.chunk <- cores
avg.tree <- ceiling(total.tree/num.chunk)

time <- system.time({
  rf_fit <- foreach(ntree = rep(avg.tree, num.chunk), .combine = combine, 
                    .packages = c("randomForest")) %dopar% {
                      randomForest(training[1:length(training)-1], training$fault_severity, ntree = ntree,importance=TRUE)
                    }
})

stopCluster(cl)

print("Time to build model was ")
time
# Time with 2 cores 
# user  system elapsed 
# 14.36    5.70  709.97 

save(rf_fit, file="rf_fit_mini.RData")

pred <- predict(rf_fit, newdata=testing)

results <- data.frame(observations=testing$fault_severity, predictions=pred)
results$observations <- as.numeric(results$observations)  
results$predictions <- as.numeric(results$predictions)

require(e1701)
confusionMatrix(results$predictions, results$observations)

##########################################################
## MODEL IS GOOD!
##########################################################
# Confusion Matrix and Statistics
# 
# Reference
# Prediction     1     2     3
# 1 10065   349    45
# 2   397  5746    19
# 3    57    49  2974
# 
# Overall Statistics
# 
# Accuracy : 0.9535          
# 95% CI : (0.9505, 0.9564)
# No Information Rate : 0.5339          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.9218          
# Mcnemar's Test P-Value : 0.0004987       
# 
# Statistics by Class:
# 
# Class: 1 Class: 2 Class: 3
# Sensitivity            0.9568   0.9352   0.9789
# Specificity            0.9571   0.9693   0.9936
# Pos Pred Value         0.9623   0.9325   0.9656
# Neg Pred Value         0.9509   0.9706   0.9961
# Prevalence             0.5339   0.3119   0.1542
# Detection Rate         0.5109   0.2917   0.1510
# Detection Prevalence   0.5309   0.3128   0.1563
# Balanced Accuracy      0.9570   0.9523   0.9863


pred2 <- predict(rf_fit, newdata=validation)

results2 <- data.frame(observations=validation$fault_severity, predictions=pred2)
results2$observations <- as.numeric(results$observations)  
results2$predictions <- as.numeric(results$predictions)

confusionMatrix(results2$predictions, results2$observations)



# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1     2
# 0 10093   354    38
# 1   372  5758    28
# 2    53    31  2972
# 
# Overall Statistics
# 
# Accuracy : 0.9555          
# 95% CI : (0.9526, 0.9584)
# No Information Rate : 0.5339          
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.9252          
# Mcnemar's Test P-Value : 0.3808          
# 
# Statistics by Class:
# 
# Class: 0 Class: 1 Class: 2
# Sensitivity            0.9596   0.9373   0.9783
# Specificity            0.9573   0.9705   0.9950
# Pos Pred Value         0.9626   0.9350   0.9725
# Neg Pred Value         0.9539   0.9716   0.9960
# Prevalence             0.5339   0.3118   0.1542
# Detection Rate         0.5124   0.2923   0.1509
# Detection Prevalence   0.5323   0.3126   0.1551
# Balanced Accuracy      0.9584   0.9539   0.9866
