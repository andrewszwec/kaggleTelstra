##########################################################
# Kaggle - Telstra Comp
#
# BUILD FAULT ANALYTIC RECORD - TEST SET
#
#
##########################################################
setwd("~/kaggle/telstra")

step1 <- merge(raw_test, event, by="id", all.x = TRUE, sort=TRUE) 
unique(step1[is.na(step1$event_type),]$id)
# no losses

##########################################################
## Merge log features
##########################################################
step2 <- merge(step1, log_features, by="id", all.x = TRUE, sort=TRUE) 
rm(step1) # no longer needed

unique(step2[is.na(step2$PC3),]$id)
unique(step2[is.na(step2$PC1),]$id)
# no losses

##########################################################
## Merge resource data
##########################################################

step3 <- merge(step2, resource, by="id", all.x = TRUE, sort=TRUE) 
rm(step2) # no longer needed

unique(step3[is.na(step3$ resource_type),]$id)
# no losses

##########################################################
## Merge serverity type data
##########################################################
step4 <- merge(step3, severity, by="id", all.x = TRUE, sort=TRUE) 
rm(step3) # no longer needed

unique(step4[is.na(step4$severity_type),]$id)
# no losses

##########################################################
## Prepare for modelling
##########################################################


# replace words in some classes
step4$location <- as.numeric(gsub("location ","",step4$location, ignore.case = TRUE, perl=TRUE))
step4$event_type <- gsub("event_type ","",step4$event_type, ignore.case = TRUE, perl=TRUE)
step4$resource_type <- gsub("resource_type ","",step4$resource_type, ignore.case = TRUE, perl=TRUE)
step4$severity_type <- gsub("severity_type ","",step4$severity_type, ignore.case = TRUE, perl=TRUE)


## Bucket the categoricals up to reduce below 53
source("bucket_categoricals-test-set.r")

unique(step6[is.na(step6$event_type.group),]$id)
# no losses

## Convert back to factors for modelling
step6$location      <- as.factor(step6$location )
step6$event_type    <- as.factor(step6$event_type )
step6$resource_type <- as.factor(step6$resource_type )
step6$severity_type <- as.factor(step6$severity_type )

step6 <- subset(step6, select=c( -location.group,  -event_type.group) )
step6 <- step6[complete.cases(step6), ]
# there are no NAs in any row!

buffer2_test <- subset(step6, select=c( -location, -id, -event_type, -location.group,  -event_type.group) )
#rm(step6, b)



## Take test set in data frame buffer2_test and make prediction
pred <- as.numeric(predict(rf_fit, newdata=buffer2_test))

##########################################################
## Now output into format used for subssmissions
##########################################################
s1 <- data.frame(id=step6$id, prediction=pred )

s1$predict_0 <- ifelse(s1$prediction == 0,1,0 )
s1$predict_1 <- ifelse(s1$prediction == 1,1,0 )
s1$predict_2 <- ifelse(s1$prediction == 2,1,0 )

nrow(raw_test); nrow(s1)
# = 11,171, 138,285

require(sqldf)
submission <- sqldf("
      select  id
              ,max(predict_0) as predict_0
              ,max(predict_1) as predict_1
              ,max(predict_2) as predict_2
      from s1
      group by id
      ")

missing.ids <- raw_test[which(!(raw_test$id %in% submission$id)),]$id
num_miss <- length(missing.ids)

fill.in <- data.frame(missing.ids, rep(0,num_miss),rep(0,num_miss),rep(0,num_miss))

names(fill.in) <- names(submission)

final <- rbind(submission, fill.in)

final <- final[order(final$id),]

filename <- paste0("submission ",gsub(":", "_", Sys.time(), ignore.case = TRUE, perl = TRUE),".csv")
write.csv(final, file=filename ,row.names = FALSE )

nrow(final)
# 10790
# 11171 -- corretct number rows




