##########################################################
# Kaggle - Telstra Comp
#
# BUCKET CATEGORICALS TEST SET
#
#
##########################################################
setwd("~/kaggle/telstra")


## Add location groups to step4 data frame
step5 <- merge(step4, b.cut.location[,c("location", "location.group")], by="location", all.x = TRUE)

unique(step5[is.na(step5$location.group),]$location)



## EVENTS

## Add event_type groups to step4 data frame
step6 <- merge(step5, b.cut.event_type[,c("event_type", "event_type.group")], by="event_type", all.x = TRUE)

unique(step6[is.na(step6$event_type.group),]$event_type)

## Remove some variables
#rm(step4, step5)
