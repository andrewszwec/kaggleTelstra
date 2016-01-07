## Bucket categorical variables up

# summary(step4)
# 
# unique(step4$location)
# unique(step4$event_type)
# unique(step4$resource_type)
# unique(step4$severity_type)
# 
# cor(cbind(step4$location, step4$fault_severity))
# #0.1122797
# cor(cbind(step4$event_type, step4$fault_severity))
# #-0.1918627
# cor(cbind(step4$resource_type, step4$fault_severity))
# #0.2581834
# cor(cbind(step4$severity_type, step4$fault_severity))
# #-0.2664559
##########################################################
# Kaggle - Telstra Comp
#
# BUCKET CATEGORICALS
#
#
##########################################################
setwd("~/kaggle/telstra")

## LOCATIONS
# How often does each location occur with each target variable?
a <- data.frame(cbind(step4$location, step4$fault_severity, rep(1,length(step4$event_type))))
names(a) <- c("location", "fault_severity", "count")

# Group by a$fault_severity,a$location count how many of each
b <- aggregate(a$count, by=list(fault_severity=a$fault_severity, location=a$location), sum )
names(b)[3] <- "count"

b <- b[order(b$count, decreasing = TRUE), ]

b1 <- b[ b$fault_severity == 1, ]
b2 <- b[ b$fault_severity == 2, ]
b3 <- b[ b$fault_severity == 3, ]


require(Hmisc)
num_groups <- 5
b1.cut <- data.frame(b1, location.group = paste("fault_sev_1", cut2(b1$count, g=num_groups)))
b2.cut <- data.frame(b2, location.group = paste("fault_sev_2", cut2(b2$count, g=num_groups)))
b3.cut <- data.frame(b3, location.group = paste("fault_sev_3", cut2(b3$count, g=num_groups)))

b.cut.location <- data.frame(rbind(b1.cut, b2.cut, b3.cut))

## view it
aggregate(b.cut.location$count, by=list(b.cut.location$location.group), sum)

# add the word location back into the location column
#b.cut$location <- paste("location",b.cut$location)

# make location a factor
b.cut.location$location <- as.factor(b.cut.location$location)

## Add location groups to step4 data frame
step5 <- merge(step4, b.cut.location[,c("location", "location.group")], by="location", all.x = TRUE)

unique(step5[is.na(step5$location.group),]$location)



## EVENTS



# How often does each event_type occur with each target variable?
a <- data.frame(cbind(step4$event_type, step4$fault_severity, rep(1,length(step4$event_type))))
names(a) <- c("event_type", "fault_severity", "count")

a$count <- as.numeric(a$count)

# Group by a$fault_severity,a$event_type count how many of each
b <- aggregate(a$count, by=list(fault_severity=a$fault_severity, event_type=a$event_type), sum )
names(b)[3] <- "count"

#b <- b[order(b$count, decreasing = TRUE), ]

b1 <- b[ b$fault_severity == 1, ]
b2 <- b[ b$fault_severity == 2, ]
b3 <- b[ b$fault_severity == 3, ]


require(Hmisc)
num_groups <- 5
b1.cut <- data.frame(b1, event_type.group = paste("fault_sev_1", cut2(b1$count, g=num_groups)))
b2.cut <- data.frame(b2, event_type.group = paste("fault_sev_2", cut2(b2$count, g=num_groups)))
b3.cut <- data.frame(b3, event_type.group = paste("fault_sev_3", cut2(b3$count, g=num_groups)))

b.cut.event_type <- data.frame(rbind(b1.cut, b2.cut, b3.cut))

## view it
aggregate(b.cut.event_type$count, by=list(b.cut.event_type$event_type.group), sum)


require(ggplot2)
c <- ggplot(b3.cut, aes(factor(event_type.group)))
c + geom_bar()


# add the word event_type back into the event_type column
#b.cut$event_type <- paste("event_type",b.cut$event_type)

# make event_type a factor
b.cut.event_type$event_type <- as.factor(b.cut.event_type$event_type)

## Add event_type groups to step4 data frame
step6 <- merge(step5, b.cut.event_type[,c("event_type", "event_type.group")], by="event_type", all.x = TRUE)

unique(step6[is.na(step6$event_type.group),]$event_type)

rm(step4, step5)
rm(a
   ,b1.cut
   ,b2.cut
   ,b3.cut
   ,c
   ,b1
   ,b2
   ,b3
   ,feature.pivot
   )