##########################################################
# Kaggle - Telstra Comp
#
# LOAD DATA
#
#
##########################################################
setwd("~/kaggle/telstra")

dir = "~/kaggle/telstra/data/"

event     <- read.csv(paste0(dir,"event_type.csv"))
feature   <- read.csv(paste0(dir,"log_feature.csv"))
resource  <- read.csv(paste0(dir,"resource_type.csv"))
severity   <- read.csv(paste0(dir,"severity_type.csv"))
raw_train  <- read.csv(paste0(dir,"train.csv"))
raw_test   <- read.csv(paste0(dir,"test.csv"))
