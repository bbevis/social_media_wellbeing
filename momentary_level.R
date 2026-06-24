#############################################
#
#       Momentary level analysis
#
#############################################

library(dplyr)


ema_1 <- read.csv("data/coco_ut1_ema_survey.csv", header = TRUE, sep = ";")
ema_2 <- read.csv("data/coco_ut2_ema_survey.csv", header = TRUE, sep = ";")

nrow(ema_1) + nrow(ema_2)
