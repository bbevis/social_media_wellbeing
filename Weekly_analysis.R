#############################################
#
#       Weekly level analysis
#
#############################################

library(dplyr)
library(stringr)
library(lme4)
library(tidyr)
# install.packages("multilevelmediation")
# library(multilevelmediation)

# Read data
appusage_1 <- read.csv("data/coco_ut1_appusage_survey.csv", header = TRUE, sep = ";")
ema_1 <- read.csv("data/coco_ut1_ema_survey.csv", header = TRUE, sep = ";")
post_1 <- read.csv("data/coco_ut1_postsurvey.csv", header = TRUE, sep = ";")
pre_1 <- read.csv("data/coco_ut1_presurvey.csv", header = TRUE, sep = ";")
appusage_2 <- read.csv("data/coco_ut2_appusage_survey.csv", header = TRUE, sep = ";")
ema_2 <- read.csv("data/coco_ut2_ema_survey.csv", header = TRUE, sep = ";")
post_2 <- read.csv("data/coco_ut2_postsurvey.csv", header = TRUE, sep = ";")
pre_2 <- read.csv("data/coco_ut2_presurvey.csv", header = TRUE, sep = ";")
demo_fall <- read.csv("data/demographics_fall_anonymized.csv", header = TRUE)
demo_spring <- read.csv("data/demographics_spring_anonymized.csv", header = TRUE)