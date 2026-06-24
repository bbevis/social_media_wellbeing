### --------------------------------
### Variable-Selection-Code CoCo-UT2
### --------------------------------

# Dear collaborators!
# Thank you for your interest in our CoCo-UT2 data!
# Below, you can find the code required to select your data sources and variables of interest.
# ATTENTION: You only have to modify steps 1 and 2.
# Every step marked "NOTE TO WWU WORK GROUP" will be modified and executed by our colleagues only.
# If you have any questions, please do not hesitate to contact us via coco.projects@wwu.de.
# Kind regards,
# your CoCo-team


### ----------------------
### Note TO WWU WORK GROUP
### ----------------------

# 1) Install & access required packages.
install.packages("dplyr")
library(dplyr)

# 2) Set working directory.
# The final data can be found in the CoCo project folder (12_coco) under 
# 02_CoCo_UT --> 02_documentation_osf --> 02_finaldata --> CoCo-UT2.
# To set the working directory, please make sure to adapt the correct network drive letter used for the
# ae_back_extern network drive on your computer (here D).

setwd("D:/03_forschung/01_projekte/12_coco/02_CoCo_UT/02_documentation_osf/02_finaldata/CoCo_UT2")


### ---------------------------
### STEP 1: Select data sources
### ---------------------------
# Please select the data sets you are interested in. Each data set represents one data source.
# Therefore, remove the "#" in front of your requested lines.

### Step 1: Pre- and Onboarding Survey ###

# presurvey <- read.csv("01_presurvey_coco_ut2.csv")
# onboarding_survey <- read.csv("02_onboarding_survey_coco_ut2.csv")

### Step 2: Daily Experience Report ###

# daily_survey <- read.csv("03_daily_survey_coco_ut2.csv")
# ema_survey <- read.csv("04_ema_survey_coco_ut2.csv")
# appusage_survey <- read.csv("05_appusage_survey_coco_ut2.csv")
# postsurvey <- read.csv("06_postsurvey_coco_ut2.csv")

### Step 3: Writing Prompts ###

# writing_prompts <- read.csv("07_writing_prompts_coco_ut2.csv")


### ------------------------
### STEP 2: Select variables
### ------------------------
# Please remove the "#" in front of each line to activate variable selection for this data source.
# Then, please replace the placeholders *please add variable* by inserting your variables of interest and removing the stars.
# Use a comma to separate the variables.
# You can find all available variables in the respective codebook on OSF.
# Please make sure to select the "id" variable from all data sets of interest
# if you intend to merge data sets.

# presurvey_select <- select(presurvey, *please add variable*, *please add variable*)
# onboarding_survey_select <- select(onboarding_survey, *please add variable*, *please add variable*)
# daily_survey_select <- select(daily_survey, *please add variable*, *please add variable*)
# ema_survey_select <- select(ema_survey, *please add variable*, *please add variable*)
# appusage_survey_select <- select(appusage_survey, *please add variable*, *please add variable*)
# postsurvey_select <- select(postsurvey, *please add variable*, *please add variable*)
# writing_prompts_select <- select(onboarding_survey, *please add variable*, *please add variable*)


### ------------------------------------
### NOTE TO WWU WORK GROUP: Write tables
### ------------------------------------
# Please remove the "#" in front of each write.table-command including a data subset as requested by the collaborator.
# Please save all tables in the CoCo project folder (12_coco) under 02_CoCo_UT --> 03_collaborations.
# There, each collaborator is assigned an individual sub-folder titled *lastname_firstname*. It will include all subsets
# that resulted from this variable selection code.
# In each write.table-command, please replace the placeholder *collaborator's name* with the collaborator's last and first name.
# Also, make sure to adapt the correct network drive letter used for the ae_back_extern network drive on your computer (here D).
# Lastly, send (e.g., via email) or share (e.g., via Sciebo) the resulting table (i.e., data sets) with the collaborator.

# write.table(presurvey_select, file = "D:/03_forschung/01_projekte/12_coco/02_CoCo_UT/03_collaborations/*collaborator's name*/coco_ut2_presurvey.csv", row.names = F, sep = ";", dec = ".")
# write.table(onboarding_survey_select, file = "D:/03_forschung/01_projekte/12_coco/02_CoCo_UT/03_collaborations/*collaborator's name*/coco_ut2_onboarding_survey.csv", row.names = F, sep = ";", dec = ".")
# write.table(daily_survey_select, file = "D:/03_forschung/01_projekte/12_coco/02_CoCo_UT/03_collaborations/*collaborator's name*/coco_ut2_daily_survey.csv", row.names = F, sep = ";", dec = ".")
# write.table(ema_survey_select, file = "D:/03_forschung/01_projekte/12_coco/02_CoCo_UT/03_collaborations/*collaborator's name*/coco_ut2_ema_survey.csv", row.names = F, sep = ";", dec = ".")
# write.table(appusage_survey_select, file = "D:/03_forschung/01_projekte/12_coco/02_CoCo_UT/03_collaborations/*collaborator's name*/coco_ut2_appusage_survey.csv", row.names = F, sep = ";", dec = ".")
# write.table(postsurvey_select, file = "D:/03_forschung/01_projekte/12_coco/02_CoCo_UT/03_collaborations/*collaborator's name*/coco_ut2_postsurvey.csv", row.names = F, sep = ";", dec = ".")
# write.table(writing_prompts_select, file = "D:/03_forschung/01_projekte/12_coco/02_CoCo_UT/03_collaborations/*collaborator's name*/coco_ut2_writing_prompts.csv", row.names = F, sep = ";", dec = ".")
