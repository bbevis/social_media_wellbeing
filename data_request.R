### --------------------------------
### Variable-Selection-Code CoCo-UT1
### --------------------------------

# Dear collaborators!
# Thank you for your interest in our CoCo-UT1 data!
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
# 02_CoCo_UT --> 02_documentation_osf --> 02_finaldata --> CoCo_UT1.
# To set the working directory, please make sure to adapt the correct network drive letter used for the
# ae_back_extern network drive on your computer (here D).

setwd("D:/03_forschung/01_projekte/12_coco/02_CoCo_UT/02_documentation_osf/02_finaldata/CoCo_UT1")


### ---------------------------
### STEP 1: Select data sources
### ---------------------------
# Please select the data sets you are interested in. Each data set represents one data source.
# Therefore, remove the "#" in front of your requested lines.

### Step 1: Pre- and Onboarding Survey ###

presurvey <- read.csv("01_presurvey_coco_ut1.csv")
# onboarding_survey <- read.csv("02_onboarding_survey_coco_ut1.csv")

### Step 2: Daily Experience Report ###

# daily_survey <- read.csv("03_daily_survey_coco_ut1.csv")
ema_survey <- read.csv("04_ema_survey_coco_ut1.csv")
appusage_survey <- read.csv("05_appusage_survey_coco_ut1.csv")
postsurvey <- read.csv("06_postsurvey_coco_ut1.csv")

### Step 3: Writing Prompts ###

# writing_prompts <- read.csv("07_writing_prompts_coco_ut1.csv")


### ------------------------
### STEP 2: Select variables
### ------------------------
# Please remove the "#" in front of each line to activate variable selection for this data source.
# Then, please replace the placeholders *please add variable* by inserting your variables of interest and removing the stars.
# Use a comma to separate the variables.
# You can find all available variables in the respective codebook on OSF.
# Please make sure to select the "id" variable from all data sets of interest
# if you intend to merge data sets.

presurvey_select <- select(presurvey, 
                           swls_1_t1, swls_2_t1, swls_3_t1, swls_4_t1, swls_5_t1, # Satisfaction with life scale
                           uls_1_t1, uls_2_t1, uls_3_t1, uls_4_t1, uls_5_t1, uls_6_t1, uls_7_t1, uls_8_t1, uls_9_t1, # UCLA Loneliness Scale
                           awb_1_t1, awb_2_t1, awb_3_t1, awb_4_t1, awb_5_t1, awb_6_t1, # Affective Well-Being
                           demog_ses_t1 # demographic ses 
                           ) 


# onboarding_survey_select <- select(onboarding_survey, *please add variable*, *please add variable*)
# daily_survey_select <- select(daily_survey, *please add variable*, *please add variable*)
ema_survey_select <- select(ema_survey,
                            momentary_wellbeing_angry_ema, momentary_wellbeing_worried_ema, momentary_wellbeing_happy_ema,
                            momentary_wellbeing_sad_ema, momentary_wellbeing_stressed_ema, momentary_wellbeing_lonely_ema, 
                            momentary_wellbeing_accepted_ema,
                            momentary_context_activity_ema,
                            momentary_context_social_media_ema
                            )


appusage_survey_select <- select(appusage_survey,
                                 # Individual social media apps: How much time did you spend using the following apps over the PAST WEEK? 
                                 hours_facebook_w[1,2,3], hours_twitter_w[1,2,3], hours_tiktok_w[1,2,3],
                                 hours_instagram_w[1,2,3], hours_youtube_w[1,2,3], hours_reddit_w[1,2,3],
                                 hours_snapchat_w[1,2,3],
                                 minutes_facebook_w[1,2,3], minutes_twitter_w[1,2,3], minutes_tiktok_w[1,2,3],
                                 minutes_instagram_w[1,2,3], minutes_youtube_w[1,2,3], minutes_reddit_w[1,2,3],
                                 minutes_snapchat_w[1,2,3],
                                 no_facebook_w[1,2,3], no_twitter_w[1,2,3], no_tiktok_w[1,2,3],
                                 no_instagram_w[1,2,3], no_youtube_w[1,2,3], no_reddit_w[1,2,3],
                                 no_snapchat_w[1,2,3],
                                 appusage_accuracy_w[1,2,3],
                                 # App categories: How much time did you spend using the following app categories over the PAST WEEK? 
                                 hours_information_w[1,2,3], hours_social_w[1,2,3], hours_productivity_w[1,2,3],
                                 hours_education_w[1,2,3], hours_entertainment_w[1,2,3], hours_creativity_w[1,2,3],
                                 hours_games_w[1,2,3], hours_shopping_w[1,2,3], hours_utilities_w[1,2,3], hours_other_w[1,2,3],
                                 minutes_information_w[1,2,3], minutes_social_w[1,2,3], minutes_productivity_w[1,2,3],
                                 minutes_education_w[1,2,3], minutes_entertainment_w[1,2,3], minutes_creativity_w[1,2,3],
                                 minutes_games_w[1,2,3], minutes_shopping_w[1,2,3], minutes_utilities_w[1,2,3], minutes_other_w[1,2,3],
                                 no_information_w[1,2,3], no_social_w[1,2,3], no_productivity_w[1,2,3],
                                 no_education_w[1,2,3], no_entertainment_w[1,2,3], no_creativity_w[1,2,3],
                                 no_games_w[1,2,3], no_shopping_w[1,2,3], no_utilities_w[1,2,3], no_other_w[1,2,3],
                                 appcategory_accuracy_w[1,2,3]
                                 )


postsurvey_select <- select(postsurvey, political_orientation_t2, # IV
                            swls_1_t2, swls_2_t2, swls_3_t2, swls_4_t2, swls_5_t2, # Satisfaction with life scale
                            uls_1_t2, uls_2_t2, uls_3_t2, uls_4_t2, uls_5_t2, uls_6_t2, uls_7_t2, uls_8_t2, uls_9_t2, # UCLA Loneliness Scale
                            awb_1_t2, awb_2_t2, awb_3_t2, awb_4_t2, awb_5_t2, awb_6_t2, # Affective Well-Being
                            hours_facebook_w4, hours_twitter_w4, hours_tiktok_w4,
                            hours_instagram_w4, hours_youtube_w4, hours_reddit_w4,
                            hours_snapchat_w4,
                            minutes_facebook_w4, minutes_twitter_w4, minutes_tiktok_w4,
                            minutes_instagram_w4, minutes_youtube_w4, minutes_reddit_w4,
                            minutes_snapchat_w4,
                            no_facebook_w4, no_twitter_w4, no_tiktok_w4,
                            no_instagram_w4, no_youtube_w4, no_reddit_w4,
                            no_snapchat_w4,
                            appusage_accuracy_w4,
                            hours_information_w4, hours_social_w4, hours_productivity_w4,
                            hours_education_w4, hours_entertainment_w4, hours_creativity_w4,
                            hours_games_w4, hours_shopping_w4, hours_utilities_w4, hours_other_w4,
                            minutes_information_w4, minutes_social_w4, minutes_productivity_w4,
                            minutes_education_w4, minutes_entertainment_w4, minutes_creativity_w4,
                            minutes_games_w4, minutes_shopping_w4, minutes_utilities_w4, minutes_other_w4,
                            no_information_w4, no_social_w4, no_productivity_w4,
                            no_education_w4, no_entertainment_w4, no_creativity_w4,
                            no_games_w4, no_shopping_w4, no_utilities_w4, no_other_w4,
                            appcategory_accuracy_w4
                            )



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

# write.table(presurvey_select, file = "D:/03_forschung/01_projekte/12_coco/02_CoCo_UT/03_collaborations/*collaborator's name*/coco_ut1_presurvey.csv", row.names = F, sep = ";", dec = ".")
# write.table(onboarding_survey_select, file = "D:/03_forschung/01_projekte/12_coco/02_CoCo_UT/03_collaborations/*collaborator's name*/coco_ut1_onboarding_survey.csv", row.names = F, sep = ";", dec = ".")
# write.table(daily_survey_select, file = "D:/03_forschung/01_projekte/12_coco/02_CoCo_UT/03_collaborations/*collaborator's name*/coco_ut1_daily_survey.csv", row.names = F, sep = ";", dec = ".")
# write.table(ema_survey_select, file = "D:/03_forschung/01_projekte/12_coco/02_CoCo_UT/03_collaborations/*collaborator's name*/coco_ut1_ema_survey.csv", row.names = F, sep = ";", dec = ".")
# write.table(appusage_survey_select, file = "D:/03_forschung/01_projekte/12_coco/02_CoCo_UT/03_collaborations/*collaborator's name*/coco_ut1_appusage_survey.csv", row.names = F, sep = ";", dec = ".")
# write.table(postsurvey_select, file = "D:/03_forschung/01_projekte/12_coco/02_CoCo_UT/03_collaborations/*collaborator's name*/coco_ut1_postsurvey.csv", row.names = F, sep = ";", dec = ".")
# write.table(writing_prompts_select, file = "D:/03_forschung/01_projekte/12_coco/02_CoCo_UT/03_collaborations/*collaborator's name*/coco_ut1_writing_prompts.csv", row.names = F, sep = ";", dec = ".")