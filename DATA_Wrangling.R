setwd("C:/Users/JLiu/Desktop/EnrollmentSurvey")
SAO<- read.csv(file="SAO Data.csv", header=TRUE)
###########################################################
##########MANIPULATING ENGLISH/MATH TEACHER ITEMS##########
###########################################################

###Recode responses from English/Math teacher survey
SAO$response[SAO$response=="below_average"]<-1
SAO$response[SAO$response=="average"]<-2
SAO$response[SAO$response=="good_average"]<-3
SAO$response[SAO$response=="very_good"]<-4
SAO$response[SAO$response=="excellent"]<-5
SAO$response[SAO$response=="no_basis_for_judging"]<-NA
SAO$response[SAO$response=="emerging"]<-1
SAO$response[SAO$response=="developing"]<-2
SAO$response[SAO$response=="demonstrating"]<-3
SAO$response[SAO$response=="no_basis_judge"]<-NA

### Select Recommendation Forms
English <- subset(SAO, name=="English Recommendation")
Math <- subset(SAO, name=="Mathematics Recommendation")

###match the variable names of Math with English
Math$question[Math$question=="willingness_intellectual"]<-"willingness_risks"
Math$question[Math$question=="response_setbacks"]<-"response_setback"
Math$question[Math$question=="response_criticism"]<-"response_critique"
Math$question[Math$question=="pursues_learning"]<-"pursues_opportunities"
Math$question[Math$question=="organization_prepare"]<-"organization_prep"
Math$question[Math$question=="open_ideas_opinions"]<-"open_ideas"
Math$question[Math$question=="listens_contemplates"]<-"listens_contemplate"
Math$question[Math$question=="conduct_classroom"]<-"conduct_behavior"
Math$question[Math$question=="concern_of_others"]<-"concern_others"
Math$question[Math$question=="accountability_actions"]<-"accountability"
Math$question[Math$question=="ability_working_independ"]<-"ability_work"
Math$question[Math$question=="ability_overcome_obstacles"]<-"ability_overcome"
Math$question[Math$question=="ability_leading"]<-"ability_lead"
Math$question[Math$question=="ability_handing_homework"]<-"ability_homework"
Math$question[Math$question=="ability_follow_directions"]<-"ability_direction"
###########################################################
############ MANIPULATING PARENT SURVEY DATA ##############
###########################################################

Parent_wide<-read.csv(file="SAO Parent Statement - wide file.csv",header=T, stringsAsFactors = FALSE)
colnames(SAO)[colnames(SAO)=="?..user_id"] <- "ID"
colnames(Parent_wide)[colnames(Parent_wide)=="User_Id"]<-"ID"
###Recode responses for wide format Parent Form
Parent_wide$initiative[Parent_wide$initiative=="emerging"]<-1
Parent_wide$initiative[Parent_wide$initiative=="developing"]<-2
Parent_wide$initiative[Parent_wide$initiative=="demonstrating"]<-3
Parent_wide$initiative[Parent_wide$initiative=="no_basis"]<-NA

Parent_wide$intellectual_engagement[Parent_wide$intellectual_engagement=="emerging"]<-1
Parent_wide$intellectual_engagement[Parent_wide$intellectual_engagement=="developing"]<-2
Parent_wide$intellectual_engagement[Parent_wide$intellectual_engagement=="demonstrating"]<-3
Parent_wide$intellectual_engagement[Parent_wide$intellectual_engagement=="no_basis"]<-NA

Parent_wide$open_minded[Parent_wide$open_minded=="emerging"]<-1
Parent_wide$open_minded[Parent_wide$open_minded=="developing"]<-2
Parent_wide$open_minded[Parent_wide$open_minded=="demonstrating"]<-3
Parent_wide$open_minded[Parent_wide$open_minded=="no_basis"]<-NA

Parent_wide$resilience[Parent_wide$resilience=="emerging"]<-1
Parent_wide$resilience[Parent_wide$resilience=="developing"]<-2
Parent_wide$resilience[Parent_wide$resilience=="demonstrating"]<-3
Parent_wide$resilience[Parent_wide$resilience=="no_basis"]<-NA

Parent_wide$self_control[Parent_wide$self_control=="emerging"]<-1
Parent_wide$self_control[Parent_wide$self_control=="developing"]<-2
Parent_wide$self_control[Parent_wide$self_control=="demonstrating"]<-3
Parent_wide$self_control[Parent_wide$self_control=="no_basis"]<-NA

Parent_wide$social_awareness[Parent_wide$social_awareness=="emerging"]<-1
Parent_wide$social_awareness[Parent_wide$social_awareness=="developing"]<-2
Parent_wide$social_awareness[Parent_wide$social_awareness=="demonstrating"]<-3
Parent_wide$social_awareness[Parent_wide$social_awareness=="no_basis"]<-NA

Parent_wide$teamwork[Parent_wide$teamwork=="emerging"]<-1
Parent_wide$teamwork[Parent_wide$teamwork=="developing"]<-2
Parent_wide$teamwork[Parent_wide$teamwork=="demonstrating"]<-3
Parent_wide$teamwork[Parent_wide$teamwork=="no_basis"]<-NA

###rename variables in parent_wide
colnames(Parent_wide)[colnames(Parent_wide)=="initiative"]<-"IN_parent"
colnames(Parent_wide)[colnames(Parent_wide)=="intellectual_engagement"]<-"IE_parent"
colnames(Parent_wide)[colnames(Parent_wide)=="open_minded"]<-"OP_parent"
colnames(Parent_wide)[colnames(Parent_wide)=="resilience"]<-"RS_parent"
colnames(Parent_wide)[colnames(Parent_wide)=="self_control"]<-"SC_parent"
colnames(Parent_wide)[colnames(Parent_wide)=="social_awareness"]<-"SA_parent"
colnames(Parent_wide)[colnames(Parent_wide)=="teamwork"]<-"TW_parent"
########################################################################
##########GENERATING MERGED DATA(ENGLISH/MATH TEACHER,PARENT) ##########
########################################################################

## Reframe data from long to wide
library(dplyr)
English$new_question<-paste(English$question,"_","English")
English <- English[,-5]
English <- English[,-7]
Math$new_question<-paste(Math$question,"_","Math")
Math<-Math[,-5]
Math<-Math[,-7]

library(tidyr)
English_wide<-spread(English, new_question, response)
Math_wide<-spread(Math, new_question, response)
Parent_wide<-spread(Parent, question, response)
Math_wide2<-merge(Math_wide, math_tardi2,by="ID")

###combine Snapshot score to each of the reccomendation form
snapshot<- read.csv(file="snapshot_scores_raw_wide (5_18_19).csv", header=TRUE)
English_snapshot<-merge(English_wide, snapshot, by="ID")
Math_snapshot<-merge(Math_wide2, snapshot, by="ID")
Parent_snapshot<-merge(Parent_wide, snapshot, by="ID")

###add SSAT scores to the data set
ssat<-read.csv(file="ssat scores (for snapshot users - 6_12_19).csv", header=T)
English_full<-merge(English_snapshot, ssat, by="ID")
Math_full<-merge(Math_snapshot, ssat, by="ID")
Parent_full<-merge(Parent_snapshot, ssat, by="ID")

###merge all of data into one wide_format
EandM<-merge(English_full,Math_full, by="ID")
full_data<-merge(EandM, Parent_full, by="ID")

### export datasets
write.csv(English_full, "English_full.csv")
write.csv(Math_full,"Math_full.csv")
write.csv(Parent_full, "Parent_full.csv")
write.csv(full_data, "full_data.csv")







