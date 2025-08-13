# --------------------------------------------------
# NOTE: This script requires private datasets that are not included due to confidentiality. 
# Replace the paths with your own files structured similarly to the expected data frames.
# --------------------------------------------------

#Import libraries
library("readxl")
library("tidyr")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("readxl")
library("boot")
library("table1")
library("Hmisc")
library("sjmisc")
library("gt")


#The table shows the proportions (as percentages) of patients with and without Restless Leg Syndrome (RLS) across three clinical groups:
#      Healthy Control
#      Prodromal
#      Parkinson's Disease


#Import data
ParticipantStatus <- read_csv("data/ParticipantStatus.csv")
PDDiagHistory <- read.csv("data/PDDiagHistory.csv")
Demographics <- read_csv("data/Demographics.csv")
MedConditions <- read.csv("data/MedConditions.csv")

#Combine ParticipantStatus and DiagHistory
cohToTest <- ParticipantStatus
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all=T)

### Sleep Problems ###
cohToTest <- ParticipantStatus
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all=T)

#Import sleep problems data
Sleep <- read_csv("data/MedConditions.csv")
Sleep <- Sleep[grepl("(sleep\\b|sleeping|somnia\\b|dream\\b|nightmare\\b|REM\\b)",Sleep$MHTERM,ignore.case = T),]
Sleep = Sleep[!duplicated(Sleep$PATNO),]
RLS <- MedConditions[grepl("(Restless Leg Syndrome\\b|RLS\\b)",MedConditions$MHTERM,ignore.case = T),]

#Mutate medical conditions to only keep sleep problems
Sleep1 <- Sleep %>%
  mutate(Term=case_when(
    MHTERM == "adult obstructive sleep apnea" ~ "Obstructive sleep apnea",
    MHTERM == "central sleep apnea" ~ "Central sleep apnea",
    MHTERM == "Central Sleep Apnea" ~ "Central sleep apnea",
    MHTERM == "Central sleep apnea- uses APAP" ~ "Central sleep apnea",
    MHTERM == "Complex Sleep Apnea" ~ "Complex sleep apnea",
    MHTERM == "D/x: Atrial Fibrillation - Started after d/x of obstructive sleep apnea" ~ "Obstructive sleep apnea",
    MHTERM == "D/x: Obstructive Sleep Apnea" ~ "Obstructive sleep apnea",
    MHTERM == "Mild Sleep Apnea" ~ "Mild sleep apnea",
    MHTERM == "Moderate sleep apnea" ~ "Mild sleep apnea",
    MHTERM == "obstructive sleep apnea" ~ "Obstructive sleep apnea",
    MHTERM == "Obstructive sleep Apnea" ~ "Obstructive sleep apnea",
    MHTERM == "Obstructive Sleep apnea" ~ "Obstructive sleep apnea",
    MHTERM == "Obstructive Sleep Apnea" ~ "Obstructive sleep apnea",
    MHTERM == "OBSTRUCTIVE SLEEP APNEA" ~ "Obstructive sleep apnea",
    MHTERM == "Obstructive sleep apnea - uses CPAP machine" ~ "Obstructive sleep apnea",
    MHTERM == "obstructive sleep apnea controlled with nightly CPAP use " ~ "Obstructive sleep apnea",
    MHTERM == "Obstructive sleep apnea- utilizes CPAP" ~ "Obstructive sleep apnea",
    MHTERM == "Obstructive sleep apnea, now on CPAP" ~ "Obstructive sleep apnea",
    MHTERM == "Obstructive sleep apnea, resolved post surgery" ~ "Obstructive sleep apnea",
    MHTERM == "obstructive sleep apnea, uses oral appliance to sleep" ~ "Obstructive sleep apnea",
    MHTERM == "obstructive sleep apnea. on CPAP" ~ "Obstructive sleep apnea",
    MHTERM == "OSA- Obstructive Sleep Apnea" ~ "Obstructive sleep apnea",
    MHTERM == "Severe obstructive sleep apnea, uses CPAP" ~ "Obstructive sleep apnea",
    MHTERM == "sleep apnea" ~ "Sleep apnea",
    MHTERM == "sleep Apnea" ~ "Sleep apnea",
    MHTERM == "Sleep Apnea" ~ "Sleep apnea",
    MHTERM == "Sleep APnea" ~ "Sleep apnea",
    MHTERM == "SLEEP APNEA" ~ "Sleep apnea",
    MHTERM == "Sleep apnea - Central" ~ "Sleep apnea",
    MHTERM == "Sleep apnea - Obstructive" ~ "Obstructive sleep apnea",
    MHTERM == "sleep apnea (CPAP)" ~ "Sleep apnea",
    MHTERM == "Sleep apnea (stable and improving, uses CPAP)" ~ "Sleep apnea",
    MHTERM == "Sleep apnea (uses CPAP machine)" ~ "Sleep apnea",
    MHTERM == "Sleep Apnea & Insomnia" ~ "Sleep apnea & Insomnia",
    MHTERM == "Sleep apnea and hypopnea syndrome" ~ "Sleep apnea",
    MHTERM == "Sleep apnea on Bipap" ~ "Sleep apnea",
    MHTERM == "Sleep apnea requiring cpap." ~ "Sleep apnea",
    MHTERM == "Sleep apnea syndrome" ~ "Sleep apnea",
    MHTERM == "sleep apnea treated with automatic positive airway pressure (APAP)" ~ "Sleep apnea",
    MHTERM == "Sleep apnea with CPAP" ~ "Sleep apnea",
    MHTERM == "Sleep apnea- CPAP" ~ "Sleep apnea",
    MHTERM == "Sleep apnea, currently well controlled" ~ "Sleep apnea",
    MHTERM == "Sleep apnea, obstructive" ~ "Obstructive sleep apnea",
    MHTERM == "sleep apnea, on CPAP" ~ "Sleep apnea",
    MHTERM == "Sleep apnea, on CPAP since early 2023" ~ "Sleep apnea",
    MHTERM == "Sleep apnea, uses CPAP" ~ "Sleep apnea",
    MHTERM == "Sleep apnea, using a positional sleeping position trainer works really wel" ~ "Sleep apnea",
    MHTERM == "Sleep apnea; symptoms include snoring and difficulty with daytime wakefulness" ~ "Sleep apnea",
    MHTERM == "Sleep disturbance and mild sleep apnea, contributes to daytime fatigue" ~ "Sleep apnea",
    MHTERM == "UPPP surgery- Sleep Apnea" ~ "Sleep apnea",
    MHTERM == "UPPP Surgery- Sleep Apnea" ~ "Sleep apnea",
    MHTERM == "Acting out dreams/ RBD, sleep study suggested REM related disorder" ~ "REM Sleep",
    MHTERM == "Anxiety, insomnia and Depression since 2016, requiring intermittent citalopram treatment" ~ "Insomnia",
    MHTERM == "CHRONIC EXCESSIVE DROOLING DURING SLEEP" ~ NA,
    MHTERM == "Depression, anxiety, and anxiety-related insomnia" ~ "Insomnia",
    MHTERM == "Difficulty staying asleep" ~ "Difficulty staying asleep",
    MHTERM == "dream enactment behavior" ~ "REM Sleep",
    MHTERM == "Dream reenactment" ~ "REM Sleep",
    MHTERM == "Frequent Sleep Disturbances" ~ "Frequent Sleep Disturbances",
    MHTERM == "He already has an overactive bladder for 10 years, after a while this interupted his sleep. Since a couple of months he also has lose of urine, which was the reason of starting the medication" ~ NA,
    MHTERM == "inconclusive sleep studies but clear symptoms of idiopathic REM sleep behavior disorder" ~ "REM Sleep",
    MHTERM == "insomnia" ~ "Insomnia",
    MHTERM == "Insomnia" ~ "Insomnia",
    MHTERM == "INSOMNIA" ~ "Insomnia",
    MHTERM == "Insomnia & RBD" ~ "Insomnia and REM",
    MHTERM == "Insomnia secondary to anxiety" ~ "Insomnia",
    MHTERM == "Insomnia since heart bypass surgery" ~ "Insomnia",
    MHTERM == "insomnia, middle" ~ "Insomnia",
    MHTERM == "Insomnia; Unknown start month and day" ~ "Insomnia",
    MHTERM == "Insomnia/anxiety" ~ "Insomnia",
    MHTERM == "Insomnia/Chronic fatigue disorder" ~ "Insomnia",
    MHTERM == "Intermittent Insomnia" ~ "Insomnia",
    MHTERM == "Mild insomnia" ~ "Insomnia",
    MHTERM == "Muscular Cramps- Calfs (during sleep)" ~ NA,
    MHTERM == "Obstructive Sleep Apnoea" ~ "Obstructive sleep apnea",
    MHTERM == "Obstructive sleep apnoea" ~ "Obstructive sleep apnea",
    MHTERM == "obstructive sleep disorder" ~ "Obstructive sleep disorder",
    MHTERM == "Obstructuve sleep apnoea" ~ "Obstructive sleep apnea",
    MHTERM == "Occasional Insomnia" ~ "Insomnia",
    MHTERM == "OSA / REM Sleep disorder" ~ "REM Sleep",
    MHTERM == "Parkinson's disease Symptoms began in 2018, pt noticed that my hand writing became difficult December 2019/January 2020, left hand occasionally shaking, hand writing is difficult. In 2021, left hand lost some flexibility and lost some strength.  Left hand shaking increase frequency.  However, pt reports that she can do everything by herself. In November 2021,  pt reported noticing right hand had lost some strength.   Sleeping pattern has changed in last two years. Pt feels it is difficult to fall asleep during night.  Now pt takes melatonin 5 mg per day at 10:30 pm.  Sleep improved to some degree but some days pt still cannot fall in sleep" ~ "Sleep disorder",
    MHTERM == "Parkinson's disease; pt is not aware of any PD symptoms, but reports she may have non-motor symptoms like sleep issues;  Dx based on DaTscan; Awaiting pt to send physician report regarding diagnosis to determine if pt truly has PD" ~ "Sleep issues",
    MHTERM == "Participant endorsed REM behavior disorder (RBD) symptoms, specifically the nightmares, began at least 20 years ago, but unsure of when it officially started. Polysomnogram confirmed the RBD diagnosis on October 19, 2020." ~ "REM Sleep",
    MHTERM == "possible REM sleep behavior disorder" ~ "REM Sleep",
    MHTERM == "Possible REM sleep behavior disorder (not confirmed by sleep study)" ~ "REM Sleep",
    MHTERM == "Primary insomnia" ~ "Insomnia",
    MHTERM == "Rapid eye movement (REM) sleep behavior disorder" ~ "REM Sleep",
    MHTERM == "Rapid eye movement (REM) sleep behavior disorder (RBD)" ~ "REM Sleep",
    MHTERM == "Rapid eye movement sleep behavior disorder" ~ "REM Sleep",
    MHTERM == "Rapid Eye Movement Sleep Behavior Disorder" ~ "REM Sleep",
    MHTERM == "Recurrent insomnia" ~ "Insomnia",
    MHTERM == "REM" ~ "REM Sleep",
    MHTERM == "REM behavior" ~ "REM Sleep",
    MHTERM == "Rem behavior disorder" ~ "REM Sleep",
    MHTERM == "REM behavior disorder" ~ "REM Sleep",
    MHTERM == "REM Behavior disorder" ~ "REM Sleep",
    MHTERM == "REM Behavior Disorder" ~ "REM Sleep",
    MHTERM == "REM Behavior Disorder (RBD)" ~ "REM Sleep",
    MHTERM == "Rem Behavior Disorders" ~ "REM Sleep",
    MHTERM == "REM Behavior Sleep Disorder" ~ "REM Sleep",
    MHTERM == "REM behavioral disorder" ~ "REM Sleep",
    MHTERM == "REM Behavioral disorder" ~ "REM Sleep",
    MHTERM == "REM Behavioral Disorder" ~ "REM Sleep",
    MHTERM == "REM behavioral sleep disorder" ~ "REM Sleep",
    MHTERM == "REM Behaviour Sleep Disorder" ~ "REM Sleep",
    MHTERM == "rem sleep behavior" ~ "REM Sleep",
    MHTERM == "REM sleep Behavior" ~ "REM Sleep",
    MHTERM == "REM Sleep Behavior" ~ "REM Sleep",
    MHTERM == "REM SLEEP BEHAVIOR" ~ "REM Sleep",
    MHTERM == "Rem sleep behavior disorder" ~ "REM Sleep",
    MHTERM == "Rem Sleep Behavior Disorder" ~ "REM Sleep",
    MHTERM == "REM sleep behavior disorder" ~ "REM Sleep",
    MHTERM == "REM sleep Behavior Disorder" ~ "REM Sleep",
    MHTERM == "REM Sleep behavior disorder" ~ "REM Sleep",
    MHTERM == "REM Sleep Behavior Disorder" ~ "REM Sleep",
    MHTERM == "REM SLEEP BEHAVIOR DISORDER" ~ "REM Sleep",
    MHTERM == "REM sleep behavior disorder (RBD)" ~ "REM Sleep",
    MHTERM == "REM sleep Behavior Disorder (RBD)" ~ "REM Sleep",
    MHTERM == "REM Sleep Behavior Study" ~ "REM Sleep",
    MHTERM == "REM sleep behavioral disorder" ~ "REM Sleep",
    MHTERM == "REM Sleep Behavioral Disorder" ~ "REM Sleep",
    MHTERM == "REM sleep behavioral disorder (onset as early as 15 years ago, confirmed by a PSG on 8/26/2015), under control with clonazepam" ~ "REM Sleep",
    MHTERM == "REM Sleep behaviors disorder" ~ "REM Sleep",
    MHTERM == "REM sleep behaviour disorder" ~ "REM Sleep",
    MHTERM == "REM sleep behaviour Disorder" ~ "REM Sleep",
    MHTERM == "REM sleep Behaviour disorder" ~ "REM Sleep",
    MHTERM == "REM Sleep Behavioural Disorder" ~ "REM Sleep",
    MHTERM == "rem sleep disorder" ~ "REM Sleep",
    MHTERM == "Rem sleep disorder" ~ "REM Sleep",
    MHTERM == "Rem Sleep Disorder" ~ "REM Sleep",
    MHTERM == "REM sleep disorder" ~ "REM Sleep",
    MHTERM == "REM Sleep Disorder" ~ "REM Sleep",
    MHTERM == "REM SLEEP DISORDER" ~ "REM Sleep",
    MHTERM == "REM Sleep Disorder, Periodic Limb movement disorder" ~ "REM Sleep",
    MHTERM == "Rem-Behavior Sleep Disorder" ~ "REM Sleep",
    MHTERM == "REM-behavior sleep disorder" ~ "REM Sleep",
    MHTERM == "REM-Behavior Sleep Disorder" ~ "REM Sleep",
    MHTERM == "REM-behavioral sleep disorder" ~ "REM Sleep",
    MHTERM == "REM-sleep behavior disorder" ~ "REM Sleep",
    MHTERM == "REM-Sleep Behavior Disorder" ~ "REM Sleep",
    MHTERM == "REM-sleep behaviour disorder" ~ "REM Sleep",
    MHTERM == "REM-sleep Behaviour Disorder" ~ "REM Sleep",
    MHTERM == "REM-Sleep Behaviour Disorder" ~ "REM Sleep",
    MHTERM == "REM-sleep disorder" ~ "REM Sleep",
    MHTERM == "REM-sleep-behaviour disorder" ~ "REM Sleep",
    MHTERM == "Secondary insomnia" ~ "Insomnia",
    MHTERM == "sleep apnoea" ~ "Sleep apnea",
    MHTERM == "Sleep apnoea" ~ "Sleep apnea",
    MHTERM == "Sleep Apnoea" ~ "Sleep apnea",
    MHTERM == "sleep apnoea (CPAP)" ~ "Sleep apnea",
    MHTERM == "Sleep Arousal Disorder" ~ "Sleep arousal disorder",
    MHTERM == "Sleep Deficit" ~ "Sleep deficit",
    MHTERM == "sleep difficulty" ~ "Sleep difficulty",
    MHTERM == "Sleep Difficulty" ~ "Sleep difficulty",
    MHTERM == "Sleep disorder" ~ "Sleep disorder",
    MHTERM == "Sleep Disorder" ~ "Sleep disorder",
    MHTERM == "SLEEP DISORDER" ~ "Sleep disorder",
    MHTERM == "sleep disorder (ein- und durchschlafstörung)" ~ "Sleep disorder",
    MHTERM == "Sleep disturbance" ~ "Sleep disturbance",
    MHTERM == "Sleep Disturbance" ~ "Sleep disturbance",
    MHTERM == "Sleep Disturbance/Insomnia" ~ "Insomnia",
    MHTERM == "sleep disturbances" ~ "Sleep disturbance",
    MHTERM == "Sleep disturbances" ~ "Sleep disturbance",
    MHTERM == "Sleep Disturbances" ~ "Sleep disturbance",
    MHTERM == "sleep hygiene" ~ "Sleep hygiene",
    MHTERM == "sleep-related breathing disorder" ~ "Sleep apnea",
    MHTERM == "sleeping problems due to Parkinsons Disease" ~ "Sleeping problems",
    MHTERM == "suspected parasomnia overlap (RBD/somnambulism)" ~ "REM Sleep",
    MHTERM == "Trouble falling asleep" ~ "Trouble sleeping",
    MHTERM == "trouble sleeping" ~ "Trouble sleeping",
    MHTERM == "Trouble sleeping" ~ "Trouble sleeping",
    MHTERM == "Very little REM sleep, twitching, talking and lots of moving while sleeping" ~ "REM Sleep",
    MHTERM == "Mild sleep apnea" ~ "Sleep apnea",
    MHTERM == "Sleep apnea" ~ "Sleep apnea",
    MHTERM == "Obstructive sleep apnea" ~ "Obstructive sleep apnea",
    MHTERM == "Episodes of sleep apnea" ~ "Sleep apnea",
    MHTERM == "Central sleep apnea" ~ "Sleep apnea",
    MHTERM == "Adult obstructive sleep apnea" ~ "Obstructive sleep apnea",
    MHTERM == "obstructive sleep apnea syndrome" ~ "Obstructive sleep apnea",
    MHTERM == "Moderate obstructive sleep apnea" ~ "Sleep apnea",
    MHTERM == "obstructive sleep apnea controlled with nightly CPAP use" ~ "Obstructive sleep apnea"
  ))

#Sort into different types of sleep problems
Sleepp <- Sleep1[Sleep1$Term %in% c("Central sleep apnea", "Mild sleep apnea", "Obstructive sleep apnea", "Sleep apnea", "Sleep apnea & Insomnia"),]
REM <- Sleep1[Sleep1$Term %in% c("Insomnia and REM", "REM Sleep"),]
Insomnia <- Sleep1[Sleep1$Term %in% c("Insomnia and REM", "Insomnia", "Sleep apnea & Insomnia"),]

tmp <- RLS
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"

refff <- merge(ParticipantStatus,tmp,by="PATNO",all.y=T)

#Merge data
cohToTest1 <- merge(cohToTest,tmp,by="PATNO",all=T)
cohToTest2 <- merge(cohToTest1,Demographics,by="PATNO",all=T)
cohToTest2 <- merge(cohToTest2,MedConditions,by="PATNO",all.y=T)

Tes1 <- cohToTest2
Tes1[c("MHTERM.x")][is.na(Tes1[c("MHTERM.x")])] <- FALSE

Tes1 <- Tes1 %>%
  mutate(MHTER=case_when(
    MHTERM.x == FALSE ~ 0
  ))
Tes1[c("MHTER")][is.na(Tes1[c("MHTER")])] <- 1

MHTERMM <- Tes1$MHTER
Tes1 <- Tes1[order(-MHTERMM),]
Tes1 <- Tes1[!is.na(Tes1$COHORT_DEFINITION),]
Tes1 = Tes1[!duplicated(Tes1$PATNO),]

#Keep SWEDD as Healthy Controls
Tes1 <- Tes1 %>%
  mutate(COHORT_DEFINITION=case_when(
    COHORT_DEFINITION == "SWEDD" ~ "Healthy Control",
    COHORT_DEFINITION == "Parkinson's Disease" ~ "Parkinson's Disease",
    COHORT_DEFINITION == "Prodromal" ~ "Prodromal",
    COHORT_DEFINITION == "Healthy Control" ~ "Healthy Control"
  ))

df <- data.frame(matrix(ncol = 4, nrow = 2))

#Keep certain columns
cols_to_keep <- c("PATNO", "COHORT_DEFINITION", "MHTERM.x", "MHTER")
Tes3 <- Tes1[,cols_to_keep]

#Make four copies of the dataframe
Par <- Tes3
Par1 <- Tes3
Par2 <- Tes3
Par3 <- Tes3

#Assign each dataframe one status of Parkinson's disease
Par <- Tes3[Tes3$COHORT_DEFINITION %in% c("Parkinson's Disease"),]
Par1 <- Tes3[Tes3$COHORT_DEFINITION %in% c("Healthy Control"),]
Par2 <- Tes3[Tes3$COHORT_DEFINITION %in% c("Prodromal"),]
Par3 <- Tes3[Tes3$COHORT_DEFINITION %in% c("SWEDD"),]

#Change column names
names(Par)[names(Par) == "COHORT_DEFINITION"] <- "definition1"
names(Par1)[names(Par1) == "COHORT_DEFINITION"] <- "definition2"
names(Par2)[names(Par2) == "COHORT_DEFINITION"] <- "definition3"
names(Par3)[names(Par3) == "COHORT_DEFINITION"] <- "definition4"

#  rm("Participant", "FOUND_RFQ_Head_Injury_13Jun2024", "MedConditions")

#Change dataframe names
Parkinson <- Par
Healthy <- Par1
SWEDD <- Par3
Prodromal <- Par2

#Add a column of consecutive numbers
Healthy$consecutive<-1:nrow(Healthy)
Parkinson$consecutive<-1:nrow(Parkinson)
Prodromal$consecutive<-1:nrow(Prodromal)

#Change column names
names(Healthy)[names(Healthy) == "PATNO"] <- "patient1"
names(Parkinson)[names(Parkinson) == "PATNO"] <- "patient2"
names(Prodromal)[names(Prodromal) == "PATNO"] <- "patient3"

names(Healthy)[names(Healthy) == "MHTER"] <- "Head1"
names(Parkinson)[names(Parkinson) == "MHTER"] <- "Head2"
names(Prodromal)[names(Prodromal) == "MHTER"] <- "Head3"

names(Healthy)[names(Healthy) == "MHTERM"] <- "term1"
names(Parkinson)[names(Parkinson) == "MHTERM"] <- "term2"
names(Prodromal)[names(Prodromal) == "MHTERM"] <- "term3"

Healthy <- Healthy %>%
  add_column(add_column = "TRUE")

#Mutate column values
Healthy <- Healthy %>%
  mutate(Head1=case_when(
    (Head1) == 0 ~ "No",
    (Head1) == 1 ~ "Yes"
  ))

Parkinson <- Parkinson %>%
  add_column(add_column = "TRUE")

#Mutate column values
Parkinson <- Parkinson %>%
  mutate(Head2=case_when(
    (Head2) == 0 ~ "No",
    (Head2) == 1 ~ "Yes"
  ))

Prodromal <- Prodromal %>%
  add_column(add_column = "TRUE")

#Mutate column values
Prodromal <- Prodromal %>%
  mutate(Head3=case_when(
    (Head3) == 0 ~ "No",
    (Head3) == 1 ~ "Yes"
  ))

df <- data.frame(matrix(ncol = 4, nrow = 2))

#Create proportions
df[1,1] = "Yes"
df[1, 2] = (length(which(Healthy$Head1=="Yes"))/3013)*100
df[1, 3] = (length(which(Prodromal$Head3=="Yes"))/3013)*100
df[1, 4] = (length(which(Parkinson$Head2=="Yes"))/3013)*100

df[2,1] = "No"
df[2, 2] = (length(which(Healthy$Head1=="No"))/3013)*100
df[2, 3] = (length(which(Prodromal$Head3=="No"))/3013)*100
df[2, 4] = (length(which(Parkinson$Head2=="No"))/3013)*100

rm("healthyRatio", "ParkinsonRatio", "ProdromalRatio", "healthyRatio1", "ParkinsonRatio1", "ProdromalRatio1")
rm("Healthy", "Parkinson", "Prodromal", "SWEDD")

#Change column names
names(df)[names(df) == "X1"] <- "Restless Leg Syndrome"
names(df)[names(df) == "X2"] <- "Healthy Control"
names(df)[names(df) == "X3"] <- "Prodromal"
names(df)[names(df) == "X4"] <- "Parkinson"

library(gt)

#Create table
df %>%
  gt() %>%
  tab_header(
    title = "Proportion Table",
    subtitle = "Proportions of Restless Leg Syndrome to Total Patients"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      weight = px(0.9)),
    locations = cells_body(
      columns = c(Parkinson)
    )
  )%>%
  tab_style(
    style = cell_borders(
      sides = c("all"),
      weight = px(0.9)),
    locations = cells_body(
      columns = c("Restless Leg Syndrome")
    ))


