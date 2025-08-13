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
library("DescTools")
library("arsenal")

#This is the weight and height of the minimum and maximum BMI values
#This also shows the weight and height of the extreme small and large BMI values


#Import datasets
ParticipantStatus <- read_csv("data/ParticipantStatus.csv")
PDDiagHistory <- read.csv("data/PDDiagHistory.csv")
Demographics <- read_csv("data/Demographics.csv")
Demo <- read_csv("data/Demographics.csv")
MedConditions <- read.csv("data/MedConditions.csv")
GeneticConsensus <- read.csv("data/GeneticConsensus.csv")
Vitals <- read.csv("data/VitalSigns.csv")
Vital <- Vitals

#Merge data
Ran <- merge(MedConditions1,ParticipantStatus[,c("PATNO","COHORT_DEFINITION")],by="PATNO",all.x=T)

#Combine ParticipantStatus and DiagHistory
cohToTest <- ParticipantStatus
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all=T)

#Modify sleeping disorders in medical conditions
MedConditions$MHTERM[MedConditions$MHTERM == "adult obstructive sleep apnea"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "central sleep apnea"] <- "Central sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Central Sleep Apnea"] <- "Central sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Central sleep apnea- uses APAP"] <- "Central sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Complex Sleep Apnea"] <- "Complex sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "D/x: Atrial Fibrillation - Started after d/x of obstructive sleep apnea"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "D/x: Obstructive Sleep Apnea"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Mild Sleep Apnea"] <- "Mild sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Moderate sleep apnea"] <- "Mild sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "obstructive sleep apnea"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Obstructive sleep Apnea"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Obstructive Sleep apnea"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Obstructive Sleep Apnea"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "OBSTRUCTIVE SLEEP APNEA"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Obstructive sleep apnea - uses CPAP machine"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "obstructive sleep apnea controlled with nightly CPAP use "] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Obstructive sleep apnea- utilizes CPAP"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Obstructive sleep apnea, now on CPAP"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Obstructive sleep apnea, resolved post surgery"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "obstructive sleep apnea, uses oral appliance to sleep"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "obstructive sleep apnea. on CPAP"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "OSA- Obstructive Sleep Apnea"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Severe obstructive sleep apnea, uses CPAP"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "sleep apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "sleep Apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep Apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep APnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "SLEEP APNEA"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea - Central"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea - Obstructive"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "sleep apnea (CPAP)"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea (stable and improving uses CPAP)"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea (uses CPAP machine)"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep Apnea & Insomnia"] <- "Sleep apnea & Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea and hypopnea syndrome"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea on Bipap"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea requiring cpap."] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea syndrome"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "sleep apnea treated with automatic positive airway pressure (APAP)"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea with CPAP"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea- CPAP"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea, currently well controlled"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea, obstructive"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "sleep apnea, on CPAP"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea, on CPAP since early 2023"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea, uses CPAP"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea, using a positional sleeping position trainer works really wel"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea; symptoms include snoring and difficulty with daytime wakefulness"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep disturbance and mild sleep apnea, contributes to daytime fatigue"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "UPPP surgery- Sleep Apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "UPPP Surgery- Sleep Apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Acting out dreams/ RBD, sleep study suggested REM related disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Anxiety, insomnia and Depression since 2016, requiring intermittent citalopram treatment"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "CHRONIC EXCESSIVE DROOLING DURING SLEEP"] <- NA
MedConditions$MHTERM[MedConditions$MHTERM == "Depression, anxiety, and anxiety-related insomnia"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "Difficulty staying asleep"] <- "Difficulty staying asleep"
MedConditions$MHTERM[MedConditions$MHTERM == "dream enactment behavior"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Dream reenactment"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Frequent Sleep Disturbances"] <- "Frequent Sleep Disturbances"
MedConditions$MHTERM[MedConditions$MHTERM == "He already has an overactive bladder for 10 years, after a while this interupted his sleep. Since a couple of months he also has lose of urine, which was the reason of starting the medication"] <- NA
MedConditions$MHTERM[MedConditions$MHTERM == "inconclusive sleep studies but clear symptoms of idiopathic REM sleep behavior disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "insomnia"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "Insomnia"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "INSOMNIA"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "Insomnia & RBD"] <- "Insomnia and REM"
MedConditions$MHTERM[MedConditions$MHTERM == "Insomnia secondary to anxiety"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "Insomnia since heart bypass surgery"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "insomnia, middle"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "Insomnia; Unknown start month and day"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "Insomnia/anxiety"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "Insomnia/Chronic fatigue disorder"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "Intermittent Insomnia"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "Mild insomnia"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "Muscular Cramps- Calfs (during sleep)"] <- NA
MedConditions$MHTERM[MedConditions$MHTERM == "Obstructive Sleep Apnoea"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Obstructive sleep apnoea"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "obstructive sleep disorder"] <- "Obstructive sleep disorder"
MedConditions$MHTERM[MedConditions$MHTERM == "Obstructuve sleep apnoea"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Occasional Insomnia"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "OSA / REM Sleep disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Parkinson's disease Symptoms began in 2018, pt noticed that my hand writing became difficult December 2019/January 2020, left hand occasionally shaking, hand writing is difficult. In 2021, left hand lost some flexibility and lost some strength.  Left hand shaking increase frequency.  However, pt reports that she can do everything by herself. In November 2021,  pt reported noticing right hand had lost some strength.   Sleeping pattern has changed in last two years. Pt feels it is difficult to fall asleep during night.  Now pt takes melatonin 5 mg per day at 10:30 pm.  Sleep improved to some degree but some days pt still cannot fall in sleep"] <- "Sleep disorder"
MedConditions$MHTERM[MedConditions$MHTERM == "Parkinson's disease; pt is not aware of any PD symptoms, but reports she may have non-motor symptoms like sleep issues;  Dx based on DaTscan; Awaiting pt to send physician report regarding diagnosis to determine if pt truly has PD"] <- "Sleep issues"
MedConditions$MHTERM[MedConditions$MHTERM == "Participant endorsed REM behavior disorder (RBD) symptoms, specifically the nightmares, began at least 20 years ago, but unsure of when it officially started. Polysomnogram confirmed the RBD diagnosis on October 19, 2020."] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "possible REM sleep behavior disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Possible REM sleep behavior disorder (not confirmed by sleep study)"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Primary insomnia"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "Rapid eye movement (REM) sleep behavior disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Rapid eye movement (REM) sleep behavior disorder (RBD)"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Rapid eye movement sleep behavior disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Rapid Eye Movement Sleep Behavior Disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Recurrent insomnia"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "REM"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM behavior"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Rem behavior disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM behavior disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Behavior disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Behavior Disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Behavior Disorder (RBD)"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Rem Behavior Disorders"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Behavior Sleep Disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM behavioral disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Behavioral disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Behavioral Disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM behavioral sleep disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Behaviour Sleep Disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "rem sleep behavior"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM sleep Behavior"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Sleep Behavior"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM SLEEP BEHAVIOR"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Rem sleep behavior disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Rem Sleep Behavior Disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM sleep behavior disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM sleep Behavior Disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Sleep behavior disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Sleep Behavior Disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM SLEEP BEHAVIOR DISORDER"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM sleep behavior disorder (RBD)"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM sleep Behavior Disorder (RBD)"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Sleep Behavior Study"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM sleep behavioral disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Sleep Behavioral Disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM sleep behavioral disorder (onset as early as 15 years ago, confirmed by a PSG on 8/26/2015), under control with clonazepam"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Sleep behaviors disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM sleep behaviour disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM sleep behaviour Disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM sleep Behaviour disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Sleep Behavioural Disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "rem sleep disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Rem sleep disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Rem Sleep Disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM sleep disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Sleep Disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM SLEEP DISORDER"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Sleep Disorder, Periodic Limb movement disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Rem-Behavior Sleep Disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM-behavior sleep disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM-Behavior Sleep Disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM-behavioral sleep disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM-sleep behavior disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM-Sleep Behavior Disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM-sleep behaviour disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM-sleep Behaviour Disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM-Sleep Behaviour Disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM-sleep disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "REM-sleep-behaviour disorder"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Secondary insomnia"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "sleep apnoea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnoea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep Apnoea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "sleep apnoea (CPAP)"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep Arousal Disorder"] <- "Sleep arousal disorder"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep Deficit"] <- "Sleep deficit"
MedConditions$MHTERM[MedConditions$MHTERM == "sleep difficulty"] <- "Sleep difficulty"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep Difficulty"] <- "Sleep difficulty"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep disorder"] <- "Sleep disorder"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep Disorder"] <- "Sleep disorder"
MedConditions$MHTERM[MedConditions$MHTERM == "SLEEP DISORDER"] <- "Sleep disorder"
MedConditions$MHTERM[MedConditions$MHTERM == "sleep disorder(ein- und durchschlafstörung)"] <- "Sleep disorder"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep disturbance"] <- "Sleep disturbance"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep Disturbance"] <- "Sleep disturbance"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep Disturbance/Insomnia"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "sleep disturbances"] <- "Sleep disturbance"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep disturbances"] <- "Sleep disturbance"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep Disturbances"] <- "Sleep disturbance"
MedConditions$MHTERM[MedConditions$MHTERM == "sleep hygiene"] <- "Sleep hygiene"
MedConditions$MHTERM[MedConditions$MHTERM == "sleep-related breathing disorder"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "sleeping problems due to Parkinsons Disease"] <- "Sleeping problems"
MedConditions$MHTERM[MedConditions$MHTERM == "suspected parasomnia overlap (RBD/somnambulism)"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Trouble falling asleep"] <- "Trouble sleeping"
MedConditions$MHTERM[MedConditions$MHTERM == "trouble sleeping"] <- "Trouble sleeping"
MedConditions$MHTERM[MedConditions$MHTERM == "Trouble sleeping"] <- "Trouble sleeping"
MedConditions$MHTERM[MedConditions$MHTERM == "Very little REM sleep, twitching, talking and lots of moving while sleeping"] <- "REM Sleep"
MedConditions$MHTERM[MedConditions$MHTERM == "Mild sleep apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Obstructive sleep apnea"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Episodes of sleep apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Central sleep apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Adult obstructive sleep apnea"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "obstructive sleep apnea syndrome"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Moderate obstructive sleep apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "obstructive sleep apnea controlled with nightly CPAP use"] <- "Obstructive sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Central sleep apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Central Sleep Apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "central sleep apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Difficulty staying asleep"] <- "Sleeping problems"
MedConditions$MHTERM[MedConditions$MHTERM == "Frequent Sleep Disturbances"] <- "Sleeping problems"
MedConditions$MHTERM[MedConditions$MHTERM == "Insomnia"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "Insomnia and REM"] <- "Insomnia"
MedConditions$MHTERM[MedConditions$MHTERM == "Mild sleep apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Obstructive sleep apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Obstructive Sleep Apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Obstructive sleep disorder"] <- "Sleeping problems"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Sleep"] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea & Insomnia"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep arousal disorder"] <- "Sleeping problems"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep deficit"] <- "Sleeping problems"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep difficulty"] <- "Sleeping problems"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep disorder"] <- "Sleeping problems"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep disturbance"] <- "Sleeping problems"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep hygiene"] <- "Sleeping problems"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep issues"] <- "Sleeping problems"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleeping problems"] <- "Sleeping problems"
MedConditions$MHTERM[MedConditions$MHTERM == "Trouble sleeping"] <- "Sleeping problems"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep Apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Obstructive Sleep Apnea "] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Obstructive sleep apnea "] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Central sleep apnea "] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Complex sleep apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "D/x: Obstructive Sleep Apnea  "] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "D/x: Atrial Fibrillation - Started after d/x of obstructive sleep apnea."] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea (stable and improving, uses CPAP)"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Obstructive Sleep apnea "] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep apnea; symptoms include snoring and difficulty with daytime wakefulness "] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Sleep Apnea "] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == " Central sleep apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "sleep apnea "] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "sleep apnea"] <- "Sleep apnea"
MedConditions$MHTERM[MedConditions$MHTERM == "Rapid eye movement behavior disorder (RBD)"] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "RBD (not PSG+)"] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "Rapid eye movement behavior disorder (RBD)"] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "Deviated Septum, broken in 2019 during RBD episode."] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "Rapid eye movement behavior disorder (RBD) "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "REM sleep behavior disorder (RBD) "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "rem sleep behavior "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Sleep Behavior Disorder "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "REM behavior sleep disorder"] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "REM sleep behavior disorder "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "Rem Sleep Behavior Disorder "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Behavior Disorder "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Sleep Behavioral Disorder without atonia"] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "REM sleep behaviour disorder "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Sleep behavior disorder "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Sleep behaviors disorder "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Sleep Behavioral Disorder "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "REM sleep behavioral disorder (onset as early as 15 years ago, confirmed by a PSG on 8/26/2015), under control with clonazepam "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "REM sleep Behavior "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "Rem Sleep Disorder "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "Rem sleep disorder "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "Rem sleep disorder"] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "REM Sleep Disorder "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "REM "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "REM disorder"] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "REM sleep disorder "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "Rem Behavior Disorder"] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "REM behavior "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "REM sleep disorder "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "Rapid eye movement sleep behavior disorder "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "Rapid eye movement sleep behavior disorder "] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == " Rapid eye movement behavior disorder (RBD)"] <- "RBD"

#Modify sleeping and RBD disorders
df12 <- MedConditions %>%
  group_by(PATNO) %>%
  summarise(MHTERM=toString(MHTERM))

df12$apnea <- ifelse(grepl("Sleep apnea", df12$MHTERM), "yes", "no")
df12$RBD <- ifelse(grepl("RBD", df12$MHTERM), "yes", "no")

#Merge data
dff <- merge(cohToTest[,c("PATNO","COHORT_DEFINITION", "ENROLL_AGE")],df12,by="PATNO",all.y=T)
dff <- merge(Demographics[,c("PATNO","SEX")],dff,by="PATNO",all.y=T)

#Mutate columns into binary
dff <- dff %>%
  mutate(apnea=case_when(
    apnea == "yes" ~ 1,
    apnea == "no" ~ 0,
  ))
dff <- dff %>%
  mutate(RBD=case_when(
    RBD == "yes" ~ 1,
    RBD == "no" ~ 0,
  ))

#Mutate columns and make SWEDD part of healthy controls
dff <- dff %>%
  mutate(Parkinsons1=case_when(
    COHORT_DEFINITION == "Parkinson's Disease" ~ "Parkinson's Disease",
    COHORT_DEFINITION == "Prodromal" ~ "Prodromal",
    COHORT_DEFINITION == "Healthy Control" ~ "Healthy Control",
    COHORT_DEFINITION == "SWEDD" ~ "Healthy Control"
  ))
dff$Parkinsons1 <- as.factor(dff$Parkinsons1)

#Change order of Parkinson's status
dff <- dff %>%
  mutate(Parkinsons1 = factor(Parkinsons1, levels=c("Healthy Control", "Parkinson's Disease", "Prodromal")))

#Merge data
dff <- merge(dff, GeneticConsensus[,c("PATNO","APOE", "GBA", "LRRK2")], by="PATNO",all.x=T)

#Mutate column values
dff <- dff %>%
  mutate(APOE = case_when(
    APOE == "E2/E2" ~ "E2/E2",
    APOE == "E2/E3" ~ "E2/E3",
    APOE == "E2/E4" ~ "E2/E4",
    APOE == "E3/E3" ~ "E3/E3",
    APOE == "E3/E4" ~ "E3/E4",
    APOE == "E4/E4" ~ "E4/E4",
    APOE == 0 ~ "None"
  ))
dff <- dff %>%
  mutate(GBA = case_when(
    GBA == 0 ~ 0,
    GBA != 0 ~ 1
  ))
dff <- dff %>%
  mutate(LRRK2 = case_when(
    LRRK2 == 0 ~ 0,
    LRRK2 != 0 ~ 1
  ))

#NA rows mean that the person does not have a genetic mutation
dff[c("APOE")][is.na(dff[c("APOE")])] <- 0
dff[c("LRRK2")][is.na(dff[c("LRRK2")])] <- 0
dff[c("GBA")][is.na(dff[c("GBA")])] <- 0

#Merge data
data <- merge(dff, PDDiagHistory[,c("PATNO","PDDXDT")], by="PATNO",all.x=T)
data <- merge(data, Vital[,c("PATNO","WGTKG", "HTCM", "INFODT")], by="PATNO",all.x=T)

#Mutate date columns
data$DiagDate <- as.Date(paste0("01/",data$PDDXDT),format="%d/%m/%Y")
data$BMIDate <- as.Date(paste0("01/",data$INFODT),format="%d/%m/%Y")
data$diff <- difftime(data$DiagDate, data$BMIDate, units = "days")

#Mutate height, weight, and BMI columns
examp <- data |>
  group_by(PATNO) |>
  mutate(
    min = ifelse(all(is.na(diff)), NA, diff[which.min(abs(diff))])
  )
examp <- examp %>%
  mutate(min2 = case_when(
    min == diff ~ 1,
    min != diff ~ 0
  ))
examp1 <- examp[examp$min2 %in% c(1), ]
examp1 = examp1[!duplicated(examp1$PATNO),]
examp1 <- examp1[!is.na(examp1$WGTKG),]
examp2 <- examp[examp$min2 %in% c(NA), ]
examp2 <- examp2[!is.na(examp2$WGTKG),]
examp2 = examp2[!duplicated(examp2$PATNO),]
examp3 <- merge(examp1, examp2, by=c("PATNO", "PATNO", "SEX", "COHORT_DEFINITION", "ENROLL_AGE", "MHTERM", "apnea", "RBD", "Parkinsons1", "APOE", "GBA", "LRRK2", "PDDXDT", "WGTKG", "HTCM", "INFODT", "DiagDate", "BMIDate", "diff", "min", "min2"),all=T)
summary(comparedf(examp3, dff, by = "PATNO"))
examp4 <- examp3[!is.na(examp3$WGTKG),]
examp4$height = ((examp4$HTCM)/100) * ((examp4$HTCM)/100)
examp4$BMI = examp4$WGTKG/examp4$height

#Mutate SWEDD to healthy controls
examp4 <- examp4 %>%
  mutate(COHORT_DEFINITION = case_when(
    COHORT_DEFINITION == "Healthy Control" ~ "Healthy Control",
    COHORT_DEFINITION == "Parkinson's Disease" ~ "Parkinson's Disease",
    COHORT_DEFINITION == "Prodromal" ~ "Prodromal",
    COHORT_DEFINITION == "SWEDD" ~ "Healthy Control"
  ))

#Select certain patient numbers
examp10 <- examp4
examp4 <- examp4[!(examp4$PATNO %in% "238253"),]
examp4 <- examp4[!(examp4$PATNO %in% "126138"),]
examp4 <- examp4[!(examp4$PATNO %in% "169320"),]
examp4 <- examp4[!(examp4$PATNO %in% "42446"),]
examp4 <- examp4[!(examp4$PATNO %in% "292776"),]
examp4 <- examp4[!(examp4$PATNO %in% "266897"),]

examp4 <- examp4[!(examp4$PATNO %in% "264326"),]
examp4 <- examp4[!(examp4$PATNO %in% "226242"),]
examp4 <- examp4[!(examp4$PATNO %in% "220373"),]
examp4 <- examp4[!(examp4$PATNO %in% "266897"),]
examp4 <- examp4[!(examp4$PATNO %in% "241159"),]
examp4 <- examp4[!(examp4$PATNO %in% "74307"),]
examp4 <- examp4[!(examp4$PATNO %in% "156194"),]
examp4 <- examp4[!(examp4$PATNO %in% "222290"),]
examp4 <- examp4[!(examp4$PATNO %in% "102336"),]
examp4 <- examp4[!(examp4$PATNO %in% "189644"),]
examp4 <- examp4[!(examp4$PATNO %in% "162898"),]
examp4 <- examp4[!(examp4$PATNO %in% "271112"),]

examp4 <- examp4[!(examp4$PATNO %in% "50670"),]
examp4 <- examp4[!(examp4$PATNO %in% "232928"),]
examp4 <- examp4[!(examp4$PATNO %in% "150110"),]
examp4 <- examp4[!(examp4$PATNO %in% "177158"),]
examp4 <- examp4[!(examp4$PATNO %in% "200164"),]
examp4 <- examp4[!(examp4$PATNO %in% "179803"),]
examp4 <- examp4[!(examp4$PATNO %in% "248922"),]
examp4 <- examp4[!(examp4$PATNO %in% "108754"),]
examp4 <- examp4[!(examp4$PATNO %in% "220370"),]
examp4 <- examp4[!(examp4$PATNO %in% "220079"),]
examp4 <- examp4[!(examp4$PATNO %in% "55380"),]
examp4 <- examp4[!(examp4$PATNO %in% "116259"),]

examp4 <- examp4[order(examp4$BMI),]
examp4 <- examp4[!is.na(examp4$BMI),]
examp <- examp[order(examp$WGTKG),]

table1(~ BMI | COHORT_DEFINITION, data=examp4)

#Select the minimum and maximum weight, height, and BMI
MinWeight <- min(examp10$WGTKG, na.rm = TRUE)
MaxWeight <- max(examp10$WGTKG, na.rm = TRUE)
MinHeight <- min(examp10$HTCM, na.rm = TRUE)
MaxHeight <- max(examp10$HTCM, na.rm = TRUE)
MinBMI <- min(examp10$BMI, na.rm = TRUE)
MaxBMI <- max(examp10$BMI, na.rm = TRUE)
df <- data.frame(matrix(ncol = 4, nrow = 2))
df[1, 1] = "Minimum"
df[2, 1] = "Maximum"
df[1, 2] = MinWeight
df[2, 2] = MaxWeight
df[1, 3] = MinHeight
df[2, 3] = MaxHeight
df[1, 4] = MinBMI
df[2, 4] = MaxBMI
names(df)[names(df) == "X1"] <- "Variables"
names(df)[names(df) == "X2"] <- "Weight"
names(df)[names(df) == "X3"] <- "Height"
names(df)[names(df) == "X4"] <- "BMI"
library(gt)

#Make a table lof minimum and maximum
df %>%
  gt() %>%
  tab_header(
    title = "Minimum and Maximu"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      weight = px(1.1)),
    locations = cells_body(
      columns = c(BMI)
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("left"),
      weight = px(1.1)),
    locations = cells_body(
      columns = c(Variables)
    )
  )

#Merge data
data <- merge(dff, PDDiagHistory[,c("PATNO","PDDXDT")], by="PATNO",all.x=T)
data <- merge(data, Vital[,c("PATNO","WGTKG", "HTCM", "INFODT")], by="PATNO",all.x=T)

#Mutate dates
data$DiagDate <- as.Date(paste0("01/",data$PDDXDT),format="%d/%m/%Y")
data$BMIDate <- as.Date(paste0("01/",data$INFODT),format="%d/%m/%Y")
data$diff <- difftime(data$DiagDate, data$BMIDate, units = "days")
examp <- data |>
  group_by(PATNO) |>
  mutate(
    min = ifelse(all(is.na(diff)), NA, diff[which.min(abs(diff))])
  )
examp <- examp %>%
  mutate(min2 = case_when(
    min == diff ~ 1,
    min != diff ~ 0
  ))

#Mutate weight, height, and BMI columns
examp1 <- examp[examp$min2 %in% c(1), ]
examp1 = examp1[!duplicated(examp1$PATNO),]
examp1 <- examp1[!is.na(examp1$WGTKG),]
examp2 <- examp[examp$min2 %in% c(NA), ]
examp2 <- examp2[!is.na(examp2$WGTKG),]
examp2 = examp2[!duplicated(examp2$PATNO),]
examp3 <- merge(examp1, examp2, by=c("PATNO", "PATNO", "SEX", "COHORT_DEFINITION", "ENROLL_AGE", "MHTERM", "apnea", "RBD", "Parkinsons1", "APOE", "GBA", "LRRK2", "PDDXDT", "WGTKG", "HTCM", "INFODT", "DiagDate", "BMIDate", "diff", "min", "min2"),all=T)
summary(comparedf(examp3, dff, by = "PATNO"))
examp4 <- examp3[!is.na(examp3$WGTKG),]

examp4$height = ((examp4$HTCM)/100) * ((examp4$HTCM)/100)
examp4$BMI = examp4$WGTKG/examp4$height

examp4 <- examp4[!is.na(examp4$COHORT_DEFINITION),]
examp4 <- examp4[!(examp4$PATNO %in% "238253"),]
examp4 <- examp4[!(examp4$PATNO %in% "126138"),]
examp4 <- examp4[!(examp4$PATNO %in% "169320"),]

table1(~ BMI | COHORT_DEFINITION, data=examp4)

#Select BMIs that seem extreme
examp5 <- examp10[(examp10$PATNO %in% c("238253", "126138", "126138", "169320", "42446", "292776",
                                        "266897", "264326", "226242", "220373", "266897", "241159",
                                        "74307", "156194", "222290", "102336", "189644", "162898",
                                        "271112", "50670", "232928", "150110", "177158", "200164",
                                        "179803", "248922", "108754", "220370", "220079", "55380",
                                        "116259")),]
examp5 <- select(examp5, c("WGTKG", "HTCM", "BMI"))
examp5$htft <- (examp5$HTCM) / 30.48
examp5$WGTLB <- (examp5$WGTKG) * 2.205
colnames(examp5)[which(names(examp5) == "WGTKG")] <- "Weight(kg)"
colnames(examp5)[which(names(examp5) == "WGTLB")] <- "Height(lbs)"
colnames(examp5)[which(names(examp5) == "HTCM")] <- "Height(cm)"
colnames(examp5)[which(names(examp5) == "htft")] <- "Height(ft)"

#Graph extreme BMIs
examp5 %>%
  gt() %>%
  tab_header(
    title = "Weight and Height of Small and Large BMI Values"
  )
