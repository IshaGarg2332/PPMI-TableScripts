# --------------------------------------------------
# NOTE: This script requires private datasets that are not included due to confidentiality. 
# Replace the paths with your own files structured similarly to the expected data frames.
# --------------------------------------------------

#Import libraries
library("readxl")
library("tidyr")
library("dplyr")
library("tidyverse")
library("readxl")
library("table1")


#Shows the distribution of APOE, GBA, and LRRK2 genetic variants across Parkinson’s Disease, Prodromal, and Healthy Control groups.
#Helps identify whether these gene variants are more common in Parkinson’s or prodromal participants compared to healthy controls.


#Import datasets
ParticipantStatus <- read_csv("data/ParticipantStatus.csv")
PDDiagHistory <- read.csv("data/PDDiagHistory.csv")
Demographics <- read_csv("data/Demographics.csv")
Demo <- read_csv("data/Demographics.csv")
MedConditions <- read.csv("data/MedConditions.csv")
GeneticConsensus <- read.csv("data/GeneticConsensus.csv")

#Combine ParticipantStatus and DiagHistory
cohToTest <- ParticipantStatus
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all=T)

#Sort out sleep apnea and obstructive sleep apnea from MedConditions
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
MedConditions$MHTERM[MedConditions$MHTERM == "sleep disorder (ein- und durchschlafstörung)"] <- "Sleep disorder"
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
MedConditions$MHTERM[MedConditions$MHTERM == "Rapid eye movement behavior disorder (RBD)"] <- "RBD"
MedConditions$MHTERM[MedConditions$MHTERM == "Obstructive sleep apnea"] <- "Sleep apnea"

#Combine all medical condition terms per participant into a single string
df12 <- MedConditions %>%
  group_by(PATNO) %>%
  summarise(MHTERM=toString(MHTERM))

#Merge data
df12 <- merge(ParticipantStatus[,c("PATNO","COHORT_DEFINITION", "ENROLL_AGE")], df12, by="PATNO",all.y=T)

#Mutate GeneticConsensus and merge datasets
GeneticConsensus1 <- GeneticConsensus
GeneticConsensus1 <- GeneticConsensus1 %>%
  mutate(APOE = case_when(
    APOE == "E2/E2" ~ "E2/E2",
    APOE == "E2/E3" ~ "E2/E3",
    APOE == "E2/E4" ~ "E2/E4",
    APOE == "E3/E3" ~ "E3/E3",
    APOE == "E3/E4" ~ "E3/E4",
    APOE == "E4/E4" ~ "E4/E4",
    APOE == NA ~ NA,
    APOE == 0 ~ "None"
  ))
GeneticConsensus1 <- GeneticConsensus1 %>%
  mutate(GBA = case_when(
    GBA == NA ~ NA,
    GBA == 0 ~ "None",
    GBA != 0 ~ "GBA mutation"
  ))
GeneticConsensus1 <- GeneticConsensus1 %>%
  mutate(LRRK2 = case_when(
    LRRK2 == NA ~ NA,
    LRRK2 == 0 ~ "None",
    LRRK2 != 0 ~ "LRRK2 mutation"
  ))

#Merge data
df12 <- merge(df12, GeneticConsensus1[,c("PATNO","APOE", "GBA", "LRRK2")], by="PATNO",all.x=T)

#Any NA values means there is no gene mutation
df12[c("APOE")][is.na(df12[c("APOE")])] <- "None"
df12[c("LRRK2")][is.na(df12[c("LRRK2")])] <- "None"
df12[c("GBA")][is.na(df12[c("GBA")])] <- "None"

#SWEDD is classified as Healthy Controls
df12 <- df12 %>%
  mutate(ID = case_when(
    COHORT_DEFINITION == "Healthy Control" ~ "Healthy Control",
    COHORT_DEFINITION == "Parkinson's Disease" ~ "Parkinson's Disease",
    COHORT_DEFINITION == "Prodromal" ~ "Prodromal",
    COHORT_DEFINITION == "SWEDD" ~ "Healthy Control"
  ))

#Make a table of Parkinson's disease status against APOE, GBA, and LRRK2
table1(~ APOE + GBA + LRRK2 | ID, data=df12,
       title = "Demographics",
)
