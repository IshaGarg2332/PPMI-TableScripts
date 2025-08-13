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
library("lubridate")

# Answers the question "When were you enrolled into this study?"
# Gives the ages for rows: "<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", ">=75" and "missing"
# Columns are healthy control, Parkinson's disease, and prodromal and the amount of people per age group in these three categories


#Import Data
HeadInjury <- read_csv("data/HeadInjury.csv")
HeadInjury[HeadInjury == '[not completed]'] <- NA
HeadInjury <- HeadInjury[!is.na(HeadInjury$head_injury_timestamp),]
Participant <- read_csv("data/ParticipantStatus.csv")
hloss <- read_csv("data/MedConditions.csv")

#Modify Hearing Loss
hloss <- hloss[grepl("(hear\\b|hearing)",hloss$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]
total <- merge(hloss,Participant,by="PATNO")
rm("hloss", "Participant")

# Select relevant columns
HeadInjury1 <- select(HeadInjury, c(patno, hiq1, hiq2, hiqa1_age, hiqb1_age, hiqc3_age, hiqc4_age, hiqc5_age))

# Rename columns for clarity
names(HeadInjury1)[names(HeadInjury1) == 'patno'] <- 'Patient'
names(HeadInjury1)[names(HeadInjury1) == 'hiq1'] <- 'EverHad'
names(HeadInjury1)[names(HeadInjury1) == 'hiq2'] <- 'HowMany'
names(HeadInjury1)[names(HeadInjury1) == 'hiqa1_age'] <- 'age1'
names(HeadInjury1)[names(HeadInjury1) == 'hiqb1_age'] <- 'age2'
names(HeadInjury1)[names(HeadInjury1) == 'hiqc3_age'] <- 'age3'
names(HeadInjury1)[names(HeadInjury1) == 'hiqc4_age'] <- 'age4'
names(HeadInjury1)[names(HeadInjury1) == 'hiqc5_age'] <- 'age5'

# Remove rows where the first age of injury is missing
data2 <- HeadInjury1[!is.na(HeadInjury1$age1),]

# Create flags indicating the presence of each age entry
HeadInjury3 <- HeadInjury1 %>%
  mutate(age11=case_when(
    age1 != "" ~ "1"
  )) %>%
  mutate(age12=case_when(
    age2 != "" ~ "2"
  )) %>%
  mutate(age13=case_when(
    age3 != "" ~ "3"
  )) %>%
  mutate(age14=case_when(
    age4 != "" ~ "4"
  )) %>%
  mutate(age15=case_when(
    age5 != "" ~ "5"
  ))

#Combine the columns
HeadInjury4 <- HeadInjury3 %>%
  pivot_longer(
    cols = c(age11, age12, age13, age14, age15),
    names_to = "Age",
    values_to = "Count"
  )

# Assign "Yes", "No", or "Unsure" for each age entry
HeadInjury5 <- HeadInjury4 %>%
  mutate(ascat1=case_when(
    Count == 1 & age1<18 ~ "<18",
    Count == 1 & age1>=18 & age1<=24 ~ "18-24",
    Count == 1 & age1>=25 & age1<=34 ~ "25-34",
    Count == 1 & age1>=35 & age1<=44 ~ "35-44",
    Count == 1 & age1>=45 & age1<=54 ~ "45-54",
    Count == 1 & age1>=55 ~ ">=55")) %>%
  mutate(ascat2=case_when(
    Count == 2 & age2<18 & age2>=0 ~ "<18",
    Count == 2 & age2>=18 & age2<=24 ~ "18-24",
    Count == 2 & age2>=25 & age2<=34 ~ "25-34",
    Count == 2 & age2>=35 & age2<=44 ~ "35-44",
    Count == 2 & age2>=45 & age2<=54 ~ "45-54",
    Count == 2 & age2>=55 ~ ">=55")) %>%
  mutate(ascat3=case_when(
    Count == 3 & age3<18 & age3>=0 ~ "<18",
    Count == 3 & age3>=18 & age3<=24 ~ "18-24",
    Count == 3 & age3>=25 & age3<=34 ~ "25-34",
    Count == 3 & age3>=35 & age3<=44 ~ "35-44",
    Count == 3 & age3>=45 & age3<=54 ~ "45-54",
    Count == 3 & age3>=55 ~ ">=55")) %>%
  mutate(ascat4=case_when(
    Count == 4 & age4<18 & age4>=0 ~ "<18",
    Count == 4 & age4>=18 & age4<=24 ~ "18-24",
    Count == 4 & age4>=25 & age4<=34 ~ "25-34",
    Count == 4 & age4>=35 & age4<=44 ~ "35-44",
    Count == 4 & age4>=45 & age4<=54 ~ "45-54",
    Count == 4 & age4>=55 ~ ">=55")) %>%
  mutate(ascat5=case_when(
    Count == 5 & age5<18 & age5>0 ~ "<18",
    Count == 5 & age5>=18 & age5<=24 ~ "18-24",
    Count == 5 & age5>=25 & age5<=34 ~ "25-34",
    Count == 5 & age5>=35 & age5<=44 ~ "35-44",
    Count == 5 & age5>=45 & age5<=54 ~ "45-54",
    Count == 5 & age5>=55 ~ ">=55"
  ))

#Combine the columns
HeadInjury6 <- HeadInjury5 %>%
  pivot_longer(
    cols = c(ascat1, ascat2, ascat3, ascat4, ascat5),
    names_to = "Agess",
    values_to = "Countss"
  )

#Remove NA values
HeadInjury6 <- HeadInjury6[!is.na(HeadInjury6$Countss),]

#Remove temporary datasets
rm("HeadInjury5")

#Add consecutive numbers
naframe <- as.data.frame(matrix(NA, nrow = 271, ncol = 54))
names(naframe) <- names(total)
total <- rbind(total, naframe)
HeadInjury6$consecutive<-1:nrow(HeadInjury6)
total$consecutive<-1:nrow(total)

#Select what we need
total <- select(total, c(PATNO, MHDIAGYR, MHTERM, COHORT_DEFINITION, ENROLL_AGE))
HeadInjury6 <- select(HeadInjury6, c(Patient, EverHad, Countss))

#Add necessary columns
place1 <- c("PATNO", "MHDIAGYR", "MHTERM", "COHORT_DEFINITION", "ENROLL_AGE")
place2 <- c("Patient", "EverHad", "Countss")
HeadInjury6[ , place1] <- NA
total[ , place2] <- NA

#Merge them
total1 <- rbind(HeadInjury6, total)
total1$dates <- paste(total1$MHDIAGYR,total1$data)
total2 <- total1 %>%
  mutate_at("dates", str_replace, "NA ", "") %>%
  mutate_at("dates", str_replace, " NA", "")

#Clean up
rm("total1")



#Fix hearing loss
total1 <- total2 %>%
  mutate(MHTERM = recode(MHTERM, "bilateral hearing loss" = 'Bilateral hearing loss',
                         "hearing impaired" = 'Impaired hearing',
                         "hearing loss" = 'Hearing loss',
                         "hearing loss- subject wears a hearing aid" = 'Hearing loss with hearing aid',
                         "neurosensory hearing loss" = 'Neurosensory hearing loss',
                         "hearing loss - hearing aid use" = 'Hearing loss with hearing aid',
                         "hearing loss of both ears" = 'Hearing loss',
                         "acute hear loss with tinnitus" = 'Hearing loss with tinnitus',
                         "acute hearing loss" = 'Acute hearing loss	',
                         "Bilateral hearing aids" = 'Bilateral hearing loss',
                         "bilateral hearing deficit" = 'Bilateral hearing loss',
                         "decreased hearing bilaterally" = 'Bilateral hearing loss',
                         "hearing impairment" = 'Hearing loss',
                         "hearing loss and tinnitus both sides" = 'Hearing loss with tinnitus',
                         "high end loss of hearing" = 'High end loss of hearing',
                         "mild bilateral hearing loss" = 'Mild hearing loss',
                         "sensorineural hearing loss" = 'Sensorineural hearing loss',
                         "bilateral hardness of hearing" = 'Bilateral hearing loss',
                         "bilateral sensorineural hearing loss" = 'Bilateral sensorineural hearing loss',
                         "d/x: hearing impairment - hearing aids given about 3 years ago; occasionally wears it." = 'Hearing loss with hearing aid',
                         "hearing aids	" = 'Hearing loss with hearing aid',
                         "hearing loss bilateral" = 'Bilateral hearing loss',
                         "hearing loss, bilateral" = 'Bilateral hearing loss',
                         "hearing loss, mild" = 'Mild hearing loss',
                         "hearing loss, progressive" = 'Hearing loss',
                         "loss of hearing" = 'Hearing loss',
                         "mild hearing loss" = 'Mild hearing loss',
                         "mild loss of hearing (high frequencies)" = 'Mild hearing loss',
                         "partial hearing loss" = 'Mild hearing loss',
                         "sensorineural hearing loss, bilateral" = 'Bilateral sensorineural hearing loss',
                         "tinitis and hearing loss" = 'Hearing loss with tinnitus',
                         "bilateral hearing loss - hearing aids	" = 'Hearing loss with hearing aid',
                         "deterioration in hearing requiring hearing aids." = 'Hearing loss with hearing aid',
                         "hearing loss in both ears" = 'Bilateral hearing loss',
                         "hearing loss, b/l" = 'Bilateral hearing loss',
                         "hearing loss, normal, bilateral" = 'Bilateral hearing loss',
                         "sensorineural hearing loss + vertigo" = 'Sensorineural hearing loss',
                         "slight hearing loss" = 'Hearing loss',
                         "bilateral hearing loss - hearing aids" = 'Bilateral hearing loss',
                         "bilateral hearing aids" = 'Hearing loss with hearing aid',
                         "hearing aids" = 'Hearing loss with hearing aid',
                         "hearing loss with calcification of the stapedius, s/p bilateral stapedectomy in 1979	" = 'Otosclerosis',
                         "Hearing loss- subject wears a hearing aid	" = 'Hearing loss with hearing aid',
                         "Hearing Loss - Hearing Aid Use" = 'Hearing loss with hearing aid',
                         "hearing loss right ear" = 'Hearing loss in right ear',
                         "Bilateral Hearing Deficit" = 'Bilateral hearing loss',
                         "Decreased Hearing Bilaterally" = 'Bilateral hearing loss',
                         "Hearing Loss" = 'Hearing loss',
                         "HEARING LOSS" = 'Hearing loss',
                         "Hearing loss and tinnitus both sides" = 'Hearing loss with tinnitus',
                         "Hearing Loss left ear" = 'Hearing loss in left ear',
                         "Hearing loss on right side" = 'Hearing loss in right ear',
                         "left ear hearing loss" = 'Hearing loss in left ear',
                         "Left Ear- decreased hearing" = 'Hearing loss in left ear',
                         "MILD BILATERAL HEARING LOSS" = 'Mild hearing loss',
                         "Mild hearing loss in left ear" = 'Hearing loss in left ear',
                         "reduced hearing left ear" = 'Hearing loss in left ear',
                         "Right side hearing loss" = 'Hearing loss in right ear',
                         "Acoustic neuroma resulting in R hearing loss." = 'Acoustic neuroma',
                         "Bilateral hardness of hearing" = 'Bilateral hearing loss',
                         "BILATERAL HEARING LOSS" = 'Bilateral hearing loss',
                         "D/x: Hearing impairment - Hearing aids given about 3 years ago; occasionally wears it.	" = 'Hearing loss with hearing aid',
                         "decreased hearing Right ear" = 'Hearing loss in right ear',
                         "Hearing loss bilateral" = 'Bilateral hearing loss',
                         "Hearing loss Bilateral" = 'Bilateral hearing loss',
                         "Hearing loss on R side" = 'Hearing loss in right ear',
                         "Left hear mild hearing loss" = 'Hearing loss in left ear',
                         "LOSS OF HEARING" = 'Hearing loss',
                         "Mild Bilateral Hearing Loss" = 'Mild hearing loss',
                         "Mild hearing loss" = 'Mild hearing loss',
                         "neurosensory hearing loss - left ear" = 'Hearing loss in left ear',
                         "Partial hearing loss" = 'Partial hearing loss',
                         "Asymmetric hearing loss" = 'Partial hearing loss',
                         "right ear hearing loss" = 'Hearing loss in right ear',
                         "Right Ear Hearing Loss	" = 'Hearing loss in right ear',
                         "Right sided sensorineural hearing loss (due to neuroma)" = 'Hearing loss in right ear',
                         "Sensorineural hearing loss, bilateral" = 'Bilateral sensorineural hearing loss',
                         "Tinitis and hearing loss" = 'Hearing loss with tinnitus',
                         "Asymmetrical hearing loss" = 'Partial hearing loss',
                         "Bilateral hearing loss - Hearing aids" = 'Hearing loss with hearing aid',
                         "Deterioration in hearing requiring hearing aids." = 'Hearing loss with hearing aid',
                         "Hearing Loss Bilateral" = 'Bilateral hearing loss',
                         "Hearing loss in the right ear" = 'Hearing loss in right ear',
                         "Hearing Loss Right Ear" = 'Hearing loss in right ear',
                         "Implant bone-conducting hearing device" = 'Hearing loss with implant',
                         "Left-sided Hearing Loss" = 'Hearing loss in left ear',
                         "Loss of hearing in left ear" = 'Hearing loss in left ear',
                         "Mixed hearing loss with Eustachian tube dysfunction- right ear" = 'Hearing loss in left ear',
                         "Pt had a routine hearing test on October 20, 2021, results found that the pt's left ear has slightly reduced ability at higher frequencies; pt's doctor asked pt to get an MRI to evaluate why the left ear is has reduced functioning compared to the right ear; MRI scheduled for November 10, 2021" = 'Hearing loss in left ear',
                         "Sensorineural hearing loss + vertigo" = 'Sensorineural hearing loss',
                         "Slight hearing loss" = 'Mild hearing loss',
                         "hearing loss with calcification of the stapedius, s/p bilateral stapedectomy in 1979" = 'Otosclerosis',
                         "D/x: Hearing impairment  - Hearing aids given about 3 years ago; occasionally wears it." = 'Hearing loss with hearing aid',
                         "Bilateral Sensorineural Hearing Loss" = 'Bilateral sensorineural hearing loss',
                         "Hearing loss- subject wears a hearing aid" = 'Hearing loss in right ear',
                         "Right Ear Hearing Loss" = 'Hearing loss in right ear',
                         "Hearing impaired" = 'Impaired hearing',
                         "Hearing loss left ear" = 'Hearing loss in left ear',
                         "Sudden hearing loss L ear" = 'Hearing loss in left ear',
                         
  ))

# Categorize age into groups
total1 <- total1 %>%
  mutate(ascat1=case_when(
    ENROLL_AGE<18 ~ "<18",
    ENROLL_AGE>18 & ENROLL_AGE<=24 ~ "18-24",
    ENROLL_AGE>24 & ENROLL_AGE<=34 ~ "25-34",
    ENROLL_AGE>34 & ENROLL_AGE<=44 ~ "35-44",
    ENROLL_AGE>44 & ENROLL_AGE<=54 ~ "45-54",
    ENROLL_AGE>54 & ENROLL_AGE<=64~ "55-64",
    ENROLL_AGE>64 & ENROLL_AGE<=74~ "65-74",
    ENROLL_AGE>74~ ">=75"
  ))

# Label and order the age groups
total1 <- labelled::set_variable_labels(total1, ascat1="When were you enrolled into this study?")
total1$ascat1 = factor(total1$ascat1, levels = c("<18",
                                                 "18-24",
                                                 "25-34",
                                                 "35-44",
                                                 "45-54",
                                                 "55-64",
                                                 "65-74",
                                                 ">=75"), ordered = TRUE)
label(total1$ascat1) <- 'When were you enrolled into this study?'

total1 <- total1[!is.na(total1$COHORT_DEFINITION),]

# Create a summary table by Parkinson's status
table1(~ ascat1
       | COHORT_DEFINITION, data=total1)
