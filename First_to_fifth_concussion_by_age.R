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

# Answers the question "How old were you when you got your concussion?"
# Gives the ages for rows: "<18", "18-24", "25-34", "35-44", "45-54", >=55", and "missing"
# Gives the amount of people per age with first concussion, second, third, fourth, and fifth

####Age####
# Shows the age people were when they got a concussion
# Covers first through fifth concussion using age ranges

# Import head injury data
HeadInjury <- read_csv("data/HeadInjury.csv")

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

# Categorize each age for each variable based on the corresponding 'Count'
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

rm("HeadInjury", "HeadInjury1", "HeadInjury3", "HeadInjury4")
HeadInjury6 <- HeadInjury5 %>%
  pivot_longer(
    cols = c(ascat1, ascat2, ascat3, ascat4, ascat5),
    names_to = "Agess",
    values_to = "Countss"
  )

#Turn values into characters
HeadInjury7 <- HeadInjury6 %>%
  mutate(HowMany=case_when(
    Count == 1 ~ "1",
    Count == 2 ~ "2",
    Count == 3 ~ "3",
    Count == 4 ~ "4",
    Count == 5 ~ "5"
  ))

#Cleanup
HeadInjury7 <- HeadInjury7[!is.na(HeadInjury7$Count),]
mydata <- HeadInjury7[!is.na(HeadInjury7$Countss),]
rm("data2", "FOUND_RFQ_Head_Injury_13Jun2024", "HeadInjury5", "HeadInjury6", "HeadInjury7")
mydata$Age <- NULL
mydata$Agess <- NULL



####Unconscious####
# Shows whether each concussion caused unconsciousness
# Covers first through fifth concussion, including unsure values

# Import head injury data
HeadInjury <- read_csv("data/HeadInjury.csv")

# Select relevant columns
HeadInjury1 <- select(HeadInjury, c(patno, hiq1, hiq2, hiqa2, hiqb2, hiqc3unc, hiqc4unc, hiqc5unc))

# Rename columns for clarity
names(HeadInjury1)[names(HeadInjury1) == 'patno'] <- 'Patient'
names(HeadInjury1)[names(HeadInjury1) == 'hiq1'] <- 'EverHad'
names(HeadInjury1)[names(HeadInjury1) == 'hiq2'] <- 'HowMany'
names(HeadInjury1)[names(HeadInjury1) == 'hiqa2'] <- 'uncon1'
names(HeadInjury1)[names(HeadInjury1) == 'hiqb2'] <- 'uncon2'
names(HeadInjury1)[names(HeadInjury1) == 'hiqc3unc'] <- 'uncon3'
names(HeadInjury1)[names(HeadInjury1) == 'hiqc4unc'] <- 'uncon4'
names(HeadInjury1)[names(HeadInjury1) == 'hiqc5unc'] <- 'uncon5'

# Remove people with no data for the first concussion
data2 <- HeadInjury1[!is.na(HeadInjury1$uncon1),]

# Create flags indicating the presence of each unconsciousness entry
HeadInjury3 <- HeadInjury1 %>%
  mutate(uncon11=case_when(
    uncon1 != "" ~ "1"
  )) %>%
  mutate(uncon12=case_when(
    uncon2 != "" ~ "2"
  )) %>%
  mutate(uncon13=case_when(
    uncon3 != "" ~ "3"
  )) %>%
  mutate(uncon14=case_when(
    uncon4 != "" ~ "4"
  )) %>%
  mutate(uncon15=case_when(
    uncon5 != "" ~ "5"
  ))

#Combine the columns
HeadInjury4 <- HeadInjury3 %>%
  pivot_longer(
    cols = c(uncon11, uncon12, uncon13, uncon14, uncon15),
    names_to = "Unconscious",
    values_to = "Count1"
  )

# Assign "Yes", "No", or "Unsure" for each concussion
HeadInjury5 <- HeadInjury4 %>%
  mutate(asuncon1=case_when(
    Count1 == 1 & uncon1 == 1 ~ "Yes",
    Count1 == 1 & uncon1 == 0 ~ "No",
    Count1 == 1 & uncon1 == 9999 ~ "Unsure")) %>%
  mutate(asuncon2=case_when(
    Count1 == 2 & uncon2 == 1 ~ "Yes",
    Count1 == 2 & uncon2 == 0 ~ "No",
    Count1 == 2 & uncon2 == 9999 ~ "Unsure")) %>%
  mutate(asuncon3=case_when(
    Count1 == 3 & uncon3 == 1 ~ "Yes",
    Count1 == 3 & uncon3 == 0 ~ "No",
    Count1 == 3 & uncon3 == 9999 ~ "Unsure")) %>%
  mutate(asuncon4=case_when(
    Count1 == 4 & uncon4 == 1 ~ "Yes",
    Count1 == 4 & uncon4 == 0 ~ "No",
    Count1 == 4 & uncon4 == 9999 ~ "Unsure")) %>%
  mutate(asuncon5=case_when(
    Count1 == 5 & uncon5 == 1 ~ "Yes",
    Count1 == 5 & uncon5 == 0 ~ "No",
    Count1 == 5 & uncon5 == 9999 ~ "Unsure"))

# Remove temporary datasets
rm("HeadInjury", "HeadInjury1", "HeadInjury3", "HeadInjury4")

# Rename columns for clarity
HeadInjury6 <- HeadInjury5 %>%
  pivot_longer(
    cols = c(asuncon1, asuncon2, asuncon3, asuncon4, asuncon5),
    names_to = "Unconsciouss",
    values_to = "Countss1"
  )

#Turn values into characters
HeadInjury7 <- HeadInjury6 %>%
  mutate(HowMany=case_when(
    Count1 == 1 ~ "1",
    Count1 == 2 ~ "2",
    Count1 == 3 ~ "3",
    Count1 == 4 ~ "4",
    Count1 == 5 ~ "5"
  ))

# Final cleanup
HeadInjury7 <- HeadInjury7[!is.na(HeadInjury7$Count1),]
mydata1 <- HeadInjury7[!is.na(HeadInjury7$Countss1),]
rm("data2", "FOUND_RFQ_Head_Injury_13Jun2024", "HeadInjury5", "HeadInjury6", "HeadInjury7")
mydata1$Unconscious <- NULL
mydata1$Unconsciouss <- NULL


####Skull####
# Shows whether the head injury caused a skull fracture
# Includes first through fifth injury, with Yes/No/Unsure values

# Import head injury data
HeadInjury <- read_csv("data/HeadInjury.csv")

# Select relevant columns
HeadInjury1 <- select(HeadInjury, c(patno, hiq1, hiq2, hiqa4, hiqb4, hiqc34fracture, hiqc44fracture, hiqc54fracture))

# Rename columns for clarity
names(HeadInjury1)[names(HeadInjury1) == 'patno'] <- 'Patient'
names(HeadInjury1)[names(HeadInjury1) == 'hiq1'] <- 'EverHad'
names(HeadInjury1)[names(HeadInjury1) == 'hiq2'] <- 'HowMany'
names(HeadInjury1)[names(HeadInjury1) == 'hiqa4'] <- 'skull1'
names(HeadInjury1)[names(HeadInjury1) == 'hiqb4'] <- 'skull2'
names(HeadInjury1)[names(HeadInjury1) == 'hiqc34fracture'] <- 'skull3'
names(HeadInjury1)[names(HeadInjury1) == 'hiqc44fracture'] <- 'skull4'
names(HeadInjury1)[names(HeadInjury1) == 'hiqc54fracture'] <- 'skull5'

# Remove rows where the first skull fracture is missing
data2 <- HeadInjury1[!is.na(HeadInjury1$skull1),]

# Create flags indicating the presence of each skull fracture entry
HeadInjury3 <- HeadInjury1 %>%
  mutate(skull11=case_when(
    skull1 != "" ~ "1"
  )) %>%
  mutate(skull12=case_when(
    skull2 != "" ~ "2"
  )) %>%
  mutate(skull13=case_when(
    skull3 != "" ~ "3"
  )) %>%
  mutate(skull14=case_when(
    skull4 != "" ~ "4"
  )) %>%
  mutate(skull15=case_when(
    skull5 != "" ~ "5"
  ))

#Combine the columns
HeadInjury4 <- HeadInjury3 %>%
  pivot_longer(
    cols = c(skull11, skull12, skull13, skull14, skull15),
    names_to = "Skull",
    values_to = "Count2"
  )

# Assign "Yes", "No", or "Unsure" for each skull fracture
HeadInjury5 <- HeadInjury4 %>%
  mutate(asskull1=case_when(
    Count2 == 1 & skull1 == 1 ~ "Yes",
    Count2 == 1 & skull1 == 0 ~ "No",
    Count2 == 1 & skull1 == 9999 ~ "Unsure")) %>%
  mutate(asskull2=case_when(
    Count2 == 2 & skull2 == 1 ~ "Yes",
    Count2 == 2 & skull2 == 0 ~ "No",
    Count2 == 2 & skull2 == 9999 ~ "Unsure")) %>%
  mutate(asskull3=case_when(
    Count2 == 3 & skull3 == 1 ~ "Yes",
    Count2 == 3 & skull3 == 0 ~ "No",
    Count2 == 3 & skull3 == 9999 ~ "Unsure")) %>%
  mutate(asskull4=case_when(
    Count2 == 4 & skull4 == 1 ~ "Yes",
    Count2 == 4 & skull4 == 0 ~ "No",
    Count2 == 4 & skull4 == 9999 ~ "Unsure")) %>%
  mutate(asskull5=case_when(
    Count2 == 5 & skull5 == 1 ~ "Yes",
    Count2 == 5 & skull5 == 0 ~ "No",
    Count2 == 5 & skull5 == 9999 ~ "Unsure"))

#Remove temporary datasets
rm("HeadInjury", "HeadInjury1", "HeadInjury3", "HeadInjury4")

#Combine the columns
HeadInjury6 <- HeadInjury5 %>%
  pivot_longer(
    cols = c(asskull1, asskull2, asskull3, asskull4, asskull5),
    names_to = "SkullFracture",
    values_to = "Countss2"
  )

#Turn values into characters
HeadInjury7 <- HeadInjury6 %>%
  mutate(HowMany=case_when(
    Count2 == 1 ~ "1",
    Count2 == 2 ~ "2",
    Count2 == 3 ~ "3",
    Count2 == 4 ~ "4",
    Count2 == 5 ~ "5"
  ))

#Final cleanup
HeadInjury7 <- HeadInjury7[!is.na(HeadInjury7$Count2),]
mydata2 <- HeadInjury7[!is.na(HeadInjury7$Countss2),]
rm("data2", "FOUND_RFQ_Head_Injury_13Jun2024", "HeadInjury5", "HeadInjury6", "HeadInjury7")
mydata2$Skull <- NULL
mydata2$SkullFracture <- NULL


####Seizure####
# Shows whether each concussion caused seizures
# Covers first through fifth concussion, including unsure and special codes

# Import head injury data
HeadInjury <- read_csv("data/HeadInjury.csv")

# Select relevant columns
HeadInjury1 <- select(HeadInjury, c(patno, hiq1, hiq2, hiqa5, hiqb5, hiqc35seizure, hiqc45seizure, hiqc55seizure))

# Rename columns for clarity
names(HeadInjury1)[names(HeadInjury1) == 'patno'] <- 'Patient'
names(HeadInjury1)[names(HeadInjury1) == 'hiq1'] <- 'EverHad'
names(HeadInjury1)[names(HeadInjury1) == 'hiq2'] <- 'HowMany'
names(HeadInjury1)[names(HeadInjury1) == 'hiqa5'] <- 'seizure1'
names(HeadInjury1)[names(HeadInjury1) == 'hiqb5'] <- 'seizure2'
names(HeadInjury1)[names(HeadInjury1) == 'hiqc35seizure'] <- 'seizure3'
names(HeadInjury1)[names(HeadInjury1) == 'hiqc45seizure'] <- 'seizure4'
names(HeadInjury1)[names(HeadInjury1) == 'hiqc55seizure'] <- 'seizure5'

# Remove rows where the first seizure is missing
data2 <- HeadInjury1[!is.na(HeadInjury1$seizure1),]

# Create flags indicating the presence of each seizure
HeadInjury3 <- HeadInjury1 %>%
  mutate(seizure11=case_when(
    seizure1 != "" ~ "1"
  )) %>%
  mutate(seizure12=case_when(
    seizure2 != "" ~ "2"
  )) %>%
  mutate(seizure13=case_when(
    seizure3 != "" ~ "3"
  )) %>%
  mutate(seizure14=case_when(
    seizure4 != "" ~ "4"
  )) %>%
  mutate(seizure15=case_when(
    seizure5 != "" ~ "5"
  ))

#Combine the columns
HeadInjury4 <- HeadInjury3 %>%
  pivot_longer(
    cols = c(seizure11, seizure12, seizure13, seizure14, seizure15),
    names_to = "seizure",
    values_to = "Count3"
  )

# Assign "Yes", "No", or "Unsure" for each seizure
HeadInjury5 <- HeadInjury4 %>%
  mutate(sss1=case_when(
    Count3 == 1 & seizure1 == 1 ~ "Yes",
    Count3 == 1 & seizure1 == 0 ~ "No",
    Count3 == 1 & seizure1 == 9999 ~ "Unsure",
    Count3 == 1 & seizure1 == 7777 ~ "7777")) %>%
  mutate(sss2=case_when(
    Count3 == 2 & seizure2 == 1 ~ "Yes",
    Count3 == 2 & seizure2 == 0 ~ "No",
    Count3 == 2 & seizure2 == 9999 ~ "Unsure",
    Count3 == 2 & seizure2 == 7777 ~ "7777")) %>%
  mutate(sss3=case_when(
    Count3 == 3 & seizure3 == 1 ~ "Yes",
    Count3 == 3 & seizure3 == 0 ~ "No",
    Count3 == 3 & seizure3 == 9999 ~ "Unsure",
    Count3 == 3 & seizure3 == 7777 ~ "7777")) %>%
  mutate(sss4=case_when(
    Count3 == 4 & seizure4 == 1 ~ "Yes",
    Count3 == 4 & seizure4 == 0 ~ "No",
    Count3 == 4 & seizure4 == 9999 ~ "Unsure",
    Count3 == 4 & seizure4 == 7777 ~ "7777")) %>%
  mutate(sss5=case_when(
    Count3 == 5 & seizure5 == 1 ~ "Yes",
    Count3 == 5 & seizure5 == 0 ~ "No",
    Count3 == 5 & seizure5 == 9999 ~ "Unsure",
    Count3 == 5 & seizure5 == 7777 ~ "7777"))

#Remove temporary datasets
rm("HeadInjury", "HeadInjury1", "HeadInjury3", "HeadInjury4")

#Combine the columns
HeadInjury6 <- HeadInjury5 %>%
  pivot_longer(
    cols = c(sss1, sss2, sss3, sss4, sss5),
    names_to = "seizure22",
    values_to = "Countss3"
  )

#Turn values into characters
HeadInjury7 <- HeadInjury6 %>%
  mutate(HowMany=case_when(
    Count3 == 1 ~ "1",
    Count3 == 2 ~ "2",
    Count3 == 3 ~ "3",
    Count3 == 4 ~ "4",
    Count3 == 5 ~ "5"
  ))

#Final cleanup
HeadInjury7 <- HeadInjury7[!is.na(HeadInjury7$Count3),]
mydata3 <- HeadInjury7[!is.na(HeadInjury7$Countss3),]
rm("data2", "FOUND_RFQ_Head_Injury_13Jun2024", "HeadInjury5", "HeadInjury6", "HeadInjury7")
mydata3$seizure <- NULL
mydata3$seizure22 <- NULL


####MemoryLoss####
# Shows whether each concussion caused memory loss (amnesia)
# Covers first through fifth concussion, including unsure values

# Import head injury data
HeadInjury <- read_csv("data/HeadInjury.csv")

# Select relevant columns
HeadInjury1 <- select(HeadInjury, c(patno, hiq1, hiq2, hiqa6, hiqb6, hiqc36amneisa, hiqc46amneisa, hiqc56amneisa))

# Rename columns for clarity
names(HeadInjury1)[names(HeadInjury1) == 'patno'] <- 'Patient'
names(HeadInjury1)[names(HeadInjury1) == 'hiq1'] <- 'EverHad'
names(HeadInjury1)[names(HeadInjury1) == 'hiq2'] <- 'HowMany'
names(HeadInjury1)[names(HeadInjury1) == 'hiqa6'] <- 'amneisa1'
names(HeadInjury1)[names(HeadInjury1) == 'hiqb6'] <- 'amneisa2'
names(HeadInjury1)[names(HeadInjury1) == 'hiqc36amneisa'] <- 'amneisa3'
names(HeadInjury1)[names(HeadInjury1) == 'hiqc46amneisa'] <- 'amneisa4'
names(HeadInjury1)[names(HeadInjury1) == 'hiqc56amneisa'] <- 'amneisa5'

# Remove rows where the first memory loss from concussion is missing
data2 <- HeadInjury1[!is.na(HeadInjury1$amneisa1),]

# Create flags indicating the presence of each time memory loss occured from concussion
HeadInjury3 <- HeadInjury1 %>%
  mutate(amneisa11=case_when(
    amneisa1 != "" ~ "1"
  )) %>%
  mutate(amneisa12=case_when(
    amneisa2 != "" ~ "2"
  )) %>%
  mutate(amneisa13=case_when(
    amneisa3 != "" ~ "3"
  )) %>%
  mutate(amneisa14=case_when(
    amneisa4 != "" ~ "4"
  )) %>%
  mutate(amneisa15=case_when(
    amneisa5 != "" ~ "5"
  ))

#Combine the columns
HeadInjury4 <- HeadInjury3 %>%
  pivot_longer(
    cols = c(amneisa11, amneisa12, amneisa13, amneisa14, amneisa15),
    names_to = "amneisa111",
    values_to = "Count4"
  )

# Assign "Yes", "No", or "Unsure" for each time memory loss occured from concussion
HeadInjury5 <- HeadInjury4 %>%
  mutate(asamneisa1=case_when(
    Count4 == 1 & amneisa1 == 1 ~ "Yes",
    Count4 == 1 & amneisa1 == 0 ~ "No",
    Count4 == 1 & amneisa1 == 9999 ~ "Unsure")) %>%
  mutate(asamneisa2=case_when(
    Count4 == 2 & amneisa2 == 1 ~ "Yes",
    Count4 == 2 & amneisa2 == 0 ~ "No",
    Count4 == 2 & amneisa2 == 9999 ~ "Unsure")) %>%
  mutate(asamneisa3=case_when(
    Count4 == 3 & amneisa3 == 1 ~ "Yes",
    Count4 == 3 & amneisa3 == 0 ~ "No",
    Count4 == 3 & amneisa3 == 9999 ~ "Unsure")) %>%
  mutate(asamneisa4=case_when(
    Count4 == 4 & amneisa4 == 1 ~ "Yes",
    Count4 == 4 & amneisa4 == 0 ~ "No",
    Count4 == 4 & amneisa4 == 9999 ~ "Unsure")) %>%
  mutate(asamneisa5=case_when(
    Count4 == 5 & amneisa5 == 1 ~ "Yes",
    Count4 == 5 & amneisa5 == 0 ~ "No",
    Count4 == 5 & amneisa5 == 9999 ~ "Unsure"))

#Remove temporary datasets
rm("HeadInjury", "HeadInjury1", "HeadInjury3", "HeadInjury4")

#Combine the columns
HeadInjury6 <- HeadInjury5 %>%
  pivot_longer(
    cols = c(asamneisa1, asamneisa2, asamneisa3, asamneisa4, asamneisa5),
    names_to = "amneisa22",
    values_to = "Countss4"
  )

#Turn values into characters
HeadInjury7 <- HeadInjury6 %>%
  mutate(HowMany=case_when(
    Count4 == 1 ~ "1",
    Count4 == 2 ~ "2",
    Count4 == 3 ~ "3",
    Count4 == 4 ~ "4",
    Count4 == 5 ~ "5"
  ))

#Final cleanup
HeadInjury7 <- HeadInjury7[!is.na(HeadInjury7$Count4),]
mydata4 <- HeadInjury7[!is.na(HeadInjury7$Countss4),]
rm("data2", "FOUND_RFQ_Head_Injury_13Jun2024", "HeadInjury5", "HeadInjury6", "HeadInjury7")
mydata4$amneisa111 <- NULL
mydata4$amneisa22 <- NULL


####Hospitalized####
# Shows whether each concussion led to hospitalization
# Covers first through fifth concussion, including unsure values

# Import head injury data
HeadInjury <- read_csv("data/HeadInjury.csv")

# Select relevant columns
HeadInjury1 <- select(HeadInjury, c(patno, hiq1, hiq2, hiqa7, hiqb7, hiqc37hospitaliz, hiqc47hospitaliz, hiqc57hospitaliz))

# Rename columns for clarity
names(HeadInjury1)[names(HeadInjury1) == 'patno'] <- 'Patient'
names(HeadInjury1)[names(HeadInjury1) == 'hiq1'] <- 'EverHad'
names(HeadInjury1)[names(HeadInjury1) == 'hiq2'] <- 'HowMany'
names(HeadInjury1)[names(HeadInjury1) == 'hiqa7'] <- 'hospitalized1'
names(HeadInjury1)[names(HeadInjury1) == 'hiqb7'] <- 'hospitalized2'
names(HeadInjury1)[names(HeadInjury1) == 'hiqc37hospitaliz'] <- 'hospitalized3'
names(HeadInjury1)[names(HeadInjury1) == 'hiqc47hospitaliz'] <- 'hospitalized4'
names(HeadInjury1)[names(HeadInjury1) == 'hiqc57hospitaliz'] <- 'hospitalized5'

# Remove rows where the first hospitalization from concussion is missing
data2 <- HeadInjury1[!is.na(HeadInjury1$hospitalized1),]

# Create flags indicating the presence of each time hospitalization from concussion occurred
HeadInjury3 <- HeadInjury1 %>%
  mutate(hospitalized11=case_when(
    hospitalized1 != "" ~ "1"
  )) %>%
  mutate(hospitalized12=case_when(
    hospitalized2 != "" ~ "2"
  )) %>%
  mutate(hospitalized13=case_when(
    hospitalized3 != "" ~ "3"
  )) %>%
  mutate(hospitalized14=case_when(
    hospitalized4 != "" ~ "4"
  )) %>%
  mutate(hospitalized15=case_when(
    hospitalized5 != "" ~ "5"
  ))

#Combine the columns
HeadInjury4 <- HeadInjury3 %>%
  pivot_longer(
    cols = c(hospitalized11, hospitalized12, hospitalized13, hospitalized14, hospitalized15),
    names_to = "hospitalized111",
    values_to = "Count5"
  )

# Assign "Yes", "No", or "Unsure" for each time hospitalization from concussion occurred
HeadInjury5 <- HeadInjury4 %>%
  mutate(ashospitalized1=case_when(
    Count5 == 1 & hospitalized1 == 1 ~ "Yes",
    Count5 == 1 & hospitalized1 == 0 ~ "No",
    Count5 == 1 & hospitalized1 == 9999 ~ "Unsure")) %>%
  mutate(ashospitalized2=case_when(
    Count5 == 2 & hospitalized2 == 1 ~ "Yes",
    Count5 == 2 & hospitalized2 == 0 ~ "No",
    Count5 == 2 & hospitalized2 == 9999 ~ "Unsure")) %>%
  mutate(ashospitalized3=case_when(
    Count5 == 3 & hospitalized3 == 1 ~ "Yes",
    Count5 == 3 & hospitalized3 == 0 ~ "No",
    Count5 == 3 & hospitalized3 == 9999 ~ "Unsure")) %>%
  mutate(ashospitalized4=case_when(
    Count5 == 4 & hospitalized4 == 1 ~ "Yes",
    Count5 == 4 & hospitalized4 == 0 ~ "No",
    Count5 == 4 & hospitalized4 == 9999 ~ "Unsure")) %>%
  mutate(ashospitalized5=case_when(
    Count5 == 5 & hospitalized5 == 1 ~ "Yes",
    Count5 == 5 & hospitalized5 == 0 ~ "No",
    Count5 == 5 & hospitalized5 == 9999 ~ "Unsure"))

#Remove temporary datasets
rm("HeadInjury", "HeadInjury1", "HeadInjury3", "HeadInjury4")

#Combine the columns
HeadInjury6 <- HeadInjury5 %>%
  pivot_longer(
    cols = c(ashospitalized1, ashospitalized2, ashospitalized3, ashospitalized4, ashospitalized5),
    names_to = "hospitalized22",
    values_to = "Countss5"
  )

#Turn values into characters
HeadInjury7 <- HeadInjury6 %>%
  mutate(HowMany=case_when(
    Count5 == 1 ~ "1",
    Count5 == 2 ~ "2",
    Count5 == 3 ~ "3",
    Count5 == 4 ~ "4",
    Count5 == 5 ~ "5"
  ))

#Final cleanup
HeadInjury7 <- HeadInjury7[!is.na(HeadInjury7$Count5),]
mydata5 <- HeadInjury7[!is.na(HeadInjury7$Countss5),]
rm("data2", "FOUND_RFQ_Head_Injury_13Jun2024", "HeadInjury5", "HeadInjury6", "HeadInjury7")
mydata5$hospitalized111 <- NULL
mydata5$hospitalized22 <- NULL


####Combine####

# Define the columns to add (with NA) for each dataset so they all have matching columns before binding
place1 <- c("uncon1", "uncon2", "uncon3", "uncon4", "uncon5",
            "skull1", "skull2", "skull3", "skull4", "skull5",
            "seizure1", "seizure2", "seizure3", "seizure4", "seizure5",
            "amneisa1", "amneisa2", "amneisa3", "amneisa4", "amneisa5",
            "hospitalized1", "hospitalized2", "hospitalized3", "hospitalized4", "hospitalized5",
            "Count1", "Countss1", "Count2", "Countss2", "Count3", "Countss3",
            "Count4", "Countss4", "Count5", "Countss5")
place2 <- c("age1", "age2", "age3", "age4", "age5",
            "skull1", "skull2", "skull3", "skull4", "skull5",
            "seizure1", "seizure2", "seizure3", "seizure4", "seizure5",
            "amneisa1", "amneisa2", "amneisa3", "amneisa4", "amneisa5",
            "hospitalized1", "hospitalized2", "hospitalized3", "hospitalized4", "hospitalized5",
            "Count", "Countss", "Count2", "Countss2", "Count3", "Countss3",
            "Count4", "Countss4", "Count5", "Countss5")
place3 <- c("age1", "age2", "age3", "age4", "age5",
            "uncon1", "uncon2", "uncon3", "uncon4", "uncon5",
            "seizure1", "seizure2", "seizure3", "seizure4", "seizure5",
            "amneisa1", "amneisa2", "amneisa3", "amneisa4", "amneisa5",
            "hospitalized1", "hospitalized2", "hospitalized3", "hospitalized4", "hospitalized5",
            "Count", "Countss", "Count1", "Countss1", "Count3", "Countss3",
            "Count4", "Countss4", "Count5", "Countss5")
place4 <- c("age1", "age2", "age3", "age4", "age5",
            "uncon1", "uncon2", "uncon3", "uncon4", "uncon5",
            "skull1", "skull2", "skull3", "skull4", "skull5",
            "amneisa1", "amneisa2", "amneisa3", "amneisa4", "amneisa5",
            "hospitalized1", "hospitalized2", "hospitalized3", "hospitalized4", "hospitalized5",
            "Count", "Countss", "Count1", "Countss1", "Count2", "Countss2",
            "Count4", "Countss4", "Count5", "Countss5")
place5 <- c("age1", "age2", "age3", "age4", "age5",
            "uncon1", "uncon2", "uncon3", "uncon4", "uncon5",
            "skull1", "skull2", "skull3", "skull4", "skull5",
            "seizure1", "seizure2", "seizure3", "seizure4", "seizure5",
            "hospitalized1", "hospitalized2", "hospitalized3", "hospitalized4", "hospitalized5",
            "Count", "Countss", "Count1", "Countss1", "Count2", "Countss2", "Count3", "Countss3",
            "Count5", "Countss5")
place6 <- c("age1", "age2", "age3", "age4", "age5",
            "uncon1", "uncon2", "uncon3", "uncon4", "uncon5",
            "skull1", "skull2", "skull3", "skull4", "skull5",
            "seizure1", "seizure2", "seizure3", "seizure4", "seizure5",
            "amneisa1", "amneisa2", "amneisa3", "amneisa4", "amneisa5",
            "Count", "Countss", "Count1", "Countss1", "Count2", "Countss2", "Count3", "Countss3",
            "Count4", "Countss4")

# Add missing columns (filled with NA) so all data frames have same columns before merging
mydata[ , place1] <- NA
mydata1[ , place2] <- NA
mydata2[ , place3] <- NA
mydata3[ , place4] <- NA
mydata4[ , place5] <- NA
mydata5[ , place6] <- NA

# Remove temporary datasets
rm("place1", "place2", "place3", "place4", "place5", "place6")

# Bind all datasets row-wise into a single dataset
total <- rbind(mydata, mydata1)
total1 <- rbind(total, mydata2)
total2 <- rbind(total1, mydata3)
total3 <- rbind(total2, mydata4)
total4 <- rbind(total3, mydata5)

# Remove temporary datasets
rm("mydata", "mydata1", "mydata2", "mydata3", "mydata4", "mydata5")
rm("total", "total1", "total2", "total3")

# Remove Count columns that are no longer needed
total4$Count <- NULL
total4$Count1 <- NULL
total4$Count2 <- NULL
total4$Count3 <- NULL
total4$Count4 <- NULL
total4$Count5 <- NULL

# Keep only rows with a valid concussion number
total4 <- total4[!is.na(total4$HowMany),]

# Create a descriptive factor for concussion number
total5 <- total4 %>%
  mutate(HowMany1 = recode(HowMany,
                           '1' = 'First Concussion',
                           '2' = 'Second Concussion',
                           '3' = 'Third Concussion',
                           '4' = 'Fourth Concussion',
                           '5' = 'Fifth Concussion'))

# Recode factor levels to descriptive labels
total5 <- total5 %>%
  mutate(Countss = factor(Countss, labels=c("<18", "18-24", "25-34", "35-44", "45-54", ">=55")),
         Countss1 = factor(Countss1, labels=c("Yes", "No", "Unsure")),
         Countss2 = factor(Countss2, labels=c("Yes", "No", "Unsure")),
         Countss3 = factor(Countss3, labels=c("Yes", "No", "Unsure", "7777?")),
         Countss4 = factor(Countss4, labels=c("Yes", "No", "Unsure")),
         Countss5 = factor(Countss5, labels=c("Yes", "No", "Unsure")),
         HowMany1 = factor(HowMany1, labels=c("First Concussion", "Second Concussion", "Third Concussion", "Fourth Concussion", "Fifth Concussion")))

# Assign descriptive labels for variables
label(total5$Countss) <- 'Age'
label(total5$Countss1) <- 'Did you lose consciousness?'
label(total5$Countss2) <- 'Did you have a skull fracture?'
label(total5$Countss3) <- 'Did you have a seizure?'
label(total5$Countss4) <- 'Did you have memory loss, amnesia, or trouble thinking?'
label(total5$Countss5) <- 'Were you hospitalized?'

# Convert factor columns back to characters
total5$Countss <- as.character(total5$Countss)
total5$Countss1 <- as.character(total5$Countss1)
total5$Countss2 <- as.character(total5$Countss2)
total5$Countss3 <- as.character(total5$Countss3)
total5$Countss4 <- as.character(total5$Countss4)
total5$Countss5 <- as.character(total5$Countss5)

####Table####
#Create the final table 

total5$ascat1 = factor(total5$Countss, levels = c("<18",
                                                  "18-24",
                                                  "25-34",
                                                  "35-44",
                                                  "45-54",
                                                  ">=55"), ordered = TRUE)

label(total5$Countss) <- 'How old were you when you got your concussion?'


table1(~ Countss
       | HowMany1, data=total5)
