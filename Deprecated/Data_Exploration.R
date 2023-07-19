#### Load Packages####
library(haven)
library(dplyr)
library(tidyverse)
library(tableone)
library(magrittr)
library(ggplot2)


#### Read CSV files ####
raw_data_2019 <- read.csv("Raw_Data/namcs2019.csv")
raw_data_2018 <- read.csv("Raw_Data/namcs2018.csv")
raw_data_2016 <- read.csv("Raw_Data/namcs2016.csv")

codes <-read.csv("Raw_Data/namcs_medication_codes.csv")
RFVcodes <- read.csv("Raw_Data/namcs_rfv_codes_3.csv")
ICD10codes <- read.csv("Raw_Data/ICD-10_codes.csv")


#### Create a filtered dataset for each year with only children and selected variables, then merge these three filtered datasets ####

# Filter 2019 data
peds_data_2019 <- raw_data_2019 %>% 
  filter(AGE <= 18) %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,             # Demographic characteristics
         <<<<<<< HEAD:Data_Exploration.R
         VMONTH, VDAYR,                                 # Timing
         =======
           VMONTH, VDAYR,
         >>>>>>> 0080d2fd1c914fc6efc3e74e87a7aa8557a8fdb5:Table1_Data_Exploration.R
         ERADMHOS, REFOTHMD,                            # Referrals
         MAJOR, RFV1,                                   # Reason for Visit
         ASTHMA, ASTH_SEV, ASTH_CON, ASTHMAED, ASTHMAP, # Asthma
         MENTAL, ADD,                                   # Mental Health 
         STREP, URINE, BLDCX, TRTCX, URNCX, OTHCX,      # Tests and cultures
         DIAG1,DIAG2,DIAG3,DIAG4,DIAG5,                 # Diagnoses
         CPSUM,CSTRATM, PATWT,                          # Weighting
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,RX11CAT1,RX12CAT1,RX13CAT1,RX14CAT1,RX15CAT1,RX16CAT1,RX17CAT1,RX18CAT1,RX19CAT1,RX20CAT1,RX21CAT1,RX22CAT1,RX23CAT1,RX24CAT1,RX25CAT1,RX26CAT1,RX27CAT1,RX28CAT1,RX29CAT1,RX30CAT1, # Drug Categories
         MED1,MED2,MED3,MED4,MED5,MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14,MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22,MED23,MED24,MED25,MED26,MED27,MED28,MED29,MED30) %>% # Medications
  droplevels() 

# Filter 2018 data
peds_data_2018 <- raw_data_2018 %>% 
  filter(AGE <= 18) %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,             # Demographic characteristics
         <<<<<<< HEAD:Data_Exploration.R
         VMONTH, VDAYR,                                 # Timing
         =======
           VMONTH, VDAYR,
         >>>>>>> 0080d2fd1c914fc6efc3e74e87a7aa8557a8fdb5:Table1_Data_Exploration.R
         ERADMHOS, REFOTHMD,                            # Referrals
         MAJOR, RFV1,                                   # Reason for Visit
         ASTHMA, ASTH_SEV, ASTH_CON, ASTHMAED, ASTHMAP, # Asthma
         MENTAL, ADD,                                   # Mental Health 
         STREP, URINE, BLDCX, TRTCX, URNCX, OTHCX,      # Tests and cultures
         DIAG1,DIAG2,DIAG3,DIAG4,DIAG5,                 # Diagnoses
         CPSUM,CSTRATM, PATWT,                          # Weighting
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,RX11CAT1,RX12CAT1,RX13CAT1,RX14CAT1,RX15CAT1,RX16CAT1,RX17CAT1,RX18CAT1,RX19CAT1,RX20CAT1,RX21CAT1,RX22CAT1,RX23CAT1,RX24CAT1,RX25CAT1,RX26CAT1,RX27CAT1,RX28CAT1,RX29CAT1,RX30CAT1, # Drug Categories
         MED1,MED2,MED3,MED4,MED5,MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14,MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22,MED23,MED24,MED25,MED26,MED27,MED28,MED29,MED30) %>% # Medications
  droplevels() 

# Filter 2016 data
peds_data_2016 <- raw_data_2016 %>% 
  filter(AGE <= 18) %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,             # Demographic characteristics
         <<<<<<< HEAD:Data_Exploration.R
         VMONTH, VDAYR,                                 # Timing
         =======
           VMONTH, VDAYR,
         >>>>>>> 0080d2fd1c914fc6efc3e74e87a7aa8557a8fdb5:Table1_Data_Exploration.R
         ERADMHOS, REFOTHMD,                            # Referrals
         MAJOR, RFV1,                                   # Reason for Visit
         ASTHMA, ASTH_SEV, ASTH_CON, ASTHMAED, ASTHMAP, # Asthma
         MENTAL, ADD,                                   # Mental Health 
         STREP, URINE, BLDCX, TRTCX, URNCX, OTHCX,      # Tests and cultures
         DIAG1,DIAG2,DIAG3,DIAG4,DIAG5,                 # Diagnoses
         CPSUM,CSTRATM, PATWT,                          # Weighting
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,RX11CAT1,RX12CAT1,RX13CAT1,RX14CAT1,RX15CAT1,RX16CAT1,RX17CAT1,RX18CAT1,RX19CAT1,RX20CAT1,RX21CAT1,RX22CAT1,RX23CAT1,RX24CAT1,RX25CAT1,RX26CAT1,RX27CAT1,RX28CAT1,RX29CAT1,RX30CAT1, # Drug Categories
         MED1,MED2,MED3,MED4,MED5,MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14,MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22,MED23,MED24,MED25,MED26,MED27,MED28,MED29,MED30) %>% # Medications
  droplevels() 


# Combine 2016, 2018, and 2019 data 
peds_data_combined <- rbind(peds_data_2019,peds_data_2018,peds_data_2016)

#### Rename the numerical codes of categorical variables ####
peds_data_combined$RACERETH = factor(peds_data_combined$RACERETH,
                                     levels = c(1,2,3,4),
                                     labels = c("Non-Hispanic White", 
                                                "Non-Hispanic Black", 
                                                "Hispanic", 
                                                "Non-Hispanic Other/Multiple Race"))

peds_data_2019$RACERETH = factor(peds_data_2019$RACERETH,
                                 levels = c(1,2,3,4),
                                 labels = c("Non-Hispanic White", 
                                            "Non-Hispanic Black", 
                                            "Hispanic", 
                                            "Non-Hispanic Other/Multiple Race"))

peds_data_2018$RACERETH = factor(peds_data_2018$RACERETH,
                                 levels = c(1,2,3,4),
                                 labels = c("Non-Hispanic White", 
                                            "Non-Hispanic Black", 
                                            "Hispanic", 
                                            "Non-Hispanic Other/Multiple Race"))

peds_data_2016$RACERETH = factor(peds_data_2016$RACERETH,
                                 levels = c(1,2,3,4),
                                 labels = c("Non-Hispanic White", 
                                            "Non-Hispanic Black", 
                                            "Hispanic", 
                                            "Non-Hispanic Other/Multiple Race"))

<<<<<<< HEAD:Data_Exploration.R
=======
  
  >>>>>>> 0080d2fd1c914fc6efc3e74e87a7aa8557a8fdb5:Table1_Data_Exploration.R
peds_data_combined$SEX = factor(peds_data_combined$SEX,
                                levels = c(1,2),
                                labels = c("Female", 
                                           "Male"))

peds_data_combined$MSA = factor(peds_data_combined$MSA,
                                levels = c(1,2),
                                labels = c("Metropolitan Statistical Area",
                                           "Non-Metropolitan Statistical Area"))

peds_data_combined$PAYTYPER = factor(peds_data_combined$PAYTYPER,
                                     levels = c(-9,-8,1,2,3,4,5,6,7),
                                     labels = c("Blank",
                                                "Unknown",
                                                "Private Insurance",
                                                "Medicare",
                                                "Medicaid or CHIP",
                                                "Worker's Compensation",
                                                "Self-pay",
                                                "No Charge",
                                                "Other"))

peds_data_combined$MAJOR = factor(peds_data_combined$MAJOR,
                                  levels = c(-9,1,2,3,4,5,6),
                                  labels = c("Blank",
                                             "New problem (<3 mos. onset)",
                                             "Chronic Problem, Routine",
                                             "Chronic Problem, Flare-up",
                                             "Pre Surgery",
                                             "Post Surgery",
                                             "Preventive Care"))

peds_data_combined$ASTH_SEV = factor(peds_data_combined$ASTH_SEV,
                                     levels = c(-9,-7,1,2,3,4,5,6),
                                     labels = c("Blank",
                                                "N/A",
                                                "Intermittent",
                                                "Mild Persistent",
                                                "Moderate Persistent",
                                                "Severe Peristent",
                                                "Other",
                                                "None Recorded"))

peds_data_combined$ASTH_CON = factor(peds_data_combined$ASTH_CON,
                                     levels = c(-9,-7,1,2,3,4,5),
                                     labels = c("Blank",
                                                "N/A",
                                                "Well-Controlled",
                                                "Not Well Controlled",
                                                "Very Poorly Controlled",
                                                "Other",
                                                "None Recorded"))


#### Rename medication codes ####

peds_data_combined$MED1 = factor(peds_data_combined$MED1, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED2 = factor(peds_data_combined$MED2, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED3 = factor(peds_data_combined$MED3, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED4 = factor(peds_data_combined$MED4, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED5 = factor(peds_data_combined$MED5, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED6 = factor(peds_data_combined$MED6, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED7 = factor(peds_data_combined$MED7, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED8 = factor(peds_data_combined$MED8, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED9 = factor(peds_data_combined$MED9, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED10 = factor(peds_data_combined$MED10, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED11 = factor(peds_data_combined$MED11, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED12 = factor(peds_data_combined$MED12, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED13 = factor(peds_data_combined$MED13, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED14 = factor(peds_data_combined$MED14, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED15 = factor(peds_data_combined$MED15, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED16 = factor(peds_data_combined$MED16, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED17 = factor(peds_data_combined$MED17, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED18 = factor(peds_data_combined$MED18, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED19 = factor(peds_data_combined$MED19, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED20 = factor(peds_data_combined$MED20, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED21 = factor(peds_data_combined$MED21, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED22 = factor(peds_data_combined$MED22, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED23 = factor(peds_data_combined$MED23, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED24 = factor(peds_data_combined$MED24, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED25 = factor(peds_data_combined$MED25, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED26 = factor(peds_data_combined$MED26, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED27 = factor(peds_data_combined$MED27, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED28 = factor(peds_data_combined$MED28, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED29 = factor(peds_data_combined$MED29, levels = codes$ID,labels = codes$Name)
peds_data_combined$MED30 = factor(peds_data_combined$MED30, levels = codes$ID,labels = codes$Name)


#### Rename ICD-10 codes ####

peds_data_combined$DIAG1 = factor(peds_data_combined$DIAG1, levels = ICD10codes$ID, labels = ICD10codes$Name)
peds_data_combined$DIAG2 = factor(peds_data_combined$DIAG2, levels = ICD10codes$ID, labels = ICD10codes$Name)
peds_data_combined$DIAG3 = factor(peds_data_combined$DIAG3, levels = ICD10codes$ID, labels = ICD10codes$Name)
peds_data_combined$DIAG4 = factor(peds_data_combined$DIAG4, levels = ICD10codes$ID, labels = ICD10codes$Name)
peds_data_combined$DIAG5 = factor(peds_data_combined$DIAG5, levels = ICD10codes$ID, labels = ICD10codes$Name)


#### Format data and create summary tables  ####

# Create Demographic Data Table
continuous_table_one <- c("AGE")
categorical_table_one <- c("RACERETH", "SEX", "MSA", "PAYTYPER", "MAJOR", "REFOTHMD", "ERADMHOS")
table_one_data <- CreateTableOne(vars = c(continuous_table_one, categorical_table_one), data = peds_data_combined, factorVars = categorical_table_one)
print(table_one_data, showAllLevels = TRUE, quote = FALSE, noSpaces = FALSE)

# Create Asthma Data Table
categorical_asthma_data <- c("ASTHMA", "ASTH_SEV", "ASTH_CON", "ASTHMAED", "ASTHMAP")
asthma_table <- CreateTableOne(vars = c(categorical_asthma_data), data = peds_data_combined, factorVars = categorical_asthma_data)
print(asthma_table, showAllLevels = TRUE, quote = FALSE, noSpaces = FALSE)

# Create Mental Health Data Table
categorical_mh_data <- c("MENTAL", "ADD")
mh_table <- CreateTableOne(vars = c(categorical_mh_data), data = peds_data_combined, factorVars = categorical_mh_data)
print(mh_table, showAllLevels = TRUE, quote = FALSE, noSpaces = FALSE)

# Create ID Testing/Culture Table
categorical_id_data <- c("STREP", "URINE", "BLDCX", "TRTCX", "URNCX", "OTHCX")
id_table <- CreateTableOne(vars = c(categorical_id_data), data = peds_data_combined, factorVars = categorical_id_data)
print(id_table, showAllLevels = TRUE, quote = FALSE, noSpaces = FALSE)

#### Create a frequency table of the most important reason for the visit ####

peds_data_combined$RFV1 = factor(peds_data_combined$RFV1, levels = RFVcodes$ID, labels = RFVcodes$Name)

peds_data_combined %>% 
  group_by(RFV1) %>% 
  summarize(n=n()) %>% 
  print(n=48)


#### Create a function showing a breakdown of prescribing for a particular drug class by race/ethnicity ####

filter_drug_classes <- function(classes) {
  peds_data_combined %>% 
    filter((RX1CAT1 %in% classes | RX2CAT1 %in% classes | RX3CAT1 %in% classes | RX4CAT1 %in% classes | RX5CAT1 %in% classes | RX6CAT1 %in% classes | RX7CAT1 %in% classes | RX8CAT1 %in% classes | RX9CAT1 %in% classes | RX10CAT1 %in% classes | RX11CAT1 %in% classes | RX12CAT1 %in% classes | RX13CAT1 %in% classes | RX14CAT1 %in% classes | RX15CAT1 %in% classes | RX16CAT1 %in% classes | RX17CAT1 %in% classes | RX18CAT1 %in% classes | RX19CAT1 %in% classes | RX20CAT1 %in% classes | RX21CAT1 %in% classes | RX22CAT1 %in% classes | RX23CAT1 %in% classes | RX24CAT1 %in% classes | RX25CAT1 %in% classes | RX26CAT1 %in% classes | RX27CAT1 %in% classes | RX28CAT1 %in% classes | RX29CAT1 %in% classes | RX30CAT1)) %>% 
    group_by(RACERETH) %>% 
    summarize(n=n())
}

#### Show a breakdown of antibiotic prescribing by race/ethnicity ####
antibiotics <- c(9,11,12,13,14,15,16,17,240)
abx_by_race <- filter_drug_classes(antibiotics)
as.data.frame(abx_by_race)

total_by_race <- peds_data_combined %>% 
  group_by(RACERETH) %>% 
  summarize(n=n()) %>% 
  as.data.frame(total_by_race)

abx_rx_rate <- cbind(abx_by_race,total_by_race$n)
colnames(abx_rx_rate)<-c("Race/Ethnicity", "Number_of_Abx_Prescriptions", "Total_Number")

abx_rx_rate %>% 
  mutate("Rate_of_Prescripton (per 1000)" = Number_of_Abx_Prescriptions / Total_Number *1000) #%>% 
#write.csv("Abx_Rx_Rate.csv")


#### Show a breakdown of anti-asthmatic prescribing by race/ethnicity ####
anti_asthmatics <- c(243)
asth_by_race <- filter_drug_classes(anti_asthmatics)
as.data.frame(asth_by_race)

asth_rx_rate <- cbind(asth_by_race,total_by_race$n)
colnames(asth_rx_rate)<-c("Race/Ethnicity", "Number_of_AntiAsthmatic_Prescriptions", "Total_Number")

asth_rx_rate %>% 
  mutate("Rate_of_Prescripton (per 1000)" = Number_of_AntiAsthmatic_Prescriptions / Total_Number *1000) #%>% 
#write.csv("Asth_Rx_Rate.csv")


#### Show a breakdown of CNS stimulant prescribing by race/ethnicity ####
CNS_stimulants <- c(71)
stim_by_race <- filter_drug_classes(CNS_stimulants)
as.data.frame(stim_by_race)

stim_rx_rate <- cbind(stim_by_race,total_by_race$n)
colnames(stim_rx_rate)<-c("Race/Ethnicity", "Number_of_Stimulant_Prescriptions", "Total_Number")

stim_rx_rate %>% 
  mutate("Rate_of_Prescripton (per 1000)" = Number_of_Stimulant_Prescriptions / Total_Number *1000) #%>% 
#write.csv("Stim_Rx_Rate.csv")


#### Create a function showing a breakdown of prescribing for a particular medication by race/ethnicity ####
filter_med <- function(medication) {
  peds_data_combined %>%
    filter((MED1 == medication | MED2 == medication | MED3 == medication | MED4 == medication | MED5 == medication | MED6 == medication | MED7 == medication | MED8 == medication | MED9 == medication | MED10 == medication | MED11 == medication | MED12 == medication | MED13 == medication | MED14 == medication | MED15 == medication | MED16 == medication | MED17 == medication | MED18 == medication | MED19 == medication | MED20 == medication | MED21 == medication | MED22 == medication | MED23 == medication | MED24 == medication | MED25 == medication | MED26 == medication | MED27 == medication | MED28 == medication | MED29 == medication | MED30 == medication)) %>% 
    group_by(RACERETH) %>% 
    summarize(n=n())
}


#### Stimulant prescribing over time ####

## Get stimulant prescribing data by race/ethnicity for each year
stim2019 <- peds_data_2019 %>% 
  filter((RX1CAT1 %in% CNS_stimulants | RX2CAT1 %in% CNS_stimulants | RX3CAT1 %in% CNS_stimulants | RX4CAT1 %in% CNS_stimulants | RX5CAT1 %in% CNS_stimulants | RX6CAT1 %in% CNS_stimulants | RX7CAT1 %in% CNS_stimulants | RX8CAT1 %in% CNS_stimulants | RX9CAT1 %in% CNS_stimulants | RX10CAT1 %in% CNS_stimulants | RX11CAT1 %in% CNS_stimulants | RX12CAT1 %in% CNS_stimulants | RX13CAT1 %in% CNS_stimulants | RX14CAT1 %in% CNS_stimulants | RX15CAT1 %in% CNS_stimulants | RX16CAT1 %in% CNS_stimulants | RX17CAT1 %in% CNS_stimulants | RX18CAT1 %in% CNS_stimulants | RX19CAT1 %in% CNS_stimulants | RX20CAT1 %in% CNS_stimulants | RX21CAT1 %in% CNS_stimulants | RX22CAT1 %in% CNS_stimulants | RX23CAT1 %in% CNS_stimulants | RX24CAT1 %in% CNS_stimulants | RX25CAT1 %in% CNS_stimulants | RX26CAT1 %in% CNS_stimulants | RX27CAT1 %in% CNS_stimulants | RX28CAT1 %in% CNS_stimulants | RX29CAT1 %in% CNS_stimulants | RX30CAT1)) %>% 
  group_by(RACERETH) %>% 
  summarise(n=n())

stim2018 <- peds_data_2018 %>% 
  filter((RX1CAT1 %in% CNS_stimulants | RX2CAT1 %in% CNS_stimulants | RX3CAT1 %in% CNS_stimulants | RX4CAT1 %in% CNS_stimulants | RX5CAT1 %in% CNS_stimulants | RX6CAT1 %in% CNS_stimulants | RX7CAT1 %in% CNS_stimulants | RX8CAT1 %in% CNS_stimulants | RX9CAT1 %in% CNS_stimulants | RX10CAT1 %in% CNS_stimulants | RX11CAT1 %in% CNS_stimulants | RX12CAT1 %in% CNS_stimulants | RX13CAT1 %in% CNS_stimulants | RX14CAT1 %in% CNS_stimulants | RX15CAT1 %in% CNS_stimulants | RX16CAT1 %in% CNS_stimulants | RX17CAT1 %in% CNS_stimulants | RX18CAT1 %in% CNS_stimulants | RX19CAT1 %in% CNS_stimulants | RX20CAT1 %in% CNS_stimulants | RX21CAT1 %in% CNS_stimulants | RX22CAT1 %in% CNS_stimulants | RX23CAT1 %in% CNS_stimulants | RX24CAT1 %in% CNS_stimulants | RX25CAT1 %in% CNS_stimulants | RX26CAT1 %in% CNS_stimulants | RX27CAT1 %in% CNS_stimulants | RX28CAT1 %in% CNS_stimulants | RX29CAT1 %in% CNS_stimulants | RX30CAT1)) %>% 
  group_by(RACERETH) %>% 
  summarise(n=n())

stim2016 <- peds_data_2016 %>% 
  filter((RX1CAT1 %in% CNS_stimulants | RX2CAT1 %in% CNS_stimulants | RX3CAT1 %in% CNS_stimulants | RX4CAT1 %in% CNS_stimulants | RX5CAT1 %in% CNS_stimulants | RX6CAT1 %in% CNS_stimulants | RX7CAT1 %in% CNS_stimulants | RX8CAT1 %in% CNS_stimulants | RX9CAT1 %in% CNS_stimulants | RX10CAT1 %in% CNS_stimulants | RX11CAT1 %in% CNS_stimulants | RX12CAT1 %in% CNS_stimulants | RX13CAT1 %in% CNS_stimulants | RX14CAT1 %in% CNS_stimulants | RX15CAT1 %in% CNS_stimulants | RX16CAT1 %in% CNS_stimulants | RX17CAT1 %in% CNS_stimulants | RX18CAT1 %in% CNS_stimulants | RX19CAT1 %in% CNS_stimulants | RX20CAT1 %in% CNS_stimulants | RX21CAT1 %in% CNS_stimulants | RX22CAT1 %in% CNS_stimulants | RX23CAT1 %in% CNS_stimulants | RX24CAT1 %in% CNS_stimulants | RX25CAT1 %in% CNS_stimulants | RX26CAT1 %in% CNS_stimulants | RX27CAT1 %in% CNS_stimulants | RX28CAT1 %in% CNS_stimulants | RX29CAT1 %in% CNS_stimulants | RX30CAT1)) %>% 
  group_by(RACERETH) %>% 
  summarise(n=n())

## Get population data by race/ethnicity for each year
pop2019 <- peds_data_2019 %>% 
  group_by(RACERETH) %>% 
  summarise(n=n())

pop2018 <- peds_data_2018 %>% 
  group_by(RACERETH) %>% 
  summarise(n=n())

pop2016 <- peds_data_2016 %>% 
  group_by(RACERETH) %>% 
  summarise(n=n())

## Merge the yearly stimulant prescribing and population datasets
stim2019 <- cbind(stim2019,pop2019)
colnames(stim2019)[1] = "RaceEth"
colnames(stim2019)[2] = "prescrip"
colnames(stim2019)[4] = "pop"

stim2018 <- cbind(stim2018,pop2018) 
colnames(stim2018)[1] = "RaceEth"
colnames(stim2018)[2] = "prescrip"
colnames(stim2018)[4] = "pop"

stim2016 <- cbind(stim2016,pop2016)
colnames(stim2016)[1] = "RaceEth"
colnames(stim2016)[2] = "prescrip"
colnames(stim2016)[4] = "pop"

## Add years to the datasets, and calculate the rate of prescribing per 100 visits
stim_combined = rbind(stim2016,stim2018,stim2019)
years = c(2016,2016,2016,2016,2018,2018,2018,2018,2019,2019,2019,2019)
stim_combined <- cbind(stim_combined,years)
stim_rate_combined <- stim_combined %>% 
  mutate(rate = prescrip / pop *100)

## Plot the yearly rate of stimulant prescribing, stratified by race/ethnicity
stim_rate_combined
ggplot(stim_rate_combined, aes(x=years,y=rate,group=RACERETH)) +
  geom_line(aes(color=RACERETH),lwd=1) +
  geom_point(aes(color=RACERETH))+
  labs(x="Year", y="Rate of Stimulant Prescription per 100 visits",title = "Rate of Stimulant Prescribing (per 100 visits) by year and Race/Ethnicity")

## This is old code from calculating overall (non-stratified) rates

#stim_rate_2019 <- stim2019$n / count(peds_data_2019) *100
#stim_rate_2018 <- stim2018$n / count(peds_data_2018) *100
#stim_rate_2016 <- stim2016$n / count(peds_data_2016) *100

#stim_rate_trend <- rbind(stim_rate_2016,stim_rate_2018,stim_rate_2019)
#stim_rate_trend
#year <- c(2016,2018,2019)

#stim_rate_trend <- cbind(stim_rate_trend,year)
#stim_rate_trend

#ggplot(stim_rate_trend, aes(x=year, y=n)) +
# geom_bar(stat="identity") +
#labs(x = "Year", y = "Rate of Stimulant Prescriptions per 100 visits", title = "Rate of Stimulant Prescribing (per 100 Visits) by Year")


#### Weighting ####

svy.design <- svydesign(ids=~CPSUM, strata = ~CSTRATM, weights = ~PATWT, data=peds_data_2019)
svy.design

svytable(~ASTHMA, svy.design)
