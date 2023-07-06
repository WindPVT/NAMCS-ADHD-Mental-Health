#### LOAD PACKAGES ####
library(haven)
library(dplyr)
library(tidyverse)
library(tableone)
library(magrittr)
library(ggplot2)
library(survey)
library(ggsurvey)

# Prevent scientific notation
options(scipen=999)


#### READ CSV FILES ####
raw_data_2019 <- read.csv("Raw_Data/namcs2019.csv")
raw_data_2018 <- read.csv("Raw_Data/namcs2018.csv")
raw_data_2016 <- read.csv("Raw_Data/namcs2016.csv")
raw_data_2015 <- read.csv("Raw_Data/namcs2015.csv")
raw_data_2014 <- read.csv("Raw_Data/namcs2014.csv")
raw_data_2013 <- read.csv("Raw_Data/namcs2013.csv")
raw_data_2012 <- read.csv("Raw_Data/namcs2012.csv")
raw_data_2011 <- read.csv("Raw_Data/namcs2011.csv")
raw_data_2010 <- read.csv("Raw_Data/namcs2010.csv")
raw_data_2009 <- read.csv("Raw_Data/namcs2009.csv")

#### FILTER AND MERGE EACH YEAR'S DATASET ####
#note that medication codes 11-30 and diagnosis codes 4-5 are only for 2014 onwards

#2019
data_2019 <- raw_data_2019 %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,DIAG4,DIAG5,                                     # Diagnoses
         PRIMCARE, 
         PATWT, CSTRATM, CPSUM,
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,RX11CAT1,RX12CAT1,RX13CAT1,RX14CAT1,RX15CAT1,RX15CAT1,RX16CAT1,RX17CAT1,RX18CAT1,RX19CAT1,RX20CAT1,RX21CAT1,RX22CAT1,RX23CAT1,RX24CAT1,RX25CAT1,RX26CAT1,RX27CAT1,RX28CAT1,RX29CAT1,RX30CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,RX9CAT2,RX10CAT2,RX11CAT2,RX12CAT2,RX13CAT2,RX14CAT2,RX15CAT2,RX15CAT2,RX16CAT2,RX17CAT2,RX18CAT2,RX19CAT2,RX20CAT2,RX21CAT2,RX22CAT2,RX23CAT2,RX24CAT2,RX25CAT2,RX26CAT2,RX27CAT2,RX28CAT2,RX29CAT2,RX30CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30) %>% # Medications
  mutate(YEAR = 2019) %>% 
  droplevels() 

#2018
data_2018 <- raw_data_2018 %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,DIAG4,DIAG5,                                     # Diagnoses
         PRIMCARE, 
         PATWT, CSTRATM, CPSUM,
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,RX11CAT1,RX12CAT1,RX13CAT1,RX14CAT1,RX15CAT1,RX15CAT1,RX16CAT1,RX17CAT1,RX18CAT1,RX19CAT1,RX20CAT1,RX21CAT1,RX22CAT1,RX23CAT1,RX24CAT1,RX25CAT1,RX26CAT1,RX27CAT1,RX28CAT1,RX29CAT1,RX30CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,RX9CAT2,RX10CAT2,RX11CAT2,RX12CAT2,RX13CAT2,RX14CAT2,RX15CAT2,RX15CAT2,RX16CAT2,RX17CAT2,RX18CAT2,RX19CAT2,RX20CAT2,RX21CAT2,RX22CAT2,RX23CAT2,RX24CAT2,RX25CAT2,RX26CAT2,RX27CAT2,RX28CAT2,RX29CAT2,RX30CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30) %>% # Medications
  mutate(YEAR = 2018) %>% 
  droplevels() 

#2016
data_2016 <- raw_data_2016 %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,DIAG4,DIAG5,                                     # Diagnoses
         PRIMCARE, 
         PATWT, CSTRATM, CPSUM,
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,RX11CAT1,RX12CAT1,RX13CAT1,RX14CAT1,RX15CAT1,RX15CAT1,RX16CAT1,RX17CAT1,RX18CAT1,RX19CAT1,RX20CAT1,RX21CAT1,RX22CAT1,RX23CAT1,RX24CAT1,RX25CAT1,RX26CAT1,RX27CAT1,RX28CAT1,RX29CAT1,RX30CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,RX9CAT2,RX10CAT2,RX11CAT2,RX12CAT2,RX13CAT2,RX14CAT2,RX15CAT2,RX15CAT2,RX16CAT2,RX17CAT2,RX18CAT2,RX19CAT2,RX20CAT2,RX21CAT2,RX22CAT2,RX23CAT2,RX24CAT2,RX25CAT2,RX26CAT2,RX27CAT2,RX28CAT2,RX29CAT2,RX30CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30) %>% # Medications
  mutate(YEAR = 2016) %>% 
  droplevels() 

#2015
data_2015 <- raw_data_2015 %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,DIAG4,DIAG5,                                     # Diagnoses
         PRIMCARE,
         PATWT, CSTRATM, CPSUM,
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,RX11CAT1,RX12CAT1,RX13CAT1,RX14CAT1,RX15CAT1,RX15CAT1,RX16CAT1,RX17CAT1,RX18CAT1,RX19CAT1,RX20CAT1,RX21CAT1,RX22CAT1,RX23CAT1,RX24CAT1,RX25CAT1,RX26CAT1,RX27CAT1,RX28CAT1,RX29CAT1,RX30CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,RX9CAT2,RX10CAT2,RX11CAT2,RX12CAT2,RX13CAT2,RX14CAT2,RX15CAT2,RX15CAT2,RX16CAT2,RX17CAT2,RX18CAT2,RX19CAT2,RX20CAT2,RX21CAT2,RX22CAT2,RX23CAT2,RX24CAT2,RX25CAT2,RX26CAT2,RX27CAT2,RX28CAT2,RX29CAT2,RX30CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30) %>% # Medications
  mutate(YEAR = 2015) %>% 
  droplevels() 

#2014
data_2014 <- raw_data_2014 %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,DIAG4,DIAG5,                                     # Diagnoses
         PRIMCARE, 
         PATWT, CSTRATM, CPSUM,
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,RX11CAT1,RX12CAT1,RX13CAT1,RX14CAT1,RX15CAT1,RX15CAT1,RX16CAT1,RX17CAT1,RX18CAT1,RX19CAT1,RX20CAT1,RX21CAT1,RX22CAT1,RX23CAT1,RX24CAT1,RX25CAT1,RX26CAT1,RX27CAT1,RX28CAT1,RX29CAT1,RX30CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,RX9CAT2,RX10CAT2,RX11CAT2,RX12CAT2,RX13CAT2,RX14CAT2,RX15CAT2,RX15CAT2,RX16CAT2,RX17CAT2,RX18CAT2,RX19CAT2,RX20CAT2,RX21CAT2,RX22CAT2,RX23CAT2,RX24CAT2,RX25CAT2,RX26CAT2,RX27CAT2,RX28CAT2,RX29CAT2,RX30CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30) %>% # Medications
  mutate(YEAR = 2014) %>% 
  droplevels() 

#2013
data_2013 <- raw_data_2013 %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,                                                 # Diagnoses
         PRIMCARE, 
         PATWT, CSTRATM, CPSUM,
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8 ) %>%                      # Medications
  mutate(YEAR = 2013) %>% 
  droplevels() 

#2012
data_2012 <- raw_data_2012 %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,                                                 # Diagnoses
         PRIMCARE, 
         PATWT, CSTRATM, CPSUM,
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8 ) %>%                      # Medications
  mutate(YEAR = 2012) %>% 
  droplevels() 

#2011
data_2011 <- raw_data_2011 %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,                                                 # Diagnoses
         PRIMCARE, 
         PATWT, CSTRATM, CPSUM,
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8 ) %>%                      # Medications
  mutate(YEAR = 2011) %>% 
  droplevels() 

#2010
data_2010 <- raw_data_2010 %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,                                                 # Diagnoses
         PRIMCARE, 
         PATWT, CSTRATM, CPSUM,
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8 ) %>%                      # Medications
  mutate(YEAR = 2010) %>% 
  droplevels() 

#2009
data_2009 <- raw_data_2009 %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,                                                 # Diagnoses
         PRIMCARE, 
         PATWT, CSTRATM, CPSUM,
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8 ) %>%                      # Medications
  mutate(YEAR = 2009) %>% 
  droplevels() 

#merge each year's dataset
data_combined <- full_join(full_join(full_join(full_join(full_join(full_join(full_join(full_join(full_join(data_2019,data_2018),data_2016),data_2015),data_2014),data_2013),data_2012),data_2011),data_2010),data_2009)

#### RENAME NUMERICAL CODES OF CATEGORICAL VARIABLES ####

data_combined$RACERETH = factor(data_combined$RACERETH,
                                     levels = c(1,2,3,4),
                                     labels = c("Non-Hispanic White", 
                                                "Non-Hispanic Black", 
                                                "Hispanic", 
                                                "Non-Hispanic Other/Multiple Race"))

data_combined$SEX = factor(data_combined$SEX,
                                levels = c(1,2),
                                labels = c("Female", 
                                           "Male"))

data_combined$MSA = factor(data_combined$MSA,
                                levels = c(1,2),
                                labels = c("Metropolitan Statistical Area",
                                           "Non-Metropolitan Statistical Area"))

data_combined$PAYTYPER = factor(data_combined$PAYTYPER,
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

data_combined$MAJOR = factor(data_combined$MAJOR,
                                  levels = c(-9,1,2,3,4,5,6),
                                  labels = c("Blank",
                                             "New problem (<3 mos. onset)",
                                             "Chronic Problem, Routine",
                                             "Chronic Problem, Flare-up",
                                             "Pre Surgery",
                                             "Post Surgery",
                                             "Preventive Care"))

data_combined$PRIMCARE = factor(data_combined$PRIMCARE,
                                     levels = c(-9,-8,1,2),
                                     labels = c("Blank",
                                                "Unknown",
                                                "Yes",
                                                "No"))

#convert continuous to numeric
data_combined <- data_combined %>% 
  mutate(YEAR = as.numeric(YEAR), AGE = as.numeric(AGE))

#group ages
data_combined <- data_combined %>% 
  mutate(AGE_GROUPED = cut(AGE, 
                           breaks=c(-Inf,18,65,Inf),
                           labels=c("<18","18-65",">65")))

#### ADD VARIABLES FOR ADHD DIAGNOSIS AND MEDICATIONS ####

#define ICD-9 and ICD-10 codes for ADHD
ADHD_ICD_codes <- c("F900","F901","F902","F908","F909", #these are ICD-10 codes
                    "314--","3140-","31400","31401","3141-","31410","3142-","31420","3148-","31480","3149-","31490") #these are ICD-9 codes

#define Multum drug category code for stimulants
ADHD_stimulant_codes <- c(71)

#define Multum drugID codes for non-stimulant ADHD medications (list of medications from: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3000197/)
ADHD_non_stimulant_codes <- c("d00259", #imipramine
                              "d00145", #desipramine
                              "d00144", #nortriptyline
                              "d00181", #bupropion
                              "d03181", #venlafaxine
                              "d04827", #atomoxetine
                              "d00044", #clonidine
                              "d00717", #guanfacine
                              "d04378", #modafinil
                              "d00032", #propranolol
                              "d00018", #nadolol
                              "d00058", #carbamazepine
                              "d00182") #buspirone

#add yes/no variables for ADHD diagnosis, stimulant prescription,  non-stimulant prescription, and both stimulant and non-stimulant prescription
data_combined <- data_combined %>% 
  mutate(ADHD = case_when(DIAG1 %in% ADHD_ICD_codes | DIAG2 %in% ADHD_ICD_codes | DIAG3 %in% ADHD_ICD_codes | DIAG4 %in% ADHD_ICD_codes | DIAG5 %in% ADHD_ICD_codes ~"Y",
         .default = "N")) %>% 
  mutate(STIMULANT = case_when((RX1CAT1 %in% ADHD_stimulant_codes | RX2CAT1 %in% ADHD_stimulant_codes | RX3CAT1 %in% ADHD_stimulant_codes | RX4CAT1 %in% ADHD_stimulant_codes | RX5CAT1 %in% ADHD_stimulant_codes | RX6CAT1 %in% ADHD_stimulant_codes | RX7CAT1 %in% ADHD_stimulant_codes | RX8CAT1 %in% ADHD_stimulant_codes | RX9CAT1 %in% ADHD_stimulant_codes | RX10CAT1 %in% ADHD_stimulant_codes | RX11CAT1 %in% ADHD_stimulant_codes | RX12CAT1 %in% ADHD_stimulant_codes | RX13CAT1 %in% ADHD_stimulant_codes | RX14CAT1 %in% ADHD_stimulant_codes | RX15CAT1 %in% ADHD_stimulant_codes | RX15CAT1 %in% ADHD_stimulant_codes | RX16CAT1 %in% ADHD_stimulant_codes | RX17CAT1 %in% ADHD_stimulant_codes | RX18CAT1 %in% ADHD_stimulant_codes | RX19CAT1 %in% ADHD_stimulant_codes | RX20CAT1 %in% ADHD_stimulant_codes | RX21CAT1 %in% ADHD_stimulant_codes | RX22CAT1 %in% ADHD_stimulant_codes | RX23CAT1 %in% ADHD_stimulant_codes | RX24CAT1 %in% ADHD_stimulant_codes | RX25CAT1 %in% ADHD_stimulant_codes | RX26CAT1 %in% ADHD_stimulant_codes | RX27CAT1 %in% ADHD_stimulant_codes | RX28CAT1 %in% ADHD_stimulant_codes | RX29CAT1 %in% ADHD_stimulant_codes | RX30CAT1 %in% ADHD_stimulant_codes |  
                                RX1CAT2 %in% ADHD_stimulant_codes | RX2CAT2 %in% ADHD_stimulant_codes | RX3CAT2 %in% ADHD_stimulant_codes | RX4CAT2 %in% ADHD_stimulant_codes | RX5CAT2 %in% ADHD_stimulant_codes | RX6CAT2 %in% ADHD_stimulant_codes | RX7CAT2 %in% ADHD_stimulant_codes | RX8CAT2 %in% ADHD_stimulant_codes | RX9CAT2 %in% ADHD_stimulant_codes | RX10CAT2 %in% ADHD_stimulant_codes | RX11CAT2 %in% ADHD_stimulant_codes | RX12CAT2 %in% ADHD_stimulant_codes | RX13CAT2 %in% ADHD_stimulant_codes | RX14CAT2 %in% ADHD_stimulant_codes | RX15CAT2 %in% ADHD_stimulant_codes | RX15CAT2 %in% ADHD_stimulant_codes | RX16CAT2 %in% ADHD_stimulant_codes | RX17CAT2 %in% ADHD_stimulant_codes | RX18CAT2 %in% ADHD_stimulant_codes | RX19CAT2 %in% ADHD_stimulant_codes | RX20CAT2 %in% ADHD_stimulant_codes | RX21CAT2 %in% ADHD_stimulant_codes | RX22CAT2 %in% ADHD_stimulant_codes | RX23CAT2 %in% ADHD_stimulant_codes | RX24CAT2 %in% ADHD_stimulant_codes | RX25CAT2 %in% ADHD_stimulant_codes | RX26CAT2 %in% ADHD_stimulant_codes | RX27CAT2 %in% ADHD_stimulant_codes | RX28CAT2 %in% ADHD_stimulant_codes | RX29CAT2 %in% ADHD_stimulant_codes | RX30CAT2) 
                               & !(DRUGID1 %in% ADHD_non_stimulant_codes | DRUGID2 %in% ADHD_non_stimulant_codes | DRUGID3 %in% ADHD_non_stimulant_codes | DRUGID4 %in% ADHD_non_stimulant_codes | DRUGID5 %in% ADHD_non_stimulant_codes | DRUGID6 %in% ADHD_non_stimulant_codes | DRUGID7 %in% ADHD_non_stimulant_codes | DRUGID8 %in% ADHD_non_stimulant_codes | DRUGID9 %in% ADHD_non_stimulant_codes | DRUGID10 %in% ADHD_non_stimulant_codes | DRUGID11 %in% ADHD_non_stimulant_codes | DRUGID12 %in% ADHD_non_stimulant_codes | DRUGID13 %in% ADHD_non_stimulant_codes | DRUGID14 %in% ADHD_non_stimulant_codes | DRUGID15 %in% ADHD_non_stimulant_codes | DRUGID16 %in% ADHD_non_stimulant_codes | DRUGID17 %in% ADHD_non_stimulant_codes | DRUGID18 %in% ADHD_non_stimulant_codes | DRUGID19 %in% ADHD_non_stimulant_codes | DRUGID20 %in% ADHD_non_stimulant_codes | DRUGID21 %in% ADHD_non_stimulant_codes | DRUGID22 %in% ADHD_non_stimulant_codes | DRUGID23 %in% ADHD_non_stimulant_codes | DRUGID24 %in% ADHD_non_stimulant_codes | DRUGID25 %in% ADHD_non_stimulant_codes | DRUGID26 %in% ADHD_non_stimulant_codes | DRUGID27 %in% ADHD_non_stimulant_codes | DRUGID28 %in% ADHD_non_stimulant_codes | DRUGID29 %in% ADHD_non_stimulant_codes | DRUGID30 %in% ADHD_non_stimulant_codes) ~"Y",
                               .default = "N")) %>%  
  mutate(NON_STIM = case_when((DRUGID1 %in% ADHD_non_stimulant_codes | DRUGID2 %in% ADHD_non_stimulant_codes | DRUGID3 %in% ADHD_non_stimulant_codes | DRUGID4 %in% ADHD_non_stimulant_codes | DRUGID5 %in% ADHD_non_stimulant_codes | DRUGID6 %in% ADHD_non_stimulant_codes | DRUGID7 %in% ADHD_non_stimulant_codes | DRUGID8 %in% ADHD_non_stimulant_codes | DRUGID9 %in% ADHD_non_stimulant_codes | DRUGID10 %in% ADHD_non_stimulant_codes | DRUGID11 %in% ADHD_non_stimulant_codes | DRUGID12 %in% ADHD_non_stimulant_codes | DRUGID13 %in% ADHD_non_stimulant_codes | DRUGID14 %in% ADHD_non_stimulant_codes | DRUGID15 %in% ADHD_non_stimulant_codes | DRUGID16 %in% ADHD_non_stimulant_codes | DRUGID17 %in% ADHD_non_stimulant_codes | DRUGID18 %in% ADHD_non_stimulant_codes | DRUGID19 %in% ADHD_non_stimulant_codes | DRUGID20 %in% ADHD_non_stimulant_codes | DRUGID21 %in% ADHD_non_stimulant_codes | DRUGID22 %in% ADHD_non_stimulant_codes | DRUGID23 %in% ADHD_non_stimulant_codes | DRUGID24 %in% ADHD_non_stimulant_codes | DRUGID25 %in% ADHD_non_stimulant_codes | DRUGID26 %in% ADHD_non_stimulant_codes | DRUGID27 %in% ADHD_non_stimulant_codes | DRUGID28 %in% ADHD_non_stimulant_codes | DRUGID29 %in% ADHD_non_stimulant_codes | DRUGID30 %in% ADHD_non_stimulant_codes)
                              & (STIMULANT == "N") ~"Y",
                              .default = "N")) %>% 
  mutate(BOTH = case_when((RX1CAT1 %in% ADHD_stimulant_codes | RX2CAT1 %in% ADHD_stimulant_codes | RX3CAT1 %in% ADHD_stimulant_codes | RX4CAT1 %in% ADHD_stimulant_codes | RX5CAT1 %in% ADHD_stimulant_codes | RX6CAT1 %in% ADHD_stimulant_codes | RX7CAT1 %in% ADHD_stimulant_codes | RX8CAT1 %in% ADHD_stimulant_codes | RX9CAT1 %in% ADHD_stimulant_codes | RX10CAT1 %in% ADHD_stimulant_codes | RX11CAT1 %in% ADHD_stimulant_codes | RX12CAT1 %in% ADHD_stimulant_codes | RX13CAT1 %in% ADHD_stimulant_codes | RX14CAT1 %in% ADHD_stimulant_codes | RX15CAT1 %in% ADHD_stimulant_codes | RX15CAT1 %in% ADHD_stimulant_codes | RX16CAT1 %in% ADHD_stimulant_codes | RX17CAT1 %in% ADHD_stimulant_codes | RX18CAT1 %in% ADHD_stimulant_codes | RX19CAT1 %in% ADHD_stimulant_codes | RX20CAT1 %in% ADHD_stimulant_codes | RX21CAT1 %in% ADHD_stimulant_codes | RX22CAT1 %in% ADHD_stimulant_codes | RX23CAT1 %in% ADHD_stimulant_codes | RX24CAT1 %in% ADHD_stimulant_codes | RX25CAT1 %in% ADHD_stimulant_codes | RX26CAT1 %in% ADHD_stimulant_codes | RX27CAT1 %in% ADHD_stimulant_codes | RX28CAT1 %in% ADHD_stimulant_codes | RX29CAT1 %in% ADHD_stimulant_codes | RX30CAT1 %in% ADHD_stimulant_codes |  
                             RX1CAT2 %in% ADHD_stimulant_codes | RX2CAT2 %in% ADHD_stimulant_codes | RX3CAT2 %in% ADHD_stimulant_codes | RX4CAT2 %in% ADHD_stimulant_codes | RX5CAT2 %in% ADHD_stimulant_codes | RX6CAT2 %in% ADHD_stimulant_codes | RX7CAT2 %in% ADHD_stimulant_codes | RX8CAT2 %in% ADHD_stimulant_codes | RX9CAT2 %in% ADHD_stimulant_codes | RX10CAT2 %in% ADHD_stimulant_codes | RX11CAT2 %in% ADHD_stimulant_codes | RX12CAT2 %in% ADHD_stimulant_codes | RX13CAT2 %in% ADHD_stimulant_codes | RX14CAT2 %in% ADHD_stimulant_codes | RX15CAT2 %in% ADHD_stimulant_codes | RX15CAT2 %in% ADHD_stimulant_codes | RX16CAT2 %in% ADHD_stimulant_codes | RX17CAT2 %in% ADHD_stimulant_codes | RX18CAT2 %in% ADHD_stimulant_codes | RX19CAT2 %in% ADHD_stimulant_codes | RX20CAT2 %in% ADHD_stimulant_codes | RX21CAT2 %in% ADHD_stimulant_codes | RX22CAT2 %in% ADHD_stimulant_codes | RX23CAT2 %in% ADHD_stimulant_codes | RX24CAT2 %in% ADHD_stimulant_codes | RX25CAT2 %in% ADHD_stimulant_codes | RX26CAT2 %in% ADHD_stimulant_codes | RX27CAT2 %in% ADHD_stimulant_codes | RX28CAT2 %in% ADHD_stimulant_codes | RX29CAT2 %in% ADHD_stimulant_codes | RX30CAT2) 
                          & (DRUGID1 %in% ADHD_non_stimulant_codes | DRUGID2 %in% ADHD_non_stimulant_codes | DRUGID3 %in% ADHD_non_stimulant_codes | DRUGID4 %in% ADHD_non_stimulant_codes | DRUGID5 %in% ADHD_non_stimulant_codes | DRUGID6 %in% ADHD_non_stimulant_codes | DRUGID7 %in% ADHD_non_stimulant_codes | DRUGID8 %in% ADHD_non_stimulant_codes | DRUGID9 %in% ADHD_non_stimulant_codes | DRUGID10 %in% ADHD_non_stimulant_codes | DRUGID11 %in% ADHD_non_stimulant_codes | DRUGID12 %in% ADHD_non_stimulant_codes | DRUGID13 %in% ADHD_non_stimulant_codes | DRUGID14 %in% ADHD_non_stimulant_codes | DRUGID15 %in% ADHD_non_stimulant_codes | DRUGID16 %in% ADHD_non_stimulant_codes | DRUGID17 %in% ADHD_non_stimulant_codes | DRUGID18 %in% ADHD_non_stimulant_codes | DRUGID19 %in% ADHD_non_stimulant_codes | DRUGID20 %in% ADHD_non_stimulant_codes | DRUGID21 %in% ADHD_non_stimulant_codes | DRUGID22 %in% ADHD_non_stimulant_codes | DRUGID23 %in% ADHD_non_stimulant_codes | DRUGID24 %in% ADHD_non_stimulant_codes | DRUGID25 %in% ADHD_non_stimulant_codes | DRUGID26 %in% ADHD_non_stimulant_codes | DRUGID27 %in% ADHD_non_stimulant_codes | DRUGID28 %in% ADHD_non_stimulant_codes | DRUGID29 %in% ADHD_non_stimulant_codes | DRUGID30 %in% ADHD_non_stimulant_codes) ~"Y",
                          .default = "N")) %>% 
  mutate(ANY_ADHD_MED = case_when((STIMULANT == "Y" | NON_STIM == "Y" | BOTH == "Y") ~"Y",
                                  .default = "N"))


#### APPLY WEIGHTING ####

weighting_design_namcs <- svydesign(id=~CPSUM, strata=~CSTRATM, weight=~PATWT,data=data_combined,nest=TRUE)


#### CREATE FILTERED DATABASES ####

age_lim_weighted <-subset(weighting_design_namcs,(AGE>=5 & AGE<65))
ADHD_weighted <- subset(age_lim_weighted,ADHD=="Y")

#### CREATE AN OVERALL COHORT SUMMARY TABLE ####

svytable(~AGE_GROUPED, design=age_lim_weighted)
svytable(~YEAR, design=age_lim_weighted)
svytable(~RACERETH, design=age_lim_weighted)
svytable(~SEX, design=age_lim_weighted)
svytable(~MSA, design=age_lim_weighted)
svytable(~PAYTYPER, design=age_lim_weighted)
svytable(~MAJOR, design=age_lim_weighted)

#### NUMBER OF VISITS WITH AN ADHD DIAGNOSIS, BY SELECTED CHARACTERISTICS ####

adhd_by_sex <- svyby(~ADHD, ~SEX, age_lim_weighted, na=TRUE, svytotal)
ggplot(adhd_by_sex, aes(x=SEX, y=ADHDY))+
  geom_bar(stat="identity")

adhd_by_racereth <- svyby(~ADHD, ~RACERETH, age_lim_weighted, na=TRUE, svytotal)
ggplot(adhd_by_racereth, aes(x=RACERETH, y=ADHDY))+
  geom_bar(stat="identity")

adhd_by_msa <- svyby(~ADHD, ~MSA, age_lim_weighted, na=TRUE, svytotal)
ggplot(adhd_by_msa, aes(x=MSA, y=ADHDY))+
  geom_bar(stat="identity")

adhd_by_paytyper <- svyby(~ADHD, ~PAYTYPER, age_lim_weighted, na=TRUE, svytotal)
ggplot(adhd_by_paytyper, aes(x=PAYTYPER, y=ADHDY))+
  geom_bar(stat="identity")

adhd_by_age <- svyby(~ADHD, ~AGE_GROUPED, age_lim_weighted, na=TRUE, svytotal)
ggplot(adhd_by_age, aes(x=AGE_GROUPED, y=ADHDY))+
  geom_bar(stat="identity")

adhd_by_year <- svyby(~ADHD, ~YEAR+AGE_GROUPED, age_lim_weighted, na=TRUE, svymean) 
adhd_by_year <- cbind(adhd_by_year, confint(adhd_by_year)[21:40,])
colnames(adhd_by_year) <- c("YEAR", "AGE", "ADHDY", "ADHDN", "seN", "seY", "LOWER_CI", "UPPER_CI")
ggplot(adhd_by_year, aes(x=YEAR, y=ADHDY))+
  geom_line(aes(color=AGE))+
  geom_point(aes(color=AGE))+
  geom_errorbar(aes(ymin=LOWER_CI,ymax=UPPER_CI))+
  labs(title = "Proportion of Total Visits with an ADHD Diagnosis, by Age Group", y = "Proportion of Total Visits With an ADHD Diagnosis Code", x = "Year")


#### PROPORTION OF ADHD VISITS WITH ADHD MEDICATION PRESCRIBED, BY YEAR ####

stim_by_year <- svyby(~STIMULANT, ~YEAR, ADHD_weighted, na=TRUE, svymean) 
stim_by_year <- cbind(stim_by_year,confint(stim_by_year)[11:20,]) %>% 
  mutate(CLASS = "STIM") 
colnames(stim_by_year) <- c("YEAR","N","Y","se_Y","se_N","LOWER_CI", "UPPER_CI", "CLASS")

non_stim_by_year <- svyby(~NON_STIM, ~YEAR, ADHD_weighted, na=TRUE, svymean)
non_stim_by_year <- cbind(non_stim_by_year,confint(non_stim_by_year)[11:20,]) %>% 
  mutate(CLASS = "NON_STIM") 
colnames(non_stim_by_year) <- c("YEAR","N","Y","se_Y","se_N","LOWER_CI", "UPPER_CI", "CLASS")

both_by_year <- svyby(~BOTH, ~YEAR, ADHD_weighted, na=TRUE, svymean)
both_by_year <- cbind(both_by_year,confint(both_by_year)[11:20,]) %>% 
  mutate(CLASS = "BOTH") 
colnames(both_by_year) <- c("YEAR","N","Y","se_Y","se_N","LOWER_CI", "UPPER_CI", "CLASS")

any_by_year <- svyby(~ANY_ADHD_MED, ~YEAR, ADHD_weighted, na=TRUE, svymean)
any_by_year <- cbind(any_by_year,confint(any_by_year)[11:20,]) %>% 
  mutate(CLASS = "ANY") 
colnames(any_by_year) <- c("YEAR","N","Y","se_Y","se_N","LOWER_CI", "UPPER_CI", "CLASS")

yearly_rx <- rbind(stim_by_year,non_stim_by_year,both_by_year,any_by_year) 

ggplot(data=yearly_rx, aes(x=YEAR, y=Y, color = CLASS)) +
  geom_line() + 
  geom_point() +
  geom_ribbon(aes(ymin=LOWER_CI, ymax=UPPER_CI, fill = CLASS), 
               alpha=0.2) +
  labs(title = "ADHD Prescribing Among 5-18 yos", x = "Year", y = "% of ADHD Visits with Medication Prescribed")
  



#### ANY ADHD PRESCRIBING BY SELECTED CHARS ####
any_by_racereth <- svyby(~ANY_ADHD_MED, ~RACERETH, ADHD_weighted, na=TRUE, svymean)
any_by_racereth <- cbind(any_by_racereth,confint(any_by_racereth)[5:8,])
colnames(any_by_racereth) <- c("RACERETH","N","Y","se_Y","se_N","LOWER_CI", "UPPER_CI")

ggplot(data=any_by_racereth, aes(x=RACERETH,y=Y))+
  geom_bar(stat="identity")+
  geom_point()+
  geom_errorbar(aes(ymin=LOWER_CI,ymax=UPPER_CI),width=0.5)



any_by_paytyper <- svyby(~ANY_ADHD_MED, ~PAYTYPER, ADHD_weighted, na=TRUE, svymean)
any_by_paytyper <- cbind(any_by_paytyper,confint(any_by_paytyper)[10:18,])
colnames(any_by_paytyper) <- c("PAYTYPER","N","Y","se_Y","se_N","LOWER_CI", "UPPER_CI")

ggplot(data=any_by_paytyper, aes(x=PAYTYPER,y=Y))+
  geom_bar(stat="identity")+
  geom_point()+
  geom_errorbar(aes(ymin=LOWER_CI,ymax=UPPER_CI),width=0.5)




#### OLD UNWEIGHTED ####


# function that outputs a table with the proportion of ADHD visits at which a stimulant, non-stimulant, or both were prescribed, stratified by a selected characteristic
ADHD_Rx_By1 <- function(characteristic) {
  
  ADHD_visits_by <- ADHD_dataset %>% 
    group_by(!!as.name(characteristic)) %>% 
    summarise(n=n())
  colnames(ADHD_visits_by) <- c(characteristic, "ADHD_VISITS")
  
  any_rx_by <- ADHD_dataset %>% 
    filter(ANY_ADHD_MED == 1) %>% 
    mutate(CLASS = "ANY") %>% 
    group_by(!!as.name(characteristic),CLASS) %>% 
    summarise(n=n())
  colnames(any_rx_by) <- c(characteristic, "CLASS", "RX")
  any_rx_by <- merge(any_rx_by,ADHD_visits_by,all=TRUE) %>% 
    mutate(PROP = RX/ADHD_VISITS*100)
  
  stim_rx_by <- ADHD_dataset %>% 
    filter(STIMULANT == 1) %>% 
    filter(is.na(NON_STIM)) %>% 
    mutate(CLASS = "STIM") %>% 
    group_by(!!as.name(characteristic),CLASS) %>% 
    summarise(n=n())
  colnames(stim_rx_by) <- c(characteristic, "CLASS", "RX")
  stim_rx_by <- merge(stim_rx_by,ADHD_visits_by,all=TRUE) %>% 
    mutate(PROP = RX/ADHD_VISITS*100)
  
  non_stim_rx_by <- ADHD_dataset %>% 
    filter(NON_STIM == 1) %>% 
    filter(is.na(STIMULANT)) %>% 
    mutate(CLASS = "NON_STIM") %>% 
    group_by(!!as.name(characteristic),CLASS) %>% 
    summarise(n=n())
  colnames(non_stim_rx_by) <- c(characteristic, "CLASS", "RX")
  non_stim_rx_by <- merge(non_stim_rx_by,ADHD_visits_by,all=TRUE) %>% 
    mutate(PROP = RX/ADHD_VISITS*100)
  
  both_rx_by <- ADHD_dataset %>% 
    filter(BOTH == 1) %>% 
    mutate(CLASS = "BOTH") %>% 
    group_by(!!as.name(characteristic), CLASS) %>%
    summarise(n=n())
  colnames(both_rx_by) <- c(characteristic, "CLASS", "RX")
  both_rx_by <- merge(both_rx_by,ADHD_visits_by,all=TRUE) %>% 
    mutate(PROP = RX/ADHD_VISITS*100)
  
  prop_ADHD_visits_rx_by <- rbind(stim_rx_by,non_stim_rx_by,both_rx_by,any_rx_by)
  prop_ADHD_visits_rx_by
  
}

# line graph of ADHD medication prescribing by year
ADHD_rx_by_year <- ADHD_Rx_By1("YEAR") 
ggplot(ADHD_rx_by_year, aes(x=YEAR,y=PROP,colour=CLASS)) +
  geom_line(lwd = 1.5) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)) +
  labs(x="YEAR", y="PERCENTAGE OF ADHD VISITS")

# stacked bar graph of ADHD medication prescribing by sex
ADHD_rx_by_sex <- ADHD_Rx_By1("SEX") %>% 
  filter(CLASS != "ANY")
ggplot(ADHD_rx_by_sex, aes(x=SEX,y=PROP,fill=CLASS)) +
  geom_bar(position = "stack", stat = "identity")

# stacked bar graph of ADHD medication prescribing by race/ethnicity
ADHD_rx_by_racereth <- ADHD_Rx_By1("RACERETH") %>% 
  filter(CLASS != "ANY")
ggplot(ADHD_rx_by_racereth, aes(x=RACERETH,y=PROP,fill=CLASS)) +
  geom_bar(position = "stack", stat = "identity")

# stacked bar graph of ADHD medication prescribing by MSA
ADHD_rx_by_MSA <- ADHD_Rx_By1("MSA") %>% 
  filter(CLASS != "ANY")
ggplot(ADHD_rx_by_MSA, aes(x=MSA,y=PROP,fill=CLASS)) +
  geom_bar(position = "stack", stat = "identity")

# stacked bar graph of ADHD medication prescribing by payer
ADHD_rx_by_paytyper <- ADHD_Rx_By1("PAYTYPER") %>% 
  filter(CLASS != "ANY")
ggplot(ADHD_rx_by_paytyper, aes(x=PAYTYPER,y=PROP,fill=CLASS)) +
  geom_bar(position = "stack", stat = "identity")

# stacked bar graph of ADHD medication prescribing by age
ADHD_rx_by_age <- ADHD_Rx_By1("AGE") %>% 
  filter(CLASS != "ANY")
ggplot(ADHD_rx_by_age, aes(x=AGE,y=PROP,fill=CLASS)) +
  geom_bar(position = "stack", stat = "identity")

#  stacked bar graph of ADHD medication prescribing by primary care (y/n)
ADHD_rx_by_primcare <- ADHD_Rx_By1("PRIMCARE") %>% 
  filter(CLASS != "ANY")
ggplot(ADHD_rx_by_primcare, aes(x=PRIMCARE,y=PROP,fill=CLASS)) +
  geom_bar(position = "stack", stat = "identity")

# GRAPH ADHD VISITS AND PRESCRIBING AS A PERCENTAGE OF ALL VISITS #

all_visits_yearly <- data_combined %>% 
  group_by(YEAR) %>% 
  summarise(n=n()) 
colnames(all_visits_yearly) <- c("YEAR", "ALL_VISITS")

adhd_visits_yearly <- data_combined %>% 
  filter(ADHD == 1) %>% 
  mutate(CLASS = "ADHD_DX") %>% 
  group_by(YEAR,CLASS) %>% 
  summarise(n=n())
colnames(adhd_visits_yearly) <- c("YEAR","CLASS","RX")
adhd_visits_yearly <- merge(adhd_visits_yearly,all_visits_yearly,all=TRUE) %>% 
  mutate(PROP = RX/ALL_VISITS*100)

all_stim_yearly <- data_combined %>% 
  filter(STIMULANT == 1) %>% 
  mutate(CLASS = "STIM_RX") %>% 
  group_by(YEAR,CLASS) %>% 
  summarise(n=n())
colnames(all_stim_yearly) <- c("YEAR","CLASS","RX")
all_stim_yearly <- merge(all_stim_yearly,all_visits_yearly,all=TRUE) %>% 
  mutate(PROP = RX/ALL_VISITS*100)

all_non_stim_yearly <- data_combined %>% 
  filter(NON_STIM == 1) %>% 
  mutate(CLASS = "NON_STIM_RX") %>% 
  group_by(YEAR,CLASS) %>% 
  summarise(n=n())
colnames(all_non_stim_yearly) <- c("YEAR","CLASS","RX")
all_non_stim_yearly <- merge(all_non_stim_yearly,all_visits_yearly,all=TRUE) %>% 
  mutate(PROP = RX/ALL_VISITS*100)

all_both_yearly <- data_combined %>% 
  filter(BOTH == 1) %>% 
  mutate(CLASS = "BOTH") %>% 
  group_by(YEAR,CLASS) %>% 
  summarise(n=n())
colnames(all_both_yearly) <- c("YEAR","CLASS","RX")
all_both_yearly <- merge(all_both_yearly,all_visits_yearly,all=TRUE) %>% 
  mutate(PROP = RX/ALL_VISITS*100)

yearly_rx_and_dx <- rbind(adhd_visits_yearly,all_stim_yearly,all_non_stim_yearly,all_both_yearly)

ggplot(yearly_rx_and_dx, aes(x=YEAR,y=PROP,colour=CLASS)) +
  geom_line(lwd = 1.5) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)) +
  labs(x="YEAR", y="PERCENTAGE OF ALL VISITS")
