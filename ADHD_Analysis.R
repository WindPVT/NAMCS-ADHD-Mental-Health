#### LOAD PACKAGES ####
library(haven)
library(dplyr)
library(tidyverse)
library(tableone)
library(magrittr)
library(ggplot2)


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
peds_data_2019 <- raw_data_2019 %>% 
  filter(AGE <= 18) %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,DIAG4,DIAG5,                                     # Diagnoses
         PRIMCARE, 
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,RX11CAT1,RX12CAT1,RX13CAT1,RX14CAT1,RX15CAT1,RX15CAT1,RX16CAT1,RX17CAT1,RX18CAT1,RX19CAT1,RX20CAT1,RX21CAT1,RX22CAT1,RX23CAT1,RX24CAT1,RX25CAT1,RX26CAT1,RX27CAT1,RX28CAT1,RX29CAT1,RX30CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,RX9CAT2,RX10CAT2,RX11CAT2,RX12CAT2,RX13CAT2,RX14CAT2,RX15CAT2,RX15CAT2,RX16CAT2,RX17CAT2,RX18CAT2,RX19CAT2,RX20CAT2,RX21CAT2,RX22CAT2,RX23CAT2,RX24CAT2,RX25CAT2,RX26CAT2,RX27CAT2,RX28CAT2,RX29CAT2,RX30CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30) %>% # Medications
  mutate(YEAR = 2019) %>% 
  droplevels() 

#2018
peds_data_2018 <- raw_data_2018 %>% 
  filter(AGE <= 18) %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,DIAG4,DIAG5,                                     # Diagnoses
         PRIMCARE, 
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,RX11CAT1,RX12CAT1,RX13CAT1,RX14CAT1,RX15CAT1,RX15CAT1,RX16CAT1,RX17CAT1,RX18CAT1,RX19CAT1,RX20CAT1,RX21CAT1,RX22CAT1,RX23CAT1,RX24CAT1,RX25CAT1,RX26CAT1,RX27CAT1,RX28CAT1,RX29CAT1,RX30CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,RX9CAT2,RX10CAT2,RX11CAT2,RX12CAT2,RX13CAT2,RX14CAT2,RX15CAT2,RX15CAT2,RX16CAT2,RX17CAT2,RX18CAT2,RX19CAT2,RX20CAT2,RX21CAT2,RX22CAT2,RX23CAT2,RX24CAT2,RX25CAT2,RX26CAT2,RX27CAT2,RX28CAT2,RX29CAT2,RX30CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30) %>% # Medications
  mutate(YEAR = 2018) %>% 
  droplevels() 

#2016
peds_data_2016 <- raw_data_2016 %>% 
  filter(AGE <= 18) %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,DIAG4,DIAG5,                                     # Diagnoses
         PRIMCARE, 
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,RX11CAT1,RX12CAT1,RX13CAT1,RX14CAT1,RX15CAT1,RX15CAT1,RX16CAT1,RX17CAT1,RX18CAT1,RX19CAT1,RX20CAT1,RX21CAT1,RX22CAT1,RX23CAT1,RX24CAT1,RX25CAT1,RX26CAT1,RX27CAT1,RX28CAT1,RX29CAT1,RX30CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,RX9CAT2,RX10CAT2,RX11CAT2,RX12CAT2,RX13CAT2,RX14CAT2,RX15CAT2,RX15CAT2,RX16CAT2,RX17CAT2,RX18CAT2,RX19CAT2,RX20CAT2,RX21CAT2,RX22CAT2,RX23CAT2,RX24CAT2,RX25CAT2,RX26CAT2,RX27CAT2,RX28CAT2,RX29CAT2,RX30CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30) %>% # Medications
  mutate(YEAR = 2016) %>% 
  droplevels() 

#2015
peds_data_2015 <- raw_data_2015 %>% 
  filter(AGE <= 18) %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,DIAG4,DIAG5,                                     # Diagnoses
         PRIMCARE, 
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,RX11CAT1,RX12CAT1,RX13CAT1,RX14CAT1,RX15CAT1,RX15CAT1,RX16CAT1,RX17CAT1,RX18CAT1,RX19CAT1,RX20CAT1,RX21CAT1,RX22CAT1,RX23CAT1,RX24CAT1,RX25CAT1,RX26CAT1,RX27CAT1,RX28CAT1,RX29CAT1,RX30CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,RX9CAT2,RX10CAT2,RX11CAT2,RX12CAT2,RX13CAT2,RX14CAT2,RX15CAT2,RX15CAT2,RX16CAT2,RX17CAT2,RX18CAT2,RX19CAT2,RX20CAT2,RX21CAT2,RX22CAT2,RX23CAT2,RX24CAT2,RX25CAT2,RX26CAT2,RX27CAT2,RX28CAT2,RX29CAT2,RX30CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30) %>% # Medications
  mutate(YEAR = 2015) %>% 
  droplevels() 

#2014
peds_data_2014 <- raw_data_2014 %>% 
  filter(AGE <= 18) %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,DIAG4,DIAG5,                                     # Diagnoses
         PRIMCARE, 
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,RX11CAT1,RX12CAT1,RX13CAT1,RX14CAT1,RX15CAT1,RX15CAT1,RX16CAT1,RX17CAT1,RX18CAT1,RX19CAT1,RX20CAT1,RX21CAT1,RX22CAT1,RX23CAT1,RX24CAT1,RX25CAT1,RX26CAT1,RX27CAT1,RX28CAT1,RX29CAT1,RX30CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,RX9CAT2,RX10CAT2,RX11CAT2,RX12CAT2,RX13CAT2,RX14CAT2,RX15CAT2,RX15CAT2,RX16CAT2,RX17CAT2,RX18CAT2,RX19CAT2,RX20CAT2,RX21CAT2,RX22CAT2,RX23CAT2,RX24CAT2,RX25CAT2,RX26CAT2,RX27CAT2,RX28CAT2,RX29CAT2,RX30CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30) %>% # Medications
  mutate(YEAR = 2014) %>% 
  droplevels() 

#2013
peds_data_2013 <- raw_data_2013 %>% 
  filter(AGE <= 18) %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,                                                 # Diagnoses
         PRIMCARE, 
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8 ) %>%                      # Medications
  mutate(YEAR = 2013) %>% 
  droplevels() 

#2012
peds_data_2012 <- raw_data_2012 %>% 
  filter(AGE <= 18) %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,                                                 # Diagnoses
         PRIMCARE, 
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8 ) %>%                      # Medications
  mutate(YEAR = 2012) %>% 
  droplevels() 

#2011
peds_data_2011 <- raw_data_2011 %>% 
  filter(AGE <= 18) %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,                                                 # Diagnoses
         PRIMCARE, 
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8 ) %>%                      # Medications
  mutate(YEAR = 2011) %>% 
  droplevels() 

#2010
peds_data_2010 <- raw_data_2010 %>% 
  filter(AGE <= 18) %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,                                                 # Diagnoses
         PRIMCARE, 
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8 ) %>%                      # Medications
  mutate(YEAR = 2010) %>% 
  droplevels() 

#2009
peds_data_2009 <- raw_data_2009 %>% 
  filter(AGE <= 18) %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,                                 # Demographic characteristics
         MAJOR, RFV1,                                                       # Reason for Visit
         DIAG1,DIAG2,DIAG3,                                                 # Diagnoses
         PRIMCARE, 
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8 ) %>%                      # Medications
  mutate(YEAR = 2009) %>% 
  droplevels() 

#merge each year's dataset
peds_data_combined <- full_join(full_join(full_join(full_join(full_join(full_join(full_join(full_join(full_join(peds_data_2019,peds_data_2018),peds_data_2016),peds_data_2015),peds_data_2014),peds_data_2013),peds_data_2012),peds_data_2011),peds_data_2010),peds_data_2009)


#### RENAME NUMERICAL CODES OF CATEGORICAL VARIABLES ####

peds_data_combined$RACERETH = factor(peds_data_combined$RACERETH,
                                     levels = c(1,2,3,4),
                                     labels = c("Non-Hispanic White", 
                                                "Non-Hispanic Black", 
                                                "Hispanic", 
                                                "Non-Hispanic Other/Multiple Race"))

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

peds_data_combined$PRIMCARE = factor(peds_data_combined$PRIMCARE,
                                     levels = c(-9,-8,1,2),
                                     labels = c("Blank",
                                                "Unknown",
                                                "Yes",
                                                "No"))

#convert continuous to numeric
peds_data_combined <- peds_data_combined %>% 
  mutate(YEAR = as.numeric(YEAR), AGE = as.numeric(AGE))


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
peds_data_combined <- peds_data_combined %>% 
  mutate(ADHD = case_when(DIAG1 %in% ADHD_ICD_codes | DIAG2 %in% ADHD_ICD_codes | DIAG3 %in% ADHD_ICD_codes | DIAG4 %in% ADHD_ICD_codes | DIAG5 %in% ADHD_ICD_codes ~1)) %>% 
  mutate(STIMULANT = case_when(RX1CAT1 %in% ADHD_stimulant_codes | RX2CAT1 %in% ADHD_stimulant_codes | RX3CAT1 %in% ADHD_stimulant_codes | RX4CAT1 %in% ADHD_stimulant_codes | RX5CAT1 %in% ADHD_stimulant_codes | RX6CAT1 %in% ADHD_stimulant_codes | RX7CAT1 %in% ADHD_stimulant_codes | RX8CAT1 %in% ADHD_stimulant_codes | RX9CAT1 %in% ADHD_stimulant_codes | RX10CAT1 %in% ADHD_stimulant_codes | RX11CAT1 %in% ADHD_stimulant_codes | RX12CAT1 %in% ADHD_stimulant_codes | RX13CAT1 %in% ADHD_stimulant_codes | RX14CAT1 %in% ADHD_stimulant_codes | RX15CAT1 %in% ADHD_stimulant_codes | RX15CAT1 %in% ADHD_stimulant_codes | RX16CAT1 %in% ADHD_stimulant_codes | RX17CAT1 %in% ADHD_stimulant_codes | RX18CAT1 %in% ADHD_stimulant_codes | RX19CAT1 %in% ADHD_stimulant_codes | RX20CAT1 %in% ADHD_stimulant_codes | RX21CAT1 %in% ADHD_stimulant_codes | RX22CAT1 %in% ADHD_stimulant_codes | RX23CAT1 %in% ADHD_stimulant_codes | RX24CAT1 %in% ADHD_stimulant_codes | RX25CAT1 %in% ADHD_stimulant_codes | RX26CAT1 %in% ADHD_stimulant_codes | RX27CAT1 %in% ADHD_stimulant_codes | RX28CAT1 %in% ADHD_stimulant_codes | RX29CAT1 %in% ADHD_stimulant_codes | RX30CAT1 %in% ADHD_stimulant_codes |  
                                 RX1CAT2 %in% ADHD_stimulant_codes | RX2CAT2 %in% ADHD_stimulant_codes | RX3CAT2 %in% ADHD_stimulant_codes | RX4CAT2 %in% ADHD_stimulant_codes | RX5CAT2 %in% ADHD_stimulant_codes | RX6CAT2 %in% ADHD_stimulant_codes | RX7CAT2 %in% ADHD_stimulant_codes | RX8CAT2 %in% ADHD_stimulant_codes | RX9CAT2 %in% ADHD_stimulant_codes | RX10CAT2 %in% ADHD_stimulant_codes | RX11CAT2 %in% ADHD_stimulant_codes | RX12CAT2 %in% ADHD_stimulant_codes | RX13CAT2 %in% ADHD_stimulant_codes | RX14CAT2 %in% ADHD_stimulant_codes | RX15CAT2 %in% ADHD_stimulant_codes | RX15CAT2 %in% ADHD_stimulant_codes | RX16CAT2 %in% ADHD_stimulant_codes | RX17CAT2 %in% ADHD_stimulant_codes | RX18CAT2 %in% ADHD_stimulant_codes | RX19CAT2 %in% ADHD_stimulant_codes | RX20CAT2 %in% ADHD_stimulant_codes | RX21CAT2 %in% ADHD_stimulant_codes | RX22CAT2 %in% ADHD_stimulant_codes | RX23CAT2 %in% ADHD_stimulant_codes | RX24CAT2 %in% ADHD_stimulant_codes | RX25CAT2 %in% ADHD_stimulant_codes | RX26CAT2 %in% ADHD_stimulant_codes | RX27CAT2 %in% ADHD_stimulant_codes | RX28CAT2 %in% ADHD_stimulant_codes | RX29CAT2 %in% ADHD_stimulant_codes | RX30CAT2 ~ 1)) %>%  
  mutate(NON_STIM = case_when(DRUGID1 %in% ADHD_non_stimulant_codes | DRUGID2 %in% ADHD_non_stimulant_codes | DRUGID3 %in% ADHD_non_stimulant_codes | DRUGID4 %in% ADHD_non_stimulant_codes | DRUGID5 %in% ADHD_non_stimulant_codes | DRUGID6 %in% ADHD_non_stimulant_codes | DRUGID7 %in% ADHD_non_stimulant_codes | DRUGID8 %in% ADHD_non_stimulant_codes | DRUGID9 %in% ADHD_non_stimulant_codes | DRUGID10 %in% ADHD_non_stimulant_codes | DRUGID11 %in% ADHD_non_stimulant_codes | DRUGID12 %in% ADHD_non_stimulant_codes | DRUGID13 %in% ADHD_non_stimulant_codes | DRUGID14 %in% ADHD_non_stimulant_codes | DRUGID15 %in% ADHD_non_stimulant_codes | DRUGID16 %in% ADHD_non_stimulant_codes | DRUGID17 %in% ADHD_non_stimulant_codes | DRUGID18 %in% ADHD_non_stimulant_codes | DRUGID19 %in% ADHD_non_stimulant_codes | DRUGID20 %in% ADHD_non_stimulant_codes | DRUGID21 %in% ADHD_non_stimulant_codes | DRUGID22 %in% ADHD_non_stimulant_codes | DRUGID23 %in% ADHD_non_stimulant_codes | DRUGID24 %in% ADHD_non_stimulant_codes | DRUGID25 %in% ADHD_non_stimulant_codes | DRUGID26 %in% ADHD_non_stimulant_codes | DRUGID27 %in% ADHD_non_stimulant_codes | DRUGID28 %in% ADHD_non_stimulant_codes | DRUGID29 %in% ADHD_non_stimulant_codes | DRUGID30 %in% ADHD_non_stimulant_codes ~1)) %>% 
  mutate(BOTH = case_when((STIMULANT == 1 & NON_STIM == 1) ~1)) %>% 
  mutate(ANY_ADHD_MED = case_when((STIMULANT == 1 | NON_STIM == 1) ~1))


#### CREATE A FILTERED DATABASE ONLY INCLUDING VISITS WITH AN ADHD ICD CODE ####

ADHD_dataset <- peds_data_combined %>% 
  filter(ADHD == 1)


#### CREATE AN OVERALL COHORT SUMMARY TABLE ####

continuous_table_one <- c("AGE")
categorical_table_one <- c("RACERETH", "SEX", "MSA", "PAYTYPER", "MAJOR")
table_one_data <- CreateTableOne(vars = c(continuous_table_one, categorical_table_one), data = peds_data_combined, factorVars = categorical_table_one)
print(table_one_data, showAllLevels = TRUE, quote = FALSE, noSpaces = FALSE)


#### PROPORTION OF ALL VISITS WITH AN ADHD DIAGNOSIS, BY SELECTED CHARACTERISTICS ####

# function that outputs a table with the proportion of visits with an ADHD diagnosis, stratified by a selected characteristic
ADHD_Dx_By <- function(characteristic) {
  ADHD_visits_by <- ADHD_dataset %>% 
    group_by(!!as.name(characteristic)) %>% 
    summarise(n=n())
  colnames(ADHD_visits_by) <- c(characteristic,"ADHD_DX")
  
  all_visits_by <- peds_data_combined %>% 
    group_by(!!as.name(characteristic)) %>% 
    summarise(n=n())
  colnames(all_visits_by) <- c(characteristic,"ALL_VISITS")
  
  prop_visits_ADHD_by <- merge(x=ADHD_visits_by, y=all_visits_by,all=TRUE)
  prop_visits_ADHD_by
  
  prop_visits_ADHD_by <- prop_visits_ADHD_by %>% 
    mutate(PROP_VISITS_FOR_ADHD = ADHD_DX/ALL_VISITS*100)
  prop_visits_ADHD_by
  
}

# graph the rate of ADHD diagnosis by sex
ADHD_dx_by_sex <- ADHD_Dx_By("SEX")
ggplot(ADHD_dx_by_sex, aes(x=SEX,y=PROP_VISITS_FOR_ADHD)) +
  geom_bar(stat="identity")

# graph the rate of ADHD diagnosis by race/ethnicity
ADHD_dx_by_racereth <- ADHD_Dx_By("RACERETH")
ggplot(ADHD_dx_by_racereth, aes(x=RACERETH,y=PROP_VISITS_FOR_ADHD)) +
  geom_bar(stat="identity")

# graph the rate of ADHD diagnosis by MSA
ADHD_dx_by_MSA <- ADHD_Dx_By("MSA")
ggplot(ADHD_dx_by_MSA, aes(x=MSA,y=PROP_VISITS_FOR_ADHD)) +
  geom_bar(stat="identity")

# graph the rate of ADHD diagnosis by payer
ADHD_dx_by_paytyper <- ADHD_Dx_By("PAYTYPER")
ggplot(ADHD_dx_by_paytyper, aes(x=PAYTYPER,y=PROP_VISITS_FOR_ADHD)) +
  geom_bar(stat="identity")

# graph the rate of ADHD diagnosis by age
ADHD_dx_by_age <- ADHD_Dx_By("AGE")
ggplot(ADHD_dx_by_age, aes(x=AGE,y=PROP_VISITS_FOR_ADHD)) +
  geom_bar(stat="identity")

# graph the rate of ADHD diagnosis by year (line graph) 
ADHD_dx_by_year <- ADHD_Dx_By("YEAR")
ggplot(ADHD_dx_by_year, aes(x=YEAR,y=PROP_VISITS_FOR_ADHD)) +
  geom_line()


#### PROPORTION OF ADHD VISITS WITH ADHD MEDICATION PRESCRIBED, BY SELECTED CHARACTERISTICS ####

# function that outputs a table with the proportion of ADHD visits at which a stimulant, non-stimulant, or both were prescribed, stratified by a selected characteristic
ADHD_Rx_By1 <- function(characteristic) {
  
  ADHD_visits_by <- ADHD_dataset %>% 
    group_by(!!as.name(characteristic)) %>% 
    summarise(n=n())
  colnames(ADHD_visits_by) <- c(characteristic, "ADHD_VISITS")
  
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
  
  prop_ADHD_visits_rx_by <- rbind(stim_rx_by,non_stim_rx_by,both_rx_by)
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
ADHD_rx_by_sex <- ADHD_Rx_By1("SEX")
ggplot(ADHD_rx_by_sex, aes(x=SEX,y=PROP,fill=CLASS)) +
  geom_bar(position = "stack", stat = "identity")

# stacked bar graph of ADHD medication prescribing by race/ethnicity
ADHD_rx_by_racereth <- ADHD_Rx_By1("RACERETH")
ggplot(ADHD_rx_by_racereth, aes(x=RACERETH,y=PROP,fill=CLASS)) +
  geom_bar(position = "stack", stat = "identity")

# stacked bar graph of ADHD medication prescribing by MSA
ADHD_rx_by_MSA <- ADHD_Rx_By1("MSA")
ggplot(ADHD_rx_by_MSA, aes(x=MSA,y=PROP,fill=CLASS)) +
  geom_bar(position = "stack", stat = "identity")

# stacked bar graph of ADHD medication prescribing by payer
ADHD_rx_by_paytyper <- ADHD_Rx_By1("PAYTYPER")
ggplot(ADHD_rx_by_paytyper, aes(x=PAYTYPER,y=PROP,fill=CLASS)) +
  geom_bar(position = "stack", stat = "identity")

# stacked bar graph of ADHD medication prescribing by age
ADHD_rx_by_age <- ADHD_Rx_By1("AGE")
ggplot(ADHD_rx_by_age, aes(x=AGE,y=PROP,fill=CLASS)) +
  geom_bar(position = "stack", stat = "identity")

#  stacked bar graph of ADHD medication prescribing by primary care (y/n)
ADHD_rx_by_primcare <- ADHD_Rx_By1("PRIMCARE")
ggplot(ADHD_rx_by_primcare, aes(x=PRIMCARE,y=PROP,fill=CLASS)) +
  geom_bar(position = "stack", stat = "identity")

ADHD_dataset %>% 
  group_by(AGE) %>% 
  summarise(n=n())
