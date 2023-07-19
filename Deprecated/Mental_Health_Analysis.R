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


