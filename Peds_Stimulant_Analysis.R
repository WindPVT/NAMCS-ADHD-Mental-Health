#### LOAD PACKAGES ####
library(haven)
library(dplyr)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(survey)
library(ggsurvey)
library(broom)

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
         NCMED1,NCMED2,NCMED3,NCMED4,NCMED5,NCMED6,NCMED7,NCMED8,NCMED9,NCMED10,NCMED11,NCMED12,NCMED12,NCMED13,NCMED14,NCMED15,NCMED16,NCMED17,NCMED18,NCMED19,NCMED20,NCMED21,NCMED22,NCMED23,NCMED24,NCMED25,NCMED26,NCMED27,NCMED28,NCMED29,NCMED30,
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30,
         MDDO,PRIMCARE,PHYSASST,NPNMW,MHP,OTHPROV,RNLPN) %>% # Medications
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
         NCMED1,NCMED2,NCMED3,NCMED4,NCMED5,NCMED6,NCMED7,NCMED8,NCMED9,NCMED10,NCMED11,NCMED12,NCMED12,NCMED13,NCMED14,NCMED15,NCMED16,NCMED17,NCMED18,NCMED19,NCMED20,NCMED21,NCMED22,NCMED23,NCMED24,NCMED25,NCMED26,NCMED27,NCMED28,NCMED29,NCMED30,
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30,
         MDDO,PRIMCARE,PHYSASST,NPNMW,MHP,OTHPROV,RNLPN) %>% # Medications
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
         NCMED1,NCMED2,NCMED3,NCMED4,NCMED5,NCMED6,NCMED7,NCMED8,NCMED9,NCMED10,NCMED11,NCMED12,NCMED12,NCMED13,NCMED14,NCMED15,NCMED16,NCMED17,NCMED18,NCMED19,NCMED20,NCMED21,NCMED22,NCMED23,NCMED24,NCMED25,NCMED26,NCMED27,NCMED28,NCMED29,NCMED30,
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30,
         MDDO,PRIMCARE,PHYSASST,NPNMW,MHP,OTHPROV,RNLPN) %>% # Medications
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
         NCMED1,NCMED2,NCMED3,NCMED4,NCMED5,NCMED6,NCMED7,NCMED8,NCMED9,NCMED10,NCMED11,NCMED12,NCMED12,NCMED13,NCMED14,NCMED15,NCMED16,NCMED17,NCMED18,NCMED19,NCMED20,NCMED21,NCMED22,NCMED23,NCMED24,NCMED25,NCMED26,NCMED27,NCMED28,NCMED29,NCMED30,
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30,
         MDDO,PRIMCARE,PHYSASST,NPNMW,MHP,OTHPROV,RNLPN) %>% # Medications
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
         NCMED1,NCMED2,NCMED3,NCMED4,NCMED5,NCMED6,NCMED7,NCMED8,NCMED9,NCMED10,NCMED11,NCMED12,NCMED12,NCMED13,NCMED14,NCMED15,NCMED16,NCMED17,NCMED18,NCMED19,NCMED20,NCMED21,NCMED22,NCMED23,NCMED24,NCMED25,NCMED26,NCMED27,NCMED28,NCMED29,NCMED30,
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30,
         MDDO,PRIMCARE,PHYSASST,NPNMW,MHP,OTHPROV,RNLPN) %>% # Medications
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
         NCMED1,NCMED2,NCMED3,NCMED4,NCMED5,NCMED6,NCMED7,NCMED8,
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,
         MDDO,PRIMCARE,PHYSASST,NPNMW,MHP,OTHPROV,RNLPN) %>%                      # Medications
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
         NCMED1,NCMED2,NCMED3,NCMED4,NCMED5,NCMED6,NCMED7,NCMED8,
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,
         MDDO,PRIMCARE,PHYSASST,NPNMW,MHP,OTHPROV,RNLPN) %>%                      # Medications
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
         NCMED1,NCMED2,NCMED3,NCMED4,NCMED5,NCMED6,NCMED7,NCMED8,
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,
         MDDO,PRIMCARE,PHYSASST,NPNMW,MHP,OTHPROV,RNLPN) %>%                      # Medications
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
         NCMED1,NCMED2,NCMED3,NCMED4,NCMED5,NCMED6,NCMED7,NCMED8,
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,
         MDDO,PRIMCARE,PHYSASST,NPNMW,MHP,OTHPROV,RNLPN) %>%                      # Medications
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
         NCMED1,NCMED2,NCMED3,NCMED4,NCMED5,NCMED6,NCMED7,NCMED8,
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,
         MDDO,PRIMCARE,PHYSASST,NPNMW,MHP,OTHPROV,RNLPN) %>%                      # Medications
  mutate(YEAR = 2009) %>% 
  droplevels() 

#merge each year's dataset
data_combined <- full_join(full_join(full_join(full_join(full_join(full_join(full_join(full_join(full_join(data_2019,data_2018),data_2016),data_2015),data_2014),data_2013),data_2012),data_2011),data_2010),data_2009)



#### FORMAT THE DATASET: RENAME, GROUP, AND ADD VARIABLES ####

## Rename continuous variable codes
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


### Define Multum Drug Codes and ICD Codes of Interest ###
Stimulant_Rx_Codes <- c(71)

Non_Stimulant_Rx_Codes <- c("d00259", #imipramine
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

ADHD_ICD_codes <- c("F900","F901","F902","F908","F909", #these are ICD-10 codes
                    "314--","3140-","31400","31401","3141-","31410","3142-","31420","3148-","31480","3149-","31490") #these are ICD-9 codes

Narcolepsy_ICD_codes <- c("G474","G4741","G47411","G47419","G4742","G47421","G47429", #these are ICD-10 codes
                          "34700","3470-","347--","34701","34710","3471-","34711") #these are ICD-9 codes

Binge_Eating_ICD_codes <- c("E66-", "E660", "E6601", "E6609", "E661", "E662", "E663", "E668", "E669", #these are ICD-10 codes
                            "278", "278--", "278-", "2780-", "27800", "27801", "27802", "27803")  #these are ICD-9 codes
Tourette_ICD_codes <- c("F95.2", #this is an ICD-10 code
                        "307.23") #this is an ICD-9 code
Glaucoma_ICD_codes <- c("H40-","H400","H401","H402","H403","H404","H405","H406","H408","H409", #these are ICD-10 codes
                        "3659-","36590","36520","36521","36522","36523","36524","36512","36513","36511","36510","36515","36501") #these are ICD-9 codes
Hyperthyrodisim_ICD_codes <- c("E05-","E050","E051","E052","E053","E054","E058","E059", #these are ICD-10 codes
                               "2420","2421","2422","2423","2424","2428","2429","2420-","2421-","2422-","2423-","2424-","2428-","2429-","24200","24210","24220","24230","24240","24280","24290") #these are ICD-9 codes
MAOI_codes <- c(250)

## Group and recode selected variables
data_combined <- data_combined %>% 
  mutate(COUNTER = 1) %>% 
  mutate(YEAR = as.numeric(YEAR), AGE = as.numeric(AGE)) %>% 
  mutate(AGE_RECODE_1 = case_when(AGE<5 ~"<5",
                                  AGE>=5 & AGE<18 ~"5-18",
                                  AGE>=18 & AGE<30 ~"18-30",
                                  AGE>=30 ~">30")) %>% 
  mutate(AGE_RECODE_2 = case_when(AGE<5 ~"<5",
                                  AGE>=5 & AGE<18 ~"5-18",
                                  AGE>=18 & AGE<65 ~"18-65",
                                  AGE>=65 ~">65")) %>% 
  mutate(AGE_RECODE_3 = case_when(AGE<5 ~"<5",
                                  AGE>=5 & AGE<18 ~"5-18",
                                  AGE>=18 & AGE<30 ~"18-30",
                                  AGE>=30 & AGE<65 ~"30-65",
                                  AGE>=65 ~ ">65")) %>% 
  mutate(PAYTYPER_RECODE = case_when(PAYTYPER == 1 ~"Private Insurance",
                                     PAYTYPER == 2 | PAYTYPER == 3 ~"Public Insurance",
                                     .default = "Other/Unknown")) %>%
  mutate(PRIMCARE_RECODE = case_when(PRIMCARE == 2 ~ "Specialist",
                                     PRIMCARE == 1 ~ "PCP", 
                                     PRIMCARE == -9 | PRIMCARE == -8 ~ "Blank/Unknown")) %>% 
  mutate(MDDO_RECODE = case_when(MDDO == 1 ~ "MD",
                                 MDDO == 2 ~ "DO")) %>% 
  mutate(PHYSASST_RECODE = case_when(PHYSASST == 0 ~ "No PA",
                                     PHYSASST == 1 ~ "PA")) %>% 
  mutate(NPNMW_RECODE = case_when(NPNMW == 0 ~ "No NP/MW",
                                  NPNMW == 1 ~ "NP/MW")) %>%
  mutate(RNLPN_RECODE = case_when(RNLPN == 0 ~ "No RN/LPN",
                                  RNLPN ==1 ~ "RN/LPN")) %>% 
  mutate(MHP_RECODE = case_when(MHP == 0 ~ "No MHP",
                                MHP ==1 ~ "MHP")) %>%
  mutate(OTHPROV_RECODE = case_when(OTHPROV == 0 ~ "No Other Provider",
                                    OTHPROV ==1 ~ "Other Provider")) %>%
  mutate(ANY_STIM = case_when(((RX1CAT1 %in% Stimulant_Rx_Codes | RX2CAT1 %in% Stimulant_Rx_Codes | RX3CAT1 %in% Stimulant_Rx_Codes | RX4CAT1 %in% Stimulant_Rx_Codes | RX5CAT1 %in% Stimulant_Rx_Codes | RX6CAT1 %in% Stimulant_Rx_Codes | RX7CAT1 %in% Stimulant_Rx_Codes | RX8CAT1 %in% Stimulant_Rx_Codes |
                                  RX1CAT2 %in% Stimulant_Rx_Codes | RX2CAT2 %in% Stimulant_Rx_Codes | RX3CAT2 %in% Stimulant_Rx_Codes | RX4CAT2 %in% Stimulant_Rx_Codes | RX5CAT2 %in% Stimulant_Rx_Codes | RX6CAT2 %in% Stimulant_Rx_Codes | RX7CAT2 %in% Stimulant_Rx_Codes | RX8CAT2 %in% Stimulant_Rx_Codes)) ~1,
                              .default = 0))  %>% 
  mutate(NEW_STIM = case_when((((RX1CAT1 %in% Stimulant_Rx_Codes & NCMED1==1) | (RX2CAT1 %in% Stimulant_Rx_Codes & NCMED2==1) | (RX3CAT1 %in% Stimulant_Rx_Codes & NCMED3==1) | (RX4CAT1 %in% Stimulant_Rx_Codes & NCMED4==1) | (RX5CAT1 %in% Stimulant_Rx_Codes & NCMED5==1) | (RX6CAT1 %in% Stimulant_Rx_Codes & NCMED6==1) | (RX7CAT1 %in% Stimulant_Rx_Codes & NCMED7==1) | (RX8CAT1 %in% Stimulant_Rx_Codes & NCMED8==1) |  
                                (RX1CAT2 %in% Stimulant_Rx_Codes & NCMED1==1) | (RX2CAT2 %in% Stimulant_Rx_Codes & NCMED2==1) | (RX3CAT2 %in% Stimulant_Rx_Codes & NCMED3==1) | (RX4CAT2 %in% Stimulant_Rx_Codes & NCMED4==1) | (RX5CAT2 %in% Stimulant_Rx_Codes & NCMED5==1) | (RX6CAT2 %in% Stimulant_Rx_Codes & NCMED6==1) | (RX7CAT2 %in% Stimulant_Rx_Codes & NCMED7==1) | (RX8CAT2 %in% Stimulant_Rx_Codes & NCMED8==1))) ~1,
                              (((RX1CAT1 %in% Stimulant_Rx_Codes & NCMED1==2) | (RX2CAT1 %in% Stimulant_Rx_Codes & NCMED2==2) | (RX3CAT1 %in% Stimulant_Rx_Codes & NCMED3==2) | (RX4CAT1 %in% Stimulant_Rx_Codes & NCMED4==2) | (RX5CAT1 %in% Stimulant_Rx_Codes & NCMED5==2) | (RX6CAT1 %in% Stimulant_Rx_Codes & NCMED6==2) | (RX7CAT1 %in% Stimulant_Rx_Codes & NCMED7==2) | (RX8CAT1 %in% Stimulant_Rx_Codes & NCMED8==2) |  
                                  (RX1CAT2 %in% Stimulant_Rx_Codes & NCMED1==2) | (RX2CAT2 %in% Stimulant_Rx_Codes & NCMED2==2) | (RX3CAT2 %in% Stimulant_Rx_Codes & NCMED3==2) | (RX4CAT2 %in% Stimulant_Rx_Codes & NCMED4==2) | (RX5CAT2 %in% Stimulant_Rx_Codes & NCMED5==1) | (RX6CAT2 %in% Stimulant_Rx_Codes & NCMED6==2) | (RX7CAT2 %in% Stimulant_Rx_Codes & NCMED7==1) | (RX8CAT2 %in% Stimulant_Rx_Codes & NCMED8==1) )) ~-9,
                              (((RX1CAT1 %in% Stimulant_Rx_Codes & NCMED1==-9) | (RX2CAT1 %in% Stimulant_Rx_Codes & NCMED2==-9) | (RX3CAT1 %in% Stimulant_Rx_Codes & NCMED3==-9) | (RX4CAT1 %in% Stimulant_Rx_Codes & NCMED4==-9) | (RX5CAT1 %in% Stimulant_Rx_Codes & NCMED5==-9) | (RX6CAT1 %in% Stimulant_Rx_Codes & NCMED6==-9) | (RX7CAT1 %in% Stimulant_Rx_Codes & NCMED7==-9) | (RX8CAT1 %in% Stimulant_Rx_Codes & NCMED8==-9) | 
                                  (RX1CAT2 %in% Stimulant_Rx_Codes & NCMED1==-9) | (RX2CAT2 %in% Stimulant_Rx_Codes & NCMED2==-9) | (RX3CAT2 %in% Stimulant_Rx_Codes & NCMED3==-9) | (RX4CAT2 %in% Stimulant_Rx_Codes & NCMED4==-9) | (RX5CAT2 %in% Stimulant_Rx_Codes & NCMED5==-9) | (RX6CAT2 %in% Stimulant_Rx_Codes & NCMED6==-9) | (RX7CAT2 %in% Stimulant_Rx_Codes & NCMED7==-9) | (RX8CAT2 %in% Stimulant_Rx_Codes & NCMED8==-9) )) ~-9,
                              .default = 0))  %>% 
  mutate(NEW_STIM_BINARY = case_when(NEW_STIM == 1 ~1,
                                     NEW_STIM == 0 ~0)) %>% 
  mutate(ANY_NON_STIM = case_when((DRUGID1 %in% Non_Stimulant_Rx_Codes | DRUGID2 %in% Non_Stimulant_Rx_Codes | DRUGID3 %in% Non_Stimulant_Rx_Codes | DRUGID4 %in% Non_Stimulant_Rx_Codes | DRUGID5 %in% Non_Stimulant_Rx_Codes | DRUGID6 %in% Non_Stimulant_Rx_Codes | DRUGID7 %in% Non_Stimulant_Rx_Codes | DRUGID8 %in% Non_Stimulant_Rx_Codes) ~1,
                              .default = 0)) %>% 
  mutate(NEW_NON_STIM = case_when(((DRUGID1 %in% Non_Stimulant_Rx_Codes & NCMED1==1) | (DRUGID2 %in% Non_Stimulant_Rx_Codes & NCMED2==1) | (DRUGID3 %in% Non_Stimulant_Rx_Codes & NCMED3==1) | (DRUGID4 %in% Non_Stimulant_Rx_Codes & NCMED4==1) | (DRUGID5 %in% Non_Stimulant_Rx_Codes & NCMED5==1) | (DRUGID6 %in% Non_Stimulant_Rx_Codes & NCMED6==1) | (DRUGID7 %in% Non_Stimulant_Rx_Codes & NCMED7==1) | (DRUGID8 %in% Non_Stimulant_Rx_Codes & NCMED8==1)) ~1,
                                  ((DRUGID1 %in% Non_Stimulant_Rx_Codes & NCMED1==2) | (DRUGID2 %in% Non_Stimulant_Rx_Codes & NCMED2==2) | (DRUGID3 %in% Non_Stimulant_Rx_Codes & NCMED3==2) | (DRUGID4 %in% Non_Stimulant_Rx_Codes & NCMED4==2) | (DRUGID5 %in% Non_Stimulant_Rx_Codes & NCMED5==2) | (DRUGID6 %in% Non_Stimulant_Rx_Codes & NCMED6==2) | (DRUGID7 %in% Non_Stimulant_Rx_Codes & NCMED7==2) | (DRUGID8 %in% Non_Stimulant_Rx_Codes & NCMED8==2)) ~-9,
                                  ((DRUGID1 %in% Non_Stimulant_Rx_Codes & NCMED1==-9) | (DRUGID2 %in% Non_Stimulant_Rx_Codes & NCMED2==-9) | (DRUGID3 %in% Non_Stimulant_Rx_Codes & NCMED3==-9) | (DRUGID4 %in% Non_Stimulant_Rx_Codes & NCMED4==-9) | (DRUGID5 %in% Non_Stimulant_Rx_Codes & NCMED5==-9) | (DRUGID6 %in% Non_Stimulant_Rx_Codes & NCMED6==-9) | (DRUGID7 %in% Non_Stimulant_Rx_Codes & NCMED7==-9) | (DRUGID8 %in% Non_Stimulant_Rx_Codes & NCMED8==-9)) ~-9,
                                  .default = 0)) %>% 
  mutate(NEW_NON_STIM_BINARY = case_when(NEW_NON_STIM == 1 ~1,
                                     NEW_NON_STIM == 0 ~0)) %>% 
  mutate(ANY_STIM_ALONE = case_when(ANY_STIM==1 & ANY_NON_STIM==0 ~1,
                                    .default = 0)) %>% 
  mutate(NEW_STIM_ALONE = case_when(NEW_STIM==1 & ANY_NON_STIM==0 ~1,
                                    .default = 0)) %>% 
  mutate(ANY_NON_STIM_ALONE = case_when(ANY_NON_STIM==1 & ANY_STIM==0 ~1,
                                    .default = 0)) %>% 
  mutate(NEW_NON_STIM_ALONE = case_when(NEW_NON_STIM==1 & ANY_STIM==0 ~1,
                                    .default = 0)) %>% 
  mutate(ANY_BOTH = case_when(ANY_STIM==1 & ANY_NON_STIM==1 ~1,
                              .default = 0)) %>% 
  mutate(NEW_BOTH = case_when(NEW_STIM==1 & NEW_NON_STIM==1 ~1,
                              .default = 0)) %>% 
  #mutate(MAOI = case_when(((RX1CAT1 %in% MAOI_codes | RX2CAT1 %in% MAOI_codes | RX3CAT1 %in% MAOI_codes | RX4CAT1 %in% MAOI_codes | RX5CAT1 %in% MAOI_codes | RX6CAT1 %in% MAOI_codes | RX7CAT1 %in% MAOI_codes | RX8CAT1 %in% MAOI_codes | RX9CAT1 %in% MAOI_codes | RX10CAT1 %in% MAOI_codes | RX11CAT1 %in% MAOI_codes | RX12CAT1 %in% MAOI_codes | RX13CAT1 %in% MAOI_codes | RX14CAT1 %in% MAOI_codes | RX15CAT1 %in% MAOI_codes | RX15CAT1 %in% MAOI_codes | RX16CAT1 %in% MAOI_codes | RX17CAT1 %in% MAOI_codes | RX18CAT1 %in% MAOI_codes | RX19CAT1 %in% MAOI_codes | RX20CAT1 %in% MAOI_codes | RX21CAT1 %in% MAOI_codes | RX22CAT1 %in% MAOI_codes | RX23CAT1 %in% MAOI_codes | RX24CAT1 %in% MAOI_codes | RX25CAT1 %in% MAOI_codes | RX26CAT1 %in% MAOI_codes | RX27CAT1 %in% MAOI_codes | RX28CAT1 %in% MAOI_codes | RX29CAT1 %in% MAOI_codes | RX30CAT1 %in% MAOI_codes |  
   #                           RX1CAT2 %in% MAOI_codes | RX2CAT2 %in% MAOI_codes | RX3CAT2 %in% MAOI_codes | RX4CAT2 %in% MAOI_codes | RX5CAT2 %in% MAOI_codes | RX6CAT2 %in% MAOI_codes | RX7CAT2 %in% MAOI_codes | RX8CAT2 %in% MAOI_codes | RX9CAT2 %in% MAOI_codes | RX10CAT2 %in% MAOI_codes | RX11CAT2 %in% MAOI_codes | RX12CAT2 %in% MAOI_codes | RX13CAT2 %in% MAOI_codes | RX14CAT2 %in% MAOI_codes | RX15CAT2 %in% MAOI_codes | RX15CAT2 %in% MAOI_codes | RX16CAT2 %in% MAOI_codes | RX17CAT2 %in% MAOI_codes | RX18CAT2 %in% MAOI_codes | RX19CAT2 %in% MAOI_codes | RX20CAT2 %in% MAOI_codes | RX21CAT2 %in% MAOI_codes | RX22CAT2 %in% MAOI_codes | RX23CAT2 %in% MAOI_codes | RX24CAT2 %in% MAOI_codes | RX25CAT2 %in% MAOI_codes | RX26CAT2 %in% MAOI_codes | RX27CAT2 %in% MAOI_codes | RX28CAT2 %in% MAOI_codes | RX29CAT2 %in% MAOI_codes | RX30CAT2 %in% MAOI_codes |
    #                          RX1CAT3 %in% MAOI_codes | RX2CAT3 %in% MAOI_codes | RX3CAT3 %in% MAOI_codes | RX4CAT3 %in% MAOI_codes | RX5CAT3 %in% MAOI_codes | RX6CAT3 %in% MAOI_codes | RX7CAT3 %in% MAOI_codes | RX8CAT3 %in% MAOI_codes | RX9CAT3 %in% MAOI_codes | RX10CAT3 %in% MAOI_codes | RX11CAT3 %in% MAOI_codes | RX12CAT3 %in% MAOI_codes | RX13CAT3 %in% MAOI_codes | RX14CAT3 %in% MAOI_codes | RX15CAT3 %in% MAOI_codes | RX15CAT3 %in% MAOI_codes | RX16CAT3 %in% MAOI_codes | RX17CAT3 %in% MAOI_codes | RX18CAT3 %in% MAOI_codes | RX19CAT3 %in% MAOI_codes | RX20CAT3 %in% MAOI_codes | RX21CAT3 %in% MAOI_codes | RX22CAT3 %in% MAOI_codes | RX23CAT3 %in% MAOI_codes | RX24CAT3 %in% MAOI_codes | RX25CAT3 %in% MAOI_codes | RX26CAT3 %in% MAOI_codes | RX27CAT3 %in% MAOI_codes | RX28CAT3 %in% MAOI_codes | RX29CAT3 %in% MAOI_codes | RX30CAT3 %in% MAOI_codes)) ~1,
     #                     .default = 0))  %>% 
  mutate(ADHD = case_when(DIAG1 %in% ADHD_ICD_codes | DIAG2 %in% ADHD_ICD_codes | DIAG3 %in% ADHD_ICD_codes | DIAG4 %in% ADHD_ICD_codes | DIAG5 %in% ADHD_ICD_codes ~"ADHD Dx Code",
                          .default = "No ADHD Dx Code")) %>% 
  mutate(NARCOLEPSY = case_when(DIAG1 %in% Narcolepsy_ICD_codes | DIAG2 %in% Narcolepsy_ICD_codes | DIAG3 %in% Narcolepsy_ICD_codes | DIAG4 %in% Narcolepsy_ICD_codes | DIAG5 %in% Narcolepsy_ICD_codes ~"Narcolepsy Dx Code",
                                .default = "No Narcolepsy Dx Code")) %>% 
  mutate(BINGE_EATING = case_when(DIAG1 %in% Binge_Eating_ICD_codes | DIAG2 %in% Binge_Eating_ICD_codes | DIAG3 %in% Binge_Eating_ICD_codes | DIAG4 %in% Binge_Eating_ICD_codes | DIAG5 %in% Binge_Eating_ICD_codes ~"Binge Eating Disorder",
                                  .default = "No Binge Eating Dx Code")) %>% 
  mutate(TOURETTE = case_when(DIAG1 %in% Tourette_ICD_codes | DIAG2 %in% Tourette_ICD_codes | DIAG3 %in% Tourette_ICD_codes | DIAG4 %in% Tourette_ICD_codes | DIAG5 %in% Tourette_ICD_codes ~1,
                              .default = 0)) %>% 
  mutate(GLAUCOMA = case_when(DIAG1 %in% Glaucoma_ICD_codes | DIAG2 %in% Glaucoma_ICD_codes | DIAG3 %in% Glaucoma_ICD_codes | DIAG4 %in% Glaucoma_ICD_codes | DIAG5 %in% Glaucoma_ICD_codes ~1,
                              .default = 0)) %>% 
  mutate(HYPERTHYROIDISM = case_when(DIAG1 %in% Hyperthyrodisim_ICD_codes | DIAG2 %in% Hyperthyrodisim_ICD_codes | DIAG3 %in% Hyperthyrodisim_ICD_codes | DIAG4 %in% Hyperthyrodisim_ICD_codes | DIAG5 %in% Hyperthyrodisim_ICD_codes ~1,
                                     .default = 0)) 


#### WEIGHTING ####

## Define survey design
weighting_design_namcs <- svydesign(id=~CPSUM, strata=~CSTRATM, weight=~PATWT,data=data_combined,nest=TRUE)

## Create weighted, filtered peds dataset
peds_weighted <- subset(weighting_design_namcs,(AGE>=5 & AGE<18))
peds_ADHD_weighted <-subset(peds_weighted,ADHD==1)
peds_stim_weighted <- subset(peds_weighted, ANY_STIM==1)


#### STIMULANT PRESCRIBING ***WITHOUT A CONCOMITANT ADHD DX CODE*** ####

## Graph of the proportion of total visits with an stimulant rx, by year and age group
stim_by_year <- svyby(~ANY_STIM, ~YEAR+AGE_RECODE_3, peds_weighted, na=TRUE, svymean) 
stim_by_year <- cbind(stim_by_year, confint(stim_by_year))
colnames(stim_by_year) <- c("YEAR", "AGE_RECODE_3", "PROPORTION", "SE", "LOWER_CI", "UPPER_CI")
stim_by_year <- stim_by_year %>% 
  mutate(CLASS = "Stimulant")

non_stim_by_year <- svyby(~ANY_NON_STIM, ~YEAR+AGE_RECODE_3, peds_weighted, na=TRUE, svymean) 
non_stim_by_year <- cbind(non_stim_by_year, confint(non_stim_by_year))
colnames(non_stim_by_year) <- c("YEAR", "AGE_RECODE_3", "PROPORTION", "SE", "LOWER_CI", "UPPER_CI")
non_stim_by_year <- non_stim_by_year %>% 
  mutate(CLASS = "Non-Stimulant")

combined <- rbind(stim_by_year,non_stim_by_year)

ggplot(combined, aes(x=YEAR, y=PROPORTION, colour=CLASS))+
  geom_line()+
  geom_point()+
  geom_ribbon(aes(ymin=LOWER_CI,ymax=UPPER_CI, fill=CLASS), alpha=0.2)+
  labs(y = "Proportion of Total Visits", x = "Year")


## Multivariable logistic regression of factors associated with a stimulant prescription **WITHOUT AN ADHD DX CODE** ##
summary(svyglm(ANY_STIM~
                 factor(YEAR)+
                 factor(AGE)+
                 SEX+
                 RACERETH+
                 relevel(factor(PAYTYPER_RECODE), ref = "Private Insurance")+
                 relevel(factor(PRIMCARE_RECODE), ref = "PCP")+
                 MDDO_RECODE+
                 PHYSASST_RECODE+
                 NPNMW_RECODE+
                 RNLPN_RECODE+
                 relevel(factor(MHP_RECODE), ref = "No MHP")+
                 OTHPROV_RECODE, 
               design=peds_weighted,family=quasibinomial()))
summary(svyglm(NEW_STIM_BINARY~factor(YEAR)+SEX+RACERETH+relevel(factor(PAYTYPER_RECODE), ref = "Private Insurance"), design=peds_weighted,family=quasibinomial()))

summary(svyglm(ANY_NON_STIM~factor(YEAR)+SEX+RACERETH+relevel(factor(PAYTYPER_RECODE), ref = "Private Insurance"), design=peds_weighted,family=quasibinomial()))
summary(svyglm(NEW_NON_STIM_BINARY~factor(YEAR)+SEX+RACERETH+relevel(factor(PAYTYPER_RECODE), ref = "Private Insurance"), design=peds_weighted,family=quasibinomial()))

