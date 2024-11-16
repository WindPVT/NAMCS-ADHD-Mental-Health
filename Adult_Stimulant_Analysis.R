#### LOAD PACKAGES ####
library(haven)
library(dplyr)
library(tidyr)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(survey)
library(ggsurvey)
library(broom)
library(tableone)

# Prevent scientific notation
options(scipen=999)




#### READ CSV FILES ####
raw_data_2019 <- read.csv("NAMCS-ADHD-Mental-Health/Raw_Data/namcs2019.csv")
raw_data_2018 <- read.csv("NAMCS-ADHD-Mental-Health/Raw_Data/namcs2018.csv")
raw_data_2016 <- read.csv("NAMCS-ADHD-Mental-Health/Raw_Data/namcs2016.csv")
raw_data_2015 <- read.csv("NAMCS-ADHD-Mental-Health/Raw_Data/namcs2015.csv")
raw_data_2014 <- read.csv("NAMCS-ADHD-Mental-Health/Raw_Data/namcs2014.csv")
raw_data_2013 <- read.csv("NAMCS-ADHD-Mental-Health/Raw_Data/namcs2013.csv")
raw_data_2012 <- read.csv("NAMCS-ADHD-Mental-Health/Raw_Data/namcs2012.csv")




#### FILTER AND MERGE EACH YEAR'S DATASET ####

#2019
data_2019 <- raw_data_2019 %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,  # Demographic characteristics
         DIAG1,DIAG2,DIAG3,DIAG4,DIAG5, # Diagnoses
         PATWT, CSTRATM, CPSUM,  # Weighting
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,RX11CAT1,RX12CAT1,RX13CAT1,RX14CAT1,RX15CAT1,RX15CAT1,RX16CAT1,RX17CAT1,RX18CAT1,RX19CAT1,RX20CAT1,RX21CAT1,RX22CAT1,RX23CAT1,RX24CAT1,RX25CAT1,RX26CAT1,RX27CAT1,RX28CAT1,RX29CAT1,RX30CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,RX9CAT2,RX10CAT2,RX11CAT2,RX12CAT2,RX13CAT2,RX14CAT2,RX15CAT2,RX15CAT2,RX16CAT2,RX17CAT2,RX18CAT2,RX19CAT2,RX20CAT2,RX21CAT2,RX22CAT2,RX23CAT2,RX24CAT2,RX25CAT2,RX26CAT2,RX27CAT2,RX28CAT2,RX29CAT2,RX30CAT2,   # Drug Category 2
         RX1CAT3,RX2CAT3,RX3CAT3,RX4CAT3,RX5CAT3,RX6CAT3,RX7CAT3,RX8CAT3,RX9CAT3,RX10CAT3,RX11CAT3,RX12CAT3,RX13CAT3,RX14CAT3,RX15CAT3,RX15CAT3,RX16CAT3,RX17CAT3,RX18CAT3,RX19CAT3,RX20CAT3,RX21CAT3,RX22CAT3,RX23CAT3,RX24CAT3,RX25CAT3,RX26CAT3,RX27CAT3,RX28CAT3,RX29CAT3,RX30CAT3,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30,   # Drug IDs
         NCMED1,NCMED2,NCMED3,NCMED4,NCMED5,NCMED6,NCMED7,NCMED8,NCMED9,NCMED10,NCMED11,NCMED12,NCMED12,NCMED13,NCMED14,NCMED15,NCMED16,NCMED17,NCMED18,NCMED19,NCMED20,NCMED21,NCMED22,NCMED23,NCMED24,NCMED25,NCMED26,NCMED27,NCMED28,NCMED29,NCMED30,   # New or continued
         MDDO,PRIMCARE,PHYSASST,NPNMW,MHP,OTHPROV,RNLPN,PHYS,  # Provider characteristics
         CAD, # Coronary artery disease
         DEPRN, # Depression
         HTN, # Hypertension
         OBESITY, # Obesity
         SUBSTAB, # Alcohol Drug Use/Dependence
         ESRD, # End-Stage Renal Disease
         CEBVD, # Cerebrovascular Disease
         PREGNANT) %>% # Pregnancy
  mutate(YEAR = 2019) %>% 
  droplevels() 

#2018
data_2018 <- raw_data_2018 %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,  # Demographic characteristics
         DIAG1,DIAG2,DIAG3,DIAG4,DIAG5, # Diagnoses
         PATWT, CSTRATM, CPSUM,  # Weighting
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,RX11CAT1,RX12CAT1,RX13CAT1,RX14CAT1,RX15CAT1,RX15CAT1,RX16CAT1,RX17CAT1,RX18CAT1,RX19CAT1,RX20CAT1,RX21CAT1,RX22CAT1,RX23CAT1,RX24CAT1,RX25CAT1,RX26CAT1,RX27CAT1,RX28CAT1,RX29CAT1,RX30CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,RX9CAT2,RX10CAT2,RX11CAT2,RX12CAT2,RX13CAT2,RX14CAT2,RX15CAT2,RX15CAT2,RX16CAT2,RX17CAT2,RX18CAT2,RX19CAT2,RX20CAT2,RX21CAT2,RX22CAT2,RX23CAT2,RX24CAT2,RX25CAT2,RX26CAT2,RX27CAT2,RX28CAT2,RX29CAT2,RX30CAT2,   # Drug Category 2
         RX1CAT3,RX2CAT3,RX3CAT3,RX4CAT3,RX5CAT3,RX6CAT3,RX7CAT3,RX8CAT3,RX9CAT3,RX10CAT3,RX11CAT3,RX12CAT3,RX13CAT3,RX14CAT3,RX15CAT3,RX15CAT3,RX16CAT3,RX17CAT3,RX18CAT3,RX19CAT3,RX20CAT3,RX21CAT3,RX22CAT3,RX23CAT3,RX24CAT3,RX25CAT3,RX26CAT3,RX27CAT3,RX28CAT3,RX29CAT3,RX30CAT3,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30,   # Drug IDs
         NCMED1,NCMED2,NCMED3,NCMED4,NCMED5,NCMED6,NCMED7,NCMED8,NCMED9,NCMED10,NCMED11,NCMED12,NCMED12,NCMED13,NCMED14,NCMED15,NCMED16,NCMED17,NCMED18,NCMED19,NCMED20,NCMED21,NCMED22,NCMED23,NCMED24,NCMED25,NCMED26,NCMED27,NCMED28,NCMED29,NCMED30,   # New or continued
         MDDO,PRIMCARE,PHYSASST,NPNMW,MHP,OTHPROV,RNLPN,PHYS,  # Provider characteristics
         CAD, # Coronary artery disease
         DEPRN, # Depression
         HTN, # Hypertension
         OBESITY, # Obesity
         SUBSTAB, # Alcohol Drug Use/Dependence
         ESRD, # End-Stage Renal Disease
         CEBVD, # Cerebrovascular Disease
         PREGNANT) %>% # Pregnancy
  mutate(YEAR = 2018) %>% 
  droplevels() 

#2016
data_2016 <- raw_data_2016 %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,  # Demographic characteristics
         DIAG1,DIAG2,DIAG3,DIAG4,DIAG5, # Diagnoses
         PATWT, CSTRATM, CPSUM,  # Weighting
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,RX11CAT1,RX12CAT1,RX13CAT1,RX14CAT1,RX15CAT1,RX15CAT1,RX16CAT1,RX17CAT1,RX18CAT1,RX19CAT1,RX20CAT1,RX21CAT1,RX22CAT1,RX23CAT1,RX24CAT1,RX25CAT1,RX26CAT1,RX27CAT1,RX28CAT1,RX29CAT1,RX30CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,RX9CAT2,RX10CAT2,RX11CAT2,RX12CAT2,RX13CAT2,RX14CAT2,RX15CAT2,RX15CAT2,RX16CAT2,RX17CAT2,RX18CAT2,RX19CAT2,RX20CAT2,RX21CAT2,RX22CAT2,RX23CAT2,RX24CAT2,RX25CAT2,RX26CAT2,RX27CAT2,RX28CAT2,RX29CAT2,RX30CAT2,   # Drug Category 2
         RX1CAT3,RX2CAT3,RX3CAT3,RX4CAT3,RX5CAT3,RX6CAT3,RX7CAT3,RX8CAT3,RX9CAT3,RX10CAT3,RX11CAT3,RX12CAT3,RX13CAT3,RX14CAT3,RX15CAT3,RX15CAT3,RX16CAT3,RX17CAT3,RX18CAT3,RX19CAT3,RX20CAT3,RX21CAT3,RX22CAT3,RX23CAT3,RX24CAT3,RX25CAT3,RX26CAT3,RX27CAT3,RX28CAT3,RX29CAT3,RX30CAT3,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30,   # Drug IDs
         NCMED1,NCMED2,NCMED3,NCMED4,NCMED5,NCMED6,NCMED7,NCMED8,NCMED9,NCMED10,NCMED11,NCMED12,NCMED12,NCMED13,NCMED14,NCMED15,NCMED16,NCMED17,NCMED18,NCMED19,NCMED20,NCMED21,NCMED22,NCMED23,NCMED24,NCMED25,NCMED26,NCMED27,NCMED28,NCMED29,NCMED30,   # New or continued
         MDDO,PRIMCARE,PHYSASST,NPNMW,MHP,OTHPROV,RNLPN,PHYS,  # Provider characteristics
         CAD, # Coronary artery disease
         DEPRN, # Depression
         HTN, # Hypertension
         OBESITY, # Obesity
         SUBSTAB, # Alcohol Drug Use/Dependence
         ESRD, # End-Stage Renal Disease
         CEBVD, # Cerebrovascular Disease
         PREGNANT) %>% # Pregnancy
  mutate(YEAR = 2016) %>% 
  droplevels() 

#2015
data_2015 <- raw_data_2015 %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,  # Demographic characteristics
         DIAG1,DIAG2,DIAG3,DIAG4,DIAG5, # Diagnoses
         PATWT, CSTRATM, CPSUM,  # Weighting
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,RX11CAT1,RX12CAT1,RX13CAT1,RX14CAT1,RX15CAT1,RX15CAT1,RX16CAT1,RX17CAT1,RX18CAT1,RX19CAT1,RX20CAT1,RX21CAT1,RX22CAT1,RX23CAT1,RX24CAT1,RX25CAT1,RX26CAT1,RX27CAT1,RX28CAT1,RX29CAT1,RX30CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,RX9CAT2,RX10CAT2,RX11CAT2,RX12CAT2,RX13CAT2,RX14CAT2,RX15CAT2,RX15CAT2,RX16CAT2,RX17CAT2,RX18CAT2,RX19CAT2,RX20CAT2,RX21CAT2,RX22CAT2,RX23CAT2,RX24CAT2,RX25CAT2,RX26CAT2,RX27CAT2,RX28CAT2,RX29CAT2,RX30CAT2,   # Drug Category 2
         RX1CAT3,RX2CAT3,RX3CAT3,RX4CAT3,RX5CAT3,RX6CAT3,RX7CAT3,RX8CAT3,RX9CAT3,RX10CAT3,RX11CAT3,RX12CAT3,RX13CAT3,RX14CAT3,RX15CAT3,RX15CAT3,RX16CAT3,RX17CAT3,RX18CAT3,RX19CAT3,RX20CAT3,RX21CAT3,RX22CAT3,RX23CAT3,RX24CAT3,RX25CAT3,RX26CAT3,RX27CAT3,RX28CAT3,RX29CAT3,RX30CAT3,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30,   # Drug IDs
         NCMED1,NCMED2,NCMED3,NCMED4,NCMED5,NCMED6,NCMED7,NCMED8,NCMED9,NCMED10,NCMED11,NCMED12,NCMED12,NCMED13,NCMED14,NCMED15,NCMED16,NCMED17,NCMED18,NCMED19,NCMED20,NCMED21,NCMED22,NCMED23,NCMED24,NCMED25,NCMED26,NCMED27,NCMED28,NCMED29,NCMED30,   # New or continued
         MDDO,PRIMCARE,PHYSASST,NPNMW,MHP,OTHPROV,RNLPN,PHYS,  # Provider characteristics
         CAD, # Coronary artery disease
         DEPRN, # Depression
         HTN, # Hypertension
         OBESITY, # Obesity
         SUBSTAB, # Alcohol Drug Use/Dependence
         ESRD, # End-Stage Renal Disease
         CEBVD, # Cerebrovascular Disease
         PREGNANT) %>% # Pregnancy
  mutate(YEAR = 2015) %>% 
  droplevels() 

#2014
data_2014 <- raw_data_2014 %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,  # Demographic characteristics
         DIAG1,DIAG2,DIAG3,DIAG4,DIAG5, # Diagnoses
         PATWT, CSTRATM, CPSUM,  # Weighting
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,RX11CAT1,RX12CAT1,RX13CAT1,RX14CAT1,RX15CAT1,RX15CAT1,RX16CAT1,RX17CAT1,RX18CAT1,RX19CAT1,RX20CAT1,RX21CAT1,RX22CAT1,RX23CAT1,RX24CAT1,RX25CAT1,RX26CAT1,RX27CAT1,RX28CAT1,RX29CAT1,RX30CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,RX9CAT2,RX10CAT2,RX11CAT2,RX12CAT2,RX13CAT2,RX14CAT2,RX15CAT2,RX15CAT2,RX16CAT2,RX17CAT2,RX18CAT2,RX19CAT2,RX20CAT2,RX21CAT2,RX22CAT2,RX23CAT2,RX24CAT2,RX25CAT2,RX26CAT2,RX27CAT2,RX28CAT2,RX29CAT2,RX30CAT2,   # Drug Category 2
         RX1CAT3,RX2CAT3,RX3CAT3,RX4CAT3,RX5CAT3,RX6CAT3,RX7CAT3,RX8CAT3,RX9CAT3,RX10CAT3,RX11CAT3,RX12CAT3,RX13CAT3,RX14CAT3,RX15CAT3,RX15CAT3,RX16CAT3,RX17CAT3,RX18CAT3,RX19CAT3,RX20CAT3,RX21CAT3,RX22CAT3,RX23CAT3,RX24CAT3,RX25CAT3,RX26CAT3,RX27CAT3,RX28CAT3,RX29CAT3,RX30CAT3,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,DRUGID11,DRUGID12,DRUGID13,DRUGID14,DRUGID15,DRUGID16,DRUGID17,DRUGID18,DRUGID19,DRUGID20,DRUGID21,DRUGID22,DRUGID23,DRUGID24,DRUGID25,DRUGID26,DRUGID27,DRUGID28,DRUGID29,DRUGID30,   # Drug IDs
         NCMED1,NCMED2,NCMED3,NCMED4,NCMED5,NCMED6,NCMED7,NCMED8,NCMED9,NCMED10,NCMED11,NCMED12,NCMED12,NCMED13,NCMED14,NCMED15,NCMED16,NCMED17,NCMED18,NCMED19,NCMED20,NCMED21,NCMED22,NCMED23,NCMED24,NCMED25,NCMED26,NCMED27,NCMED28,NCMED29,NCMED30,   # New or continued
         MDDO,PRIMCARE,PHYSASST,NPNMW,MHP,OTHPROV,RNLPN,PHYS,  # Provider characteristics
         CAD, # Coronary artery disease
         DEPRN, # Depression
         HTN, # Hypertension
         OBESITY, # Obesity
         SUBSTAB, # Alcohol Drug Use/Dependence
         ESRD, # End-Stage Renal Disease
         CEBVD, # Cerebrovascular Disease
         PREGNANT) %>% # Pregnancy
  mutate(YEAR = 2014) %>% 
  droplevels() 

#2013
data_2013 <- raw_data_2013 %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,  # Demographic characteristics
         DIAG1,DIAG2,DIAG3, # Diagnoses
         PATWT, CSTRATM, CPSUM,  # Weighting
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,RX9CAT2,RX10CAT2,   # Drug Category 2
         RX1CAT3,RX2CAT3,RX3CAT3,RX4CAT3,RX5CAT3,RX6CAT3,RX7CAT3,RX8CAT3,RX9CAT3,RX10CAT3,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,   # Drug IDs
         NCMED1,NCMED2,NCMED3,NCMED4,NCMED5,NCMED6,NCMED7,NCMED8,NCMED9,NCMED10,   # New or continued
         MDDO,PRIMCARE,PHYSASST,NPNMW,MHP,OTHPROV,RNLPN,PHYS,  # Provider characteristics
         IHD, # Coronary artery disease
         DEPRN, # Depression
         HTN, # Hypertension
         OBESITY, # Obesity
         CEBVD, # Cerebrovascular Disease
         PREGNANT) %>% # Pregnancy
  mutate(YEAR = 2013) %>% 
  droplevels() 

#2012
data_2012 <- raw_data_2012 %>% 
  select(AGE, RACERETH, SEX, MSA, PAYTYPER,  # Demographic characteristics
         DIAG1,DIAG2,DIAG3, # Diagnoses
         PATWT, CSTRATM, CPSUM,  # Weighting
         RX1CAT1,RX2CAT1,RX3CAT1,RX4CAT1,RX5CAT1,RX6CAT1,RX7CAT1,RX8CAT1,RX9CAT1,RX10CAT1,   # Drug Category 1
         RX1CAT2,RX2CAT2,RX3CAT2,RX4CAT2,RX5CAT2,RX6CAT2,RX7CAT2,RX8CAT2,RX9CAT2,RX10CAT2,   # Drug Category 2
         RX1CAT3,RX2CAT3,RX3CAT3,RX4CAT3,RX5CAT3,RX6CAT3,RX7CAT3,RX8CAT3,RX9CAT3,RX10CAT3,   # Drug Category 2
         DRUGID1,DRUGID2,DRUGID3,DRUGID4,DRUGID5,DRUGID6,DRUGID7,DRUGID8,DRUGID9,DRUGID10,   # Drug IDs
         NCMED1,NCMED2,NCMED3,NCMED4,NCMED5,NCMED6,NCMED7,NCMED8,NCMED9,NCMED10,   # New or continued
         MDDO,PRIMCARE,PHYSASST,NPNMW,MHP,OTHPROV,RNLPN,PHYS,  # Provider characteristics
         IHD, # Coronary artery disease
         DEPRN, # Depression
         HTN, # Hypertension
         OBESITY, # Obesity
         CEBVD, # Cerebrovascular Disease
         PREGNANT) %>% # Pregnancy
  mutate(YEAR = 2012) %>% 
  droplevels() 




#### MERGE AND REFORMAT THE DATASET: RENAME, GROUP, AND ADD VARIABLES ####

### Merge each year's dataset ###
data_combined <- full_join(full_join(full_join(full_join(full_join(full_join(data_2014,data_2015),data_2016),data_2018),data_2019),data_2013),data_2012)

### Rename continuous variable codes ###
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


### Define Multum drug codes and ICD codes of interest ###
Stimulant_Rx_Codes <- c(71) # This is Multum category code

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

ADHD_ICD_codes <- c("F900","F901","F902","F908","F909", # These are ICD-10 codes
                    "314--","3140-","31400","31401","3141-","31410","3142-","31420","3148-","31480","3149-","31490") # These are ICD-9 codes

Narcolepsy_ICD_codes <- c("G474","G4741","G47411","G47419","G4742","G47421","G47429", # These are ICD-10 codes
                          "34700","3470-","347--","34701","34710","3471-","34711") # These are ICD-9 codes

Binge_Eating_ICD_codes <- c("E66-", "E660", "E6601", "E6609", "E661", "E662", "E663", "E668", "E669", # These are ICD-10 codes
                            "278", "278--", "278-", "2780-", "27800", "27801", "27802", "27803")  # These are ICD-9 codes

Tourette_ICD_codes <- c("F95.2", # This is an ICD-10 codes
                        "307.23") #this is an ICD-9 code

Glaucoma_ICD_codes <- c("H40-","H400","H401","H402","H403","H404","H405","H406","H408","H409", # These are ICD-10 codes
                        "3659-","36590","36520","36521","36522","36523","36524","36512","36513","36511","36510","36515","36501") # These are ICD-9 codes

Hyperthyrodisim_ICD_codes <- c("E05-","E050","E051","E052","E053","E054","E058","E059", # These are ICD-10 codes
                               "2420","2421","2422","2423","2424","2428","2429","2420-","2421-","2422-","2423-","2424-","2428-","2429-","24200","24210","24220","24230","24240","24280","24290") # These are ICD-9 codes

MAOI_codes <- c(250) # This is a Multum category code


### Group and recode selected variables ###

data_combined <- data_combined %>% 
  
  # Convert these variables to numeric
  mutate(YEAR = as.numeric(YEAR), AGE = as.numeric(AGE)) %>% 
  
  
  # Create several grouped categorical variables for age
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
  mutate(AGE_RECODE_4 = case_when(AGE<=45 ~"<=45",
                                  AGE>45 ~">45")) %>% 
  
  
  # Create grouped categorical variables and convert the numbers to the corresponding text
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
  
  
  # Create binary (0/1) variables for the occurrence of a stimulant drug codes in the data
  mutate(ANY_STIM = case_when(((RX1CAT1 %in% Stimulant_Rx_Codes | RX2CAT1 %in% Stimulant_Rx_Codes | RX3CAT1 %in% Stimulant_Rx_Codes | RX4CAT1 %in% Stimulant_Rx_Codes | RX5CAT1 %in% Stimulant_Rx_Codes | RX6CAT1 %in% Stimulant_Rx_Codes | RX7CAT1 %in% Stimulant_Rx_Codes | RX8CAT1 %in% Stimulant_Rx_Codes | RX9CAT1 %in% Stimulant_Rx_Codes | RX10CAT1 %in% Stimulant_Rx_Codes | RX11CAT1 %in% Stimulant_Rx_Codes | RX12CAT1 %in% Stimulant_Rx_Codes | RX13CAT1 %in% Stimulant_Rx_Codes | RX14CAT1 %in% Stimulant_Rx_Codes | RX15CAT1 %in% Stimulant_Rx_Codes | RX15CAT1 %in% Stimulant_Rx_Codes | RX16CAT1 %in% Stimulant_Rx_Codes | RX17CAT1 %in% Stimulant_Rx_Codes | RX18CAT1 %in% Stimulant_Rx_Codes | RX19CAT1 %in% Stimulant_Rx_Codes | RX20CAT1 %in% Stimulant_Rx_Codes | RX21CAT1 %in% Stimulant_Rx_Codes | RX22CAT1 %in% Stimulant_Rx_Codes | RX23CAT1 %in% Stimulant_Rx_Codes | RX24CAT1 %in% Stimulant_Rx_Codes | RX25CAT1 %in% Stimulant_Rx_Codes | RX26CAT1 %in% Stimulant_Rx_Codes | RX27CAT1 %in% Stimulant_Rx_Codes | RX28CAT1 %in% Stimulant_Rx_Codes | RX29CAT1 %in% Stimulant_Rx_Codes | RX30CAT1 %in% Stimulant_Rx_Codes |  
                                RX1CAT2 %in% Stimulant_Rx_Codes | RX2CAT2 %in% Stimulant_Rx_Codes | RX3CAT2 %in% Stimulant_Rx_Codes | RX4CAT2 %in% Stimulant_Rx_Codes | RX5CAT2 %in% Stimulant_Rx_Codes | RX6CAT2 %in% Stimulant_Rx_Codes | RX7CAT2 %in% Stimulant_Rx_Codes | RX8CAT2 %in% Stimulant_Rx_Codes | RX9CAT2 %in% Stimulant_Rx_Codes | RX10CAT2 %in% Stimulant_Rx_Codes | RX11CAT2 %in% Stimulant_Rx_Codes | RX12CAT2 %in% Stimulant_Rx_Codes | RX13CAT2 %in% Stimulant_Rx_Codes | RX14CAT2 %in% Stimulant_Rx_Codes | RX15CAT2 %in% Stimulant_Rx_Codes | RX15CAT2 %in% Stimulant_Rx_Codes | RX16CAT2 %in% Stimulant_Rx_Codes | RX17CAT2 %in% Stimulant_Rx_Codes | RX18CAT2 %in% Stimulant_Rx_Codes | RX19CAT2 %in% Stimulant_Rx_Codes | RX20CAT2 %in% Stimulant_Rx_Codes | RX21CAT2 %in% Stimulant_Rx_Codes | RX22CAT2 %in% Stimulant_Rx_Codes | RX23CAT2 %in% Stimulant_Rx_Codes | RX24CAT2 %in% Stimulant_Rx_Codes | RX25CAT2 %in% Stimulant_Rx_Codes | RX26CAT2 %in% Stimulant_Rx_Codes | RX27CAT2 %in% Stimulant_Rx_Codes | RX28CAT2 %in% Stimulant_Rx_Codes | RX29CAT2 %in% Stimulant_Rx_Codes | RX30CAT2 %in% Stimulant_Rx_Codes)) ~1,
                              .default = 0))  %>% 
  
  mutate(NEW_STIM = case_when((((RX1CAT1 %in% Stimulant_Rx_Codes & NCMED1==1) | (RX2CAT1 %in% Stimulant_Rx_Codes & NCMED2==1) | (RX3CAT1 %in% Stimulant_Rx_Codes & NCMED3==1) | (RX4CAT1 %in% Stimulant_Rx_Codes & NCMED4==1) | (RX5CAT1 %in% Stimulant_Rx_Codes & NCMED5==1) | (RX6CAT1 %in% Stimulant_Rx_Codes & NCMED6==1) | (RX7CAT1 %in% Stimulant_Rx_Codes & NCMED7==1) | (RX8CAT1 %in% Stimulant_Rx_Codes & NCMED8==1) | (RX9CAT1 %in% Stimulant_Rx_Codes & NCMED9==1) | (RX10CAT1 %in% Stimulant_Rx_Codes & NCMED10==1) | (RX11CAT1 %in% Stimulant_Rx_Codes & NCMED11==1) | (RX12CAT1 %in% Stimulant_Rx_Codes & NCMED12==1) | (RX13CAT1 %in% Stimulant_Rx_Codes & NCMED13==1) | (RX14CAT1 %in% Stimulant_Rx_Codes & NCMED14==1) | (RX15CAT1 %in% Stimulant_Rx_Codes & NCMED15==1) | (RX16CAT1 %in% Stimulant_Rx_Codes & NCMED16==1) | (RX17CAT1 %in% Stimulant_Rx_Codes & NCMED17==1) | (RX18CAT1 %in% Stimulant_Rx_Codes & NCMED18==1) | (RX19CAT1 %in% Stimulant_Rx_Codes & NCMED19==1) | (RX20CAT1 %in% Stimulant_Rx_Codes & NCMED20==1) | (RX21CAT1 %in% Stimulant_Rx_Codes & NCMED21==1) | (RX22CAT1 %in% Stimulant_Rx_Codes & NCMED22==1) | (RX23CAT1 %in% Stimulant_Rx_Codes & NCMED23==1) | (RX24CAT1 %in% Stimulant_Rx_Codes & NCMED24==1) | (RX25CAT1 %in% Stimulant_Rx_Codes & NCMED25==1) | (RX26CAT1 %in% Stimulant_Rx_Codes & NCMED26==1) | (RX27CAT1 %in% Stimulant_Rx_Codes & NCMED27==1) | (RX28CAT1 %in% Stimulant_Rx_Codes & NCMED28==1) | (RX29CAT1 %in% Stimulant_Rx_Codes & NCMED29==1) | (RX30CAT1 %in% Stimulant_Rx_Codes & NCMED30==1) |  
                                (RX1CAT2 %in% Stimulant_Rx_Codes & NCMED1==1) | (RX2CAT2 %in% Stimulant_Rx_Codes & NCMED2==1) | (RX3CAT2 %in% Stimulant_Rx_Codes & NCMED3==1) | (RX4CAT2 %in% Stimulant_Rx_Codes & NCMED4==1) | (RX5CAT2 %in% Stimulant_Rx_Codes & NCMED5==1) | (RX6CAT2 %in% Stimulant_Rx_Codes & NCMED6==1) | (RX7CAT2 %in% Stimulant_Rx_Codes & NCMED7==1) | (RX8CAT2 %in% Stimulant_Rx_Codes & NCMED8==1) | (RX9CAT2 %in% Stimulant_Rx_Codes & NCMED9==1) | (RX10CAT2 %in% Stimulant_Rx_Codes & NCMED10==1) | (RX11CAT2 %in% Stimulant_Rx_Codes & NCMED11==1) | (RX12CAT2 %in% Stimulant_Rx_Codes & NCMED12==1) | (RX13CAT2 %in% Stimulant_Rx_Codes & NCMED13==1) | (RX14CAT2 %in% Stimulant_Rx_Codes & NCMED14==1) | (RX15CAT2 %in% Stimulant_Rx_Codes & NCMED15==1) | (RX16CAT2 %in% Stimulant_Rx_Codes & NCMED16==1) | (RX17CAT2 %in% Stimulant_Rx_Codes & NCMED17==1) | (RX18CAT2 %in% Stimulant_Rx_Codes & NCMED18==1) | (RX19CAT2 %in% Stimulant_Rx_Codes & NCMED19==1) | (RX20CAT2 %in% Stimulant_Rx_Codes & NCMED20==1) | (RX21CAT2 %in% Stimulant_Rx_Codes & NCMED21==1) | (RX22CAT2 %in% Stimulant_Rx_Codes & NCMED22==1) | (RX23CAT2 %in% Stimulant_Rx_Codes & NCMED23==1) | (RX24CAT2 %in% Stimulant_Rx_Codes & NCMED24==1) | (RX25CAT2 %in% Stimulant_Rx_Codes & NCMED25==1) | (RX26CAT2 %in% Stimulant_Rx_Codes & NCMED26==1) | (RX27CAT2 %in% Stimulant_Rx_Codes & NCMED27==1) | (RX28CAT2 %in% Stimulant_Rx_Codes & NCMED28==1) | (RX29CAT2 %in% Stimulant_Rx_Codes & NCMED29==1) | (RX30CAT2 %in% Stimulant_Rx_Codes & NCMED30==1))) ~1,
                              (((RX1CAT1 %in% Stimulant_Rx_Codes & NCMED1==2) | (RX2CAT1 %in% Stimulant_Rx_Codes & NCMED2==2) | (RX3CAT1 %in% Stimulant_Rx_Codes & NCMED3==2) | (RX4CAT1 %in% Stimulant_Rx_Codes & NCMED4==2) | (RX5CAT1 %in% Stimulant_Rx_Codes & NCMED5==2) | (RX6CAT1 %in% Stimulant_Rx_Codes & NCMED6==2) | (RX7CAT1 %in% Stimulant_Rx_Codes & NCMED7==2) | (RX8CAT1 %in% Stimulant_Rx_Codes & NCMED8==2) | (RX9CAT1 %in% Stimulant_Rx_Codes & NCMED9==2) | (RX10CAT1 %in% Stimulant_Rx_Codes & NCMED10==2) | (RX11CAT1 %in% Stimulant_Rx_Codes & NCMED11==2) | (RX12CAT1 %in% Stimulant_Rx_Codes & NCMED12==2) | (RX13CAT1 %in% Stimulant_Rx_Codes & NCMED13==2) | (RX14CAT1 %in% Stimulant_Rx_Codes & NCMED14==2) | (RX15CAT1 %in% Stimulant_Rx_Codes & NCMED15==2) | (RX16CAT1 %in% Stimulant_Rx_Codes & NCMED16==2) | (RX17CAT1 %in% Stimulant_Rx_Codes & NCMED17==2) | (RX18CAT1 %in% Stimulant_Rx_Codes & NCMED18==2) | (RX19CAT1 %in% Stimulant_Rx_Codes & NCMED19==2) | (RX20CAT1 %in% Stimulant_Rx_Codes & NCMED20==2) | (RX21CAT1 %in% Stimulant_Rx_Codes & NCMED21==2) | (RX22CAT1 %in% Stimulant_Rx_Codes & NCMED22==2) | (RX23CAT1 %in% Stimulant_Rx_Codes & NCMED23==2) | (RX24CAT1 %in% Stimulant_Rx_Codes & NCMED24==2) | (RX25CAT1 %in% Stimulant_Rx_Codes & NCMED25==2) | (RX26CAT1 %in% Stimulant_Rx_Codes & NCMED26==2) | (RX27CAT1 %in% Stimulant_Rx_Codes & NCMED27==2) | (RX28CAT1 %in% Stimulant_Rx_Codes & NCMED28==2) | (RX29CAT1 %in% Stimulant_Rx_Codes & NCMED29==2) | (RX30CAT1 %in% Stimulant_Rx_Codes & NCMED30==2) |  
                                (RX1CAT2 %in% Stimulant_Rx_Codes & NCMED1==2) | (RX2CAT2 %in% Stimulant_Rx_Codes & NCMED2==2) | (RX3CAT2 %in% Stimulant_Rx_Codes & NCMED3==2) | (RX4CAT2 %in% Stimulant_Rx_Codes & NCMED4==2) | (RX5CAT2 %in% Stimulant_Rx_Codes & NCMED5==1) | (RX6CAT2 %in% Stimulant_Rx_Codes & NCMED6==2) | (RX7CAT2 %in% Stimulant_Rx_Codes & NCMED7==1) | (RX8CAT2 %in% Stimulant_Rx_Codes & NCMED8==1) | (RX9CAT2 %in% Stimulant_Rx_Codes & NCMED9==1) | (RX10CAT2 %in% Stimulant_Rx_Codes & NCMED10==1) | (RX11CAT2 %in% Stimulant_Rx_Codes & NCMED11==1) | (RX12CAT2 %in% Stimulant_Rx_Codes & NCMED12==1) | (RX13CAT2 %in% Stimulant_Rx_Codes & NCMED13==1) | (RX14CAT2 %in% Stimulant_Rx_Codes & NCMED14==1) | (RX15CAT2 %in% Stimulant_Rx_Codes & NCMED15==1) | (RX16CAT2 %in% Stimulant_Rx_Codes & NCMED16==1) | (RX17CAT2 %in% Stimulant_Rx_Codes & NCMED17==1) | (RX18CAT2 %in% Stimulant_Rx_Codes & NCMED18==1) | (RX19CAT2 %in% Stimulant_Rx_Codes & NCMED19==1) | (RX20CAT2 %in% Stimulant_Rx_Codes & NCMED20==1) | (RX21CAT2 %in% Stimulant_Rx_Codes & NCMED21==1) | (RX22CAT2 %in% Stimulant_Rx_Codes & NCMED22==1) | (RX23CAT2 %in% Stimulant_Rx_Codes & NCMED23==1) | (RX24CAT2 %in% Stimulant_Rx_Codes & NCMED24==1) | (RX25CAT2 %in% Stimulant_Rx_Codes & NCMED25==1) | (RX26CAT2 %in% Stimulant_Rx_Codes & NCMED26==1) | (RX27CAT2 %in% Stimulant_Rx_Codes & NCMED27==1) | (RX28CAT2 %in% Stimulant_Rx_Codes & NCMED28==1) | (RX29CAT2 %in% Stimulant_Rx_Codes & NCMED29==1) | (RX30CAT2 %in% Stimulant_Rx_Codes & NCMED30==1))) ~-9,
                              (((RX1CAT1 %in% Stimulant_Rx_Codes & NCMED1==-9) | (RX2CAT1 %in% Stimulant_Rx_Codes & NCMED2==-9) | (RX3CAT1 %in% Stimulant_Rx_Codes & NCMED3==-9) | (RX4CAT1 %in% Stimulant_Rx_Codes & NCMED4==-9) | (RX5CAT1 %in% Stimulant_Rx_Codes & NCMED5==-9) | (RX6CAT1 %in% Stimulant_Rx_Codes & NCMED6==-9) | (RX7CAT1 %in% Stimulant_Rx_Codes & NCMED7==-9) | (RX8CAT1 %in% Stimulant_Rx_Codes & NCMED8==-9) | (RX9CAT1 %in% Stimulant_Rx_Codes & NCMED9==-9) | (RX10CAT1 %in% Stimulant_Rx_Codes & NCMED10==-9) | (RX11CAT1 %in% Stimulant_Rx_Codes & NCMED11==-9) | (RX12CAT1 %in% Stimulant_Rx_Codes & NCMED12==-9) | (RX13CAT1 %in% Stimulant_Rx_Codes & NCMED13==-9) | (RX14CAT1 %in% Stimulant_Rx_Codes & NCMED14==-9) | (RX15CAT1 %in% Stimulant_Rx_Codes & NCMED15==-9) | (RX16CAT1 %in% Stimulant_Rx_Codes & NCMED16==-9) | (RX17CAT1 %in% Stimulant_Rx_Codes & NCMED17==-9) | (RX18CAT1 %in% Stimulant_Rx_Codes & NCMED18==-9) | (RX19CAT1 %in% Stimulant_Rx_Codes & NCMED19==-9) | (RX20CAT1 %in% Stimulant_Rx_Codes & NCMED20==-9) | (RX21CAT1 %in% Stimulant_Rx_Codes & NCMED21==-9) | (RX22CAT1 %in% Stimulant_Rx_Codes & NCMED22==-9) | (RX23CAT1 %in% Stimulant_Rx_Codes & NCMED23==-9) | (RX24CAT1 %in% Stimulant_Rx_Codes & NCMED24==-9) | (RX25CAT1 %in% Stimulant_Rx_Codes & NCMED25==-9) | (RX26CAT1 %in% Stimulant_Rx_Codes & NCMED26==-9) | (RX27CAT1 %in% Stimulant_Rx_Codes & NCMED27==-9) | (RX28CAT1 %in% Stimulant_Rx_Codes & NCMED28==-9) | (RX29CAT1 %in% Stimulant_Rx_Codes & NCMED29==-9) | (RX30CAT1 %in% Stimulant_Rx_Codes & NCMED30==-9) |  
                                (RX1CAT2 %in% Stimulant_Rx_Codes & NCMED1==-9) | (RX2CAT2 %in% Stimulant_Rx_Codes & NCMED2==-9) | (RX3CAT2 %in% Stimulant_Rx_Codes & NCMED3==-9) | (RX4CAT2 %in% Stimulant_Rx_Codes & NCMED4==-9) | (RX5CAT2 %in% Stimulant_Rx_Codes & NCMED5==-9) | (RX6CAT2 %in% Stimulant_Rx_Codes & NCMED6==-9) | (RX7CAT2 %in% Stimulant_Rx_Codes & NCMED7==-9) | (RX8CAT2 %in% Stimulant_Rx_Codes & NCMED8==-9) | (RX9CAT2 %in% Stimulant_Rx_Codes & NCMED9==-9) | (RX10CAT2 %in% Stimulant_Rx_Codes & NCMED10==-9) | (RX11CAT2 %in% Stimulant_Rx_Codes & NCMED11==-9) | (RX12CAT2 %in% Stimulant_Rx_Codes & NCMED12==-9) | (RX13CAT2 %in% Stimulant_Rx_Codes & NCMED13==-9) | (RX14CAT2 %in% Stimulant_Rx_Codes & NCMED14==-9) | (RX15CAT2 %in% Stimulant_Rx_Codes & NCMED15==-9) | (RX16CAT2 %in% Stimulant_Rx_Codes & NCMED16==-9) | (RX17CAT2 %in% Stimulant_Rx_Codes & NCMED17==-9) | (RX18CAT2 %in% Stimulant_Rx_Codes & NCMED18==-9) | (RX19CAT2 %in% Stimulant_Rx_Codes & NCMED19==-9) | (RX20CAT2 %in% Stimulant_Rx_Codes & NCMED20==-9) | (RX21CAT2 %in% Stimulant_Rx_Codes & NCMED21==-9) | (RX22CAT2 %in% Stimulant_Rx_Codes & NCMED22==-9) | (RX23CAT2 %in% Stimulant_Rx_Codes & NCMED23==-9) | (RX24CAT2 %in% Stimulant_Rx_Codes & NCMED24==-9) | (RX25CAT2 %in% Stimulant_Rx_Codes & NCMED25==-9) | (RX26CAT2 %in% Stimulant_Rx_Codes & NCMED26==-9) | (RX27CAT2 %in% Stimulant_Rx_Codes & NCMED27==-9) | (RX28CAT2 %in% Stimulant_Rx_Codes & NCMED28==-9) | (RX29CAT2 %in% Stimulant_Rx_Codes & NCMED29==-9) | (RX30CAT2 %in% Stimulant_Rx_Codes & NCMED30==-9))) ~-9,
                              .default = 0))  %>% 
  
  mutate(NEW_STIM_BINARY = case_when(NEW_STIM == 1 ~1,
                                     NEW_STIM == 0 ~0)) %>% 
  
  
  # Create binary (0/1) variables for the occurrence of other drug codes of interest in the data
  mutate(NON_STIM = case_when((DRUGID1 %in% Non_Stimulant_Rx_Codes | DRUGID2 %in% Non_Stimulant_Rx_Codes | DRUGID3 %in% Non_Stimulant_Rx_Codes | DRUGID4 %in% Non_Stimulant_Rx_Codes | DRUGID5 %in% Non_Stimulant_Rx_Codes | DRUGID6 %in% Non_Stimulant_Rx_Codes | DRUGID7 %in% Non_Stimulant_Rx_Codes | DRUGID8 %in% Non_Stimulant_Rx_Codes | DRUGID9 %in% Non_Stimulant_Rx_Codes | DRUGID10 %in% Non_Stimulant_Rx_Codes | DRUGID11 %in% Non_Stimulant_Rx_Codes | DRUGID12 %in% Non_Stimulant_Rx_Codes | DRUGID13 %in% Non_Stimulant_Rx_Codes | DRUGID14 %in% Non_Stimulant_Rx_Codes | DRUGID15 %in% Non_Stimulant_Rx_Codes | DRUGID16 %in% Non_Stimulant_Rx_Codes | DRUGID17 %in% Non_Stimulant_Rx_Codes | DRUGID18 %in% Non_Stimulant_Rx_Codes | DRUGID19 %in% Non_Stimulant_Rx_Codes | DRUGID20 %in% Non_Stimulant_Rx_Codes | DRUGID21 %in% Non_Stimulant_Rx_Codes | DRUGID22 %in% Non_Stimulant_Rx_Codes | DRUGID23 %in% Non_Stimulant_Rx_Codes | DRUGID24 %in% Non_Stimulant_Rx_Codes | DRUGID25 %in% Non_Stimulant_Rx_Codes | DRUGID26 %in% Non_Stimulant_Rx_Codes | DRUGID27 %in% Non_Stimulant_Rx_Codes | DRUGID28 %in% Non_Stimulant_Rx_Codes | DRUGID29 %in% Non_Stimulant_Rx_Codes | DRUGID30 %in% Non_Stimulant_Rx_Codes) ~"Non-Stim Rx",
                              .default = "No Non-Stim Rx")) %>% 
  
  mutate(MAOI = case_when(((RX1CAT1 %in% MAOI_codes | RX2CAT1 %in% MAOI_codes | RX3CAT1 %in% MAOI_codes | RX4CAT1 %in% MAOI_codes | RX5CAT1 %in% MAOI_codes | RX6CAT1 %in% MAOI_codes | RX7CAT1 %in% MAOI_codes | RX8CAT1 %in% MAOI_codes | RX9CAT1 %in% MAOI_codes | RX10CAT1 %in% MAOI_codes | RX11CAT1 %in% MAOI_codes | RX12CAT1 %in% MAOI_codes | RX13CAT1 %in% MAOI_codes | RX14CAT1 %in% MAOI_codes | RX15CAT1 %in% MAOI_codes | RX15CAT1 %in% MAOI_codes | RX16CAT1 %in% MAOI_codes | RX17CAT1 %in% MAOI_codes | RX18CAT1 %in% MAOI_codes | RX19CAT1 %in% MAOI_codes | RX20CAT1 %in% MAOI_codes | RX21CAT1 %in% MAOI_codes | RX22CAT1 %in% MAOI_codes | RX23CAT1 %in% MAOI_codes | RX24CAT1 %in% MAOI_codes | RX25CAT1 %in% MAOI_codes | RX26CAT1 %in% MAOI_codes | RX27CAT1 %in% MAOI_codes | RX28CAT1 %in% MAOI_codes | RX29CAT1 %in% MAOI_codes | RX30CAT1 %in% MAOI_codes |  
                            RX1CAT2 %in% MAOI_codes | RX2CAT2 %in% MAOI_codes | RX3CAT2 %in% MAOI_codes | RX4CAT2 %in% MAOI_codes | RX5CAT2 %in% MAOI_codes | RX6CAT2 %in% MAOI_codes | RX7CAT2 %in% MAOI_codes | RX8CAT2 %in% MAOI_codes | RX9CAT2 %in% MAOI_codes | RX10CAT2 %in% MAOI_codes | RX11CAT2 %in% MAOI_codes | RX12CAT2 %in% MAOI_codes | RX13CAT2 %in% MAOI_codes | RX14CAT2 %in% MAOI_codes | RX15CAT2 %in% MAOI_codes | RX15CAT2 %in% MAOI_codes | RX16CAT2 %in% MAOI_codes | RX17CAT2 %in% MAOI_codes | RX18CAT2 %in% MAOI_codes | RX19CAT2 %in% MAOI_codes | RX20CAT2 %in% MAOI_codes | RX21CAT2 %in% MAOI_codes | RX22CAT2 %in% MAOI_codes | RX23CAT2 %in% MAOI_codes | RX24CAT2 %in% MAOI_codes | RX25CAT2 %in% MAOI_codes | RX26CAT2 %in% MAOI_codes | RX27CAT2 %in% MAOI_codes | RX28CAT2 %in% MAOI_codes | RX29CAT2 %in% MAOI_codes | RX30CAT2 %in% MAOI_codes |
                            RX1CAT3 %in% MAOI_codes | RX2CAT3 %in% MAOI_codes | RX3CAT3 %in% MAOI_codes | RX4CAT3 %in% MAOI_codes | RX5CAT3 %in% MAOI_codes | RX6CAT3 %in% MAOI_codes | RX7CAT3 %in% MAOI_codes | RX8CAT3 %in% MAOI_codes | RX9CAT3 %in% MAOI_codes | RX10CAT3 %in% MAOI_codes | RX11CAT3 %in% MAOI_codes | RX12CAT3 %in% MAOI_codes | RX13CAT3 %in% MAOI_codes | RX14CAT3 %in% MAOI_codes | RX15CAT3 %in% MAOI_codes | RX15CAT3 %in% MAOI_codes | RX16CAT3 %in% MAOI_codes | RX17CAT3 %in% MAOI_codes | RX18CAT3 %in% MAOI_codes | RX19CAT3 %in% MAOI_codes | RX20CAT3 %in% MAOI_codes | RX21CAT3 %in% MAOI_codes | RX22CAT3 %in% MAOI_codes | RX23CAT3 %in% MAOI_codes | RX24CAT3 %in% MAOI_codes | RX25CAT3 %in% MAOI_codes | RX26CAT3 %in% MAOI_codes | RX27CAT3 %in% MAOI_codes | RX28CAT3 %in% MAOI_codes | RX29CAT3 %in% MAOI_codes | RX30CAT3 %in% MAOI_codes)) ~1,
                              .default = 0))  %>% 
  
  
  # Create binary (0/1) variables for the occurrence of condition codes of interest in the data
  mutate(CAD_RECODE = case_when(CAD==1 | IHD==1 ~1,
                                .default = 0)) %>% 
  
  mutate(PREGNANT_RECODE = case_when(PREGNANT==2 | PREGNANT==-7 ~0,
                                .default = 1)) %>% 
  
  mutate(ADHD = case_when(DIAG1 %in% ADHD_ICD_codes | DIAG2 %in% ADHD_ICD_codes | DIAG3 %in% ADHD_ICD_codes | DIAG4 %in% ADHD_ICD_codes | DIAG5 %in% ADHD_ICD_codes ~1,
                          .default = 0)) %>% 
  
  mutate(NARCOLEPSY = case_when(DIAG1 %in% Narcolepsy_ICD_codes | DIAG2 %in% Narcolepsy_ICD_codes | DIAG3 %in% Narcolepsy_ICD_codes | DIAG4 %in% Narcolepsy_ICD_codes | DIAG5 %in% Narcolepsy_ICD_codes ~1,
                          .default = 0)) %>% 
  
  mutate(BINGE_EATING = case_when(DIAG1 %in% Binge_Eating_ICD_codes | DIAG2 %in% Binge_Eating_ICD_codes | DIAG3 %in% Binge_Eating_ICD_codes | DIAG4 %in% Binge_Eating_ICD_codes | DIAG5 %in% Binge_Eating_ICD_codes ~1,
                                .default = 0)) %>% 
  
  mutate(TOURETTE = case_when(DIAG1 %in% Tourette_ICD_codes | DIAG2 %in% Tourette_ICD_codes | DIAG3 %in% Tourette_ICD_codes | DIAG4 %in% Tourette_ICD_codes | DIAG5 %in% Tourette_ICD_codes ~1,
                                .default = 0)) %>% 
  
  mutate(GLAUCOMA = case_when(DIAG1 %in% Glaucoma_ICD_codes | DIAG2 %in% Glaucoma_ICD_codes | DIAG3 %in% Glaucoma_ICD_codes | DIAG4 %in% Glaucoma_ICD_codes | DIAG5 %in% Glaucoma_ICD_codes ~1,
                              .default = 0)) %>% 
  
  mutate(HYPERTHYROIDISM = case_when(DIAG1 %in% Hyperthyrodisim_ICD_codes | DIAG2 %in% Hyperthyrodisim_ICD_codes | DIAG3 %in% Hyperthyrodisim_ICD_codes | DIAG4 %in% Hyperthyrodisim_ICD_codes | DIAG5 %in% Hyperthyrodisim_ICD_codes ~1,
                              .default = 0)) %>% 
  
  mutate(PA_ALONE = case_when(PHYSASST==1 & PHYS==0 ~1,
                              .default = 0)) %>% 
  mutate(APP_ALONE = case_when(((PHYSASST==1 & PHYS==0) | (NPNMW_RECODE==1 & PHYS==0)) ~1,
                         .default = 0)) %>% 
  
  
  # Create a binary (0/1) variable for the occurrence of ANY contraindicated condition or medication
  mutate(ANY_CONTRAINDICATED_CONDITION = case_when(CAD_RECODE == 1 | CEBVD == 1 | HTN == 1 | SUBSTAB == 1 | PREGNANT_RECODE== 1 | TOURETTE == 1 | GLAUCOMA == 1 | HYPERTHYROIDISM ==1 | MAOI == 1 ~1,
                                                    .default = 0)) %>% 
  
  # Create a variable for the number of contraindications
  mutate(NUM_CONTRAINDICATIONS = CAD_RECODE+CEBVD+HTN+TOURETTE+replace_na(SUBSTAB,0)+GLAUCOMA+PREGNANT_RECODE+HYPERTHYROIDISM+MAOI)




#### WEIGHTING ####

### Define survey design ###
weighting_design_namcs <- svydesign(id=~CPSUM, strata=~CSTRATM, weight=~PATWT,data=data_combined,nest=TRUE)


### Subset the weighted dataset ###

  ## Subset by Age ##
  all_age_weighted <- subset(weighting_design_namcs,AGE>=18)
  all_age_any_stim <- subset(all_age_weighted,ANY_STIM==1)
  all_age_new_stim <- subset(all_age_weighted,NEW_STIM==1)
  over65_weighted <- subset(weighting_design_namcs,AGE>=65)

  
  

#### DESCRIPTIVE DATA ####

### Unweighted total number of visits ###
  
      data_combined %>% 
        filter(AGE>=18) %>% 
        filter(YEAR==2013) %>% 
        filter(ANY_STIM == 1) %>% 
        summarise(n=n())
  
      data_combined %>% 
        filter(AGE>=18) %>% 
        group_by(YEAR) %>% 
        summarise(n=n())
      
  
      data_combined %>% 
        filter(AGE>=18) %>% 
        filter(APP_ALONE == 1) %>% 
        filter(ANY_STIM==1) %>% 
        summarise(n=n())
  
  svytable(~ANY_CONTRAINDICATED_CONDITION, design=all_age_any_stim)
  
  svytable(~PREGNANT+SEX+AGE_RECODE_3, design=all_age_weighted)
  svytable(~APP_ALONE, design=all_age_weighted)
  svytable(~PHYSASST+PHYS, design=all_age_weighted)

  svytable(~YEAR, design=all_age_new_stim)
  svyttest(YEAR~ANY_CONTRAINDICATED_CONDITION,all_age_new_stim)
  
  
### Descriptive table for 65+ ###  
  table_one_65 <- svyCreateTableOne(vars = c("YEAR","MSA","PAYTYPER_RECODE",
                                             "AGE_RECODE_3","SEX","RACERETH",
                                             "PRIMCARE_RECODE","MDDO_RECODE","PHYSASST_RECODE","NPNMW_RECODE","RNLPN_RECODE","MHP_RECODE","OTHPROV_RECODE",
                                             "ADHD","NARCOLEPSY","BINGE_EATING","OBESITY",
                                             "ANY_CONTRAINDICATED_CONDITION", "NUM_CONTRAINDICATIONS", "CAD_RECODE","CEBVD","HTN","SUBSTAB","PREGNANT","TOURETTE","GLAUCOMA","HYPERTHYROIDISM","MAOI",
                                             "NON_STIM"),
                                    strata = "ANY_STIM",
                                    data = over65_weighted,
                                    factorVars = c("YEAR","MSA","PAYTYPER_RECODE",
                                                   "AGE_RECODE_3","SEX","RACERETH",
                                                   "PRIMCARE_RECODE","MDDO_RECODE","PHYSASST_RECODE","NPNMW_RECODE","RNLPN_RECODE","MHP_RECODE","OTHPROV_RECODE",
                                                   "ADHD","NARCOLEPSY","BINGE_EATING","OBESITY",
                                                   "ANY_CONTRAINDICATED_CONDITION", "NUM_CONTRAINDICATIONS", "CAD_RECODE","CEBVD","HTN","SUBSTAB","PREGNANT","TOURETTE","GLAUCOMA","HYPERTHYROIDISM","MAOI",
                                                   "NON_STIM"))
  write.csv(print(table_one_65, format="f", noSpaces=TRUE, quote=FALSE, test=FALSE, showAllLevels=FALSE),"table_one_65.csv")
  

### Descriptive table for prevalent prescriptions ###
  
       table_one_any <- svyCreateTableOne(vars = c("YEAR","MSA","PAYTYPER_RECODE",
                                         "AGE_RECODE_3","SEX","RACERETH",
                                         "PRIMCARE_RECODE","MDDO_RECODE",
                                         "ADHD","NARCOLEPSY","BINGE_EATING","OBESITY",
                                         "ANY_CONTRAINDICATED_CONDITION", "NUM_CONTRAINDICATIONS", "CAD_RECODE","CEBVD","HTN","SUBSTAB","PREGNANT","TOURETTE","GLAUCOMA","HYPERTHYROIDISM","MAOI",
                                         "NON_STIM"),
                                strata = "ANY_STIM", 
                                data = all_age_weighted, 
                                factorVars = c("YEAR","MSA","PAYTYPER_RECODE",
                                               "AGE_RECODE_3","SEX","RACERETH",
                                               "PRIMCARE_RECODE","MDDO_RECODE",
                                               "ADHD","NARCOLEPSY","BINGE_EATING","OBESITY",
                                              "ANY_CONTRAINDICATED_CONDITION", "NUM_CONTRAINDICATIONS", "CAD_RECODE","CEBVD","HTN","SUBSTAB","PREGNANT","TOURETTE","GLAUCOMA","HYPERTHYROIDISM","MAOI",
                                              "NON_STIM"))
       write.csv(print(table_one_any, format="f", noSpaces=TRUE, quote=FALSE, test=FALSE, showAllLevels=FALSE),"table_one_any.csv")
 
# ### Descriptive table for new prescriptions ###
     
       table_one_new <- svyCreateTableOne(vars = c("YEAR","MSA","PAYTYPER_RECODE",
                                                   "AGE_RECODE_3","SEX","RACERETH",
                                                   "PRIMCARE_RECODE","MDDO_RECODE",
                                                   "ADHD","NARCOLEPSY","BINGE_EATING","OBESITY",
                                                   "ANY_CONTRAINDICATED_CONDITION", "NUM_CONTRAINDICATIONS", "CAD_RECODE","CEBVD","HTN","SUBSTAB","PREGNANT","TOURETTE","GLAUCOMA","HYPERTHYROIDISM","MAOI",
                                                   "NON_STIM"),
                                          strata = "NEW_STIM", 
                                          data = all_age_weighted, 
                                          factorVars = c("YEAR","MSA","PAYTYPER_RECODE",
                                                         "AGE_RECODE_3","SEX","RACERETH",
                                                         "PRIMCARE_RECODE","MDDO_RECODE",
                                                         "ADHD","NARCOLEPSY","BINGE_EATING","OBESITY",
                                                         "ANY_CONTRAINDICATED_CONDITION", "NUM_CONTRAINDICATIONS", "CAD_RECODE","CEBVD","HTN","SUBSTAB","PREGNANT","TOURETTE","GLAUCOMA","HYPERTHYROIDISM","MAOI",
                                                         "NON_STIM"))
       write.csv(print(table_one_new, format="f", noSpaces=TRUE, quote=FALSE, showAllLevels=FALSE),"table_one_new.csv")


      
      
#### PREDICTORS OF STIMULANT PRESCRIBING ####

## Prevalent prescription ##

      rx_any <- rbind(# Visit characteristics
                            svytable(~YEAR+ANY_STIM, design=all_age_weighted),
                            svytable(~MSA+ANY_STIM, design=all_age_weighted),
                            svytable(~PAYTYPER_RECODE+ANY_STIM, design=all_age_weighted),
                            # Patient characteristics
                            svytable(~AGE_RECODE_3+ANY_STIM, design=all_age_weighted),
                            svytable(~SEX+ANY_STIM, design=all_age_weighted),
                            svytable(~RACERETH+ANY_STIM, design=all_age_weighted),
                            # HCP characteristics
                            svytable(~PRIMCARE_RECODE+ANY_STIM,  design=all_age_weighted),
                            svytable(~MDDO_RECODE+ANY_STIM,  design=all_age_weighted),
                            # Contraindications
                            svytable(~CAD_RECODE+ANY_STIM,  design=all_age_weighted),
                            svytable(~CEBVD+ANY_STIM,  design=all_age_weighted),
                            svytable(~HTN+ANY_STIM,  design=all_age_weighted),
                            svytable(~SUBSTAB+ANY_STIM,  design=all_age_weighted),
                            svytable(~PREGNANT+ANY_STIM, design=all_age_weighted),
                            svytable(~GLAUCOMA+ANY_STIM, design=all_age_weighted),
                            svytable(~HYPERTHYROIDISM+ANY_STIM,  design=all_age_weighted),
                            svytable(~MAOI+ANY_STIM,  design=all_age_weighted))
      write.csv(rx_any,"rx_any.csv")

      write.csv(tidy(svyglm(ANY_STIM 
                       ~factor(YEAR)+
                       MSA+
                       relevel(factor(PAYTYPER_RECODE), ref = "Private Insurance")+
                       relevel(factor(AGE_RECODE_3), ref = "18-30")+
                       SEX+
                       RACERETH+
                       relevel(factor(PRIMCARE_RECODE), ref = "Specialist")+
                       relevel(factor(MDDO_RECODE), ref = "DO")+
                       CAD_RECODE+
                       CEBVD+
                       HTN+
                       #SUBSTAB+
                       PREGNANT_RECODE+
                       GLAUCOMA+
                       HYPERTHYROIDISM+
                       MAOI,
                     design = all_age_weighted,
                     family=quasipoisson(link=log)),
           exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95), 
        "regression-any_stim-2.csv")

## New prescription ##

      rx_new <- rbind(# Visit characteristics
                      svytable(~YEAR+NEW_STIM_BINARY, design=all_age_weighted),
                      svytable(~MSA+NEW_STIM_BINARY, design=all_age_weighted),
                      svytable(~PAYTYPER_RECODE+NEW_STIM_BINARY, design=all_age_weighted),
                      # Patient characteristics
                      svytable(~AGE_RECODE_3+NEW_STIM_BINARY, design=all_age_weighted),
                      svytable(~SEX+NEW_STIM_BINARY, design=all_age_weighted),
                      svytable(~RACERETH+NEW_STIM_BINARY, design=all_age_weighted),
                      # HCP characteristics
                      svytable(~PRIMCARE_RECODE+NEW_STIM_BINARY,  design=all_age_weighted),
                      svytable(~MDDO_RECODE+NEW_STIM_BINARY,  design=all_age_weighted),
                      # Contraindications
                      svytable(~CAD_RECODE+NEW_STIM_BINARY,  design=all_age_weighted),
                      svytable(~CEBVD+NEW_STIM_BINARY,  design=all_age_weighted),
                      svytable(~HTN+NEW_STIM_BINARY,  design=all_age_weighted),
                      svytable(~SUBSTAB+NEW_STIM_BINARY,  design=all_age_weighted),
                      svytable(~PREGNANT+NEW_STIM_BINARY, design=all_age_weighted),
                      svytable(~GLAUCOMA+NEW_STIM_BINARY, design=all_age_weighted),
                      svytable(~HYPERTHYROIDISM+NEW_STIM_BINARY,  design=all_age_weighted),
                      svytable(~MAOI+NEW_STIM_BINARY,  design=all_age_weighted))
      write.csv(rx_new,"rx_new.csv")

      write.csv(tidy(svyglm(NEW_STIM_BINARY 
                    ~factor(YEAR)+
                      MSA+
                      relevel(factor(PAYTYPER_RECODE), ref = "Private Insurance")+
                      relevel(factor(AGE_RECODE_3), ref = "18-30")+
                      SEX+
                      RACERETH+
                      relevel(factor(PRIMCARE_RECODE), ref = "Specialist")+
                      MDDO_RECODE+
                      CAD_RECODE+
                      CEBVD+
                      HTN+
                      #SUBSTAB+
                      PREGNANT_RECODE+
                      #GLAUCOMA+
                      HYPERTHYROIDISM,
                      #MAOI
                    design = all_age_weighted,
                    family=quasipoisson(link=log)),
            exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95), 
        "regression-new_stim.csv")
      


#### SECONDARY ANALYSIS WITH SUBSTAB ####
      
## Prevalent prescription ##
      
      write.csv(tidy(svyglm(ANY_STIM 
                            ~factor(YEAR)+
                              MSA+
                              relevel(factor(PAYTYPER_RECODE), ref = "Private Insurance")+
                              relevel(factor(AGE_RECODE_3), ref = "18-30")+
                              SEX+
                              RACERETH+
                              relevel(factor(PRIMCARE_RECODE), ref = "Specialist")+
                              MDDO_RECODE+
                              CAD_RECODE+
                              CEBVD+
                              HTN+
                              SUBSTAB+
                              PREGNANT_RECODE+
                              GLAUCOMA+
                              HYPERTHYROIDISM+
                              MAOI,
                            design = all_age_weighted,
                            family=quasipoisson(link=log)),
                     exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95), 
                "SECONDARY-regression-any_stim.csv")
      
## New prescription ##
      
      write.csv(tidy(svyglm(NEW_STIM_BINARY 
                            ~factor(YEAR)+
                              MSA+
                              relevel(factor(PAYTYPER_RECODE), ref = "Private Insurance")+
                              relevel(factor(AGE_RECODE_3), ref = "18-30")+
                              SEX+
                              RACERETH+
                              relevel(factor(PRIMCARE_RECODE), ref = "Specialist")+
                              MDDO_RECODE+
                              CAD_RECODE+
                              CEBVD+
                              HTN+
                              SUBSTAB+
                              PREGNANT_RECODE+
                              #GLAUCOMA+
                              HYPERTHYROIDISM,
                              #MAOI
                            design = all_age_weighted,
                            family=quasipoisson(link=log)), 
                     exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95), 
                "SECONDARY-regression-new_stim.csv")
      
      
      
      
#### PREDICTORS OF INAPPROPRIATE PRESCRIBING ####

## Prevalent inappropriate prescription ##

      inap_rx_any <- rbind(# Visit characteristics
                            svytable(~YEAR+ANY_CONTRAINDICATED_CONDITION, design=all_age_any_stim),
                            svytable(~MSA+ANY_CONTRAINDICATED_CONDITION, design=all_age_any_stim),
                            svytable(~PAYTYPER_RECODE+ANY_CONTRAINDICATED_CONDITION, design=all_age_any_stim),
                            # Patient characteristics
                            svytable(~AGE_RECODE_3+ANY_CONTRAINDICATED_CONDITION, design=all_age_any_stim),
                            svytable(~SEX+ANY_CONTRAINDICATED_CONDITION, design=all_age_any_stim),
                            svytable(~RACERETH+ANY_CONTRAINDICATED_CONDITION, design=all_age_any_stim),
                             # HCP characteristics
                            svytable(~PRIMCARE_RECODE+ANY_CONTRAINDICATED_CONDITION,  design=all_age_any_stim),
                            svytable(~MDDO_RECODE+ANY_CONTRAINDICATED_CONDITION,  design=all_age_any_stim))
      # write.csv(inap_rx_any,"inap_rx_any.csv")
      
      a173 <- tidy(svyglm(ANY_CONTRAINDICATED_CONDITION 
                    ~factor(YEAR)+
                      MSA+
                      relevel(factor(PAYTYPER_RECODE), ref = "Private Insurance")+
                      relevel(factor(AGE_RECODE_3), ref = "18-30")+
                      SEX+
                      RACERETH+
                      relevel(factor(PRIMCARE_RECODE), ref = "Specialist")+
                      MDDO_RECODE,
                    design = all_age_any_stim,
                    family=quasipoisson(link=log)), 
             exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)
      
      write.csv(a173, "regression_any_inap-2.csv")

## New inappropriate prescription ##

      inap_rx_new <- rbind(# Visit characteristics
                            svytable(~YEAR+ANY_CONTRAINDICATED_CONDITION, design=all_age_new_stim),
                            svytable(~MSA+ANY_CONTRAINDICATED_CONDITION, design=all_age_new_stim),
                            svytable(~PAYTYPER_RECODE+ANY_CONTRAINDICATED_CONDITION, design=all_age_new_stim),
                            # Patient characteristics
                            svytable(~AGE_RECODE_3+ANY_CONTRAINDICATED_CONDITION, design=all_age_new_stim),
                            svytable(~SEX+ANY_CONTRAINDICATED_CONDITION, design=all_age_new_stim),
                            svytable(~RACERETH+ANY_CONTRAINDICATED_CONDITION, design=all_age_new_stim),
                            # HCP characteristics
                            svytable(~PRIMCARE_RECODE+ANY_CONTRAINDICATED_CONDITION,  design=all_age_new_stim),
                            svytable(~MDDO_RECODE+ANY_CONTRAINDICATED_CONDITION,  design=all_age_new_stim))
      write.csv(inap_rx_new,"inap_rx_new.csv")

      a174 <- tidy(svyglm(ANY_CONTRAINDICATED_CONDITION 
                    ~factor(YEAR)+
                      MSA+
                      relevel(factor(PAYTYPER_RECODE), ref = "Private Insurance")+
                      relevel(factor(AGE_RECODE_3), ref = "18-30")+
                      SEX+
                      RACERETH+
                      relevel(factor(PRIMCARE_RECODE), ref = "Specialist")+
                      MDDO_RECODE,
                    design = all_age_new_stim,
                    family=quasipoisson(link=log)), 
              exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)
      
      write.csv(a174, "regression_new_inap-2.csv")


      

#### FIGURES ####

### Graph of the proportion of total visits with a prevalent stimulant prescription, by year and age group  ###
any_stim_by_year <- svyby(~ANY_STIM, ~YEAR+AGE_RECODE_3, all_age_weighted, na=TRUE, svymean) 
any_stim_by_year <- cbind(any_stim_by_year, confint(any_stim_by_year))
colnames(any_stim_by_year) <- c("YEAR", "AGE_RECODE_3", "STIMULANT", "SE", "LOWER_CI", "UPPER_CI")

ggplot(any_stim_by_year, aes(x=YEAR, y=STIMULANT, color = AGE_RECODE_3))+
  geom_line()+
  geom_point()+
  geom_ribbon(aes(ymin=LOWER_CI,ymax=UPPER_CI, fill = AGE_RECODE_3), alpha=0.2)+
  labs(title = "Proportion of Total Visits with a Prevalent Stimulant Prescription, by Age Group", y = "Proportion of Total Visits With ANY Stimulant Rx", x = "Year")


### Graph of the proportion of total visits with a NEW stimulant prescription, by year and age group  ###
new_stim_by_year <- svyby(~NEW_STIM_BINARY, ~YEAR+AGE_RECODE_3, all_age_weighted, na=TRUE, svymean) 
new_stim_by_year <- cbind(new_stim_by_year, confint(new_stim_by_year))
colnames(new_stim_by_year) <- c("YEAR", "AGE_RECODE_3", "STIMULANT", "SE", "LOWER_CI", "UPPER_CI")

ggplot(new_stim_by_year, aes(x=YEAR, y=STIMULANT, color = AGE_RECODE_3))+
  geom_line()+
  geom_point()+
  geom_ribbon(aes(ymin=LOWER_CI,ymax=UPPER_CI, fill = AGE_RECODE_3), alpha=0.2)+
  labs(y = "Proportion of Total Visits With a NEW Stimulant Prescription", x = "Year")


### Graph of the proportion of total visits with an inappropriate stimulant prescription, by year ###
inapp_stim_by_year <- svyby(~ANY_CONTRAINDICATED_CONDITION, ~YEAR, all_age_any_stim, na=TRUE, svymean) 
inapp_stim_by_year <- cbind(inapp_stim_by_year, confint(inapp_stim_by_year))
colnames(inapp_stim_by_year) <- c("YEAR", "STIMULANT", "SE", "LOWER_CI", "UPPER_CI")

write.csv(inapp_stim_by_year,"fig1.csv")

ggplot(inapp_stim_by_year, aes(x=YEAR, y=STIMULANT, group=YEAR))+
  geom_boxplot()+
  scale_y_continuous(labels = scales::percent)
  

## REV2 FINAL 11.16.2024
## /s/Pavan V. Thakkar
