## Load Packages 
library(haven)
library(dplyr)
library(tidyverse)

## Download files from the CDC website
download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/dataset_documentation/namcs/stata/NAMCS2019-stata.zip",destfile = "NAMCS2019-stata.zip")
download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/dataset_documentation/namcs/stata/NAMCS2018-stata.zip",destfile = "NAMCS2018-stata.zip")
download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/dataset_documentation/namcs/stata/namcs2016-stata.zip",destfile = "NAMCS2016-stata.zip")
download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/dataset_documentation/namcs/stata/namcs2015-stata.zip",destfile = "NAMCS2015-stata.zip")
download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/dataset_documentation/namcs/stata/namcs2014-stata.zip",destfile = "NAMCS2014-stata.zip")
download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/dataset_documentation/namcs/stata/namcs2013-stata.zip",destfile = "NAMCS2013-stata.zip")
download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/dataset_documentation/namcs/stata/namcs2012-stata.zip",destfile = "NAMCS2012-stata.zip")
download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/dataset_documentation/namcs/stata/namcs2011-stata.zip",destfile = "NAMCS2011-stata.zip")
download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/dataset_documentation/namcs/stata/namcs2010-stata.exe",destfile = "NAMCS2010-stata.exe")
download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/dataset_documentation/namcs/stata/namcs09-stata.exe",destfile = "NAMCS2009-stata.exe")

## Unzip data files
unzip("NAMCS2019-stata.zip")
unzip("NAMCS2018-stata.zip")
unzip("NAMCS2016-stata.zip")
unzip("NAMCS2015-stata.zip")
unzip("NAMCS2014-stata.zip")
unzip("NAMCS2013-stata.zip")
unzip("NAMCS2012-stata.zip")
unzip("NAMCS2011-stata.zip")
unzip("NAMCS2010-stata.exe")
unzip("NAMCS2009-stata.exe")

## Read data files
namcs2019 <- read_dta("NAMCS2019-stata.dta") 
namcs2018 <- read_dta("NAMCS2018-stata.dta") 
namcs2016 <- read_dta("NAMCS2016-stata.dta") 
namcs2015 <- read_dta("NAMCS2015-stata.dta") 
namcs2014 <- read_dta("NAMCS2014-stata.dta") 
namcs2013 <- read_dta("NAMCS2013-stata.dta") 
namcs2012 <- read_dta("NAMCS2012-stata.dta") 
namcs2011 <- read_dta("NAMCS2011.dta") 
namcs2010 <- read_dta("NAMCS2010-stata.dta") 
namcs2009 <- read_dta("NAMCS09-stata.dta") 

## Create CSV files
write.csv(namcs2019, "namcs2019.csv")
write.csv(namcs2018, "namcs2018.csv")
write.csv(namcs2016, "namcs2016.csv")
write.csv(namcs2015, "namcs2015.csv")
write.csv(namcs2014, "namcs2014.csv")
write.csv(namcs2013, "namcs2013.csv")
write.csv(namcs2012, "namcs2012.csv")
write.csv(namcs2011, "namcs2011.csv")
write.csv(namcs2010, "namcs2010.csv")
write.csv(namcs2009, "namcs2009.csv")

