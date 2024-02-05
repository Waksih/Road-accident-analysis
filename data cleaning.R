#install and load the necessary packages
install.packages("tidyverse")
library(tidyverse)
library(readxl)

#import the datasets and put them into an object
kenya_accidents_database_xlsx_1 <- read_excel("kenya-accidents-database-xlsx-1.xlsx", +     sheet = "2016")
rd_16<-kenya_accidents_database_xlsx_1
View(rd_16)


kenya_accidents_database_xlsx_1 <- read_excel("kenya-accidents-database-xlsx-1.xlsx", 
                                                +     sheet = "2017")
rd_17<-kenya_accidents_database_xlsx_1
View(rd_17)


#explore the data to see its structure and number of variables
str(rd_16)
str(rd_17)
glimpse(rd_16)
glimpse(rd_17)
names(rd_16)
names(rd_17)


#delete the unnecessary column from both datasets
rd_16<-rd_16[,-13]
rd_17<-rd_17[,-15]
rd_17<-rd_17[,c(-6,-8)] #delete the columns not found in rd_16

#change the classes of the columns to b suitable for analysis
rd_16$COUNTY<-as.factor(rd_16$COUNTY)



#convert the time column to POSIXct for better analysis
time<-rd_16$`TIME 24 HOURS`
time[time == "UNKNOWN TIME" | time == ""]<-NA #convert the unknown values into NA
time1<-sprintf("%04d", as.numeric(time)) #process time values to ensure they have 4 decimal places
#combine date and time
datetime<- as.POSIXct(paste(rd_16$`Date DD/MM/YYYY`,time1), format = "%Y-%m-%d %H%M", tz = "Africa/Nairobi")
rd_16$`TIME 24 HOURS`<-datetime


any(is.na(rd_17$`TIME 24 HOURS`))
unique(rd_17$`TIME 24 HOURS`) #find the unique observations in the time column
time2<-rd_17$`TIME 24 HOURS`
time2[time2 %in% c("UNKNOWN TIME", "UNKNOWN", "NAROK")]<-NA
time2<-gsub("HRS" , "", time2) #handle time values with "HRS suffix
time2<-gsub("[^0-9]", "", time2) #extract only the integers form the time values
time2<-sprintf("%04d", as.numeric(time2)) #ensure the time values have 4 decimal places
#combine  date and time
datetime2 <- as.POSIXct(paste(rd_17$`Date DD/MM/YYYY`, time2), format = "%Y-%m-%d %H%M", tz = "Africa/Nairobi")
rd_17$`TIME 24 HOURS` <- datetime2





#confirm the class of the date column in both datasets
class(rd_17$`Date DD/MM/YYYY`)
class(rd_16$`Date DD/MM/YYYY`)

#check for missing values in the datasets 
any(is.na(rd_16))
any(is.na(rd_17))

#check column by column for the missing values then replace them
any(is.na(rd_16$`TIME 24 HOURS`))
any(is.na(rd_16$`BASE/SUB BASE`))
any(is.na(rd_16$COUNTY))
any(is.na(rd_16$ROAD))
any(is.na(rd_16$PLACE))

which(is.na(rd_16$`TIME 24 HOURS`))







