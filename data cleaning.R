install.packages("tidyverse")
library(tidyverse)
library(readxl)

kenya_accidents_database_xlsx_1 <- read_excel("kenya-accidents-database-xlsx-1.xlsx", +     sheet = "2016")


rd_16<-kenya_accidents_database_xlsx_1
View(rd_16)

kenya_accidents_database_xlsx_1 <- read_excel("kenya-accidents-database-xlsx-1.xlsx", 
                                                +     sheet = "2017")

rd_17<-kenya_accidents_database_xlsx_1
View(rd_17)

str(rd_16)
str(rd_17)

glimpse(rd_16)
glimpse(rd_17)
names(rd_16)
names(rd_17)
rd_17 %>% 
  select(...15) %>% 
  unique()

rd_16 %>% 
  select(...13) %>% 
  unique()

rd_16<-rd_16[,-13]
View(rd_16)

rd_17<-rd_17[,-15]
View(rd_17)
class(rd_17$`Date DD/MM/YYYY`)
class(rd_16$`Date DD/MM/YYYY`)

any(is.na(rd_16))
any(is.na(rd_17))

any(is.na(rd_16$`TIME 24 HOURS`))
any(is.na(rd_16$`BASE/SUB BASE`))
any(is.na(rd_16$COUNTY))
any(is.na(rd_16$ROAD))
any(is.na(rd_16$PLACE))

j<-names(rd_16)
j
for (i in 1:length(j)) {
  if(any(is.na(i))){
    return(TRUE)
  }else{
    return(FALSE)
  }
}








