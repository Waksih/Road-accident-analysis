#load the necessary packages
install.packages("ggplot2")
library(ggplot2)
library(dplyr)

#extract month from the date column
monthly_casualties <- rd_16 %>% # extract the months from the Date column and sum the total casualties each month
  filter(!is.na(`Date DD/MM/YYYY`)) %>% 
  mutate(Month = format(`Date DD/MM/YYYY`, "%Y-%m")) %>% 
  group_by(Month) %>% 
  summarise(Total_casualties = sum(NO. , na.rm = TRUE))
monthly_casualties


monthly_casualties2 <- rd_17 %>% 
  filter(!is.na(`Date DD/MM/YYYY`)) %>% 
  mutate(Month = format(`Date DD/MM/YYYY`, "%Y-%m")) %>% 
  group_by(Month) %>% 
  summarise(Total_casualties = sum(NO. , na.rm = TRUE))
monthly_casualties2

#plot the casualties per month for both datasets
ggplot(monthly_casualties, aes(x = Month, y = Total_casualties))+
  geom_bar(stat = "identity", fill = "skyblue")+
  labs(title = "Number of casualties each month in 2016",
       x = "MONTH",
       y = "NUMBER OF CASUALTIES")+
  theme_minimal()+
  theme(axis.text.x= element_text(angle = 90, hjust = 1))



ggplot(monthly_casualties2, aes(x = Month, y = Total_casualties))+
  geom_bar(stat = "identity", fill = "skyblue")+
  labs(title = "Number of casualties each month in 2016",
       x = "MONTH",
       y = "NUMBER OF CASUALTIES")+
  theme_minimal()+
  theme(axis.text.x= element_text(angle = 90, hjust = 1))
