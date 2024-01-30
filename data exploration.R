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


#find out the number of casualties per county
county_casualties<- rd_16 %>% 
  filter(!is.na(COUNTY)) %>% 
  group_by(COUNTY) %>% 
  summarise(total_cas = sum(NO., na.rm = TRUE)) %>% 
  arrange(desc(total_cas)) #Arrange in descending order of total casualties
 
ggplot(county_casualties, aes(x = total_cas, y = reorder(COUNTY, total_cas))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Casualties Per County",
       x = "County",
       y = "Number of Casualties") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better readability


 county_casualties2<- rd_17 %>% 
  filter(!is.na(COUNTY)) %>% 
  group_by(COUNTY) %>% 
  summarise(total_cas = sum(NO., na.rm = TRUE)) %>% 
  arrange(desc(total_cas)) #Arrange in descending order of total casualties

ggplot(county_casualties2, aes(x = total_cas, y = reorder(COUNTY, total_cas))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Casualties Per County",
       x = "County",
       y = "Number of Casualties") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better readability


#aggregate number of accidents by road and county
accident_by_road <- rd_16 %>% 
  group_by(ROAD,COUNTY) %>% 
  summarise(Total_accidents = n(), Total_casualties = sum(NO., na.rm=TRUE)) %>%  #shorthand for counting no. of observations within each group
  arrange(desc(Total_accidents))

top_accidents <- head(accident_by_road, 10)
top_accidents

accident_by_road2 <- rd_17 %>% 
  group_by(ROAD,COUNTY) %>% 
  summarise(Total_accidents = n(), Total_casualties = sum(NO., na.rm=TRUE)) %>%  #shorthand for counting no. of observations within each group
  arrange(desc(Total_accidents))

top_accidents2 <- head(accident_by_road2, 10)
top_accidents2
