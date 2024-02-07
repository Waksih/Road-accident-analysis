library(lubridate)
install.packages("categoryEncoders")
library(scales)
library(stringi)
library(data.table)
library(hash)
library(vtreat)


#extract the hour component and find the mean and median of accident times
hr<-hour(rd_16$`TIME 24 HOURS`)
head(hr,100)
mean(hr, na.rm = TRUE)
median(hr, na.rm = TRUE)  


hr2<-hour(rd_17$`TIME 24 HOURS`)
head(hr2, 10)
mean(hr2, na.rm = TRUE)
median(hr2, na.rm = TRUE)

#from the outcome of the mean and median, find ou the common causes of accidents between 1pm and 5pm

#filter accidents that occured between 1pm and 5pm
accident_1_to_5 <- rd_16 %>% 
  filter(hour(`TIME 24 HOURS`) >= 13 & hour(`TIME 24 HOURS`) < 17)
#check the causes of accidents for the filtered subset
acc_cause <- accident_1_to_5$`BRIEF ACCIDENT DETAILS`
View(sort(table(acc_cause), decreasing = TRUE)) #sort the causes of accidents


accident2_1_to_5 <- rd_17 %>% 
  filter(hour(`TIME 24 HOURS`) >= 13 & hour(`TIME 24 HOURS`) < 17)
#check the causes of accidents for the filtered subset
acc_cause <- accident2_1_to_5$`BRIEF ACCIDENT DETAILS`
View(sort(table(acc_cause), decreasing = TRUE)) #sort the causes of accidents


#CATEGORIZE TIME INTO DAY AND NIGHT
#for rd_16
rd_16 <- rd_16 %>% 
  mutate(Time_Category = if_else(hour(`TIME 24 HOURS`) >= 6 & hour(`TIME 24 HOURS`) < 18, "Day","Night"))
#Count accidents in each category
acc_by_category <- rd_16 %>% 
  group_by(Time_Category) %>% 
  summarise(total_acc = n())
acc_by_category # display the counts

#for rd_17
rd_17 <- rd_17 %>% 
  mutate(Time_Category = if_else(hour(`TIME 24 HOURS`) >= 6 & hour(`TIME 24 HOURS`) < 18, "Day","Night"))
#Count accidents in each category
acc_by_category2 <- rd_17 %>% 
  group_by(Time_Category) %>% 
  summarise(total_acc = n())
acc_by_category2 # display the counts


#INVESTIGATE WHETHER THE TIME OF DAY AFFECTS THE SEVERITY OF ACCIDENTS
#Use hypothesis testing to compare the mean no. of victims during the day and those at night
combined_data <- bind_rows(rd_16, rd_17) #combine the datasets
View(combined_data)
#subset the data for accidents during the day and night 
day_accidents <- combined_data %>% filter(Time_Category == "Day")
night_accidents <- combined_data %>% filter(Time_Category == "Night")

#PERFORM A TWO-SAMPLE T-TEST
#Null Hypothesis (H₀): The null hypothesis is that there is no difference in the mean number of victims between accidents during the day and accidents at night.
#Alternative Hypothesis (Hₐ): The alternative hypothesis is that there is a difference in the mean number of victims between accidents during the day and accidents at night.
t_test_result <- t.test(x = day_accidents$NO., y = night_accidents$NO., alternative = "two.sided", conf.level = 0.95, var.equal = TRUE)
print(t_test_result)

#Conclusion:
#Since the p-value is less than the significance level (0.05), we reject the null hypothesis. This suggests that there is evidence to support the alternative hypothesis, indicating that there is a statistically significant difference in the mean number of victims between accidents during the day and accidents at night.

# Perform a one-tailed (right-tailed) t-test
#Null Hypothesis (H₀): The null hypothesis is that there is no difference in the mean number of victims between accidents during the day and accidents at night.
#Alternative Hypothesis (Hₐ): The alternative hypothesis is that the mean number of victims for accidents at night is greater than the mean number of victims for accidents during the day.
t_test_result2 <- t.test(x = day_accidents$NO., y = night_accidents$NO., alternative = "greater", conf.level = 0.95, var.equal = TRUE)

# Print the result
print(t_test_result2)

#Conclusion:
#Since the p-value is greater than the significance level (0.05), we fail to reject the null hypothesis. This means that there is not enough evidence to conclude that there are more casualties during night accidents than day accidents based on the chosen significance level.


#VISUALISE YOUR FINDINGS
combined_data<- combined_data %>% 
  mutate(Hour = hour(`TIME 24 HOURS`)) %>% 
  filter(!is.na(Hour)) #exclude NA values from the Hour variable

#aggregate total casualties by hour
casualties_by_hour <- combined_data %>% 
  group_by(Hour) %>% 
  summarise(total_victims = sum(NO., na.rm = TRUE)) %>% 
  ungroup()

# Convert hour to character with leading zeros
casualties_by_hour$Hour <- sprintf("%02d:00", casualties_by_hour$Hour)


#plot the bar graph
ggplot(casualties_by_hour, aes(x = Hour, y= total_victims))+
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Casualties Spread Out in 24 Hours",
       x = "Hour of Day",
       y = "Total Casualties") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#since accidents that happen at 8 result in the most victims, lets find the major accident details around that time
accidents_btwn_7to9 <- combined_data %>% 
  filter(Hour >= 19 & Hour <= 21)
#check the causes of accidents for the filtered subset
accident_cause <- accidents_btwn_7to9$`BRIEF ACCIDENT DETAILS`
View(sort(table(accident_cause), decreasing = TRUE)) #sort the causes of accidents

#INVESTIGATING WHETHER TIME OF DAY AND CAUSE CODE INFLUENCE NUMBER OF CASUALTIES
#preparing the dataset
model_data <- combined_data %>% 
  select(NO., Hour, ROAD, COUNTY) %>% 
  mutate(ROAD = as.factor(ROAD), COUNTY = as.factor(COUNTY)) %>% 
  filter(complete.cases(.))
  

#convert the categorical variables to dummy data

#use feature hashing
#perform feature hashing for ROAD
# Preprocess the ROAD column: remove special characters and normalize to lowercase
model_data$ROAD <- tolower(gsub("[^[:alnum:] ]", "", model_data$ROAD))

# Remove spaces from road names
model_data$ROAD <- gsub(" ", "", model_data$ROAD)

# Perform feature hashing for ROAD
num_features <- 596 # define number of features

feature_hash <- function(x, num_features) {
  hash_value <- digest(x)
  hash_integer <- sum(utf8ToInt(hash_value))  # Sum of Unicode code points
  return(hash_integer %% num_features + 1)
}

# Apply the feature hashing function to each element of the ROAD column
model_data$hashed_ROAD <- sapply(model_data$ROAD, feature_hash, num_features)

# Check the dataframe with hashed ROAD
View(head(model_data))


#perform target encoding for COUNTY

# Calculate the mean of the target variable for each category in the `county` column
county_means <- aggregate(NO. ~ COUNTY, data = model_data, FUN = mean)

# Merge the mean target values back into the original dataframe
model_data <- merge(model_data, county_means, by = "COUNTY", suffixes = c("", "_mean"))

# Replace the `county` categories with their corresponding mean target values
model_data$COUNTY_ENCODED <- model_data$NO._mean

# Drop the intermediate column
model_data <- model_data[, !(names(model_data) %in% c("NO._mean"))]

# Check the encoded dataframe
View(head(model_data, 20))


#model building
model <- lm(NO.~ `CAUSE CODE` + Hour , data = model_data )
summary(model)
