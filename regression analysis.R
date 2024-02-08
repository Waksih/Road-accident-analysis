#REGRESSION ANAYSIS

#preparing the dataset
model_data <- combined_data %>% 
  select(NO., Hour, ROAD, COUNTY) %>% 
  mutate(ROAD = as.factor(ROAD), COUNTY = as.factor(COUNTY)) %>% 
  filter(complete.cases(.))

#convert the categorical variables to dummy data

#use feature hashing for ROAD

# Preprocess the ROAD column: remove special characters and normalize to lowercase
model_data$ROAD <- tolower(gsub("[^[:alnum:] ]", "", model_data$ROAD))

# Remove spaces from road names
model_data$ROAD <- gsub(" ", "", model_data$ROAD)

# Perform feature hashing for ROAD
num_features <- 596 # define number of features

# Define a custom hash function for feature hashing using digest
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

# modify the data to include only values being tested
model_data<-model_data[,c("NO.","Hour","hashed_ROAD","COUNTY_ENCODED")]

# model building
# Fit Poisson regression model
model <- glm(`NO.` ~ hashed_ROAD + Hour + COUNTY_ENCODED, data = model_data, family = poisson)

model <- glm(`NO.` ~ Hour + COUNTY_ENCODED, data = model_data, family = poisson)


# Summary of the model
summary(model)

