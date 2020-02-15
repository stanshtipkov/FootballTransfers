###########################
#Football transfers project
###########################

# Football Transfers 2000 - 2018
# https://www.kaggle.com/vardan95ghazaryan/top-250-football-transfers-from-2000-to-2018

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# Download the database
url <- "https://raw.githubusercontent.com/stanshtipkov/FootballTransfers/master/top250-00-19.csv?fbclid=IwAR2sVAxtpJ5_SjCtylReHpp9Cv-SwIIbbc1g6KGmsXgaG23UNqmw6PxDzM0"

data <- read_csv(url)
download.file(url, "top250-00-19.csv")

data <-read.csv("top250-00-19.csv")

# The header of our file
head(data)

# Separate the test set from the rest of the data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = data$Transfer_fee, times = 1, p = 0.2, list = FALSE)
data1 <- data[-test_index,]
test <- data[test_index,]

test <- test %>% semi_join(data1, by="Name")

# Dividing on train and validation sets
set.seed(1, sample.kind="Rounding")
validation_index <- createDataPartition(y = data1$Transfer_fee, times = 1, p = 0.1, list = FALSE)
train <- data1[-validation_index,]
validation <- data1[validation_index,]

validation <- validation %>% semi_join(train, by="Name")

# Quick overview of the dataset
head(train)

# Compute the mean of the Transfer Fees
mu <- mean(train$Transfer_fee)
mu

# We can see how many transfers we have by sums 
train %>% 
  count(Transfer_fee) %>% 
  ggplot(aes(n)) +
  geom_histogram(bins=30,color="black")+
  scale_x_log10()+
  xlab("Number of Transfers")+
  ylab("Transfer fee")+
  ggtitle("Number of Transfers by fees")

# Explore transfers by age 
train %>% 
  ggplot(aes(Age))+
  geom_histogram(bins=30, color="black")+
  ggtitle("Age of transfered players")

# Results based on average
naive_rmse <- RMSE(validation$Transfer_fee,mu)
naive_rmse

# Save prediction in Data Frame in which we will compare the different approaches 
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

# Showing the table with results
rmse_results %>% knitr::kable()

# Model taking into account the Age
age_model <- train %>%
  group_by(Age) %>%
  summarize(b_i = mean(Transfer_fee - mu))

# Save and compare the Age model
predicted_fee <- mu+ validation %>%
  left_join(age_model, by='Age') %>%
  pull(b_i)
model_1_rmse <- RMSE(predicted_fee, validation$Transfer_fee)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Age Model",  
                                     RMSE = model_1_rmse ))

# Model taking into account the Position alongside the Age
pos_model<- train %>% 
  left_join(age_model, by='Age') %>%
  group_by(Position) %>%
  summarize(b_u = mean(Transfer_fee - mu - b_i))

# Save and compare the "Position" model
predicted_fee <- validation %>% 
  left_join(age_model, by='Age') %>%
  left_join(pos_model, by='Position') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_2_rmse <- RMSE(predicted_fee, validation$Transfer_fee)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Age + Position Model",  
                                     RMSE = model_2_rmse ))

# Model taking into account the "League to" alongside Position and Age
leag_model<- train %>% 
  left_join(age_model, by='Age') %>%
  left_join(pos_model, by='Position') %>%
  group_by(League_to) %>%
  summarize(b_l = mean(Transfer_fee - mu - b_i - b_u))

# Save and compare the "League" model
predicted_fee <- validation %>% 
  left_join(age_model, by='Age') %>%
  left_join(pos_model, by='Position') %>%
  left_join(leag_model, by='League_to') %>%
  mutate(pred = mu + b_i + b_u + b_l) %>%
  .$pred
model_3_rmse <- RMSE(predicted_fee, validation$Transfer_fee)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="League + Age + Position Model",  
                                     RMSE = model_3_rmse ))

# Print RMSE results
rmse_results %>% knitr::kable()

# Compare our result with with Test set
leag_model<- train %>% 
  left_join(age_model, by='Age') %>%
  left_join(pos_model, by='Position') %>%
  group_by(League_to) %>%
  summarize(b_l = mean(Transfer_fee - mu - b_i - b_u))

predicted_fee <- test %>% 
  left_join(age_model, by='Age') %>%
  left_join(pos_model, by='Position') %>%
  left_join(leag_model, by='League_to') %>%
  mutate(pred = mu + b_i + b_u + b_l) %>%
  .$pred
test_check <- RMSE(predicted_fee, test$Transfer_fee)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Test Check",
                                     RMSE = test_check))

# Print RMSE results
rmse_results %>% knitr::kable()