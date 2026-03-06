#==============================================================================#
#                           Data Engineering                                   #
#==============================================================================#

require(psych)
require(sjPlot)
require(corrplot)
require(Hmisc)
require(nortest)
require(car)

rm(list = ls())

# Load training dataset
data <- read.csv(file.choose(), header = TRUE, sep = ";")
View(data)

# Look at the structure of the data and see if there should be any modifications
# before starting the analysis.
str(data)

# We can see that there are 62 variables, whereas in the assignment description
# there are 61 mentioned. Most probably, the X variable is something that needs
# to be deleted from our dataset, since it has no actual meaning or purpose.
# Another issue is the class of the variables, which will have to be fixed.

# Delete redundant variables
data$X <- NULL  # unknown variable
data$url <- NULL  # non-explanatory
data$timedelta <- NULL  # non-explanatory

#------------------------------------------------------------------------------
#---- Transform Numeric Variables

# For the numeric variables that are currently character variables, we can see
# that they use commas and not dots. If we try to convert them to numeric as they
# are, all observations will become NAs. We must first fix the comma issue and
# then proceed with changing their class.

num_cols <- c(
  "n_unique_tokens", "n_non_stop_words", "n_non_stop_unique_tokens",
  "average_token_length", "kw_max_min", "kw_avg_min", "kw_avg_avg",
  "kw_avg_max", "kw_min_avg", "kw_max_avg", "kw_min_min",
  "self_reference_min_shares", "self_reference_max_shares",
  "self_reference_avg_sharess",
  "LDA_00", "LDA_01", "LDA_02", "LDA_03", "LDA_04",
  "global_subjectivity", "global_sentiment_polarity",
  "global_rate_positive_words", "global_rate_negative_words",
  "rate_positive_words", "rate_negative_words",
  "avg_positive_polarity", "min_positive_polarity",
  "max_positive_polarity", "avg_negative_polarity",
  "min_negative_polarity", "max_negative_polarity",
  "title_subjectivity", "title_sentiment_polarity",
  "abs_title_subjectivity", "abs_title_sentiment_polarity"
)

for (col in num_cols) {
  data[[col]] <- as.numeric(gsub(",", ".", data[[col]], fixed = TRUE))
}

#---- Transform Categorical Variables

cat_cols <- c(
  "data_channel_is_lifestyle", "data_channel_is_entertainment",
  "data_channel_is_bus", "data_channel_is_socmed",
  "data_channel_is_tech", "data_channel_is_world",
  "weekday_is_monday", "weekday_is_tuesday", "weekday_is_wednesday",
  "weekday_is_thursday", "weekday_is_friday", "weekday_is_saturday",
  "weekday_is_sunday", "is_weekend"
)

for (col in cat_cols) {
  data[[col]] <- factor(data[[col]], levels = c("0", "1"), labels = c("No", "Yes"))
}

# Check if everything is alright
str(data)
# All good

#------------------------------------------------------------------------------
#---- Construct Unified Data Channel Variable

data$channel <- NULL
data$channel <- ifelse(data$data_channel_is_tech          == "Yes", 1,
                ifelse(data$data_channel_is_entertainment == "Yes", 2,
                ifelse(data$data_channel_is_bus           == "Yes", 3,
                ifelse(data$data_channel_is_socmed        == "Yes", 4,
                ifelse(data$data_channel_is_world         == "Yes", 5,
                ifelse(data$data_channel_is_lifestyle     == "Yes", 6, 7))))))
data$channel <- as.factor(data$channel)
levels(data$channel) <- c("Tech", "Entertainment", "Business", "Social Media", "World", "Lifestyle", "Other")

#---- Construct Unified Day of Publication Variable

data$day <- NULL
data$day <- ifelse(data$weekday_is_monday    == "Yes", 1,
            ifelse(data$weekday_is_tuesday   == "Yes", 2,
            ifelse(data$weekday_is_wednesday == "Yes", 3,
            ifelse(data$weekday_is_thursday  == "Yes", 4,
            ifelse(data$weekday_is_friday    == "Yes", 5,
            ifelse(data$weekday_is_saturday  == "Yes", 6, 7))))))
data$day <- as.factor(data$day)
levels(data$day) <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
any(is.na(data$day))  # FALSE

#---- Construct Popularity Variable using threshold of 90th percentile

quantile(data$shares, 0.90)
viral <- NULL
viral <- ifelse(data$shares >= 6200, "Viral", "Typical")

#---- Remove Redundant Categorical Variables (channel and day dummies)

data$data_channel_is_lifestyle <- NULL
data$data_channel_is_bus <- NULL
data$data_channel_is_entertainment <- NULL
data$data_channel_is_socmed <- NULL
data$data_channel_is_world <- NULL
data$data_channel_is_tech <- NULL

data$weekday_is_monday <- NULL
data$weekday_is_tuesday <- NULL
data$weekday_is_wednesday <- NULL
data$weekday_is_thursday <- NULL
data$weekday_is_friday <- NULL
data$weekday_is_saturday <- NULL
data$weekday_is_sunday <- NULL

#------------------------------------------------------------------------------
#---- Data Engineering for Test Dataset

# Load test dataset
test <- read.csv(file.choose(), header = TRUE, sep = ";")

test$X <- NULL
test$url <- NULL
test$timedelta <- NULL

for (col in num_cols) {
  test[[col]] <- as.numeric(gsub(",", ".", test[[col]], fixed = TRUE))
}

for (col in cat_cols) {
  test[[col]] <- factor(test[[col]], levels = c("0", "1"), labels = c("No", "Yes"))
}

# Variable Day
test$day <- NULL
test$day <- ifelse(test$weekday_is_monday    == "Yes", 1,
            ifelse(test$weekday_is_tuesday   == "Yes", 2,
            ifelse(test$weekday_is_wednesday == "Yes", 3,
            ifelse(test$weekday_is_thursday  == "Yes", 4,
            ifelse(test$weekday_is_friday    == "Yes", 5,
            ifelse(test$weekday_is_saturday  == "Yes", 6, 7))))))
test$day <- as.factor(test$day)
levels(test$day) <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

test$weekday_is_monday <- NULL
test$weekday_is_tuesday <- NULL
test$weekday_is_wednesday <- NULL
test$weekday_is_thursday <- NULL
test$weekday_is_friday <- NULL
test$weekday_is_saturday <- NULL
test$weekday_is_sunday <- NULL

# Variable Channel
test$channel <- NULL
test$channel <- ifelse(test$data_channel_is_tech          == "Yes", 1,
                ifelse(test$data_channel_is_entertainment == "Yes", 2,
                ifelse(test$data_channel_is_bus           == "Yes", 3,
                ifelse(test$data_channel_is_socmed        == "Yes", 4,
                ifelse(test$data_channel_is_world         == "Yes", 5,
                ifelse(test$data_channel_is_lifestyle     == "Yes", 6, 7))))))
test$channel <- as.factor(test$channel)
levels(test$channel) <- c("Tech", "Entertainment", "Business", "Social Media", "World", "Lifestyle", "Other")

test$data_channel_is_lifestyle <- NULL
test$data_channel_is_bus <- NULL
test$data_channel_is_entertainment <- NULL
test$data_channel_is_socmed <- NULL
test$data_channel_is_world <- NULL
test$data_channel_is_tech <- NULL

# Save pre-processed data
save(data, test, viral, num_cols, cat_cols, file = "onlinenews.RData")

