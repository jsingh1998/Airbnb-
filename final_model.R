
############################################################################################################################

# Install required packages
install.packages(c("tidyverse", "lubridate", "text2vec", "tm", "SnowballC", "glmnet", "vip",
                   "naivebayes", "ranger", "xgboost", "ROCR", "textdata", "quanteda",
                   "tidytext", "pROC","caret"))

install.packages("caret")
############################################################################################################################

set.seed(123)

# Libraries
library(tidyverse)
library(lubridate)
library(tidyverse)
library(text2vec)
library(tm)
library(SnowballC)
library(glmnet)
library(vip)
#library(naivebayes)
library(ranger)
library(xgboost)
library(ROCR)
library(textdata)
library(quanteda)
library(tidytext)
library(pROC)
library(ranger)
library(xgboost)
library(caret)
library(tree)


############################################################################################################################



setwd('C:/Users/nmano/Desktop/Spring-2024/DataMining-Predictive/HW/ProjectGrp/')
# Load dataset
train_X <- read_csv("airbnb_train_merged.csv")
train_Y <- read_csv("airbnb_train_y_2024.csv")
test_X <- read_csv("airbnb_test_merged.csv")

summary(train_X)
zipcode_counts <- train_X %>%
  group_by(zipcode) %>%
  summarise(Count = n(), .groups = 'drop')



############################################################################################################################
# Split Train, Validation, Test


dataSplit  <- function(data_aggre, train_prop, valid_prop, seed = 123 )
{
  set.seed(seed)

  
  total_rows <- nrow(data_aggre)
  
  train_rows <- round(total_rows * train_prop)
  valid_rows <- round(total_rows * valid_prop)
  test_rows <- total_rows - train_rows - valid_rows
  
  shuffled_indices <- sample(seq_len(total_rows))
  
  train_indices <- shuffled_indices[seq_len(train_rows)]
  valid_indices <- shuffled_indices[(train_rows + 1):(train_rows + valid_rows)]
  test_indices <- shuffled_indices[(train_rows + valid_rows + 1):total_rows]
  
  data_aggre_train <- data_aggre[train_indices, ]
  data_aggre_valid <- data_aggre[valid_indices, ]
  data_aggre_test <- data_aggre[test_indices, ]
  
  return(list(train = data_aggre_train, valid = data_aggre_valid, test = data_aggre_test))
}

############################################################################################################################
# doc_ids var

data_aggragated <- cbind(train_X, train_Y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score),
         high_booking_rate = as.factor(high_booking_rate))
data_aggragated <- data_aggragated %>%
  mutate(doc_ids = row_number())

#->
test_X <- test_X %>%
  mutate(doc_ids = row_number())



#Type 1

results <- dataSplit(data_aggragated,  train_prop = 0.8,valid_prop = 0.2, 123)

train <- results$train
train_x <- train[, !(names(train) %in% c('high_booking_rate','perfect_rating_score') )]
train_y <- train$perfect_rating_score

Valid <- results$valid
valid_x <- Valid[, !(names(Valid) %in% c('high_booking_rate','perfect_rating_score') )]
valid_y <- Valid$perfect_rating_score

test <- results$test
test_x <- test[, !(names(test) %in% c('high_booking_rate','perfect_rating_score') )]
test_y <- test$perfect_rating_score

#Type 2
results <- dataSplit(data_aggragated, train_prop = 0.8,valid_prop = 0.2, 1)

train_data <- results$train
Valid_data <- results$valid
test_data <- results$test




#Data Cleaning

dataClean  <- function(data , seed = 123 )
  
{
  data_Staging <- data %>%
    mutate(
      Households_Median_income <- gsub("250,000\\+", "300000", Households_Median_income),
      Families_Median_income <- gsub("250,000\\+", "300000", Families_Median_income),
      Households_Median_income<- as.integer(Households_Median_income),
      Families_Median_income <- as.integer(Families_Median_income),
      Total_Population_poverty_AGE_18_64 = as.integer(ifelse(is.na(Total_Population_poverty_AGE_18_64),
                                                             mean(Total_Population_poverty_AGE_18_64, na.rm = TRUE),
                                                             Total_Population_poverty_AGE_18_64)),
      Total_Population_poverty_Civilian_labor_16_age = as.integer(ifelse(is.na(Total_Population_poverty_Civilian_labor_16_age),
                                                                         mean(Total_Population_poverty_Civilian_labor_16_age, na.rm = TRUE),
                                                                         Total_Population_poverty_Civilian_labor_16_age)),
      IND_125_poverty = as.integer(ifelse(is.na(IND_125_poverty),
                                          mean(IND_125_poverty, na.rm = TRUE),
                                          IND_125_poverty)),
      Households_Total = as.integer(ifelse(is.na(Households_Total),
                                           mean(Households_Total, na.rm = TRUE),
                                           Households_Total)),
      Households_Median_income = as.integer(ifelse(is.na(Households_Median_income),
                                                   mean(Households_Median_income, na.rm = TRUE),
                                                   Households_Median_income)),
      Households_Mean_income = as.integer(ifelse(is.na(Households_Mean_income),
                                                 mean(Households_Mean_income, na.rm = TRUE),
                                                 Households_Mean_income)),
      Families_Total = as.integer(ifelse(is.na(Families_Total),
                                         mean(Families_Total, na.rm = TRUE),
                                         Families_Total)),
      Families_Median_income = as.integer(ifelse(is.na(Families_Median_income),
                                                 mean(Families_Median_income, na.rm = TRUE),
                                                 Families_Median_income)),
      Families_Mean_income = as.integer(ifelse(is.na(Families_Mean_income),
                                               mean(Families_Mean_income, na.rm = TRUE),
                                               Families_Mean_income)),
      
      
      
      
      ifelse(is.na(accommodates), mean(accommodates, na.rm = TRUE), accommodates),,
      accommodates = ifelse(is.na(accommodates), mean(accommodates, na.rm = TRUE), accommodates),
      availability_30 = ifelse(is.na(availability_30), mean(availability_30, na.rm = TRUE), availability_30),
      availability_365 = ifelse(is.na(availability_365), mean(availability_365, na.rm = TRUE), availability_365),
      availability_60 = ifelse(is.na(availability_60), mean(availability_60, na.rm = TRUE), availability_60),
      availability_90 = ifelse(is.na(availability_90), mean(availability_90, na.rm = TRUE), availability_90),
      bathrooms = ifelse(is.na(bathrooms), mean(bathrooms, na.rm = TRUE), bathrooms),
      bedrooms = ifelse(is.na(bedrooms), mean(bedrooms, na.rm = TRUE), bedrooms),
      beds = ifelse(is.na(beds), mean(beds, na.rm = TRUE), beds),
      cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
      extra_people = ifelse(is.na(extra_people), 0, extra_people),
      guests_included = ifelse(is.na(guests_included), 0, guests_included),
      host_listings_count = ifelse(is.na(host_listings_count), mean(host_listings_count, na.rm = TRUE), host_listings_count),
      host_response_rate = ifelse(is.na(host_response_rate), mean(host_response_rate, na.rm = TRUE), host_response_rate),
      host_total_listings_count = ifelse(is.na(host_total_listings_count), mean(host_total_listings_count, na.rm = TRUE), host_total_listings_count),
      maximum_nights = ifelse(is.na(maximum_nights), mean(maximum_nights, na.rm = TRUE), maximum_nights),
      minimum_nights = ifelse(is.na(minimum_nights), mean(minimum_nights, na.rm = TRUE), minimum_nights),
      price = ifelse(is.na(price), 0, price),
      security_deposit = ifelse(is.na(security_deposit), 0, security_deposit),
      has_cleaning_fee = as.factor(ifelse(cleaning_fee > 0, "YES", "NO")),
      price_per_person = price / accommodates,
      has_security_deposit = as.factor(ifelse(security_deposit > 0, "YES", "NO")),
      has_extra_people = as.factor(ifelse(extra_people > 0, "YES", "NO")),
      bed_category = as.factor(ifelse(bed_type == "Real Bed", "bed", "other")),
      cancellation_policy = as.factor(ifelse(cancellation_policy %in% c("strict", "super_strict_30","super_strict_60"), 'strict', as.character(cancellation_policy)))
      )
  
  data_Staging <- data_Staging %>%
    mutate(property_category = case_when(
      property_type %in% c("Apartment", "Serviced apartment", "Loft") ~ "apartment",
      property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel") ~ "hotel",
      property_type %in% c("Townhouse", "Condominium") ~ "condo",
      property_type %in% c("Bungalow", "House") ~ "house",
      TRUE ~ "other"),
      property_category = as.factor(property_category)
    )
  
  data_Staging <- data_Staging %>%
    group_by(zipcode) %>%
    mutate(
      median_ppp = median(price_per_person, na.rm = TRUE),  # Calculate median price per person for each zipcode
      ppp_ind = as.factor(ifelse(price_per_person > median_ppp, 1, 0))  # Compare each price to the median
    ) %>%
    ungroup()
   
  
  
  data_Staging <- data_Staging %>%
    mutate(
      amenity_count = lengths(strsplit(as.character(amenities), ",")),
      price_per_amenity = price/amenity_count,
      host_duration = interval(as_date(host_since), today()) / dyears(1),
      time_since_first_review = interval(as_date(first_review), today()) / dyears(1),
      bathroom_per_bedroom = bathrooms / bedrooms,
      booking_availability_ratio_30 = (30 - availability_30) / 30,
      booking_availability_ratio_60 = (60 - availability_30) / 60,
      booking_availability_ratio_90 = (90 - availability_30) / 90,
      booking_availability_ratio_365 = (365 - availability_365) / 365,
      response_time_categorical = case_when(
        host_response_time %in% c("within an hour", "within a few hours") ~ "fast",
        host_response_time %in% c("within a day") ~ "moderate",
        TRUE ~ "slow"
      ),
      high_end_amenities = grepl("Pool|Hot tub", amenities),
      pet_friendly = grepl("Pets allowed", amenities),
      child_friendly = grepl("Family/kid friendly", amenities),
      public_transit_access = grepl("near subway|near metro", description),
      urban_setting = grepl("downtown|city center", description),
      rural_setting = grepl("rural|countryside", description),
      multi_listing_host = host_total_listings_count > 1,
      flexible_cancellation = cancellation_policy == "flexible",
      host_verifications_ratio = sapply(strsplit(as.character(host_verifications), ","), length)/21
    )
  
  return(data_Staging)
  
}



numerical_vars <- c("accommodates", "bedrooms","bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights",  "price_per_person", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "host_verifications_ratio", "price", "price_per_amenity")

dataScale <- function(data , seed = 123 ){
  # Define the preprocessing method
  preProcValues <- preProcess(data[numerical_vars], method = c("center", "scale"))
  
  # Apply the scaling
  df_scaled <- predict(preProcValues, data[numerical_vars])
  
  # Replace the original numerical data with scaled data
  data[numerical_vars] <- df_scaled
  
  data<- data%>%
    mutate(
      i1 = price_per_person*high_end_amenitiesTRUE,
      i2 = response_time_categoricalmoderate * host_duration,
      i2_1 = response_time_categoricalslow * host_duration,
      i3 = host_duration * host_verifications_ratio,
      i4 = booking_availability_ratio_30 * booking_availability_ratio_365,
      i5 = booking_availability_ratio_30 * urban_settingTRUE,
      i6 = amenity_count * child_friendlyTRUE,
      i7 = urban_settingTRUE * public_transit_accessTRUE,
      i8 = rural_settingTRUE * pet_friendlyTRUE,
      i9 = flexible_cancellationTRUE * multi_listing_hostTRUE,
      i10 = response_time_categoricalmoderate * booking_availability_ratio_365,
      i10_1 = response_time_categoricalslow * booking_availability_ratio_365,
      i11 = response_time_categoricalmoderate * urban_settingTRUE,
      i11_1 = response_time_categoricalslow * urban_settingTRUE,
      i12 = booking_availability_ratio_30 * price_per_person,
      i13 = booking_availability_ratio_30 * flexible_cancellationTRUE,
      i14 = rural_settingTRUE * high_end_amenitiesTRUE,
      i15 = multi_listing_hostTRUE * host_verifications_ratio,
      i16 = host_duration * has_cleaning_fee.YES
      
    )
  
  
}

#############################################################################################################################


# Cleaned data and featured data
# type 1
train_x <- dataClean(train_x)
valid_x <- dataClean(valid_x)
test_x <-  dataClean(test_x)


train_y
valid_y
test_y



# Cleaned data and featured data
# type 2
train_data <- dataClean(train_data)
Valid_data <- dataClean(Valid_data)
test_data <-  dataClean(test_data)

#############################################################################################################################
# TEST DATA
test_X <- dataClean(test_X)
  
  
  
  #############################################################################################################################




#############################################################################################################################



train_x_xg <- subset(train_x, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Total_Population_poverty_AGE_18_64", "Total_Population_poverty_Civilian_labor_16_age", "IND_125_poverty", "Households_Total" , "Households_Mean_income", "Families_Total", "Families_Mean_income"
))
valid_x_xg <- subset(valid_x, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Total_Population_poverty_AGE_18_64", "Total_Population_poverty_Civilian_labor_16_age", "IND_125_poverty", "Households_Total", "Households_Mean_income", "Families_Total", "Families_Mean_income"
))
test_x_xg <- subset(test_x, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Total_Population_poverty_AGE_18_64", "Total_Population_poverty_Civilian_labor_16_age", "IND_125_poverty", "Households_Total", "Households_Mean_income", "Families_Total", "Families_Mean_income"
))


test_X_xg <- subset(test_X, select = c("accommodates", "bedrooms", "bathrooms", "beds", "guests_included", "host_response_rate", "minimum_nights", "has_cleaning_fee", "has_extra_people", "price_per_person", "bed_category", "flexible_cancellation", "has_security_deposit", "ppp_ind", "property_category", "amenity_count", "host_duration", "time_since_first_review",  "booking_availability_ratio_30", "booking_availability_ratio_60", "booking_availability_ratio_90", "booking_availability_ratio_365", "response_time_categorical", "high_end_amenities", "pet_friendly", "child_friendly", "public_transit_access", "urban_setting", "rural_setting", "multi_listing_host", "host_verifications_ratio", "price", "price_per_amenity","Total_Population_poverty_AGE_18_64", "Total_Population_poverty_Civilian_labor_16_age", "IND_125_poverty", "Households_Total", "Households_Mean_income", "Families_Total", "Families_Mean_income"
))



train_y <- data.frame(perfect_rating_score = train_y)
test_y <- data.frame(perfect_rating_score = test_y)
valid_y <- data.frame(perfect_rating_score = valid_y)

train_y <- mutate(train_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))
test_y <- mutate(test_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))
valid_y <- mutate(valid_y, perfect_rating_score = recode(perfect_rating_score, "YES" = 1, "NO" = 0))





#unique_values <- sapply(train_x_xg, function(x) length(unique(x)))
#one_level_predictors <- names(unique_values[unique_values == 1])




#train_x_xg <- train_x_xg[, -which(names(train_x_xg) %in% one_level_predictors)]
dummy <- dummyVars( ~ . , data=train_x_xg, fullRank = TRUE)
train_x_xg <- data.frame(predict(dummy, newdata = train_x_xg))


#valid_x_xg <- valid_x_xg[, -which(names(valid_x_xg) %in% one_level_predictors)]
dummy <- dummyVars( ~ . , data=valid_x_xg, fullRank = TRUE)
valid_x_xg <- data.frame(predict(dummy, newdata = valid_x_xg))

#test_x_xg <- train_x_xg[, -which(names(test_x_xg) %in% one_level_predictors)]
dummy <- dummyVars( ~ . , data=test_x_xg, fullRank = TRUE)
test_x_xg <- data.frame(predict(dummy, newdata = test_x_xg))


#Scaling
train_x_xg <- dataScale(train_x_xg)
valid_x_xg <- dataScale(valid_x_xg)
test_x_xg <- dataScale(test_x_xg)


#test_X
dummy <- dummyVars( ~ . , data=test_X_xg, fullRank = TRUE)
test_X_xg <- data.frame(predict(dummy, newdata = test_X_xg))


#############################################################################################################################

str(train_x_xg)
str(train_y)

str(valid_x_xg)
str(valid_y)

# Check for missing values


sum(is.na(train_x_xg))
sum(is.na(valid_x_xg))
sum(is.na(test_x_xg))


train_x_xg[is.na(train_x_xg)] <- 0
valid_x_xg[is.na(valid_x_xg)] <- 0
test_x_xg[is.na(test_x_xg)] <- 0

test_X_xg[is.na(test_X_xg)] <- 0

#############################################################################################################################
##Text Classification
#############################################################################################################################


train_data_t <- train_data %>%
  select(doc_ids, amenities, perfect_rating_score)

Valid_data_t <- Valid_data %>%
  select(doc_ids, amenities, perfect_rating_score)

test_data_t <- test_data %>%
  select(doc_ids, amenities, perfect_rating_score)

test_real <- test_X %>%
  select(doc_ids, features)
  

train_data_t$perfect_rating_score <- as.factor(train_data$perfect_rating_score)
Valid_data_t$perfect_rating_score <- as.factor(Valid_data$perfect_rating_score)
test_data_t$perfect_rating_score <- as.factor(test_data$perfect_rating_score)


# Preprocess & Tokenize
cleaning_tokenizer <- function(v) {
  v %>%
    space_tokenizer(sep = ',')
}


# Iterate over the individual documents and convert them to tokens.
it_train <- itoken(train_data_t$amenities,
                   preprocessor = tolower,
                   tokenizer = cleaning_tokenizer,
                   ids = train_data_t$doc_ids,
                   progressbar = FALSE)

it_test_real <- itoken(test_real$features,
                   preprocessor = tolower,
                   tokenizer = cleaning_tokenizer,
                   ids = test_real$doc_ids,
                   progressbar = FALSE)

# Create the vocabulary
vocab <- create_vocabulary(it_train, ngram = c(1L, 2L))
vocab_small <- prune_vocabulary(vocab, term_count_min = 100,doc_proportion_max = 0.5)


# Create a vectorizer object using the vocabulary we learned
vectorizer <- vocab_vectorizer(vocab_small)

# Convert the training articles into a DTM
dtm_train_amenities <- create_dtm(it_train, vectorizer)
dim(dtm_train_amenities)

dtm_test_real_amenities <- create_dtm(it_test_real, vectorizer)
dim(dtm_test_real_amenities)

# Convert the validation articles into a DTM
it_valid <- itoken(Valid_data_t$amenities,
                   preprocessor = tolower,
                   tokenizer = cleaning_tokenizer,
                   id = Valid_data_t$doc_ids,
                   progressbar = FALSE)

dtm_valid_amenities <- create_dtm(it_valid, vectorizer)
dim(dtm_valid_amenities)


#features
dtm_train_features <- create_dtm(it_train, vectorizer)
dim(dtm_train_features)

dtm_test_real_features <- create_dtm(it_test_real, vectorizer)
dim(dtm_test_real_features)

dtm_valid_features <- create_dtm(it_valid, vectorizer)
dim(dtm_valid_features)

# Make a TFIDF DTM: first line creates a tf-idf model and second line generates the tf-idf
# matrix using the model
tfidf_model <- TfIdf$new()
dtm_train_tfidf <- fit_transform(dtm_train, tfidf_model)
dtm_valid_tfidf <- fit_transform(dtm_valid, tfidf_model)



dtm_train_amenities_tfidf <- fit_transform(dtm_train_amenities, tfidf_model)
dtm_valid_amenities_tfidf <- fit_transform(dtm_valid_amenities, tfidf_model)






it_test <- itoken(test_data_t$amenities,
                  preprocessor = tolower,
                  tokenizer = cleaning_tokenizer,
                  id = test_data_t$doc_ids,
                  progressbar = FALSE)

dtm_test <- create_dtm(it_test, vectorizer)
dim(dtm_test)


#######################################################################################



#########################################################################
# xgboost+ amenities
#########################################################################
dense_matrix_train_amenities <- as.matrix(dtm_train_amenities_tfidf)
dense_matrix_valid_amenities <- as.matrix(dtm_valid_amenities_tfidf)


dense_matrix_train_amenities <- as.matrix(dtm_train_amenities)
dense_matrix_valid_amenities <- as.matrix(dtm_valid_amenities)
dense_matrix_test_amenities <- as.matrix(dtm_test)

dense_matrix_test_real_amenities <- as.matrix(dtm_test_real_amenities)

#features
dense_matrix_train_features <- as.matrix(dtm_train_features)
dense_matrix_valid_features <- as.matrix(dtm_valid_features)
dense_matrix_test <- as.matrix(dtm_test)

dense_matrix_test_real_features <- as.matrix(dtm_test_real_features)


# Convert the dense matrix to a data frame
dtm_df_train_amenities <- as.data.frame(dense_matrix_train_amenities)
dtm_df_valid_amenities <- as.data.frame(dense_matrix_valid_amenities)
dtm_df_test_amenities <- as.data.frame(dense_matrix_test_amenities)

dtm_df_test_real_amenities <- as.data.frame(dense_matrix_test_real_amenities)

#features
dtm_df_train_features <- as.data.frame(dense_matrix_train_features)
dtm_df_valid_features <- as.data.frame(dense_matrix_valid_features)
dtm_df_test <- as.data.frame(dense_matrix_test)

dtm_df_test_real_features <- as.data.frame(dense_matrix_test_real_features)

comb_train <- cbind(train_x_xg,dtm_df_train_amenities)
comb_valid <- cbind(valid_x_xg,dtm_df_valid_amenities)
comb_test <- cbind(test_x_xg, dtm_df_test_amenities)

comb_test_real <- cbind(test_X_xg, dtm_df_test_real_amenities, dtm_df_test_real_features)

xgboost_bst <- xgboost(
  data = as.matrix(comb_train),
  label = as.numeric(train_y$perfect_rating_score),
  max.depth = 6,               # Reduced model complexity
  eta = 0.05,                   # Fine-tuned learning rate
  subsample = 0.6,             # Reduced subsample ratio
  colsample_bytree = 0.6,      # Reduced column subsampling
  lambda = 0.5,                # Increased L2 regularization
  alpha = 0.1,                 # Increased L1 regularization
  min_child_weight = 20,        # Increased minimum child weight
  gamma = 0.6,                 # Adjusted gamma
  scale_pos_weight = 3.5,        # Optimized class weights
  nrounds = 250,
  objective = "binary:logistic"
)


  #############################################################################################################################


vip(xgboost_bst)




train_colnames <- colnames(comb_train)

# Get the column names of the validation dataset
valid_colnames <- colnames(as.matrix(comb_valid))

# Compare column names between the datasets
names_only_in_train <- setdiff(train_colnames, valid_colnames)
names_only_in_valid <- setdiff(valid_colnames, train_colnames)

# Output the column names present in each dataset but not in the other
print(names_only_in_train)
print(names_only_in_valid)




#############################################################################################################################

preds_xg <- predict(xgboost_bst, as.matrix(comb_valid))
preds_xg <- as.numeric(as.character(preds_xg))


preds_xg_test <- predict(xgboost_bst, as.matrix(test_x_xg))
preds_xg_test <- as.numeric(as.character(preds_xg_test))

valid_y_xg_boost <- as.numeric(valid_y$perfect_rating_score)
bin_classify_xg_boost <- ifelse(preds_xg > 0.8739327, 1, 0)




classifications_perfect <- factor(ifelse(bin_classify_xg_boost == 1, "YES", "NO"), levels = c("YES", "NO"))


#output your predictions

write.table(classifications_perfect, "perfect_rating_score_group14.csv", row.names = FALSE)

#############################################################################################################################


#############################################################################################################################
#############################################################################################################################

# 1. create an ROCR "prediction" object
#turns your set of predictions into several vectors
#each one tabulates values for every possible cutoff in your data
pred_full <- prediction(preds_xg, valid_y_xg_boost)

##measures the TP, FP, TN, FN, and more for each cutoff
pred_full@cutoffs
pred_full@tp
pred_full@n.pos.pred


# 2. create an ROCR performance object with the measures you want
# (For ROC curve it's TPR and FPR)
roc_full <- performance(pred_full, "tpr", "fpr")

# 3. plot the (tpr, fpr) performance - you can specify the color and line weight as well
plot(roc_full, col = "red", lwd = 2)

####################################################
# Extract the TPR and FPR at each threshold
tpr <- roc_full@y.values[[1]]
fpr <- roc_full@x.values[[1]]

# Find the index of the threshold closest to 10% FPR
threshold_index <- which.min(abs(fpr - 0.1))

# Extract TPR and FPR at this threshold
tpr_at_threshold <- tpr[threshold_index]
fpr_at_threshold <- fpr[threshold_index]

# Extract the actual threshold value
thresholds <- pred_full@cutoffs[[1]]
threshold_value <- thresholds[threshold_index]

# Add a point on the plot for 10% FPR
points(fpr_at_threshold, tpr_at_threshold, pch=19, col="blue")

# Annotate the point with the threshold value and FPR=10%
text(fpr_at_threshold, tpr_at_threshold, labels=paste("FPR=8%\nThreshold=", round(threshold_value, 2)), pos=4)

abline(0, 1, lty=2, col = "gray") # Add the baseline model line

# Calculate the AUC
auc_full <- performance(pred_full, measure = "auc")
auc_value <- auc_full@y.values[[1]]  # Extracting the AUC value


#############################################################################################################################
# TUNING
#############################################################################################################################
full_dataset<- cbind(comb_train, train_y$perfect_rating_score)

set.seed(1)  # for reproducibility
sample_index <- sample(seq_len(nrow(full_dataset)), size = 15000)  # example: 10,000 records
data_subset <- full_dataset[sample_index, ]


# Define the grid of hyperparameters to search
param_grid <- expand.grid(
  max_depth = c(6),         # Depths of the trees
  eta = c(0.01, 0.03, 0.05),        # Learning rate
  subsample = c(0.6),   # Subsample ratio
  colsample_bytree = c(0.6), # Column subsample ratio
  min_child_weight = c(20),  # Minimum sum of instance weight needed in a child
  gamma = c(0.6, 0.8),         # Minimum loss reduction required to make a further partition
  lambda = c(0.5),           # L2 regularization term on weights
  alpha = c(0.1),              # L1 regularization term on weights
  scale_pos_weight = c(3.5),
  list = FALSE
)


# Prepare data
data_feat <- data_subset[, !(names(data_subset) %in% "train_y$perfect_rating_score")]
dtrain <- xgb.DMatrix(data = as.matrix(data_feat), label = as.numeric(data_subset$`train_y$perfect_rating_score`))



best_score <- 0
best_params <- NULL

for(i in seq_len(nrow(param_grid))) {
  params <- param_grid[i, ]
  # Convert to list if necessary
  params_list <- as.list(params)
  
  cv_results <- xgb.cv(
    params = params_list,
    data = dtrain,
    nrounds = 4000, # Max number of boosting rounds
    nfold = 5,      # Number of folds in CV
    metrics = "auc",
    early_stopping_rounds = 200,
    seed = 42,
    print_every_n = 100
  )
  
  # Obtain the best score observed during CV
  best_iteration_score <- max(cv_results$evaluation_log$test_auc_mean)  # Change to max and test_auc_mean
  if (best_iteration_score > best_score) {  # Change to greater than for maximizing AUC
    best_score <- best_iteration_score
    best_params <- params
    best_params$nrounds <- cv_results$best_iteration  # Capture the optimal number of rounds
  }
}

print("Best Parameters Found:")
print(best_params)
print(paste("Best Score:", best_score))






#############################################################################################################################

#############################################################################################################################

