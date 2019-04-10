# myrepo
# testing my setup
# This is a line from RStudio
rm(list =ls())
setwd("~/Documents/Kaggle")
data = read.csv('analysisData.csv')
dim(data)
head(data$price)

data$price = as.numeric(gsub('[$,]', '', data$price))
data$weekly_price = as.numeric(gsub('[$,]', '', data$weekly_price))
data$cleaning_fee = as.numeric(gsub('[$,]', '', data$cleaning_fee))
data$security_deposit = as.numeric(gsub('[$,]', '', data$security_deposit))
data$extra_people = as.numeric(gsub('[$,]', '', data$extra_people))
data$monthly_price = as.numeric(gsub('[$,]', '', data$monthly_price))

sum(is.na(data$price))
sum(is.na(data$minimum_nights))
sum(is.na(data$review_scores_rating))
sum(is.na(data$accommodates))
sum(is.na(data$guests_included))
sum(is.na(data$bathrooms))
sum(is.na(data$bedrooms))
sum(is.na(data$room_type))
sum(is.na(data$number_of_reviews))
sum(is.na(data$reviews_per_month))
sum(is.na(data$neighbourhood_group_cleansed))
sum(is.na(data$instant_bookable))
sum(is.na(data$availability_30))
sum(is.na(data$weekly_price))
sum(is.na(data$monthly_price))
sum(is.na(data$cleaning_fee))
sum(is.na(data$security_deposit))
sum(is.na(data$extra_people))


avg_review_scores_rating = mean(data$review_scores_rating, na.rm = T)
avg_bathrooms = mean(data$bathrooms, na.rm = T)
avg_bedrooms = mean(data$bedrooms, na.rm = T)
avg_reviews_per_month = mean(data$reviews_per_month, na.rm = T)
avg_cleaning_fee = mean(data$cleaning_fee, na.rm = T)
avg_security_deposit = mean(data$security_deposit, na.rm = T)
avg_monthly_price = mean(data$monthly_price, na.rm = T)

mask_NAs = is.na(data$review_scores_rating)
mask_NAs_bathrooms =is.na(data$bathrooms)
mask_NAs_bedrooms = is.na(data$bedrooms)
mask_NAs_reviews_per_month = is.na(data$reviews_per_month)
mask_NAs_cleaning_fee = is.na(data$cleaning_fee)
mask_NAs_security_deposit = is.na(data$security_deposit)
mask_NAs_monthly_price = is.na(data$security_deposit)


data[mask_NAs, 'review_scores_rating'] = avg_review_scores_rating
data[mask_NAs_bathrooms, 'bathrooms'] =avg_bathrooms
data[mask_NAs_bedrooms, 'bedrooms'] =avg_bedrooms
data[mask_NAs_reviews_per_month, 'reviews_per_month'] =avg_reviews_per_month
data[mask_NAs_cleaning_fee, 'cleaning_fee'] = avg_cleaning_fee
data[mask_NAs_security_deposit, 'security_deposit'] = avg_security_deposit
data[mask_NAs_monthly_price, 'monthly_price'] = avg_monthly_price


sum(is.na(data$security_deposit))
sum(is.na(data$cleaning_fee))
sum(is.na(data$review_scores_rating))
sum(is.na(data$review_scores_rating))
sum(is.na(data$bathrooms))
sum(is.na(data$bedrooms))
sum(is.na(data$reviews_per_month))

mean(data$review_scores_rating, na.rm = T) == avg_review_scores_rating
mean(data$bathrooms, na.rm = T) == avg_bathrooms
mean(data$bedrooms, na.rm = T) == avg_bedrooms
mean(data$reviews_per_month, na.rm = T) == avg_reviews_per_month
mean(data$cleaning_fee, na.rm = T) == avg_cleaning_fee
mean(data$security_deposit, na.rm = T) == avg_security_deposit

# load scoring data
scoringData = read.csv('scoringData.csv')

scoringData$weekly_price = as.numeric(gsub('[$,]', '', scoringData$weekly_price))
scoringData$cleaning_fee = as.numeric(gsub('[$,]', '', scoringData$cleaning_fee))
scoringData$security_deposit = as.numeric(gsub('[$,]', '', scoringData$security_deposit))
scoringData$extra_people = as.numeric(gsub('[$,]', '', scoringData$extra_people))
scoringData$monthly_price = as.numeric(gsub('[$,]', '', scoringData$monthly_price))


avg_cleaning_fee_S = mean(scoringData$cleaning_fee, na.rm = T)
avg_security_deposit_S = mean(scoringData$security_deposit, na.rm = T)
avg_monthly_price_S = mean(scoringData$monthly_price, na.rm = T)

# apply preprocessing step to the scoring data (before applying model)
# note we are replacing missing values in scoringData with values
# from training data!!
mask_NAs = is.na(scoringData$review_scores_rating)
mask_NAs_bathrooms =is.na(scoringData$bathrooms)
mask_NAs_bedrooms = is.na(scoringData$bedrooms)
mask_NAs_reviews_per_month = is.na(scoringData$reviews_per_month)
mask_NAs_cleaning_fee = is.na(scoringData$cleaning_fee)
mask_NAs_security_deposit = is.na(scoringData$security_deposit)
mask_NAs_monthly_price = is.na(scoringData$monthly_price)

scoringData[mask_NAs, 'review_scores_rating'] = avg_review_scores_rating
scoringData[mask_NAs_bathrooms, 'bathrooms'] =avg_bathrooms
scoringData[mask_NAs_bedrooms, 'bedrooms'] =avg_bedrooms
scoringData[mask_NAs_reviews_per_month, 'reviews_per_month'] =avg_reviews_per_month
scoringData[mask_NAs_cleaning_fee, 'cleaning_fee'] = avg_cleaning_fee_S
scoringData[mask_NAs_security_deposit, 'security_deposit'] = avg_security_deposit_S
scoringData[mask_NAs_monthly_price, 'monthly_price'] = avg_monthly_price_S

# verify that there are no NAs (this should return 0)

sum(is.na(scoringData$security_deposit))
sum(is.na(scoringData$cleaning_fee))
sum(is.na(scoringData$review_scores_rating))
sum(is.na(scoringData$review_scores_rating))
sum(is.na(scoringData$bathrooms))
sum(is.na(scoringData$bedrooms))
sum(is.na(scoringData$reviews_per_month))



model1= lm(price ~ minimum_nights + review_scores_rating + accommodates + guests_included + bathrooms + bedrooms + room_type + number_of_reviews +reviews_per_month + neighbourhood_group_cleansed + instant_bookable + availability_30, data)

pred = predict(model1)
sse1 = sum((pred - train$price)^2)
sst1 = sum((mean(train$price)-train$price)^2)
model1_r2 = 1 - sse1/sst1; model1_r2

#[1] 0.2202293
rmse1 = sqrt(mean((pred-train$price)^2)); rmse1
#[1] 200.5251

model2= lm(price ~ minimum_nights + review_scores_rating + accommodates + guests_included + bathrooms + bedrooms + room_type + number_of_reviews +reviews_per_month + neighbourhood_group_cleansed + instant_bookable + availability_30 + cleaning_fee, data)

pred = predict(model2)
sse2 = sum((pred - train$price)^2)
sst2 = sum((mean(train$price)-train$price)^2)
model2_r2 = 1 - sse2/sst2; model2_r2
#[1] 0.227093
rmse2 = sqrt(mean((pred-train$price)^2)); rmse2
#[1] 199.6407

model3= lm(price ~ minimum_nights + review_scores_rating + accommodates + guests_included + bathrooms + bedrooms + room_type + number_of_reviews +reviews_per_month + neighbourhood_group_cleansed + instant_bookable + availability_30 + cleaning_fee + security_deposit, data)

pred = predict(model3)
sse3 = sum((pred - train$price)^2)
sst3 = sum((mean(train$price)-train$price)^2)
model3_r2 = 1 - sse2/sst2; model3_r2
#[1] 0.227093
rmse3 = sqrt(mean((pred-train$price)^2)); rmse3
#[1] 198.9332

model4= lm(price ~ minimum_nights + review_scores_rating + accommodates + guests_included + bathrooms + bedrooms + room_type + number_of_reviews +reviews_per_month + neighbourhood_group_cleansed + instant_bookable + availability_30 + cleaning_fee + security_deposit +bed_type, data)

pred = predict(model4)
sse4 = sum((pred - train$price)^2)
sst4 = sum((mean(train$price)-train$price)^2)
model4_r2 = 1 - sse2/sst2; model4_r2
#[1] 0.227093
rmse4 = sqrt(mean((pred-train$price)^2)); rmse4
#[1] 198.933

model5= lm(price ~ minimum_nights + review_scores_rating + accommodates + guests_included + bathrooms + bedrooms + room_type + number_of_reviews +reviews_per_month + neighbourhood_group_cleansed + instant_bookable + availability_30 + cleaning_fee + security_deposit +cancellation_policy, data)


pred = predict(model5)
sse5 = sum((pred - train$price)^2)
sst5 = sum((mean(train$price)-train$price)^2)
model5_r2 = 1 - sse2/sst2; model5_r2
#[1] 0.227093

rmse5 = sqrt(mean((pred-train$price)^2)); rmse5
#[1] 198.7919


model= lm(price ~ minimum_nights + review_scores_rating + accommodates + guests_included + bathrooms + bedrooms + room_type + number_of_reviews +reviews_per_month + neighbourhood_group_cleansed + instant_bookable + availability_30 + cleaning_fee + security_deposit +cancellation_policy, data)

# score the data
pred = predict(model, newdata=scoringData)

# generate submission data.frame
submission = data.frame(id = scoringData$id, price = pred) 

# check if submission data.frame has no NAs (this should return TRUE)
sum(is.na(submission)) == 0

# write results to a csv file to upload to Kaggle
write.csv(submission, 'sample_submission.csv',row.names = F)

