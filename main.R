#--------------------------------------------------------------------------
# Author: Bhathiya Maneendra Pilanawithana
# Date: 09/04/2023
# Remark: Dataset generation part was copied from the project instructions
#--------------------------------------------------------------------------

#-----[Load packages and install if required]
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(caret)
library(tidyverse)
library(dplyr)
#-end-[Load packages and install if required]

#-----[Create edx and final_holdout_test sets]
# This is copied directly from course intructions. No Changes.
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 360)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)
#-end-[Create edx and final_holdout_test sets]

#-----[Check for missing values]
na_sum_edx <- sum(is.na(edx %>% select(userId, movieId, rating, timestamp)))
na_sum_holdout <- sum(is.na(final_holdout_test %>% select(userId, movieId, rating, timestamp)))
#No NA detected in edx and the final holdout set
#-end-[Check for missing values]

#-----[Extract rating year information from timestamp and drop timestamp]
  #This will also stratify timestamp information into bins
edx <- edx %>% mutate(rating_year = as.POSIXlt(timestamp, origin = "1970-01-01")$year+1900) %>% select(-timestamp)
final_holdout_test <- final_holdout_test %>% mutate(rating_year = as.POSIXlt(timestamp, origin = "1970-01-01")$year+1900) %>% select(-timestamp)
#-end-[Extract rating year information from timestamp and drop timestamp]

#-----[Extract release year from title and add years_to_rate]
edx <- edx %>% mutate(release_year = as.integer(substr(title, str_length(title) - 4, str_length(title) - 1))) %>% mutate(years_to_rate = rating_year - release_year)
final_holdout_test <- final_holdout_test %>% mutate(release_year = as.integer(substr(title, str_length(title) - 4, str_length(title) - 1)))  %>% mutate(years_to_rate = rating_year - release_year)
#-----[Extract release year from title and add years_to_rate]

#-----[Count number of observation in edx]
num_obs_edx <- nrow(edx)
#-end-[Count number of observation in edx]

#-----[Summary statistics by movieId]
summary_by_movieId <- edx %>% group_by(movieId) %>% summarise(mean_rating = mean(rating), sd_rating = sd(rating))

summary_by_movieId %>% 
  ggplot(aes(x = reorder(as.character(movieId), -mean_rating), y = mean_rating)) + 
  geom_bar(stat="identity", color = "palegreen2") +
  theme(axis.text.x = element_blank()) +
  labs( x = "Movie Index", y = "Average Rating") +
  theme_bw()

summary_by_movieId %>% 
  ggplot(aes(x = reorder(as.character(movieId), -mean_rating), y = sd_rating)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_blank()) +
  labs( x = "Movie Index", y = "SD of Rating") +
  theme_bw()

mean_mean_rating_movieId <- mean(summary_by_movieId$mean_rating)
summary_by_movieId %>% 
  ggplot(aes(x = mean_rating)) + 
  geom_histogram(bins = 50, color = "black", fill = "skyblue") +
  labs( x = "Average Movie Ratings", y = "Movie Count") +
  geom_vline(xintercept=mean_mean_rating_movieId, size=1.5, color="red") +
  geom_text(aes(x=mean_mean_rating_movieId, label=paste0("Average Rating\n",round(mean_mean_rating_movieId,4)), y=800)) + 
  xlim(0, 5) +
  theme_bw()

summary_by_movieId %>% 
  ggplot(aes(x = sd_rating)) + 
  geom_histogram(bins = 50, color = "black", fill = "salmon") +
  labs( x = "SD of Movie Ratings", y = "Movie Count") +
  xlim(0, 3) +
  theme_bw()
#-end-[Summary statistics by movieId]

#-----[Summary statistics by userId]
summary_by_userId <- edx %>% group_by(userId) %>% summarise(mean_rating = mean(rating), sd_rating = sd(rating))

mean_mean_rating_userId <- mean(summary_by_userId$mean_rating)
summary_by_userId %>% 
  ggplot(aes(x = mean_rating)) + 
  geom_histogram(bins = 50, color = "black", fill = "skyblue") +
  labs( x = "Average Movie Ratings", y = "User Count") +
  geom_vline(xintercept=mean_mean_rating_userId, size=1.5, color="red") +
  geom_text(aes(x=mean_mean_rating_userId, label=paste0("Average Rating\n",round(mean_mean_rating_userId,4)), y=7000)) + 
  xlim(0, 5) +
  theme_bw()

summary_by_userId %>% 
  ggplot(aes(x = sd_rating)) + 
  geom_histogram(bins = 50, color = "black", fill = "salmon") +
  labs( x = "SD of Movie Ratings", y = "User Count") +
  xlim(0, 3) +
  theme_bw()
#-end-[Summary statistics by userId]

#-----[Summary statistics by genres]
summary_by_genres <- edx %>% group_by(genres) %>% summarise(mean_rating = mean(rating), sd_rating = sd(rating))

summary_by_genres %>% 
  ggplot(aes(x = reorder(as.character(genres), -mean_rating), y = mean_rating)) + 
  geom_bar(stat="identity", color = "palegreen2") +
  theme(axis.text.x = element_blank()) +
  labs( x = "Genre Index", y = "Average Rating") +
  theme_bw()

mean_mean_rating_genres <- mean(summary_by_genres$mean_rating)
summary_by_genres %>% 
  ggplot(aes(x = mean_rating)) + 
  geom_histogram(bins = 50, color = "black", fill = "skyblue") +
  labs( x = "Average Movie Ratings", y = "Genre Count") +
  geom_vline(xintercept=mean_mean_rating_genres, size=1.5, color="red") +
  geom_text(aes(x=mean_mean_rating_genres, label=paste0("Average Rating\n",round(mean_mean_rating_genres,4)), y=80)) + 
  xlim(0, 5) +
  theme_bw()

summary_by_userId %>% 
  ggplot(aes(x = sd_rating)) + 
  geom_histogram(bins = 50, color = "black", fill = "salmon") +
  labs( x = "SD of Movie Ratings", y = "Genre Count") +
  xlim(0, 5) +
  theme_bw()
#-end-[Summary statistics by userId]

#-----[Summary statistics by years_to_rate]
summary_by_YearsToRate <- edx %>% group_by(years_to_rate) %>% summarise(mean_rating = mean(rating), sd_rating = sd(rating), count_rating = n())

summary_by_YearsToRate %>% 
  ggplot(aes(x = years_to_rate)) + 
  geom_line(aes(y = mean_rating, color = "Average Rating"), size = 1.2) +
  geom_line(aes(y = count_rating*(0.4*10^(-5)), color = "Number of Reviews"), size = 1.2) +
  theme(legend.position = "top", axis.text.x = element_blank()) +
  labs( x = "Year to Rate") +
  scale_y_continuous( "Average Rating", sec.axis = sec_axis(~ . * (0.4*10^(-5)), name = "Number of Reviews")) + 
  theme_bw()

summary_by_YearsToRate %>% 
  ggplot(aes(x = sd_rating)) + 
  geom_histogram(bins = 50, color = "black", fill = "salmon") +
  labs( x = "SD of Movie Ratings", y = "Year to Rate Count") +
  xlim(0, 5) +
  theme_bw()
#-end-[Summary statistics by userId]

#-----[Calculate mean rating per movieId, append to edX and calculate the prediction error in rating]
mean_rating_per_movieId_vec <- edx %>% group_by(movieId) %>% summarise(mean_rate_movieId = mean(rating))
edx <- edx %>% left_join(mean_rating_per_movieId_vec, by = "movieId")
edx <- edx %>% mutate(rate_error_movieId = rating - mean_rate_movieId)
#-end-[Calculate mean rating per movieId, append to edX and calculate the prediction error in rating]

#-----[1. Decide what variable to select next]
mean_error_per_userId_vec <- edx %>% group_by(userId) %>% summarise(mean_RateError_userId = mean(rate_error_movieId))
mean_error_per_genre_vec <- edx %>% group_by(genres) %>% summarise(mean_RateError_genres = mean(rate_error_movieId))
mean_error_per_YearsToRate_vec <- edx %>% group_by(years_to_rate) %>% summarise(mean_RateError_YearsToRate = mean(rate_error_movieId))

mean_error_per_userId_vec %>% 
  ggplot(aes(x = mean_RateError_userId)) + 
  geom_histogram(bins = 50, color = "black", fill = "skyblue") +
  labs( x = "Average Movie Ratings", y = "User Count") +
  theme_bw()
mean_error_per_genre_vec %>% 
  ggplot(aes(x = mean_RateError_genres)) + 
  geom_histogram(bins = 50, color = "black", fill = "skyblue") +
  labs( x = "Average Movie Ratings", y = "Genre Count") +
  theme_bw()
mean_error_per_YearsToRate_vec %>% 
  ggplot(aes(x = mean_RateError_YearsToRate)) + 
  geom_histogram(bins = 50, color = "black", fill = "skyblue") +
  labs( x = "Average Movie Ratings", y = "Years to Rate Count") +
  theme_bw()

  #The highiest variation of error can be seen from userId. 
  #Therefore, we use userId as over next variable reduce the prediction error

#-end-[1. Decide what variable to select next]

#-----[Calculate mean error rate per userId, append to edx and calculate the prediction error in rating]
edx <- edx %>% left_join(mean_error_per_userId_vec, by = "userId")
edx <- edx %>% mutate(rate_predict_movieId_userId = mean_rate_movieId + mean_RateError_userId) %>% mutate(rate_error_movieId_userID = rating - rate_predict_movieId_userId)
#-end-[Calculate mean error rate per userId, append to edx and calculate the prediction error in rating]

#-----[2. Decide what variable to select next]
mean_error_per_userId_YearsToRate_vec <- edx %>% group_by(years_to_rate) %>% summarise(mean_RateError_userId_YearsToRate = mean(rate_error_movieId_userID))
mean_error_per_userId_genre_vec <- edx %>% group_by(genres) %>% summarise(mean_RateError_userId_genres = mean(rate_error_movieId_userID))

mean_error_per_userId_YearsToRate_vec %>% 
  ggplot(aes(x = mean_RateError_userId_YearsToRate)) + 
  geom_histogram(bins = 50, color = "black", fill = "skyblue") +
  labs( x = "Average Movie Ratings", y = "Years to Rate Count") +
  theme_bw()
mean_error_per_userId_genre_vec %>% 
  ggplot(aes(x = mean_RateError_userId_genres)) + 
  geom_histogram(bins = 50, color = "black", fill = "skyblue") +
  labs( x = "Average Movie Ratings", y = "Genre Count") +
  theme_bw()

  #The highiest variation of error can be seen from genres. 
  #Therefore, we use genres as over next variable reduce the prediction error

#-end-[2. Decide what variable to select next]

#-----[Calculate mean error per genre, append to edx and calculate the prediction error in rating]
edx <- edx %>% left_join(mean_error_per_userId_genre_vec, by = "genres")
edx <- edx %>% mutate(rate_predict_movieId_userId_genres = rate_predict_movieId_userId + mean_RateError_userId_genres) %>% mutate(rate_error_movieId_userID_genres = rating - rate_predict_movieId_userId_genres)
#-----[Calculate mean error per years_to_rate, append to edx and calculate the prediction error in rating]

#-----[Calculate mean error per years_to_rate, append to edx and calculate the prediction error in rating]
mean_error_per_userId_genre_YearsToRate_vec <- edx %>% group_by(years_to_rate) %>% summarise(mean_error_per_userId_genre_YearsToRate = mean(rate_error_movieId_userID_genres))
edx <- edx %>% left_join(mean_error_per_userId_genre_YearsToRate_vec, by = "years_to_rate")
edx <- edx %>% mutate(rate_predict_movieId_userId_genre_YearsToRate = rate_predict_movieId_userId_genres + mean_error_per_userId_genre_YearsToRate) %>% mutate(rate_error_movieId_userID_genre_YearsToRate = rating - rate_predict_movieId_userId_genre_YearsToRate)
#-----[Calculate mean error per genre, append to edx and calculate the prediction error in rating]

#-----[Calculate Training RMSE]
RMSE_Train_movieId <- RMSE(edx$rating,edx$mean_rate_movieId)
RMSE_Train_movieId_userID <- RMSE(edx$rating,edx$rate_predict_movieId_userId)
RMSE_Train_movieId_userID_genre <- RMSE(edx$rating,edx$rate_predict_movieId_userId_genres)
RMSE_Train_movieId_userID_genre_YearsToRate <- RMSE(edx$rating,edx$rate_predict_movieId_userId_genre_YearsToRate)
#-end-[Calculate Training RMSE]

#-----[Calculate Testing RMSE]
#Prediction by only average rating per movieId
final_holdout_test <- final_holdout_test %>% left_join(mean_rating_per_movieId_vec, by = "movieId")
final_holdout_test <- final_holdout_test %>% mutate(rate_predict_movieID = mean_rate_movieId)

#Prediction by average rating per movieId and average error by userId
final_holdout_test <- final_holdout_test %>% left_join(mean_error_per_userId_vec, by = "userId")
final_holdout_test <- final_holdout_test %>% mutate(rate_predict_movieID_userId = mean_rate_movieId + mean_RateError_userId)

#Prediction by average rating by movieId and average error by userId and genre
final_holdout_test <- final_holdout_test %>% left_join(mean_error_per_userId_genre_vec, by = "genres")
final_holdout_test <- final_holdout_test %>% mutate(rate_predict_movieID_userId_genre = mean_rate_movieId + mean_RateError_userId + mean_RateError_userId_genres)

#Prediction by average rating by movieId and average error by userId, genree and years_to_rate
final_holdout_test <- final_holdout_test %>% left_join(mean_error_per_userId_genre_YearsToRate_vec, by = "years_to_rate")
final_holdout_test <- final_holdout_test %>% mutate(rate_predict_movieID_userId_genre_YearsToRate = mean_rate_movieId + mean_RateError_userId + mean_RateError_userId_genres + mean_error_per_userId_genre_YearsToRate)

RMSE_Test_movieID <- RMSE(final_holdout_test$rating,final_holdout_test$rate_predict_movieID)
RMSE_Test_movieID_userId <- RMSE(final_holdout_test$rating,final_holdout_test$rate_predict_movieID_userId)
RMSE_Test_movieID_userId_genre <- RMSE(final_holdout_test$rating,final_holdout_test$rate_predict_movieID_userId_genre)
RMSE_Test_movieID_userId_genre_YearsToRate <- RMSE(final_holdout_test$rating,final_holdout_test$rate_predict_movieID_userId_genre_YearsToRate)
#-end-[Calculate Testing RMSE]