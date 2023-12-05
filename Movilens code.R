################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

if (!require(knitr)) {
  install.packages("knitr")
  library(knitr)
}
if (!require(kableExtra)) {
  install.packages("kableExtra")
  library(kableExtra)
}

if (!require(xtable)) {
  install.packages("xtable")
  library(xtable)
}

if (!require(gridExtra)) {
  install.packages("gridExtra")
  library(gridExtra)
}

if (!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}

if (!require(lubridate)) {
  install.packages("lubridate")
  library(lubridate)
}

if (!require(recommenderlab)) {
  install.packages("recommenderlab")
  library(recommenderlab)
}

if (!require(glmnet)) {
  install.packages("glmnet")
  library(glmnet)
}

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

# Define column names
colnames(movies) <- c("movieId", "title", "genres")

# Convert the movieId column to characters
ratings$movieId <- as.character(ratings$movieId)

# Merge ratings and movies data frames by the 'movieId' column
movielens <- merge(ratings, movies, by = "movieId", all.x = TRUE)

# General information about movielens
glimpse(movielens)

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind = "Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)

edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

glimpse(edx)

n_distinct(edx$movieId)
n_distinct(edx$userId)

# lenght of rating
length(edx$rating)

############### DATA EXPLORATION ########################

# Histogram of rating
ggplot(edx, aes(x = rating)) +
  geom_histogram(fill = "orange", color = "black", bins = 10) +
  labs(title = "Distribution of Ratings", x = "Rating", y = "Frequency") +
  theme_bw()

# Summary of rating
summary(edx$rating)

# Calculate mean rating
movie_ratings_mean <- edx %>%
  group_by(movieId) %>%
  summarise(mean_rating = mean(rating), num_users = n_distinct(userId))

# Plot the graph of average ratings
movie_ratings_mean %>%
  ggplot(aes(x = mean_rating)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Average Ratings per Movie",
       x = "Average Rating",
       y = "Frequency") +
  theme_bw()

# Find the 5 movies with the worst mean ratings
worst_rated_movies <- movie_ratings_mean %>%
  arrange(mean_rating) %>%
  head(5)

# Get unique movie titles from the "edx" dataset
unique_worst_rated_movies <- worst_rated_movies %>%
  inner_join(edx, by = "movieId") %>%
  distinct(movieId, title, .keep_all = TRUE) %>%
  select(movieId, mean_rating, title, genres)

# Print the unique worst-rated movies
print(unique_worst_rated_movies)

# Find the 5 movies with the best mean ratings
best_rated_movies <- movie_ratings_mean %>%
  arrange(desc(mean_rating)) %>%
  head(6)

# Get unique movie titles from the "edx" dataset
unique_best_rated_movies <- best_rated_movies %>%
  inner_join(edx, by = "movieId") %>%
  distinct(movieId, title, .keep_all = TRUE) %>%
  select(movieId, mean_rating, title, genres)

# Print the unique best-rated movies
print(unique_best_rated_movies)

## GENRE
# Split genres into separate rows
genres_df <- edx %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(n = n(), mean_genre = sum(rating/n))


# Graph of count genres
count_plot <- genres_df %>%
  ggplot(aes(x = reorder(genres, -n), y = n)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "(1) Count of Movies by Genre", x = "Genres", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Graph of mean genres
mean_plot <- genres_df %>%
  ggplot(aes(x = reorder(genres, -n), y = mean_genre)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.7) +
  labs(title = "(2) Mean Rating of Movies by Genre", x = "Genres", y = "Mean Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Count genres and mean together
grid.arrange(count_plot, mean_plot, ncol = 1)


edx %>% mutate(title = str_trim(title)) %>%
  # split title column to two columns: title and year
  extract(title, c("title_temp", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F) %>%
  # for series take debut date
  mutate(year = if_else(str_length(year) > 4, as.integer(str_split(year, "-", simplify = T)[1]), as.integer(year))) %>%
  # replace title NA's with original title
  mutate(title = if_else(is.na(title_temp), title, title_temp)) %>%
  # drop title_tmp column
  select(-title_temp)

### Year movie
## Visualizing Movie Releases Over the Years

edx %>%
  extract(title, into = c("movie_title", "year"), regex = "(.*) \\((\\d{4})\\)", convert = TRUE) %>%
  filter(!is.na(year) & year > 1915) %>%
  ggplot(aes(x = factor(year))) +
  geom_bar() +
  labs(title = "Number of Movies Released Each Year",
       x = "Year",
       y = "Number of Movies") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_bw()


### Year review
# Split the year of rating
year_review <- edx %>%
  mutate(year_rating = lubridate::year(lubridate::as_datetime(timestamp)),
         year_rating = as.numeric(ifelse(!is.na(year_rating), year_rating, NA))) %>%
  filter(!is.na(year_rating))

# Plot the graph year of rating per number of reviews
year_review %>%
  ggplot(aes(x = factor(year_rating))) +
  geom_bar() +
  labs(title = "Number of Reviews Each Year",
       x = "Year",
       y = "Number of Reviews") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme_bw()

############### METHOD ##################################
# Create an object from a subset of your data
set.seed(123)
subset_users <- sample(unique(edx$userId), 1000)
subset_items <- sample(unique(edx$movieId), 500)

# Convert "userId" and "movieId" to numeric
subset_data <- edx[edx$userId %in% subset_users & edx$movieId %in% subset_items, ]
subset_data$userId <- as.numeric(subset_data$userId)
subset_data$movieId <- as.numeric(subset_data$movieId)

# Create the realRatingMatrix
rating_matrix <- as.matrix(subset_data[, c("userId", "movieId", "rating")])

# Split the data into training and test sets
set.seed(123)
train_test_split <- sample(c(TRUE, FALSE), nrow(rating_matrix), replace = TRUE, prob = c(0.8, 0.2))
train_data <- rating_matrix[train_test_split, ]
test_data <- rating_matrix[!train_test_split, ]

# Function to compute RMSE
RMSE <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Function to clamp values within a specified range
clamp <- function(x, lower, upper) {
  pmin(pmax(x, lower), upper)
}

# Check if 'train_data' is sparse
if (!is(train_data, "sparseMatrix")) {
  # Convert to sparse matrix
  train_data <- Matrix(train_data, sparse = TRUE)
}

# Check if 'test_data' is a data frame
if (!is(test_data, "sparseMatrix") && is.data.frame(test_data)) {
  numeric_columns_test <- test_data[, sapply(test_data, is.numeric)]
  test_data <- Matrix(numeric_columns_test, sparse = TRUE)
}

# Compute the average rating
avg_rat <- mean(edx$rating)

# Initialize a tibble to store RMSE results
rmse_results <- tibble(Method = character(), RMSE = numeric())

# Model: Global Average
mod_average <- RMSE(edx$rating, avg_rat)
rmse_results <- bind_rows(rmse_results, tibble(Method = "Global Average", RMSE = mod_average))

# Model: Movie Effect Model
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(movie_effect = mean(rating - avg_rat))

movie_avgs %>%
  ggplot(aes(movie_effect)) +
  geom_histogram()

# Join data and print column names
joined_data <- validation %>%
  left_join(movie_avgs, by = 'movieId')

# Pull the movie_effect column and handle missing values
predicted_ratings <- avg_rat + joined_data %>%
  pull(movie_effect)

# Display summary statistics
summary(predicted_ratings)

# Clamp predicted ratings to the specified range
predicted_ratings <- clamp(predicted_ratings, 0.5, 5)

# Calculate RMSE for Movie Effect Model
movie_effect_rmse <- RMSE(predicted_ratings, validation$rating)


# Store the RMSE result in the results table
rmse_results <- bind_rows(rmse_results, tibble(Method = "Movie Effect Model", RMSE = movie_effect_rmse))

rmse_results

# Model: Movie + User Effects Model
user_avgs <- edx %>% 
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(user_rating_deviation = mean(rating - avg_rat - movie_effect))

# Predict ratings using the Movie-Specific Effects Model
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  mutate(pred = avg_rat + movie_effect + user_rating_deviation) %>%
  pull(pred)

summary(predicted_ratings)

# Clamp predicted ratings to the specified range [0.5, 5]
predicted_ratings <- clamp(predicted_ratings, 0.5, 5)

# Calculate RMSE for Movie + User Effects Model
rmse_movie_user <- RMSE(predicted_ratings, validation$rating)  # Range for clamping predicted ratings (0.5, 5)

# Store the RMSE result in the results table
rmse_results <- bind_rows(rmse_results, tibble(Method = "Movie + User Effects Model", RMSE = rmse_movie_user))


# Creating the long version of both the train and validation datasets. With separated genres
edx_genres <- edx %>%
  separate_rows(genres, sep = "\\|", convert = TRUE)

validation_genres <- validation %>%
  separate_rows(genres, sep = "\\|", convert = TRUE)

# Model: Global average plus Movies plus Users plus Genres Effects Model
# Calculate genre-specific effects using individual genre models
genres_effects_ind <- edx_genres %>% 
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  group_by(genres, movieId) %>%
  summarize(genre_effect = mean(rating - avg_rat - movie_effect - user_rating_deviation))

# Convert movieId to double in genres_effects_ind
genres_effects_ind <- genres_effects_ind %>%
  mutate(movieId = as.double(movieId))

# Convert movieId to numeric in validation_genres
validation_genres <- validation_genres %>% mutate(movieId = as.numeric(movieId))

# Perform the left joins with consistent data types
predicted_ratings <- validation_genres %>% 
  left_join(movie_avgs %>% mutate(movieId = as.double(movieId)), by = 'movieId') %>%
  left_join(user_avgs %>% mutate(userId = as.double(userId)), by = 'userId') %>%
  left_join(genres_effects_ind %>% mutate(movieId = as.double(movieId)), by = c('genres', 'movieId')) %>%
  mutate(pred = avg_rat + movie_effect + user_rating_deviation + genre_effect) %>%
  pull(pred)

# Clamp predicted ratings to the specified range [0.5, 5]
predicted_ratings <- clamp(predicted_ratings, 0.5, 5)

# Calculate the RMSE for the Movie + User + Genres Effects Model
rmse_movie_user_genres <- RMSE(predicted_ratings, validation_genres$rating)

# Store the RMSE result in the results table
rmse_results <- bind_rows(rmse_results, tibble(Method = "Movie + User + Genres Ind. Effects Model", RMSE = rmse_movie_user_genres))

# Model: Movie + User + Genres + Genre_User Effects Model (Regularized)


# Create a smaller dataset for testing (adjust the size as needed)
small_edx <- edx[sample(nrow(edx), 1000), ]

# Assuming small_validation_genres is a subset of the original validation dataset
small_validation_genres <- validation_genres[sample(nrow(validation_genres), 100), ]

# Function to calculate RMSE for different regularization values (lambdas)
lambdas <- seq(2, 10, 0.25)
calculate_rmse <- function(l) {
  mu <- mean(small_edx$rating)
  
  movie_effect <- small_edx %>% 
    group_by(movieId) %>%
    mutate(movie_effect = sum(rating - mu) / (n()+l)) %>%
    distinct(movieId, movie_effect)
  
  movie_effect$movieId <- as.character(movie_effect$movieId)  # Convert to character
  
  user_rating_deviation <- small_edx %>% 
    left_join(movie_effect, by = "movieId") %>%
    group_by(userId) %>%
    summarize(user_rating_deviation = sum(rating - movie_effect - mu) / (n() + l))
  
  # Convert movieId to character before join
  small_validation_genres$movieId <- as.character(small_validation_genres$movieId)
  
  predicted_ratings <- small_validation_genres %>% 
    left_join(movie_effect, by = "movieId") %>%
    left_join(user_rating_deviation, by = "userId") %>%
    mutate(pred = mu + coalesce(movie_effect, 0) + coalesce(user_rating_deviation, 0)) %>%
    pull(pred)
  
  # Convert predicted_ratings to numeric
  predicted_ratings <- as.numeric(predicted_ratings)
  
  # Calculate RMSE
  rmse <- RMSE(predicted_ratings, small_validation_genres$rating)
  
  # Print some values for debugging
  print(paste("l:", l))
  print(paste("RMSE:", rmse))
  
  rmse
  
}

# # Calculate RMSE for each lambda value
rmse_values <- sapply(lambdas, calculate_rmse)

# # Calculate RMSE for each lambda value using a loop
# rmse_values <- numeric(length(lambdas))
# for (i in seq_along(lambdas)) {
#   rmse_values[i] <- calculate_rmse(lambdas[i])
# }

# Find the optimal lambda
optimal_lambda <- lambdas[which.min(rmse_values)]

# Print the optimal lambda
print(optimal_lambda)

# # Print RMSE
# cat("RMSE:", min(rmse_values), "\n")

# Update and save the RMSE results
rmse_results <- bind_rows(
  rmse_results,
  data.frame(
    Method = "Regularization with Movie and User Effects",  
    RMSE = min(rmse_values)
  )
)

rmse_results


########### VALIDATION ###########
# Function to align columns of the validation dataset
align_columns <- function(training_data, validation_data) {
  # Identify common movieId values between training and validation datasets
  common_movieIds <- intersect(training_data$movieId, validation_data$movieId)
  
  # Filter validation dataset to include only common movieId values
  common_validation_data <- filter(validation_data, movieId %in% common_movieIds)
  
  # Extract numeric columns for training
  numeric_columns <- select_if(training_data, is.numeric) %>% colnames()
  
  # Extract numeric columns for validation dataset
  validation_numeric <- select(common_validation_data, all_of(numeric_columns))
  
  return(validation_numeric)
}

# Convert genres to a factor and create dummy variables for the validation dataset
small_validation_genres$genres <- as.factor(small_validation_genres$genres)
validation_matrix <- model.matrix(~ . - rating - timestamp - title, data = small_validation_genres)[, -1]

# Train the model using cross-validation
numeric_columns <- select_if(small_edx, is.numeric) %>% colnames()
cv_model <- cv.glmnet(as.matrix(small_edx[, numeric_columns, with = FALSE]), small_edx$rating)

# Optimal lambda
optimal_lambda <- cv_model$lambda.min
print(paste("Optimal lambda:", optimal_lambda))

# Align columns of the validation dataset
common_validation_data <- align_columns(small_edx, small_validation_genres)

# RMSE on the validation set
predictions <- predict(cv_model, newx = as.matrix(common_validation_data), s = optimal_lambda)

# Calculate RMSE
rmse <- sqrt(mean((predictions - common_validation_data$rating)^2))
print(paste("RMSE:", rmse))

