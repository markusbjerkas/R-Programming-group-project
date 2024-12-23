#R project

# Load necessary libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(caret)

#Loading CSV files
ratings <- read.csv('dataset/ml-latest/ratings.csv')
tags <- read.csv('dataset/ml-latest/tags.csv')
movies <- read.csv('dataset/ml-latest/movies.csv')
links <- read.csv('dataset/ml-latest/links.csv')

#Checking for missing data
colSums(is.na(ratings))
colSums(is.na(tags))
colSums(is.na(movies))
colSums(is.na(links))

#Getting that the links data-set has missing data under tmbID column: Inspecting this:
missing_tmdb <- links[is.na(links$tmdbId), ]
print(missing_tmdb)

#Deciding not to do anything with this until needed.

#Splitting genres column of the movies data set into individual rows instead of '|' seperated
#This is useful for further analysis.
movie_genres_rows <- movies %>%
  separate_rows(genres, sep = '\\|')
tail(movie_genres_rows) #inspecting new data.

#Counting the number of movies for each genre
genre_count <- movie_genres_rows %>%
  count(genres, sort=TRUE) #Counting genres.
print(genre_count) 

#Inspecting average number of genres per movie
genres_per_movie <- movie_genres_rows %>%
  group_by(movieId, title) %>%
  summarize(genre_count = n(), .groups = 'drop')
print(genres_per_movie) #inspecting the data:

#Calculating the average genres per movie
avg_genres_per_movie <- mean(genres_per_movie$genre_count) #acessing the genre_count collumn
print(avg_genres_per_movie)

#Next step is to visualize the frequency of each genre
ggplot(genre_count, aes(x = reorder(genres, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = comma) + #Format numbers with comma.
  labs(
    title = "Frequency of Movie Genres",
    subtitle = paste("Average Genres per Movie:", round(avg_genres_per_movie, 2)),
    x = "Genres",
    y = "Count",
    ) +
  theme_minimal() + #Altering background.
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.major.y = element_blank()  # Remove horizontal gridlines for genres
  )


#TRENDS

#AVERAGE MOVIE RATING OVER TIME

#Converting timestamp in ratings
ratings$date <- as.POSIXct(ratings$timestamp, origin = "1970-01-01") #Converted Unix timestamp
ratings$year <- format(ratings$date, "%Y") #Extracted year for trend analysis

#Aggregating average rating per year
ratings_trend <- ratings %>%
  group_by(year) %>%
  summarize(avg_rating = mean(rating), rating_count = n(), .groups = 'drop')

#Plotting the trend of average ratings over time
ggplot(ratings_trend, aes(x = as.numeric(year), y = avg_rating)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Average Movie Ratings Over Time",
    x = "Year",
    y = "Average Rating"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )



#AVERAGE RATING OVER TIME BY GENRE

#Merging ratings with movies
ratings_movies <- ratings %>%
  inner_join(movies, by = "movieId") #Combined ratings and movies datasets

#Aggregating ratings by movie before splitting genres
avg_rating_by_movie <- ratings_movies %>%
  group_by(movieId, title) %>%
  summarize(avg_rating = mean(rating), rating_count = n(), .groups = 'drop') #Pre-aggregation

#Splitting genres and aggregate ratings by genre
avg_rating_by_genre <- ratings_movies %>%
  select(movieId, genres, rating) %>%
  separate_rows(genres, sep = "\\|") %>% #Split genres into separate rows
  group_by(genres) %>%
  summarize(
    avg_rating = mean(rating, na.rm = TRUE),
    total_ratings = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_rating))

#Visualising ratings by genre
ggplot(avg_rating_by_genre, aes(x = reorder(genres, avg_rating), y = avg_rating)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Average Movie Ratings by Genre",
    x = "Genres",
    y = "Average Rating"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

#Analysing rating trends over time
ratings$date <- as.POSIXct(ratings$timestamp, origin = "1970-01-01") #Converted timestamp
ratings$year <- format(ratings$date, "%Y") #Extracted year

ratings_trend <- ratings %>%
  group_by(year) %>%
  summarize(
    avg_rating = mean(rating, na.rm = TRUE),
    total_ratings = n(),
    .groups = 'drop'
  )

#Visualising rating trends over time
ggplot(ratings_trend, aes(x = as.numeric(year), y = avg_rating)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Average Movie Ratings Over Time",
    x = "Year",
    y = "Average Rating"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

#CORRELATION BETWEEN USER CHARACTERISTICS AND PREFERENCES

#Merge ratings and movies data
ratings_movies <- ratings %>%
  inner_join(movies, by = "movieId")

#Preprocess genres for regression (dummy encoding)
ratings_movies_genres <- ratings_movies %>%
  separate_rows(genres, sep = "\\|") %>%
  mutate(genre_flag = 1) %>%
  pivot_wider(names_from = genres, values_from = genre_flag, values_fill = 0)

#Aggregate ratings by movie for regression
regression_data <- ratings_movies_genres %>%
  group_by(movieId, title, year) %>%
  summarize(
    avg_rating = mean(rating, na.rm = TRUE),
    total_ratings = n(),
    across(starts_with("genre_"), sum, na.rm = TRUE),
    .groups = "drop"
  )

#Add release year as numeric
regression_data$year <- as.numeric(substr(regression_data$year, 1, 4))

#Simple Linear Regression: Predicting average rating based on total ratings and year
model <- lm(avg_rating ~ total_ratings + year, data = regression_data)
summary(model) # Inspect the model

#Multiple Linear Regression: Adding genres
genre_cols <- grep("genre_", names(regression_data), value = TRUE)
formula <- as.formula(
  paste("avg_rating ~ total_ratings + year +", paste(genre_cols, collapse = " + "))
)
model_with_genres <- lm(formula, data = regression_data)
summary(model_with_genres) # Inspect the model

#Optional: Evaluate model performance
#Splitting data for train-test validation
set.seed(123)
train_index <- createDataPartition(regression_data$avg_rating, p = 0.8, list = FALSE)
train_data <- regression_data[train_index, ]
test_data <- regression_data[-train_index, ]

#Train the model on training data
train_model <- lm(formula, data = train_data)

#Predict on test data
predictions <- predict(train_model, newdata = test_data)

#Calculate mean squared error
mse <- mean((test_data$avg_rating - predictions)^2)
print(paste("Mean Squared Error:", mse))

#Further Analysis

#Analysis of user behaviour through aggregation of user ratings
user_behavior <- ratings %>%
  group_by(userId) %>%
  summarize(avg_rating = mean(rating), total_ratings = n())
ggplot(user_behavior, aes(x = total_ratings, y = avg_rating)) +
  geom_point(alpha = 0.5) +
  labs(title = "User Rating Behavior", x = "Total Ratings", y = "Average Rating")

#Analysis of variance of pupularity of various movies by decade
movies$decade <- floor(as.numeric(substr(movies$year, 1, 4)) / 10) * 10
popularity_by_decade <- movies %>%
  inner_join(ratings, by = "movieId") %>%
  group_by(decade) %>%
  summarize(total_ratings = n())

#Future predictions
#Predictions of future trends in movie ratings
library(forecast)
ts_data <- ts(ratings_trend$avg_rating, start = as.numeric(min(ratings_trend$year)), frequency = 1)
forecast_model <- auto.arima(ts_data)
forecasted <- forecast(forecast_model, h = 5)
plot(forecasted)

#Predicting average movie ratings using regression models in relation to factors like genre,year or total ratings

# Prepare a predictive model 
genre_cols <- grep("genre_", names(regression_data), value = TRUE)
reg_formula <- as.formula(
  paste("avg_rating ~ total_ratings + year +", paste(genre_cols, collapse = " + "))
)

# Train-test split
set.seed(123)
train_index <- createDataPartition(regression_data$avg_rating, p = 0.8, list = FALSE)
train_data <- regression_data[train_index, ]
test_data <- regression_data[-train_index, ]

# Fit a regression model
lm_model <- lm(reg_formula, data = train_data)

# Predict test data
test_predictions <- predict(lm_model, newdata = test_data)

# Evaluate the model
mse <- mean((test_data$avg_rating - test_predictions)^2)
cat("Mean Squared Error on Test Data:", mse)

# Visualise and compare actual and predicted ratings
ggplot(data.frame(Actual = test_data$avg_rating, Predicted = test_predictions), aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Ratings", x = "Actual Ratings", y = "Predicted Ratings") +
  theme_minimal()


