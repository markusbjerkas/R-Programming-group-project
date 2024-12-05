#R project.
#Loading csv files.
ratings <- read.csv('dataset/ml-latest/ratings.csv')
tags <- read.csv('dataset/ml-latest/ratings.csv')
movies <- read.csv('dataset/ml-latest/movies.csv')
links <- read.csv('dataset/ml-latest/links.csv')



#Checking for missing data:

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
#install.packages("tidyr")
library(tidyr) #loading library.

movie_genres_rows <- movies %>%
  separate_rows(genres, sep = '\\|')
tail(movie_genres_rows) #inspecting new data.

#Counting the number of movies for each genre, using the dplyr library.

library(dplyr)
genre_count <- movie_genres_rows %>%
  count(genres, sort=TRUE) #Counting genres.

print(genre_count) 
#also inspecting average number of genres per movie:
genres_per_movie <- movie_genres_rows %>%
  group_by(movieId, title) %>%
  summarize(genre_count = n(), .groups = 'drop')
print(genres_per_movie) #inspecting the data:

#Calculating the average genres per movie:

avg_genres_per_movie <- mean(genres_per_movie$genre_count) #acessing the genre_count collumn
print(avg_genres_per_movie)
#Next step is to visualize the frequency of each genre:
#install.packages('ggplot2')
#install.packages('scales')
library(scales) #using scales package to get better formatting on chart.
library(ggplot2) #Using ggplot2 library to vizualise data.

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
  