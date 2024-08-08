# Michelle Tan
# Assignment 2
# Movie Dataset

#Which decade had the greatest number of movies released or highest average movie rating? 

#This question gives historical insights on how the movie industry has evolved over 
#the years in terms of quantity and quality. It helps our team to identify trends that
#determine which decades had the highest volume of movies released to understand the production 
#trends over time. The analysis of rating guides to identify the years when high-quality movies were released.  


library(dplyr)
library(ggplot2)

# read the movie dataset csv from kaggle and replace the blank values with NA
movie_df2 <- read.csv('movie_dataset.csv', na.strings = c("", "NA"))

# grab only the year part from release date and create new column year
movie_df2$year <- format(as.Date(movie_df2$release_date,format="%Y-%m-%d"),"%Y")

# check which movie has no release date -- only one movie
blank_row <- movie_df2[is.na(movie_df2$year),]

# count of movies each year
movie_df2 %>% count(year)

# categorize the years to the decade
x <- movie_df2 %>% count(year) %>% mutate(decade = floor(as.numeric(year)/10)*10)%>%  filter(!is.na(decade)) %>% group_by(decade) 
print(x)

# get the total count for each decade
decade_count <- x %>% summarise(total_count = sum(n))

ggplot(decade_count, aes(x = factor(decade), y = total_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Count of Movies by Decade", x = "Decade", y = "Number of Movies") +
  theme_minimal()


# Which decade had the highest average movie rating (average popularity, average vote count, 
# average revenue)?

# based on average popularity per decade
# create new column to categorize the year into decade
movie_df2$decade <- floor(as.numeric(movie_df2$year)/10)*10

# get the average popularity rate per decade
average_popularity <- movie_df2 %>% filter(!is.na(decade)) %>% group_by(decade) %>% summarise(average_popularity = mean(popularity, na.rm = TRUE))

# Plot the average popularity rating by decade
ggplot(average_popularity, aes(x = factor(decade), y = average_popularity)) +
  geom_line(group = 1, color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "Average Popularity Rating of Movies by Decade",
       x = "Decade", y = "Average Popularity Rating") +
  theme_minimal()

# which movie has the highest popularity rating
movie_df2[which.max(movie_df2$popularity), ]  # Minions 2015

# based on average vote count per decade
# get the average vote count per decade
average_vote_count <- movie_df2 %>% filter(!is.na(decade)) %>% group_by(decade) %>% summarise(average_vote_count = mean(vote_count, na.rm = TRUE))

# Plot the average vote count by decade
ggplot(average_vote_count, aes(x = factor(decade), y = average_vote_count)) +
  geom_line(group = 1, color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "Average Vote Count of Movies by Decade",
       x = "Decade", y = "Average Vote Count") +
  theme_minimal()


# which movie has the highest vote count
movie_df2[which.max(movie_df2$vote_count), ]  # Inception

# based on the average revenue per decade
# get the average revenue per decade
average_revenue <- movie_df2 %>% filter(!is.na(decade)) %>% group_by(decade) %>% summarise(average_revenue = mean(revenue, na.rm = TRUE))

# Plot the average revenue by decade
ggplot(average_revenue, aes(x = factor(decade), y = average_revenue)) +
  geom_line(group = 1, color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "Average Revenue of Movies by Decade",
       x = "Decade", y = "Average Revenue") +
  theme_minimal()
