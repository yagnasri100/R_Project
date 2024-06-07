#Step 1: Preparation

# Load the tidyverse
library(tidyverse)

#Using the e1071 package to calculate the skewness
library(e1071)

library(treemap) #for creating a treemap

# Modeling process packages
library(rsample)   # for resampling procedures

library(caret)  # for fitting KNN models

library(recipes)    # for feature engineering

library(tidymodels)

library(reshape2)

# Load the required library
library(Matrix)

library(stringr)

library(class)

#Read the Data
Anime<- read_csv('C:/Users/yagna/Desktop/MSIS/581/FINAL_PROJECT/anime.csv')
Rating<-read_csv('C:/Users/yagna/Desktop/MSIS/581/FINAL_PROJECT/ratings.csv')

dim(Anime)
dim(Rating)

#glimpse of anime dataset
glimpse(Anime)

#glimpse of rating dataset
glimpse(Rating)

#summary of anime and rating datasets

summary(Anime)

summary(Rating)

#STEP 2: DATA CLEANING

#Let's remove the duplicate values in Anime dataset and compare the no of rows.
Anime_dup=unique(Anime)
dim(Anime_dup) #we can see that the no of rows are: 12,294 which means we have zero duplicate values 

#Let's check for duplicate values in Rating dataset
Rating_dup=unique(Rating)
dim(Rating_dup) #it has zero duplicate entries among 46,986 entries.


#Identifying the null values in Anime and Rating datasets
Anime_null_values <-colSums(is.na(Anime_dup))
Rating_null_values <-colSums(is.na(Rating_dup))

#transposed view of null values
view(t(Anime_null_values))

#transposed view of null values
view(t(Rating_null_values)) #Ratings dataset has zero null values

#The number of null values in Anime_dup dataset is relatively small compared to the overall dataset size and dropping them doesn't significantly affect the analysis

#Dropping the null values in Anime:

Anime_df <- Anime_dup[complete.cases(Anime_dup), ] 

#Lets check for null values again

Anime_null_values <-colSums(is.na(Anime_df))

view(t(Anime_null_values)) #the null values are zero in Anime_df dataset

#Now let us merge the datasets 

merged_data <- merge(Anime_df, Rating_dup, by = "anime_id")

glimpse(merged_data)

#renaming the columns in merged_data

ColNames <- c("Anime_ID", "Name", "Genre", "Type", "Episodes", "Rating", "Members", "User_ID", "User_Rating")

colnames(merged_data) <- ColNames

glimpse(merged_data)

#We have many special characters in the Anime Name. ex: Wolf&#039;s Rain
#Lets remove the special characters from the Name
# Remove special characters from the Name column

# Defining patterns to remove
patterns <- c("&quot;", "\\.hack//", "&#039;", "A's", "I's", "&amp;")

# Iterate over patterns and remove them from the Name column
for (pattern in patterns) {
  merged_data$Name <- gsub(pattern, "", merged_data$Name)
}

# Capitalize every starting letter in the Name column
merged_data$Name <- str_to_title(merged_data$Name)

glimpse(merged_data$Name)

#STEP 3: Analysis

#1. Anime Categories 

Anime_types<-merged_data %>%
  group_by(Type) %>%
  summarise(Total= n_distinct(Anime_ID)) %>%
  arrange(desc(Total))


# Calculate the percentage of each category
Anime_types <- Anime_types %>%
  mutate(Percentage = (Total / sum(Total)) * 100)
view(Anime_types)

# Converting 'Type' variable to categorical using factor()
Anime_types$Type <- factor(Anime_types$Type)
str(Anime_types)

# Plot a pie chart
ggplot(Anime_types, aes(x = "", y = Percentage, fill = Type, label = paste0(round(Percentage, 1), "%"))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Anime Categories", fill = "Type", y = "Percentage") +
  geom_text(position = position_stack(vjust = 0.5), color = "black") +
  theme_void() +
  theme(legend.position = "right")

#From the plot we can say that majority (43.4%) of anime is aired on TV. 

#2.Most Popular Anime: Community size vs. Ratings

# Filtering the data to select the top 20 rows based on "Members"
top_20 <- merged_data %>%
  group_by(Name) %>%
  summarise(Total_Members = sum(Members),
            Rating = mean(Rating, na.rm = TRUE)) %>%
  top_n(20, Total_Members) %>%
  arrange(desc(Total_Members))

view(top_20)

# Creating the bar plot of top 20 popular anime
ggplot(data = top_20,
       mapping = aes(x = reorder(Name, -Total_Members), y = Total_Members,fill = Rating)) +  # Reorder anime names based on Members
  geom_bar(stat = "identity") +
  geom_text(aes(label = Total_Members), vjust = 0.5, color = "black", size = 3, angle = 90) +  # Add labels inside the bar
  labs(x = "Anime Name", y = "Members") +
  scale_fill_gradient(low = "cornflowerblue", high = "darkblue")+
  ggtitle("Top20 Most Popular Anime: Community size vs. Ratings")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


#3.Exploring the different genres 


glimpse(merged_data$Genre) #We can see multiple genres are separated by a ",". now lets split the genre of each row seperated by a comma(,)

# Extract distinct anime
distinct_genres <- merged_data %>%
  select(Anime_ID, Genre) %>%
  distinct()

glimpse(distinct_genres)

# Split the string in each row to extract individual genres
genre_list <- strsplit(distinct_genres$Genre, ", ")

# Convert the list of lists into a single vector
all_genres <- unlist(genre_list)

# Counting the occurrences of each genre
genre_counts <- table(all_genres)

# Converting the result into a dataframe
genre_df <- as.data.frame(genre_counts)

# Renaming the columns
colnames(genre_df) <- c("Genre", "Count")

genre_df <- genre_df %>% 
  arrange(desc(Count))

glimpse(genre_df)

#We have 43 different genre in our merged dataset

# Creating the treemap plot

treemap(genre_df, 
        index = "Genre",  # Specify the index column
        vSize = "Count",  # Specify the column for the size of the tiles
        title = "Genre Distribution",  # Title of the treemap
        fontsize.title = 14,  # Font size for the title
        fontsize.labels = 10,  # Font size for the labels
        fontsize.labels.min = 6,  # Minimum font size for labels
        align.labels = list(c("center", "center")),  # Alignment of labels
        palette = "RdYlBu",  # Color palette for the tiles
        width = 10,  # Width of the plotting area
        height = 10  # Height of the plotting area
)

#4: Finding potential outliers
#Finding the distribution of the user rating to identify any potential outliers.

# Sort merged_data by User_Rating in descending order
merged_data <- merged_data %>% 
  arrange(desc(User_Rating))

#generating a histogram of user rating 

ggplot() +
  geom_histogram(data = merged_data, aes(x = User_Rating), fill = "cornflowerblue", color = "black") +
  labs(title = "User Anime ratings distribution", x = "User rating", y = "Total")

#We can see that -1 is a outlier in user ratings. -1(If the user has watched the anime but haven't rated it)

skewness(merged_data$User_Rating)
#The skewness is -0.96<0 proves that distribution is negatively skewed.


#STEP 4: Modeling
#Before proceeding let us make sure to consider only valid scenarios.
## Let's replace -1 (non rating) in User_rating column with the possible predicted rating.i.e,Let's predict user ratings for anime they haven't rated.

#Identify unrated anime
unrated_anime <- merged_data %>%
  filter(User_Rating == -1)

glimpse(unrated_anime)
#We can see that we have 10,426 unrated entries in our data frame.


# Predict ratings for unrated anime
predicted_ratings <- unrated_anime %>%
  group_by(Name) %>%
  summarise(Predicted_Rating = mean(Rating))

# Merge predicted ratings with merged_data based on the anime name
merged_data <- merge(merged_data, predicted_ratings, by = "Name", all.x = TRUE)

# Replace "-1" with predicted ratings
merged_data$User_Rating[merged_data$User_Rating == -1] <- merged_data$Predicted_Rating[merged_data$User_Rating == -1]

# Remove the temporary column used for prediction
merged_data <- subset(merged_data, select = -c(Predicted_Rating))

merged_data <- merged_data %>% 
  arrange(desc(User_Rating))

# View updated dataset
summary(merged_data) #We have handled all the outliers. 


dim(merged_data)

# Merged_data includes columns: User_ID, Anime_ID, User_Rating

colnames(merged_data)

#split the dataset into training(70%) and testing(30%)

merged_data$Liked <- ifelse(merged_data$User_Rating >= 7, 1, 0) #Creating Liked column where if rating is >7 then its values is 1 and if <1 then its value is 0
train_index <- createDataPartition(merged_data$Liked, p = 0.7, list = FALSE)
train_data <- merged_data[train_index, ]
test_data <- merged_data[-train_index, ]

# Preprocess the data
train_X <- train_data[, c("User_ID", "Anime_ID")]
train_y <- train_data$Liked
test_X <- test_data[, c("User_ID", "Anime_ID")]
test_y <- test_data$Liked

# Training the kNN model using caret's train function
model_knn <- train(x = train_X, y = train_y, method = "knn", trControl = trainControl(method = "cv", number = 5))


# Converting train_y to a factor with two levels
train_y <- as.factor(train_y) 

# Make predictions on the test data
test_predictions <- predict(model_knn, newdata = test_X)

# Evaluate accuracy
accuracy <- mean(test_predictions == test_y)
cat("Accuracy:", accuracy, "\n")

# This shows that our model is 83% accurate. It indicates that the kNN model performs reasonably well on the test data.

recommend_similar_anime <- function(anime_id, n) {
  # Find the row corresponding to the input anime ID in the test data
  input_anime_row <- test_data[test_data$Anime_ID == anime_id, ]
  
  # Check if the input anime ID is present in the test data
  if (nrow(input_anime_row) == 0) {
    print("Anime not found in the dataset.")
    return(NULL)
  }
  
  # Extract features of the input anime
  input_features <- input_anime_row[, c("Genre", "Rating", "Members")]
  
  #Calculate similarity between input anime and all other anime based on features
  #lapply is a function in R used to apply a given function to each element of a list or vector, and returns a list containing the results. 
  
  anime_similarity <- lapply(unique(merged_data$Anime_ID), function(id) {
    anime_row <- merged_data[merged_data$Anime_ID == id, ]
    if (nrow(anime_row) > 0) {
      # Calculating the similarity score based on the intersection of genres
      similarity <- sum(input_features$Genre %in% anime_row$Genre) / length(input_features$Genre)
      return(c(id, similarity))
    } else {
      return(NULL)
    }
  })
  
  # Remove NULL values and convert to matrix
  anime_similarity <- matrix(unlist(anime_similarity), ncol = 2, byrow = TRUE)
  anime_similarity <- anime_similarity[complete.cases(anime_similarity), ]
  colnames(anime_similarity) <- c("Anime_ID", "Similarity")
  
  # Order by similarity and get top n similar anime
  similar_anime <- anime_similarity[order(-anime_similarity[, "Similarity"]), "Anime_ID"][1:n]
  
  return(similar_anime)
}

# Function to map anime names to anime IDs
get_anime_id <-function(anime_name) {
  anime_id <- merged_data$Anime_ID[merged_data$Name == anime_name]
  if (length(anime_id) == 0) {
    print("Anime name not found in the dataset.")
    return(NULL)
  }
  return(anime_id)
}

# Function to retrieve anime names
get_anime_names <- function(anime_ids) {
  anime_names <- unique(merged_data$Name[merged_data$Anime_ID %in% anime_ids])
  return(anime_names)
}

# Example 
query_anime_name <- "one piece movie 1"  # Replace with the anime name of the anime you want recommendations for
query_anime_name<-str_to_title(query_anime_name) #Handling case sensitive inputs.
query_anime_id<-get_anime_id(query_anime_name)
recommendations <- recommend_similar_anime(query_anime_id, 10)
recommended_names <- get_anime_names(recommendations)

cat(paste("Anime recommendations similar to", query_anime_name, "are:\n", paste(recommended_names, collapse = "\n"), "\n"))


