
library("recommenderlab")
library("ggplot2")
data("MovieLense")

class(MovieLense)
methods(class=class(MovieLense))
object.size(MovieLense)

similarity_users_cosine <- similarity(MovieLense[1:4,],method = "cosine" ,which = "users")
similarity_users_jaccard <- similarity(MovieLense[1:4,],method = "jaccard" ,which = "users")
class(similarity_users_cosine)
class(similarity_users_jaccard)
as.matrix(similarity_users_cosine)
as.matrix(similarity_users_jaccard)
image(as.matrix(similarity_users_cosine), main = "Similarity Between Users(cosine)")
image(as.matrix(similarity_users_jaccard), main = "Similarity Between Users(jaccard)")

str(MovieLense)
str(MovieLenseMeta)

str(similarity_users_cosine)

#Recomendation Models
recommender_models <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
names(recommender_models)

vector_ratings <- as.vector(MovieLense@data)
ratings <- as.vector(MovieLense@data)
unique(vector_ratings)

#No. of movies that are rated 0,1,2,3,4 or 5
table_ratings <- table(ratings)
table_ratings

#Remove all 0 ratings
vector_ratings <- vector_ratings[vector_ratings != 0]
ratings <- factor(vector_ratings)

#Most of the ratings are above 2, and the most common is 4.
qplot(ratings) + ggtitle("Ratings Distribution")

views_per_movie <- colCounts(MovieLense)
views_per_movie

table_views <- data.frame(movie = names(views_per_movie),views = views_per_movie)
table_views <- table_views[order(table_views$views, decreasing =TRUE), ]

ggplot(table_views[1:6, ], aes(x = movie, y = views)) +geom_bar(stat="identity") + theme(axis.text.x =element_text(angle = 45, hjust = 1)) + ggtitle("Number of views of the top movies")

average_ratings <- colMeans(MovieLense)
qplot(average_ratings) + stat_bin(binwidth = 0.1) +ggtitle("Distribution of the average movie rating")

average_ratings_relevant <- average_ratings[views_per_movie > 100]
qplot(average_ratings_relevant) + stat_bin(binwidth = 0.1) +ggtitle(paste("Distribution of the relevant average ratings"))



ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,colCounts(MovieLense) > 100] 
ratings_movies

ratings_movies_norm <- normalize(ratings_movies)

min_movies <- quantile(rowCounts(ratings_movies), 0.98)
min_users <- quantile(colCounts(ratings_movies), 0.98)

image(ratings_movies[rowCounts(ratings_movies) > min_movies,colCounts(ratings_movies) > min_users], main = "Heatmap of the top users and movies")

image(ratings_movies_norm[rowCounts(ratings_movies_norm) > min_movies,colCounts(ratings_movies_norm) > min_users], main = "Heatmap of the top users and movies")

#Binarize the data
ratings_movies_watched <- binarize(ratings_movies, minRating = 1)

min_movies_binary <- quantile(rowCounts(ratings_movies), 0.95)
min_users_binary <- quantile(colCounts(ratings_movies), 0.95)

image(ratings_movies_watched[rowCounts(ratings_movies) > min_movies_binary,colCounts(ratings_movies) > min_users_binary], main = "Heatmap of the top users and movies")

ratings_movies_good <- binarize(ratings_movies, minRating = 3)

#User Based Collabrative Filtering
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommender_models$UBCF_realRatingMatrix$parameters

#Split into train and test
which_train <- sample(x = c(TRUE, FALSE), size = nrow(ratings_movies),replace = TRUE, prob = c(0.8, 0.2))
head(which_train)

recc_data_train <- ratings_movies[which_train, ]
recc_data_test <- ratings_movies[!which_train, ]

#which_set <- sample(x = 1:5, size = nrow(ratings_movies), replace =
#TRUE)
#for(i_model in 1:5) {
#  which_train <- which_set == i_model
#  recc_data_train <- ratings_movies[which_train, ]
#  recc_data_test <- ratings_movies[!which_train, ]
#  # build the recommender
#}

recc_model <- Recommender(data = recc_data_train, method = "UBCF")
recc_model

model_details <- getModel(recc_model)
names(model_details)

model_details$data
n_recommended <- 6
recc_predicted <- predict(object = recc_model,newdata = recc_data_test, n = n_recommended)
recc_matrix <- sapply(recc_predicted@items, function(x){
  colnames(ratings_movies)[x]
})
dim(recc_matrix)

recc_matrix[, 1:4]

number_of_items <- factor(table(recc_matrix))
chart_title <- "UBCF item distribution"
qplot(number_of_items) + ggtitle(chart_title)

number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(names(number_of_items_top), number_of_items_top)
table_top
