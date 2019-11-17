
library("recommenderlab")
library("ggplot2")
data("MovieLense")
#MovieLense100 <- MovieLense[rowCounts(MovieLense) >100,]
#MovieLense100
#train <- MovieLense100[1:50]
#rec <- Recommender(train, method = "UBCF")
#rec
#pre <- predict(rec, MovieLense100[101:102], n = 10)
#pre
#as(pre, "list")
#accuracy(rec)
#summary(rec)
#calcPredictionAccuracy(rec,MovieLense100,byUser=TRUE)
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
unique(vector_ratings)

table_ratings <- table(vector_ratings)
table_ratings

#Remove all 0 ratings
vector_ratings <- vector_ratings[vector_ratings != 0]
vector_ratings <- factor(vector_ratings)

#Most of the ratings are above 2, and the most common is 4.
qplot(vector_ratings) + ggtitle("Distribution of the ratings")

views_per_movie <- colCounts(MovieLense)
views_per_movie

table_views <- data.frame(movie = names(views_per_movie),views = views_per_movie)
table_views <- table_views[order(table_views$views, decreasing =TRUE), ]

ggplot(table_views[1:6, ], aes(x = movie, y = views)) +geom_bar(stat="identity") + theme(axis.text.x =element_text(angle = 45, hjust = 1)) + ggtitle("Number of views of the top movies")
