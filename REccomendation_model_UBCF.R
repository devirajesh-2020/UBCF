library(arules)
data("Adult")
class(Adult)
View(Adult)
summary(Adult)

devi
data("Groceries")
class(Groceries)
summary(Groceries)


rules <- apriori(Adult, parameter = list(support= 0.5, confidence= 0.7))
rules
inspect(rules)

sort.rules <- sort(rules, by="lift")
sort.rules

inspect(sort.rules)
plot(sort.rules)
library(arulesViz)
quality(rules)
plot(rules, method="grouped")



rules <- apriori(Adult, parameter = list(support= 0.5, confidence=0.7),
                 appearance = list(rhs= c("race=White"), lhs = c("workclass=Private","native-country=United-States","hours-per-week=Full-time"), 
                                   default="rhs"))

inspect(rules)
rules
plot(rules, method= "grouped")

redundant = is.redundant(rules, measure="confidence")
which(redundant)
inspect(rules)
rules.pruned<- rules[!redundant]
inspect(rules.pruned)
plot(rules.pruned, method= "grouped")

quality(rules)= round(quality(rules), digits=3)
quality(rules)

library(recommenderlab)
help(package="recommenderlab")
data("MovieLense")
class(MovieLense)
str(MovieLense)
names(MovieLense)
View(MovieLense)
dim(MovieLense)
summary(MovieLense)


data("Jester5k")
class(Jester5k)
str(Jester5k)
dim(Jester5k)
View(Jester5k)
vector_ratings <-as.vector(Jester5k@data)
as.matrix(Jester5k@data[1:10, 1:10])
unique(vector_ratings)
table_ratings <- table(vector_ratings)
table_ratings

vector_ratings <- vector_ratings[vector_ratings!=0]
table(vector_ratings)
qplot(vector_ratings)+ggtitle("Distribution of Ratings")

library(ggplot2)

views_per_jokes <- colCounts(Jester5k)
table_views <- data.frame(jokes= names(views_per_jokes), views= views_per_jokes)
table_views <- table_views[order(table_views$views,decreasing = TRUE),]
head(table_views)
colnames(table_views)
ggplot(table_views[1:40,], aes(x=jokes, y = views))+ geom_bar(stat = "identity")

average_rating <- colMeans(Jester5k)

qplot(average_rating)+ stat_bin(binwidth= 0.1)+ ggtitle(("Distribution of average joke rating"))



average_ratings_relevant <- average_rating[views_per_jokes>1000]
qplot(average_ratings_relevant)+stat_bin(binwidth = 0.1)+ggtitle("Distribution of average joke rating relevant")

image(Jester5k, main= "heatmap of rating matrix")

min_n_jokes <- quantile(rowCounts(Jester5k),0.99)>min_n_jokes
min_n_jokes

min_n_users <- quantile(colCounts(Jester5k),0.99)>min_n_users
min_n_users
image(Jester5k[rowCounts(Jester5k)>min_n_jokes, colCounts(Jester5k)>min_n_users], main= "heatmap of top users and Jokes")


rating_jokes <- Jester5k[rowCounts(Jester5k)>50, colCounts(Jester5k)>100]
rating_jokes
min_jokes <- quantile(rowCounts(Jester5k),0.98)>min_jokes
min_jokes
min_users <- quantile(colCounts(Jester5k),0.98)>min_users
min_users
image(rating_jokes[rowCounts(rating_jokes)>min_jokes, colCounts(rating_jokes)>min_users], main= "heatmap of top users and Jokes")

rating_jokes_norm <- normalize(rating_jokes)
image(rating_jokes_norm[rowCounts(rating_jokes_norm)>min_jokes, colCounts(rating_jokes_norm)>min_users], main= "heatmap of the top users and jokes")


which_train <- sample(x= c(TRUE,FALSE), size= nrow(rating_jokes),replace = TRUE, prob = c(0.8,0.2))
recc_data_train <- rating_jokes[which_train, ]
recc_data_test <- rating_jokes[!which_train, ]


recc_model_UBCF <- Recommender(data = recc_data_train, method= "UBCF")
n_recommended_UBCF <- 8
recc_predicted_UBCF <- predict(object = recc_model_UBCF, newdata = recc_data_test, n=n_recommended_UBCF)
recc_predicted_UBCF



recc_predicted_UBCF@items[[10]]
recc_user_1_UBCF <- recc_predicted_UBCF@items[[10]]

jokes_User_1_UBCF <- recc_predicted_UBCF@itemLabels[recc_user_1_UBCF]
jokes_User_1_UBCF
