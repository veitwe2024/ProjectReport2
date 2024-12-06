# Import the things
library(ggplot2)
library(caret)
library(class)
library(corrplot)



# Import business dataframe
library(readr)
business <- read_csv("RStudioStuff/OA 11.7 - yelp_academic_dataset_business.json.csv")
View(business)



# Printing first 5 rows of the dataset
print(head(business, 5))



# Histogram / Bar graph of "state" column
ggplot(business) + geom_bar(aes(x=state), fill="darkgreen") + 
  labs(x="State", y="Number of Reviews", title="Number of Reviews per State")



# Pie Chart of "stars" per business
cont_table <- table(business$stars)
print(cont_table)
pie(cont_table, main="Pie Chart of Star Reviews", col=rainbow(7))



# Box Plot of Number of Reviews per Star Rating
#Remove Outliers
business_reviews <- subset(business, review_count<=18)
business_reviews$stars <- as.factor(business_reviews$stars)

ggplot(business_reviews, aes(x=stars,y=review_count, fill=stars)) + 
  geom_boxplot(show.legend=TRUE) + 
  labs(x="Stars", y="Reviews", title="Stars per Number of Reviews")


# Chi-Square Test of 1 Star vs 5 Star Reviews
one_five <- subset(business, stars==1  | stars==5)
View(one_five)

bus_cont_table <- table(one_five$stars)
print(bus_cont_table)
chisq.test(bus_cont_table)





# Import User Dataframe
library(readr)
user <- read_csv("RStudioStuff/OA 11.6 - yelp_academic_dataset_user.json.csv")
View(user)


# Print Column Names
names(user)


# Pearson R Correlation between "cool_votes", "funny_votes", and "useful_votes"
corr_cool_funny <- cor(user$cool_votes, user$funny_votes)
cat("Correlation Between Cool and Funny Votes:", corr_cool_funny)

corr_cool_useful <- cor(user$cool_votes, user$useful_votes)
cat("Correlation Between Cool and Useful Votes:", corr_cool_useful)

corr_useful_funny <- cor(user$useful_votes, user$funny_votes)
cat("Correlation Between Useful and Funny Votes:", corr_useful_funny)


# Linear Regression of "funny_votes" vs "useful_votes"
linear_model<- lm(user$funny_votes ~ user$useful_votes)
print(linear_model)

coefs <- coef(linear_model)
print(coefs)
lm_y_intercept <- coefs[1]
lm_slope <- coefs[2]
# Printing out y-intercept and slope
cat("Slope:", lm_slope, "Y-Intecept:", lm_y_intercept)

ggplot(user) + geom_point(aes(x = funny_votes, y = useful_votes)) + 
  geom_smooth(aes(x = funny_votes, y = useful_votes), method="lm", se=F)


# Correlation between "review_count" and "fans"
corr_reviews_fans <- cor(user$review_count, user$fans)
cat("Correlation Score:", corr_reviews_fans)
### Writing reviews has a slight positive effect on the number of fans a user has
### Pearson R coefficient is about halfway between 1 and 0 so as the number of
### reviews increases so does the number of fans. 


# Finding which variable has highest correlation with "review_count"
numeric_only <- user[,sapply(user, is.numeric)]
correlation_matrix <- cor(numeric_only)
View(correlation_matrix)
corrplot(correlation_matrix)
# "useful_votes" has highest correlation 0.665702285
# positive correlation, so the more useful reviews a user has the more fans they 
# are likely to get


# K_Means
library(caret)
library(class)
library(corrplot)
library(ggplot2)

# Classify by "review_count" using "fans"
# Make review_count categorical
user$review_count <- as.factor(user$reviewcount)
View(user) # all integers, but wound up being from 1 - 2000-something
#or
user$review_count <- cut(user$review_count,
                         breaks <- 5,
                         labels <- c("Low", "Medium", "High"),
                         include.lowest <- TRUE)
View(user) # error message

# I give up. I'm really struggling to make it categorical
# I still did all the rest, but it ain't gonna work

inTrain <- createDataPartition(y = user$review_count, p = 0.7, list = FALSE)
train <- user[inTrain, ]
test <- user[-inTrain, ]

y_train <- train$review_count
y_test <- test$review_count

# Names of columns I'm extracting
x_train <- train[,c("fans")]
x_test <- test[,c("fans")]

# Make predictions
predictions <- knn(train = x_train, test = x_test, cl = y_train, k = 5)

# Confusion Matrix
confusion_matrix <- table(Predicted = predictions, Actual = y_test)
# Either print or View confusion_matrix
View(confusion_matrix)
print(confusion_matrix)

# Accuracy Score
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Accuracy:", accuracy*100, "%")

# Correlation Matrix
numeric_only <- user[,sapply(user, is.numeric)]
correlation_matrix <- cor(numeric_only)
corrplot(correlation_matrix)



# Classify by Review_count again, but this time I'll use 
# useful_votes, cool_votes and fans as input

# Names of columns I'm extracting
x_train <- train[,c("useful_votes", "fans", "cool_votes")]
x_test <- test[,c("useful_votes", "fans", "cool_votes")]

# Make predictions
predictions <- knn(train = x_train, test = x_test, cl = y_train, k = 5)

# Confusion Matrix
confusion_matrix <- table(Predicted = predictions, Actual = y_test)
# Either print or View confusion_matrix
View(confusion_matrix)
print(confusion_matrix)

# Accuracy Score
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Accuracy:", accuracy*100, "%")

# Correlation Matrix
numeric_only <- user[,sapply(user, is.numeric)]
correlation_matrix <- cor(numeric_only)
corrplot(correlation_matrix)