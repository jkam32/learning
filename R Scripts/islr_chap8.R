# require R version 3.6.0 and above
library(tree)
library(ISLR)
library(MASS)
library(randomForest)
library(gbm)

# Can't reproduce the example in ISLR Chap 8 tree part;
# Pruned tree has a lower accuracy than original tree

High <- ifelse(Carseats$Sales <= 8, "No", "Yes")
Carseats <- data.frame(Carseats, High)

#### Classification Tree ####
tree_carseats <- tree(High ~ .- Sales, Carseats)
summary(tree_carseats)

# plot just the branches, without texts
plot(tree_carseats)

# fill the branches with texts
text(tree_carseats, pretty = 0)

# evaluate tree performance
set.seed(2)
train <- sample(1:nrow(Carseats), nrow(df) / 2)
# df_train <- df[train, ]
df_test <- Carseats[-train, ]
y_test <- Carseats$High[-train]
# decision tree
dtree <- tree(High ~ . - Sales, data = Carseats, subset = train)
pred <- predict(dtree, newdata = df_test, type = "class")
table(pred, y_test)

# see if pruning helps
set.seed(3)

# pruning based on misclassification rates instead of deviance
cv_tree <- cv.tree(dtree, FUN = prune.misclass)

# note that dev is the cross-validation error;
# `k` corresponds to the hyperparameter `alpha` in the text
# relative min is seen at size = 8
par(mfrow = c(1, 2))
plot(cv_tree$size, cv_tree$dev, type = "b")
plot(cv_tree$k, cv_tree$dev, type = "b")
par(mfrow = c(1, 1))

# pruned tree, where size = 8
dtree_prune <- prune.misclass(dtree, best = 8)
plot(dtree_prune)
text(dtree_prune, pretty = 0)

# check the pruned tree on the test set
new_pred <- predict(dtree_prune, newdata = df_test, type = "class")
table(y_test, new_pred)
# (89 + 62) / 200 

#### Regression Tree ####
# library(MASS)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
tree_boston <- tree(medv ~ ., data = Boston, subset = train)
summary(tree_boston)
plot(tree_boston)
text(tree_boston, pretty = 0)

# tree pruning
# by default based on deviance (RSS?)
cv_boston <- cv.tree(tree_boston)

# original tree is chosen, nodes = 7
plot(cv_boston$size, cv_boston$dev, type = "b")

# if we really want to prune the tree
tree_boston_prune <- prune.tree(tree_boston, best = 5)
plot(tree_boston_prune)
text(tree_boston_prune, pretty = 0)

# prediction; using the original tree following cross-validation
yhat <- predict(tree_boston, newdata = Boston[-train, ])
boston_test <- Boston$medv[-train]
plot(yhat, boston_test)
abline(0, 1)
sqrt(mean((yhat - boston_test) ** 2))

#### Random Forest ####
# first try bagging (i.e. m = p, use all predictors for each tree)
bag_boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13,
                           importance = TRUE)
bag_boston
yhat_bag <- predict(bag_boston, newdata = Boston[-train, ])
sqrt(mean((boston_test - yhat_bag) ** 2))

# random forest

# The function importance(randomForest) outputs two things:
# (1) the mean decrease in accuracy on oob samples if the variable is excluded 
#     from the model (so the larger the %, the more important the variable)
# (2) the total decrease in node impurity that results from splits over that variable,
#     averaged over all trees
# Just use varImpPlot(randomForest) for a shortcut in the results
rf_boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 6,
                          importance = TRUE)
yhat_rf <- predict(rf_boston, newdata = Boston[-train, ])
sqrt(mean((boston_test - yhat_rf) ** 2))
importance(rf_boston) 
varImpPlot(rf_boston)

#### Boosting ####
set.seed(1)
# "gaussian" for regression, "bernoulli" for binary classification
boost_boston <- gbm(medv ~ ., data = Boston[train, ], distribution = "gaussian", 
                    n.trees = 5000, interaction.depth = 4)
summary(boost_boston)

# (?) partial dependence plot
par(mfrow = c(1, 2))
plot(boost_boston, i = "rm") # after "integrating out" other variables => positive relationship between medv and rm
plot(boost_boston, i = "lstat")  # after "integrating out" other variables => negatie relationship between medv and lstat

yhat_boost <- predict(boost_boston, newdata = Boston[-train, ], n.trees = 5000)
sqrt(mean((yhat_boost - boston_test) ** 2)) # even lower test RMSE than rf and bagging
