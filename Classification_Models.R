library(plyr)
library(dplyr)
library(ggplot2)
termCrosssell <- read.csv(file="bank.csv", header = T, sep = ';')

# split to train/validation data sets ------------------------------------------
# option 1:
sample.ind <- sample(3, 
                     nrow(termCrosssell),
                     replace = T,
                     prob = c(0.6,0.3, 0.1))
cross.sell.train <- termCrosssell[sample.ind==1,]
cross.sell.val <- termCrosssell[sample.ind==2,]
cross.sell.test <- termCrosssell[sample.ind==3,]

# option 2:
#library(caret)
#set.seed(1234)
#splitIndex <- createDataPartition(termCrosssell$y, p = .75, list = FALSE, times = 1)
#trainDF <- termCrosssell[splitIndex,]
#testDF  <- termCrosssell[-splitIndex,]

# create model formula with colnames

varNames <- names(cross.sell.train)
# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("y")]
# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")
# Add response variable and convert to a formula object
model_formula <- as.formula(paste("y", varNames1, sep = " ~ "))

# Random Forest ----------------------------------------------------------------

#' Now, in many implementations C4.5 is used instead of CART. The main reason is to avoid expensive computation (CART has more rigorous statistical approaches, which require more computation), the results seems to be similar, the resulted trees are often smaller (since CART is binary and C4.5 not). 
#' missing values are handled in CART (surrogate splits)
#' "Although surrogate splitting works well for trees, the method may not be well suited for forests. Speed is one issue. Finding a surrogate split is computationally intensive and may become infeasible when growing a large number of trees, especially for fully saturated trees used by forests. Further, surrogate splits may not even be meaningful in a forest paradigm. RF randomly selects variables when splitting a node and, as such, variables within a node may be uncorrelated, and a reasonable surrogate split may not exist. Another concern is that surrogate splitting alters the interpretation of a variable, which affects measures such as [Variable Importance]. For these reasons, a different strategy is required for RF."
#' Random forests has two ways of replacing missing values. The first way is fast. If the mth variable is not categorical, the method computes the median of all values of this variable in class j, then it uses this value to replace all missing values of the mth variable in class j. If the mth variable is categorical, the replacement is the most frequent non-missing value in class j. These replacement values are called fills.
#' The second way of replacing missing values is computationally more expensive but has given better performance than the first, even with large amounts of missing data. It replaces missing values only in the training set. It begins by doing a rough and inaccurate filling in of the missing values. Then it does a forest run and computes proximities.

#' If x(m,n) is a missing continuous value, estimate its fill as an average over the non-missing values of the mth variables weighted by the proximities between the nth case and the non-missing value case. If it is a missing categorical variable, replace it by the most frequent non-missing value where frequency is weighted by proximity.
library(randomForest)

#' The main difference between Random Forests and boosting (such as xgboost) is
#' that in RF trees are independent and in boosting, the tree N+1 focus its learning on the loss (what has not been well modeled by the tree N).
library(e1071)
library(caret)

#' if missing value exists, impute them first
data(iris)
iris.na <- iris
set.seed(111)
## artificially drop some data values.
for (i in 1:4) iris.na[sample(150, sample(20)), i] <- NA
set.seed(222)
iris.imputed <- rfImpute(Species ~ ., iris.na)
set.seed(333)
iris.rf <- randomForest(Species ~ ., iris.imputed)
print(iris.rf)
# build RF
# If response var is a factor then classification. Otherwise Regression
fit.rf <- randomForest(model_formula, cross.sell.train, ntree=500, 
                       mtry=5, # sample 5 variables to choose from in each split
                       importance=T, do.trace = 100) # printed running output every 100 trees
# The default mtry is sqrt(p) for classification

#' For classification problems where the class frequencies are extremely 
#' unbalanced (e.g., 99% class 1 and 1% class 2), it may be necessary to
#' change the prediction rule to other than majority votes.
#' For example, in a two-class problem with 99% class 1 and 1% class 2, one may
#' want to predict the 1% of the observations with largest class 2 probabilities
#' as class 2, and use the smallest of those probabilities as threshold
#' for prediction of test data (i.e., use the type=’prob’ argument in the 
#' predict method and threshold the second column of the output).
#' eg:
#' prop.table(table(cross.sell.train$y)) # 11%
#' rf.preds.prob <- predict(fit.rf, cross.sell.train, type = 'prob')
#' threshold_p <- quantile(rf.preds.prob[,2], 0.89)
#' rf.preds.prob <- predict(fit.rf, cross.sell.test, type = 'prob')
#' pred <- ifelse(rf.preds.prob[,2] >= threshold_p, 'yes', 'no')

plot(fit.rf)

# Variable Importance Plot
varImpPlot(fit.rf, sort = T, main="Variable Importance", n.var=5)

# Variable Importance Table
var.imp <- data.frame(importance(fit.rf, type=2)) 
var.imp$Variables <- row.names(var.imp)
var.imp <- var.imp %>% 
  arrange(desc(MeanDecreaseGini))

# Partial Dependence Plot
# Say the varible(Var1) has value A, B, then Partial Dependence is the mean response when 
# Var1 = A and mean response when Var1 = B.
# https://cran.r-project.org/web/packages/randomForest/randomForest.pdf
# It's the shape of that trend that is "important". You may interpret the relative range of these plots from different predictor variables, but not the absolute range.
partialPlot(fit.rf, cross.sell.val, duration, which.class="no")
# This states that when duration is smaller, the more is more sure about which class to predict a point to

## Looping over variables ranked by importance:
imp <- importance(fit.rf, type=2)
impvar <- rownames(imp)[order(imp, decreasing=TRUE)]
op <- par(mfrow=c(2, 3))
for (i in 1:6) {
  partialPlot(fit.rf, cross.sell.val, impvar[i], "no", xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]))
}
par(op)

# Predicting response variable in validation data
rf.preds <- predict(fit.rf, cross.sell.val, type = 'class')
rf.preds.prob <- predict(fit.rf, cross.sell.val, type = 'prob')

# Confusion Matrix
confusionMatrix(data=rf.preds,
                reference=cross.sell.val$y,
                positive='yes')

# Tune hyperparameter with Grid Search -----------------------------------------

tuned.rf <- tune(randomForest, train.x = model_formula,
                 data = cross.sell.train,
                 validation.x = cross.sell.val)

best.model <- tuned.rf$best.model
predictions <- predict(best.model, cross.sell.test)
table.random.forest <- table(cross.sell.test$y, predictions)
table.random.forest

# BOOSTED TREE -----------------------------------------------------------------

library(adabag)
fit.adaboost <- boosting(model_formula, data=cross.sell.train, mfinal=50)

adaboost.preds <- predict.boosting(fit.adaboost, newdata=cross.sell.val)
adaboost.preds.prob <- adaboost.preds$prob[,2]

sum(adaboost.preds$class == cross.sell.val$y)/nrow(cross.sell.val)

# Bagging CART -----------------------------------------------------------------

#' TREE BAGGING 
#' This bootstrapping procedure leads to better model performance because it decreases the variance of the model, without increasing the bias. This means that while the predictions of a single tree are highly sensitive to noise in its training set, the average of many trees is not, as long as the trees are not correlated. Simply training many trees on a single training set would give strongly correlated trees (or even the same tree many times, if the training algorithm is deterministic); bootstrap sampling is a way of de-correlating the trees by showing them different training sets.
#' For tree bagging, boostrap sampling only exist in data points, but in RF we boostrap sample
#' both observations and features
library(ipred)
# fit model
fit.bagginng.tree <- bagging(model_formula, data=cross.sell.train,
                             control=rpart.control(minsplit=5))
# summarize the fit
summary(fit.bagginng.tree)
# make predictions
predictions <- predict(fit.bagginng.tree, cross.sell.val)
# summarize accuracy
mse <- mean((cross.sell.train$y - predictions)^2)
print(mse)

# TREE -------------------------------------------------------------------------

library(rpart)
# loss matrix
lmat <- matrix(c(0,1,2,
                 1,0,100,
                 2,100,0), ncol = 3)
lmat

fit.tree <- rpart(model_formula, method="class", data=cross.sell.train,
                  control = rpart.control(xval = 20)
                  # ,parms = list(loss = lmat)
                  )
# method: 'class' for classification tree,
# method: 'anova' for regression tree
plotcp(fit.tree) # visualize cross-validation results 
printcp(fit.tree)
#' rel error is the error computed on the training sample comparig with root node error.
#' eg. root node error = abc, when nsplit = n, rel_error = 0.8, then for the tree with n split
#' the error on training data is 0.8*abc
#' The same works for xerror, which is the cross validation error comparing with 
#' root node error (10 fold by default, can be customized with rpart.control())



# plot tree 
par(mar=c(5,1,3,2)+0.1)
plot(fit.tree, uniform=TRUE, margin=0.2,
     main="Classification Tree")
text(fit.tree, use.n=TRUE, all=TRUE, cex=.8, pretty = 2)
#Prune back the tree to avoid overfitting the data. Typically, you will want to select a tree size that minimizes the cross-validated error, the xerror column printed by printcp( ).

#Specifically, use printcp( ) to examine the cross-validated error results, select the complexity parameter associated with minimum error, and place it into the prune( ) function.
# xerror is by how many percent the error decreased compared with the baseline error(Root node error)

# for each cp value the best tree is fitted and generate the error and xerror. Thus we select that cp with the lowest xerror to prune the tree to.

# The complexity parameter (cp) is used to control the size of the decision tree and to select the optimal tree size. If the cost of adding another variable to the decision tree from the current node is above the value of cp, then tree building does not continue. 

# prune the tree 
pfit <- prune(fit.tree, cp= fit.tree$cptable[which.min(fit.tree$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

# prediction output
tree.preds <- predict(fit.tree, cross.sell.val, type = "class")
tree.preds.prob <- predict(fit.tree, cross.sell.val, type="prob")

sum(tree.preds == cross.sell.val$y)/nrow(cross.sell.val)

# for tree prunning, we can use CV to compare the performance of various tree sizes
library(tree)
fit.tree2 <- tree(model_formula, data=cross.sell.train)
cv_tree <- cv.tree(fit.tree2)
names(cv_tree)
plot(cv_tree$size, cv_tree$dev, 
     type='b', xlab='tree size', ylab='mse', main='dev vs. size')
# based on the plot size 8 is a good tree size
cv_tree$size[which.min(cv_tree$dev)]

pruned_tree <- prune.tree(fit.tree2, best = 8)

# ctree using package party ----------------------------------------------------

# Condition Decision Trees are created using statistical tests to select split points on attributes rather than a loss function.
library(party)
ctree.fit <- ctree(model_formula, data=cross.sell.train, 
                controls = ctree_control(maxsurrogate = 3)) 
plot(ctree.fit)

# XGBoost (Extreme Gradient Boosting) ------------------------------------------

#' XGBoost has an in-built routine to handle missing values.
#' XGBoost implements parallel processing and is blazingly faster as compared to GBM.
#' Standard GBM implementation has no regularization like XGBoost, therefore it also helps to reduce overfitting
#' A GBM would stop splitting a node when it encounters a negative loss in the split. Thus it is more of a greedy algorithm.
#' XGBoost on the other hand make splits upto the max_depth specified and then start pruning the tree backwards and remove splits beyond which there is no positive gain. sometimes a split of negative loss say -2 may be followed by a split of positive loss +10. GBM would stop as it encounters -2. But XGBoost will go deeper and it will see a combined effect of +8 of the split and keep both.
#' 
#' XGBoost allows user to run a cross-validation at each iteration of the boosting process and thus it is easy to get the exact optimum number of boosting iterations in a single run. This is unlike GBM where we have to run a grid-search and only a limited values can be tested.

#' XGBoost implements boosting trees or linear boosting. The only difference with previous command is booster = "gblinear" parameter. when the link between predictors and outcome
#' is non linear, boosted tree based xgboost is better, otherwise linear boosting is better.
#'  Because there is no silver bullet, we advise you to check both algorithms with your own datasets to have an idea of what to use.
#'  
#' 
#' TREE BASED XGBOOST
#' 
#' 
#' A tree always seperates only two classes. Hence, if you have 3 classes, xgb 
#' grows 3 trees per boosting round (class 1 vs rest, class 2 vs rest & class 3 vs rest).

require(xgboost)
library(data.table)
require(Matrix)
library(DiagrammeR)

#' https://cran.r-project.org/web/packages/xgboost/vignettes/discoverYourData.html

df <- data.table(cross.sell.train, keep.rownames = F)
str(df)
#' xgboost only support numerical matrix input, use 'data.matrix' to transform the data

#' sparse.model.matrix  transforms all categorical features but column Improved to binary values. 
#' The -1 is here to remove the first column which is full of 1 (this column is 
#' generated by the conversion). For more information, you can type
#'  ?sparse.model.matrix in the console.
sparse_matrix <- sparse.model.matrix(y ~ . -1, data = df)
#' In this way a categorical var of n levels is coded into n-1 dummy variables
head(sparse_matrix)

#' Create the output numeric vector
output_vector <- df$y == 'yes'

df_test <- data.table(cross.sell.test, keep.rownames = F)
sparse_matrix_test <- sparse.model.matrix(y~.-1, data = df_test)
output_vector_test <- df_test$y == 'yes'

#' train the model
set.seed(123)
bst_fit <- xgboost(data = sparse_matrix, label = output_vector, 
                   max.depth = 4, eta = 1, nthread = 2, nround = 20, 
                   objective = "binary:logistic", verbose=1)

#' use cv
bst_cv_fit <- xgb.cv(data = sparse_matrix, label = output_vector, 
                     nfold=10, eval.metric = "error", eval.metric = "logloss",
                     max.depth = 4, eta = 1, nthread = 2, nround = 20, 
                     objective = "binary:logistic", verbose=1)

#' Measure learning progress with xgb.train
#' Both xgboost (simple) and xgb.train (advanced) functions train models.
#' One of the special feature of xgb.train is the capacity to follow the progress of the learning after each round. Because of the way boosting works, there is a time when having too many rounds lead to an overfitting. You can see this feature as a cousin of cross-validation method. The following techniques will help you to avoid overfitting or optimizing the learning time in stopping it as soon as possible.

#' xgb.train() obly accept xgb.DMatrix object
dtrain <- xgb.DMatrix(data =sparse_matrix, label=output_vector)
dtest <- xgb.DMatrix(data = sparse_matrix_test, label=output_vector_test)

watchlist <- list(train=dtrain, test=dtest)
bst <- xgb.train(data=dtrain, 
                 max.depth = 4, eta = 1, nthread = 2, nround = 20, 
                 watchlist=watchlist, 
                 # specify what metric you want ot check
                 eval.metric = "error", eval.metric = "logloss", 
                 objective = "binary:logistic")

#'             parameters to tune: 
#' 
#' https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/
#' eta(dft 0.3) learning rate. usually end up witj 0.01-0.2
#' min_child_weight [default=1] Used to control over-fitting. too high values can lead to under-fitting hence, it should be tuned using CV.
#' max_depth [default=6] Used to control over-fitting. Should be tuned using CV.Typical values: 3-10
#' gamma [default=0] Gamma specifies the minimum loss reduction required to make a split.
#' max_delta_step [default=0] Usually this parameter is not needed, but it might help in logistic regression when class is extremely imbalanced.
#' subsample [default=1] Denotes the fraction of observations to be randomly samples for each tree.
#' colsample_bytree [default=1] Denotes the fraction of columns to be randomly samples for each tree
#' lambda [default=1]  L2 regularization term on weights (analogous to Ridge regression)
#' alpha [default=0] L1 regularization term on weight (analogous to Lasso regression)
#' scale_pos_weight [default=1] A value greater than 0 should be used in case of high class imbalance as it helps in faster convergence.
#' 
#' Define the optimization objective the metric to be calculated at each step:
#' objective [default=reg:linear] can be binary:logistic, multi:softmax, multi:softprob
#' eval_metric.  can be rmse , mae, logloss, error, merror, mlogloss, auc

#' tune parameter
#' Step 1: Fix learning rate and number of estimators for tuning tree-based parameters
#' # the output is not model but a data frame of eval metrics
bst_cv_fit <- xgb.cv(data = sparse_matrix, label = output_vector, 
                     nfold=5, eval.metric = "error", eval.metric = "logloss",
                     max.depth = 5, eta = 0.3, min_child_weight = 1,
                     nthread = 2, nround = 1000, gamma = 0,
                     subsample= 0.8, colsample_bytree = 0.8, scale_pos_weight = 1,
                     objective = "binary:logistic", verbose=1,
                     seed=27)

# grow all the way to 1000 iterates. increase the learning rate and re-run the command to get the reduced number of estimators.
bst_cv_fit <- xgb.cv(data = sparse_matrix, label = output_vector, 
                     nfold=10, eval.metric = "error", eval.metric = "logloss",
                     max.depth = 5, eta = 0.5, min_child_weight = 1,
                     nthread = 2, nround = 1000,  early_stopping_rounds= 8,
                     gamma = 0.005,
                     subsample= 0.8, colsample_bytree = 0.8, scale_pos_weight = 1,
                     objective = "binary:logistic", verbose=1,
                     seed=27)

# say the model stop at tree 200

#' Step 2: Tune max_depth and min_child_weight
#' To start with, let’s set wider ranges and then we will perform another iteration for smaller ranges.
#' grid search
xgb_grid_1 = expand.grid(
  nrounds = 200,
  max_depth = c(4, 6, 8, 10),
  min_child_weight= c(2, 4, 6, 8),
  gamma = 0.005
)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all", # save losses across all models
  classProbs = TRUE,  # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)


# train the model for each parameter combination in the grid,
#   using CV to evaluate
set.seed(27)
xgb_train_1 = train(
  x = sparse_matrix, y = output_vector,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree",
  metric='logloss'
  
)

# evaluate performance
probsTest <- predict(xgb_train_1, test, type = "prob")
threshold <- 0.5
pred      <- ifelse(probsTest[, "yes"] > threshold, "yes", "no") 
confusionMatrix(pred, test$response)

library(pROC)
probsTrain <- predict(xgb_train_1, train, type = "prob")
rocCurve   <- roc(response = train$response,
                  predictor = probsTrain[, "yes"],
                  levels = rev(levels(train$response)))
plot(rocCurve, print.thres = "best")

# This will take a very long time to run.
# say the final rst is 5 for max_depth and 5 for min_child_weight, then we do 
# another grid search with candidate as c(4,5,6)

# we get the optimum values as 4 for max_depth and 6 for min_child_weight. 
# then we do another grid search with 4 for max_depth and min_child_weight candidate as c(6,7,8)

# if the rst still choose 6 for min_child_weight then we know 6 is the optimal parametre

#' Step 3: Tune gamma
#' gamma: seq(0, 5, 0.1)
#' say 0 is the optimum one. 
#' Before proceeding, a good idea would be to re-calibrate the number of boosting 
#' rounds for the updated parameters. Use all the tuned parameters and still set 
#' nround = 1000 to find the number of trees given the current set of parameters

#' Step 4: Tune subsample and colsample_bytree
#' take values 0.6,0.7,0.8,0.9, with the otehr parameters the same as we just tuned
#' say .8 as the optimum value for both

#' Step 5: Tuning Regularization Parameters (controlling complexity)
#' alpha  [1e-5, 1e-2, 0.1, 1, 100]
#' say 0.01 chosen, then try [0, 0.001, 0.005, 0.01, 0.05]
#' finally choose 0.005

#' Step 6: Reducing Learning Rate
#' lower the learning rate and add more trees 5000 trees eta 0.01. other parameters remain
#' the optimal




#' 
#' LINEAR BOOSTING BASED XGBOOST
#' 
bst_linear <- xgb.train(data=dtrain, booster = "gblinear", 
                        max.depth = 4, eta = 1, nthread = 2, nround = 20, 
                        watchlist=watchlist, 
                        # specify what metric you want ot check
                        eval.metric = "error", eval.metric = "logloss",
                        objective = "binary:logistic")

#' log loss vs. error:
#' The use of log on the error provides extreme punishments for being both confident and wrong. 
#' 
#' 
##  save model to binary local file
# xgb.save(bst_linear, "xgboost_linear.model")
##  load back the saved model
# bst2 <- xgb.load("xgboost_linear.model")
# pred2 <- predict(bst2, dtest)

label <- getinfo(dtest, "label")
pred <- predict(bst_linear, dtest)
err <- as.numeric(sum(as.integer(pred > 0.5) != label))/length(label)
err

#' Feature Importance
#' feature importance is more accurate in boosting models than in RF. check
#' out this link for the reason: https://cran.r-project.org/web/packages/xgboost/vignettes/discoverYourData.html
importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst_fit)
head(importance)
#' The column Gain provide the information we are looking for.
#' Gain is the improvement in accuracy brought by a feature to the branches it is on. The idea is that before adding a new split on a feature X to the branch there was some wrongly classified elements, after adding the split on this feature, there are two new branches, and each of these branch is more accurate

#' to count the co-occurrences of a feature and a class of the classification.
#' Does a certain value of var 1 occur more in class A?
importanceRaw <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst_fit,
                                data = sparse_matrix, label = output_vector)
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]

#' new column Split. It is the split applied to the feature on a branch of one of the tree. 
#' Each split is present, therefore a feature can appear several times in this table. Here we can see the feature DURATION is used several times with different splits.
#' RealCover and RealCover %. In the first column it measures the number of observations in the dataset where the split is respected(<) and the label marked as 1. The second column is the percentage of the whole population that RealCover represents. eg, with duration < 628.5, 95 obs are marked as YES

xgb.plot.importance(importance_matrix = importance)

#' Let's check some Chi2 between each of these features and the label.
#' Higher Chi2 means better correlation.
#' if you see Warning message: Chi-squared approximation may be incorrect. That is 
#' due to chi-square approximation to the distribution of the test statistic relies on the counts being roughly normally distributed.  If many of the expected counts are very small, the approximation may be poor. set the simulate.p.value argument to TRUE; then you aren't reliant on the chi-square approximation to the distribution of the test statistic.

c2 <- chisq.test(df$duration, output_vector, simulate.p.value = TRUE)
print(c2)
# null hypo that they are not related is rejected
# Pearson correlation between duration and y is 1496.1

#' Perform the prediction with xgboost
pred <- predict(bst_fit, sparse_matrix_test) # pred outputs prob
prediction <- as.numeric(pred > 0.5) # class

#' error rate
err <- mean(as.numeric(pred > 0.5) != output_vector_test)
print(paste("test-error=", err))



# GBM (Stochastic Gradient Boosting, AKA Gradient Boosted Machine) -------------

#' https://www.analyticsvidhya.com/blog/2016/02/complete-guide-parameter-tuning-gradient-boosting-gbm-python/
library(caret)
#' http://amunategui.github.io/binary-outcome-modeling/

#' gbm supports both regression and classification. As this is a binary classification, we need to force gbm into using the classification mode. We do this by changing the outcome variable to a factor

#' the trainControl function allows you to control the resampling of your data. This will split the training data set internally and do it’s own train/test runs to figure out the best settings for your model. In this case, we’re going to cross-validate the data 5 times before settling on the best tuning parameters (for gbm it is trees, shrinkage, and interaction depth). 

objControl <- trainControl(method='cv', number=5, returnResamp='none', 
                           summaryFunction = twoClassSummary, classProbs = TRUE)

#' twoClassSummary computes sensitivity, specificity and the area under the ROC curve. mnLogLoss computes the minus log-likelihood of the multinomial distribution (without the constant term)
#' multiClassSummary computes some overall measures of for performance (e.g. overall accuracy and the Kappa statistic) and several averages of statistics calculated from "one-versus-all" configurations. For example, if there are three classes, three sets of sensitivity values are determined and the average is reported with the name ("Mean_Sensitivity"). 
#' 
#' Because this is a classification model, we’re requesting that our metrics use ROC instead of the default RMSE
GBM_Model <- train(x=cross.sell.train[, 1:16], y=cross.sell.train[, 17], 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC", # specify what metrics to optimize
                  preProc = c("center", "scale"))
#' Gives variable influence and plot the influence as bar plot
summary(GBM_Model)

#' find out what tuning parameters we settle on 
GBM_Model
-------------------------
#' Stochastic Gradient Boosting (gbm) adjust learning rate and and trees

# Max shrinkage for gbm
nl = nrow(cross.sell.train) # 4
max(0.01, 0.1*min(1, nl/10000)) # 0.02691
# Max Value for interaction.depth
floor(sqrt(NCOL(cross.sell.train)))

gbmGrid <- expand.grid(interaction.depth =  c(1,2,3,4),
                        n.trees = (0:20)*50,
                        shrinkage = seq(.0005, .03,.0005),
                       n.minobsinnode = 100)
#' run model
GBM_Model <- train(x=cross.sell.train[, 1:16], y=cross.sell.train[, 17], 
                   method='gbm', trControl=objControl, tuneGrid = gbmGrid, 
                   train.fraction = 0.5, verbose=F)
----------------------
#' evaluate the GBM model
#' #' type='raw' gives prediction as class, type='prob' as probability
predictions <- predict(object=GBM_Model, cross.sell.test[,1:16], type='raw')
head(predictions)

#' using the caret postResample function to get an accuracy score
postResample(pred=predictions, obs=as.factor(cross.sell.test$y))

#' look at the predicted prob and AUC
library(pROC)
predictions <- predict(object=GBM_Model, cross.sell.test[,1:16], type='prob')
head(predictions)
auc <- roc(ifelse(cross.sell.test[,'y']=="yes", 1, 0), predictions[[2]])
print(auc$auc)

#' multi class GBM
data(iris)
fitControl <- trainControl(method="repeatedcv",
                           number=5,
                           repeats=1,
                           verboseIter=TRUE)
set.seed(825)
gbmFit <- train(Species ~ ., data=iris,
                method="gbm", distribution='multinomial',
                trControl=fitControl,
                verbose=FALSE)
gbmFit
test <- iris[sample(120,50),]
predictions <- predict(object=gbmFit, iris[sample(120,50),], type='raw')
head(predictions)
postResample(pred=predictions, obs=as.factor(test$Species))
confusionMatrix(predictions, test$Species)

#' multi-class roc curve: 
#' http://stats.stackexchange.com/questions/71700/how-to-draw-roc-curve-with-three-response-variable/110550#110550

# Ridge Regression (Logistic Regression) ---------------------------------------

# Refer to Regression_Models.R

# Cross-Validation -------------------------------------------------------------

require(caret)
k_folds <- 10
cv_splits <- createFolds(termCrosssell$y, k = k_folds, list = TRUE, returnTrain = FALSE)
str(cv_splits) # list of length k_folds

results <- list()

for (i in 1:k_folds){

  trainingset <- termCrosssell[cv_splits[[i]],]
  testset <- termCrosssell[-cv_splits[[i]],]
  
  # run a random forest model
  mymodel <- randomForest(trainingset$y ~ ., data = trainingset, ntree = 100)
  
  # remove response column 1, Sepal.Length
  pred <- data.frame(pred=predict(mymodel, testset %>% select(-y)))
  fact <- data.frame(fact=testset$y)
  pred_fact_pair <- cbind(fact, pred)
  
  results[[i]] <- pred_fact_pair
}

accuracy <- ldply(results, function(x){sum(x$pred != x$fact)/nrow(x)})

# Confusion Matrix gives sens, spec and pred*fact matrix -----------------------

confusionMatrix(data=rf.preds,
                reference=cross.sell.val$y,
                positive='yes')

# Classification performance: precision, recall, sensitivity, ROC, AUC ---------

library(ROCR)
#fact <- as.numeric(results[[1]]$fact)
#predictions <- as.numeric(results[[1]]$pred)
fact <- factor(cross.sell.val$y, levels=c('no', 'yes'), ordered=T)
predictions <- rf.preds.prob[, 2]

# Ideally, labels should be supplied as ordered factor(s), the lower level 
# corresponding to the negative class, the upper level to the positive class.
# in this case 1('no') is negative, 2('yes') is positive
pred <- prediction(predictions, fact)

# Recall-Precision curve             
RP.perf <- performance(pred, measure="prec", x.measure="rec")
plot(RP.perf, main = 'Recall-Precision curve')
# recall        TP/(TP+FN)    how many % of real Pos are predicted as Pos

# precision     TP/(TP+FP)    how many % of predicted Pos are really Pos

# ROC curve  (x: 1-specificty    y: sensitivity AKA Recall)
ROC.perf <- performance(pred, "tpr", "fpr") # fpr: FP/(TN+FP)
plot (ROC.perf, main = 'ROC Curve')
abline(a=0, b=1)

# ROC area under the curve
auc.perf <- performance(pred,"auc")
auc <- as.numeric(auc.perf@y.values)
auc

# this looks better
roc_df <- data.frame(x=ROC.perf@x.values[[1]],y=ROC.perf@y.values[[1]])
ggplot(roc_df,aes(x=x,y=y)) + 
  geom_path(size=1) + 
  geom_segment(aes(x=0,y=0,xend=1,yend=1),colour="black",linetype= 2) +
  geom_text(aes(x=1, y= 0, hjust=1, vjust=0, 
                label=paste(sep = "", "AUC = ",round(auc,3) )),colour="black",size=4) +
  scale_x_continuous(name= "False positive rate") +
  scale_y_continuous(name= "True positive rate") +
  labs(title = "ROC Curve")

# AUC with the restriction that FPR value <= 0.1
pauc.perf = performance(pred, measure = "auc", fpr.stop=0.1)
pauc.perf@y.values

# ROC curve with capped False Positive Rate ------------------------------------

pROC <- function(pred, fpr.stop){
  perf <- performance(pred,"tpr","fpr")
  for (iperf in seq_along(perf@x.values)){
    ind = which(perf@x.values[[iperf]] <= fpr.stop)
    perf@y.values[[iperf]] = perf@y.values[[iperf]][ind]
    perf@x.values[[iperf]] = perf@x.values[[iperf]][ind]
  }
  return(perf)
}

proc.perf <- pROC(pred, 0.3)
plot(proc.perf)
abline(a=0, b=1)

# Find the optimal cutoff point on ROC curve -----------------------------------

# we want TPR(recall) to be near 1 and FPR(1-specificity) to be near 0

# this function find the cutoff value that minimizes FPR^2 + (TPR-1)^2
opt.cut <- function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(ROC.perf, pred))

# give different cost of FP and FN to find optimal cutoff point
cost.perf = performance(pred, "cost", cost.fp = 2, cost.fn = 1)
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

# accuracy by cutoff values
acc.perf = performance(pred, measure = "acc")
plot(acc.perf, main = 'Accuracy by Cutoff')
prec.perf = performance(pred, measure = "prec")
plot(prec.perf, main = 'Precision by Cutoff')

ind = which.max(slot(acc.perf, "y.values")[[1]] )
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))

# Compare multiple Model predictions on one ROC curve plot ---------------------

preds <- cbind(p1 = rf.preds.prob[, 2], 
               p2 = tree.preds.prob[, 2])

pred.mat <- prediction(preds, 
                       labels = matrix(cross.sell.val$y, 
                                       nrow = length(cross.sell.val$y), ncol = 2))

perf.mat <- performance(pred.mat, "tpr", "fpr")
plot(perf.mat, colorize = TRUE)

# or 
n <- 2 # you have n models
colors <- c('red', 'blue') # 2 colors
for (i in 1:n) {
  plot(
    performance(
      prediction(preds[,i], cross.sell.val$y),
      "tpr","fpr"), 
    add=(i!= 1),
    col=colors[i],
    lwd=2, 
    main='compare model 1 and model 2'
    )
  legend("bottomright",
         c("RF","TREE"), # puts text in the legend 
         lwd=2, col=colors)
}

# Compare multiple Model predictions using mlbench package ---------------------

library(mlbench)
library(caret)

# load the dataset
data(PimaIndiansDiabetes)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3) # compare 3 models
# train the LVQ model
set.seed(7)
modelLvq <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", trControl=control)
# train the GBM model
set.seed(7)
modelGbm <- train(diabetes~., data=PimaIndiansDiabetes, method="gbm", trControl=control, verbose=FALSE)
# train the SVM model
set.seed(7)
modelSvm <- train(diabetes~., data=PimaIndiansDiabetes, method="svmRadial", trControl=control)
# collect resamples
results <- resamples(list(LVQ=modelLvq, GBM=modelGbm, SVM=modelSvm))
# results$values gives a data frame containing performnce of three models in each cv folder
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)

# If you needed to make strong claims about which algorithm was better, you could also use statistical hypothesis tests to statistically show that the differences in the results were significant.

# Parallel Process to accelerate training and grid search ----------------------

library(doParallel)
# Set up to do parallel processing   
registerDoParallel(4, cores=4)		# Registrer a parallel backend for train
getDoParWorkers()

# Train xgboost
xgb.grid <- expand.grid(nrounds = 500, #the maximum number of iterations
                        eta = c(0.01,0.1), # shrinkage
                        max_depth = c(2,6,10))

xgb.tune <-train(x=trainX,y=trainData$Class,
                 method="xgbTree",
                 metric="ROC",
                 trControl=ctrl,
                 tuneGrid=xgb.grid)


xgb.tune$bestTune
plot(xgb.tune)  		# Plot the performance of the training models
res <- xgb.tune$results
res